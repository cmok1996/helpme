import torch
import torch.nn as nn
import torch.optim as optim
import spacy
from utils_translation import translate_sentence, bleu, save_checkpoint, load_checkpoint
from torch.utils.tensorboard import SummaryWriter
from torchtext.legacy.datasets import Multi30k
from torchtext.legacy.data import Field, BucketIterator
import random

#### Preprocessing steps required
# Tokenize sentence
# Lowercase, padding, any other preprocessing if required (stemming, lemmatizing, remove stopwords, etc)
# Build vocab (serialize tokens into numerical index)
# Get embedding (load weights from pre-trained when build_vocab)
# Batching train and validation dataset

spacy_ger = spacy.load('de_core_news_sm')
spacy_eng = spacy.load('en_core_web_sm')

def tokenizer_ger(text):
    #Hello my name is -> ['Hello', 'my', 'name', 'is'] 
    return [tok.text for tok in spacy_ger.tokenizer(text)]

def tokenizer_eng(text):
    #Hello my name is -> ['Hello', 'my', 'name', 'is'] 
    return [tok.text for tok in spacy_eng.tokenizer(text)]

german = Field(tokenize = tokenizer_ger, lower=True, init_token = '<sos>', eos_token = '<eos>')

english = Field(tokenize = tokenizer_eng, lower=True, init_token = '<sos>', eos_token = '<eos>')
#Hello my name is --> ['<sos>', 'hello', 'my', 'name', 'is', '<eos>']

#fields = {".de": ("src", german), ".en": ("trg", english)}
#train_data, validation_data, test_data = TabularDataset.splits(
#    path="./", train="train.csv",validation="valid.csv", test="test.csv", format="csv", fields=fields
#)

train_data, validation_data, test_data = Multi30k.splits(exts=('.de', '.en'),fields = (german, english))

print(train_data[0].__dict__.keys()) #src, trg
print(train_data[0].src) #['zwei', 'junge', 'weiße', 'männer', 'sind', 'im', 'freien', 'in', 'der', 'nähe', 'vieler', 'büsche', '.']
print(train_data[0].trg) #['two', 'young', ',', 'white', 'males', 'are', 'outside', 'near', 'many', 'bushes', '.']

german.build_vocab(train_data, max_size=10000, min_freq=2) #only words which occur at least two times are added into vocab
english.build_vocab(train_data, max_size = 10000, min_freq = 2)

print(english.vocab.stoi)

class Encoder(nn.Module):
    #Encoder is for german
    #input size = len(german vocab); num_layers = number of layers for lstm
    #send in entire german sentence
    def __init__(self, input_size, embedding_size, hidden_size, num_layers, p):
        super(Encoder, self).__init__()
        self.hidden_size = hidden_size
        self.num_layers = num_layers

        self.dropout = nn.Dropout(p)
        self.embedding = nn.Embedding(input_size, embedding_size)
        self.rnn = nn.LSTM(embedding_size, hidden_size, num_layers, bidirectional = True) #dropout only works if num_layers > 1

        #since we have bidirectional, rnn output will have both forward and backward
        #To avoid limiting information to only forward or backward, train a linear model to determine which is important
        self.fc_hidden = nn.Linear(hidden_size*2, hidden_size)
        self.fc_cell = nn.Linear(hidden_size*2, hidden_size)

    def forward(self, x):
        #x is the vector of german input. Tokenized + index-map to vocabulary
        #x shape : (seq_length, N)
        #if x shape is (N, seq_length), self.rnn (batch_first=True)

        embedding = self.dropout(self.embedding(x)) #shape : (seq_length, N, hidden_size) -- each word in seq_length has a mapping to a hidden_size space
        encoder_states, (hidden,cell) = self.rnn(embedding) #randomly initialized (hidden, cell) by default. size (seq_len, N, hidden_size*2) because bidrectional

        #hidden shape : (2, N, hidden_size) --> bidirectional
        hidden = self.fc_hidden(torch.cat((hidden[0:1], hidden[1:2]), dim=2)) #hidden state forward, hidden state backward
        #output shape : (1, N, hidden_size*2)
        cell = self.fc_cell(torch.cat((cell[0:1], cell[1:2]), dim=2)) #hidden state forward, hidden state backward
        
        #encoder states important for attention mechanism
        return encoder_states, hidden, cell #encoder_states include all timestamps while hidden and cell only the last timestamp
    

class Decoder(nn.Module):
    #Decoder is for english
    #input_size = output_size = len(english vocab)
    #going to output a certain vector which correspond to the probability of each word in the english vocab
    def __init__(self,input_size, embedding_size, hidden_size, output_size, num_layers, p):
        super(Decoder, self).__init__()
        self.hidden_size = hidden_size
        self.num_layers  = num_layers

        self.dropout = nn.Dropout(p)
        self.embedding = nn.Embedding(input_size, embedding_size)
        #concatenate encoder timestep (bidirectional) with target word embedding
        self.rnn = nn.LSTM(hidden_size*2 + embedding_size, hidden_size, num_layers) #dropout only works if num_layers>1
        
        #Take in hidden from our encoder, and also the hidden from previous step of decoder
        self.energy = nn.Linear(hidden_size*3, 1)
        self.softmax = nn.Softmax(dim=0)
        self.relu = nn.ReLU()
        
        self.fc = nn.Linear(hidden_size, output_size)

    def forward(self, x,encoder_states, hidden, cell):
        #hidden,cell is from decoder. encoder_states is from encoder
        #shape of x: (N) but we want (1, N) ---> Predict one word at a time. Eg, <sos> -> 'hi'
        
        x = x.unsqueeze(0)
        embedding = self.dropout(self.embedding(x)) #shape: (1, N, embedding_size)
        
        #Compute attention scores
        sequence_length = encoder_states.shape[0] #timestamp of encoder
        h_reshaped = hidden.repeat(sequence_length, 1,1) #(1, N, hidden_size) -> (seq_length, N, hidden_size)

        #Concat decoder previous hidden state (hidden_size) with encoder state (hidden_size*2)
        energy = self.relu(self.energy(torch.cat((h_reshaped, encoder_states), dim=2)))
        attention = self.softmax(energy) #shape (seq_length, N, 1); softmax normalize sum to 1 along seq_length

        #Element wise multiply attention weights with encoder states along the timestamp of encoder states (seq length) to return the new context vector with attention for each word
        #To do so, need to reshape
        attention = attention.permute(1,2,0) #(seq_length, N, 1) -> (N, 1, seq_length)
        encoder_states = encoder_states.permute(1,0,2) #(seq_length, N, hidden_size*2) -> (N, seq_length, hidden_size*2)

        context_vector = torch.bmm(attention, encoder_states).permute(1,0,2) #(N, 1, hidden_size*2) --> (1, N, hidden_size*2)
        
        rnn_input = torch.cat((context_vector, embedding), dim=2) #make dim2 shape : hidden_size*2 + embedding_size
        outputs, (hidden,cell) = self.rnn(rnn_input, (hidden,cell)) #shape : (1, N, hiden_size); initialized (hidden, cell) from encoder state
        predictions = self.fc(outputs) #shape : (1, N, length_of_vocab) --> get sent to loss function

        predictions = predictions.squeeze(0) #remove first dimension

        return predictions, hidden, cell 

class Seq2Seq(nn.Module):
    #German --> English translator
    def __init__(self, encoder, decoder):
        super(Seq2Seq, self).__init__()
        self.encoder = encoder
        self.decoder = decoder

    def forward(self, source, target, teacher_force_ratio = 0.5) :
        #source is german sentence, target is english sentence
        #Use teacher-forcing method at 0.5 rate. 50% of the time use true target, 50% of the time use predicted word as input for decoder
        batch_size = source.shape[1] #source shape : (seq_len, N)
        target_len = target.shape[0]
        target_vocab_size = len(english.vocab)

        #initialize storage of decoder LSTM output for each word of target sentence
        outputs = torch.zeros(target_len, batch_size, target_vocab_size).to(device)
        encoder_states, hidden, cell = self.encoder(source)
        
        #grab start token
        x = target[0]

        #decoder
        for t in range(1, target_len): #target[0] = <sos>
            #hidden and cell will be reused in the loop
            #encder state remains fixed for every iteration, but hidden and cell will be overwritten at every timestep
            output, hidden, cell = self.decoder(x, encoder_states, hidden, cell) #shape: (N, english_vocab_size)
            outputs[t] = output
            
            best_guess = output.argmax(1)
            
            #input for next loop. Either best_guess or target word
            x = target[t] if random.random() < teacher_force_ratio else best_guess

        return outputs

#### Training phase

# Training hyperparameters
num_epochs = 20
learning_rate = 0.001
batch_size = 1028

#Model hyperparameters
load_model = False
device = torch.device('cuda' if torch.cuda.is_available() else 'cpu')
input_size_encoder = len(german.vocab)
input_size_decoder = len(english.vocab)
output_size = len(english.vocab) #assert output_size = input_size_decoder
encoder_embedding_size = 300
decoder_embedding_size = 300
hidden_size = 1024
num_layers = 1 #for rnn, dropout will not work if num_layers = 1
enc_dropout = 0.5
dec_dropout = 0.5

#Tensorboard
writer = SummaryWriter(f"runs/loss_plot")
step = 0

train_iterator, valid_iterator, test_iterator = BucketIterator.splits(
    (train_data, validation_data, test_data),
    batch_size = batch_size,
    sort_within_batch = True,
    sort_key = lambda x : len(x.src), #prioritize to have examples of similar length in a batch to minimize padding to save on compute
    device = device
)

encoder_net = Encoder(input_size_encoder, encoder_embedding_size, hidden_size, num_layers, enc_dropout).to(device)
decoder_net = Decoder(input_size_decoder, decoder_embedding_size, hidden_size, output_size, num_layers, dec_dropout).to(device)
model = Seq2Seq(encoder_net, decoder_net).to(device)

pad_idx = english.vocab.stoi['<pad>']
criterion = nn.CrossEntropyLoss(ignore_index = pad_idx)
optimizer = optim.Adam([p for p in model.parameters() if p.requires_grad], lr=0.001)

if load_model:
    load_checkpoint(torch.load('my_checkpoint.pth.ptar'), model, optimizer)

sentence = "ein boot mit anderen männern wird von einem großen pferdegespann ans ufer gezogen."

#training
model.train()
train_losses = []
val_losses = []
for epoch in range(num_epochs):
    losses = []
    val_losses_ = []
    total=0
    print(f"Epoch [{epoch} / {num_epochs}]")

    checkpoint = {'state_dict' : model.state_dict(),
                'optimizer' : optimizer.state_dict()}
    save_checkpoint(checkpoint)

    
    model.eval()
    with torch.no_grad():
      translated_sentence = translate_sentence(model, sentence, german, english, device, max_length=50)
      print(f"Translated sentence \n {translated_sentence}")
      
    model.train()
    for batch_idx, batch in enumerate(train_iterator):
        model.zero_grad()
        inp_data = batch.src.to(device)
        target = batch.trg.to(device)

        output = model(inp_data, target) #shape (trg_len, batch_size, output_dim)

        #need to reshape to 2dim; flatten
        output = output[1:].reshape(-1, output.shape[2]) #0 is start token. Don't wait to send that to the criterion
        target = target[1:].reshape(-1)

       
        loss = criterion(output, target)

        loss.backward()

        #avoid exploding gradient problem
        torch.nn.utils.clip_grad_norm_(model.parameters(), max_norm = 1)
        optimizer.step()

        losses.append(loss.item())
        total += 1


        #update tensorboard
        writer.add_scalar("Training loss", loss, global_step = step)
        step += 1

    #Validation at end of every epoch
    total_val = 0
    for val_idx, validation_batch in enumerate(valid_iterator):
      total_val += 1
      val_inp_data = validation_batch.src.to(device)
      val_target = validation_batch.trg.to(device)
      
      with torch.no_grad():
        val_output = model(val_inp_data, val_target)
        val_output = val_output[1:].reshape(-1, val_output.shape[2]) #0 is start token. Don't wait to send that to the criterion
        val_target = val_target[1:].reshape(-1)

        val_loss = criterion(val_output, val_target)
        val_losses_.append(val_loss.item())

    epoch_loss = sum(losses)/total
    val_loss = sum(val_losses_) / total_val 
    train_losses.append(epoch_loss)
    val_losses.append(val_loss)
    
    print(f"Epoch {epoch} Training Loss : {epoch_loss} ; Validation Loss : {val_loss}")



