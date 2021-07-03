import torch
from torch._C import CudaBFloat16StorageBase
import torch.nn as nn

class SelfAttention(nn.Module):
    #Multi-head attention
    def __init__(self, embed_size, heads):
        #heads = how many parts we are splitting. Eg, if we have
        #embed size = 256, and heads = 8, we will then have 8 x 32
        super(SelfAttention, self).__init__()
        self.embed_size = embed_size #eg, 256
        self.heads = heads #eg, 8
        self.head_dim = embed_size // heads #eg, 32

        assert (self.head_dim * heads == embed_size), "Embed size needs to be div by heads"

        self.values = nn.Linear(self.head_dim, self.head_dim, bias = False)
        self.keys = nn.Linear(self.head_dim, self.head_dim, bias=False)
        self.queries = nn.Linear(self.head_dim, self.head_dim, bias=False)

        self.fc_out = nn.Linear(heads*self.head_dim, embed_size)

    def forward(self, values, keys, query, mask):
        #get number of training examples = batchsize
        N = query.shape[0]
        #sequence length, vary depending on encoder or decoder
        value_len, keys_len, query_len = values.shape[1], keys.shape[1], query.shape[1]

        #Split embedding into self.heads pieces
        #(N, seq_len, hidden_size) --> (N, seq_len, heads, head_dim) where heads*head_dim = hidden_size
        values = values.reshape(N, value_len, self.heads, self.head_dim)
        keys = keys.reshape(N, keys_len, self.heads, self.head_dim)
        queries = query.reshape(N, query_len, self.heads, self.head_dim)

        #run through fc layer returning same dimension
        values = self.values(values) 
        keys = self.keys(keys)
        queries = self.queries(queries)

        #Multiply query with keys
        energy = torch.einsum("nqhd, nkhd -> nhqk", [queries, keys]) #faster way of bmm. Equivalent of flatten -> permute -> bmm -> reshape
        #queries shape: (N, query_len, heads, heads_dim)
        #keys shape: (N, keys_len, heads, heads_dim)
        #energy shape : (N, heads, query_len, key_len) #query_len is the target sentence, keys is the source sentence. For each word in target (query), how much should you pay attention in our inputs (keys)

        if mask is not None:
            energy = energy.masked_fill(mask == 0, float("-1e20"))

        #scaled dot product attention according to formula, scaled to square root of dim
        #dim = 3 to get attention wrt the source sequence. Pay how much attention to each word in the key
        attention = torch.softmax(energy / (self.embed_size ** (1/2)), dim=3) 

        #Matrix multiply with values
        out = torch.einsum("nhql, nlhd -> nqhd", [attention, values]).reshape(N, query_len, self.heads*self.head_dim)
        #attn shape : (N, heads, query_len, key_len)
        #values shape : (N, value_len, heads, heads_dim)
        #out shape : (N, query_len, heads, head_dim)
        #after einsum, then flatten last 2 dims

        out = self.fc_out(out)
        return out

class TransformerBlock(nn.Module):
    #Transformer block
    def __init__(self, embed_size, heads, dropout, forward_expansion):
        super(TransformerBlock, self).__init__()
        self.attention = SelfAttention(embed_size, heads)
        self.norm1 = nn.LayerNorm(embed_size) #Takes average for every single example across all features. Batch norm takes average for the batch
        self.norm2 = nn.LayerNorm(embed_size)

        self.feed_forward = nn.Sequential(
            nn.Linear(embed_size, forward_expansion * embed_size),
            nn.ReLU(),
            nn.Linear(forward_expansion*embed_size, embed_size)
        )
        self.dropout = nn.Dropout(dropout)

    def forward(self,value, key, query, mask):
        attention = self.attention(value, key, query, mask)
        x = self.dropout(self.norm1(attention + query)) #skip connection then norm then dropout
        forward = self.feed_forward(x)
        out = self.dropout(self.norm2(forward + x))
        return out

class Encoder(nn.Module):
    def __init__(self, src_vocab_size, embed_size, num_layers, heads, device, forward_expansion, dropout,max_length):
        super(Encoder, self).__init__()
        self.embed_size = embed_size
        self.device = device
        self.word_embedding = nn.Embedding(src_vocab_size, embed_size)
        self.position_embedding = nn.Embedding(max_length, embed_size)

        self.layers = nn.ModuleList(
            [
                TransformerBlock(
                    embed_size,
                    heads,
                    dropout=dropout,
                    forward_expansion=forward_expansion
                )
            for _ in range(num_layers)]
        )
        self.dropout = nn.Dropout(dropout)

    def forward(self, x, mask):
        N, seq_length = x.shape #shape (N, seq_length)
        positions = torch.arange(0, seq_length).expand(N, seq_length).to(self.device) #shape = (N, seq_length)
        #position embedding allows the embedding to be aware of how words are structured in the sequence
        out = self.dropout(self.word_embedding(x) + self.position_embedding(positions)) #shape (seq_length, embed_size)

        for layer in self.layers:
            out = layer(out, out, out, mask) #key query value are all the same for encoder block
        return out

class DecoderBlock(nn.Module):
    def __init__(self, embed_size, heads, forward_expansion, dropout, device):
        super(DecoderBlock, self).__init__()
        self.attention = SelfAttention(embed_size, heads)
        self.norm = nn.LayerNorm(embed_size)
        self.transformer_block = TransformerBlock(
            embed_size, heads, dropout, forward_expansion
        )
        self.dropout = nn.Dropout(dropout)

    def forward(self, x, value, key, src_mask, trg_mask):
        attention = self.attention(x, x, x, trg_mask) #value and key and query are the same. Masked multi-head attention, trg mask is necessary
        query = self.dropout(self.norm(attention + x)) #with the skipped connection
        out = self.transformer_block(value, key, query, src_mask) #value and key from encoder block
        return out

class Decoder(nn.Module):
    def __init__(self, trg_vocab_size, embed_size, num_layers, heads, forward_expansion, dropout, device, max_length):
        super(Decoder, self).__init__()
        self.device = device
        self.word_embedding = nn.Embedding(trg_vocab_size, embed_size)
        self.position_embedding = nn.Embedding(max_length, embed_size)
        
        self.layers = nn.ModuleList(
            [DecoderBlock(embed_size, heads, forward_expansion, dropout, device) for _ in range(num_layers)]
        )

        self.fc_out = nn.Linear(embed_size, trg_vocab_size)
        self.dropout = nn.Dropout(dropout)

    def forward(self, x, enc_out, src_mask, trg_mask):
        N, seq_length = x.shape
        positions = torch.arange(0, seq_length).expand(N, seq_length).to(self.device)
        x = self.dropout((self.word_embedding(x) + self.position_embedding(positions)))

        for layer in self.layers:
            x = layer(x, enc_out, enc_out, src_mask, trg_mask ) #key and query are from encoder output

        out = self.fc_out(x)
        return out

class Transformer(nn.Module):
    def __init__(
        self, 
        src_vocab_size,
        trg_vocab_size,
        src_pad_idx,
        trg_pad_idx,
        embed_size = 256,
        num_layers = 6,
        forward_expansion = 4,
        heads = 8,
        dropout=0.5,
        device="cpu",
        max_length = 100
    ):
        super(Transformer, self).__init__()

        self.encoder = Encoder(
            src_vocab_size, embed_size, num_layers, heads, device, forward_expansion, dropout, max_length
        )
        self.decoder = Decoder(
            trg_vocab_size, embed_size, num_layers, heads, forward_expansion, dropout, device, max_length
        )

        self.src_pad_idx = src_pad_idx
        self.trg_pad_idx = trg_pad_idx
        self.device = device

    #Mask to ensure that attention is only on valid words
    def make_src_mask(self, src):
        #if the token is not a src_pad, set to 1. If it is, set to 0
        src_mask = (src != self.src_pad_idx).unsqueeze(1).unsqueeze(2) #make the shape (N, 1, 1, src_len)
        return src_mask.to(self.device)

    #Mask to ensure that attention is on previous words
    def make_trg_mask(self, trg):
        N, trg_len = trg.shape
        #create a triangular matrix
        trg_mask = torch.tril(torch.ones(trg_len, trg_len)).expand(N, 1, trg_len, trg_len)

        return trg_mask.to(self.device)

    def forward(self, src, trg):
        src_mask = self.make_src_mask(src)
        trg_mask = self.make_trg_mask (trg)
        enc_src = self.encoder(src, src_mask)
        out = self.decoder(trg, enc_src, src_mask, trg_mask)
        return out

if __name__ == "__main__":
    device = torch.device("cuda" if torch.cuda.is_available() else 'cpu')

    x = torch.tensor([[1,5,6,4,3,9,5,2,0], [1,8,7,3,4,5,6,7,2]]).to(device)

    trg = torch.tensor([[1,7,4,3,5,9,2,1,0], [1,5,6,2,4,5,7,6,2]]).to(device)

    src_pad_idx = 0
    trg_pad_idx = 0
    src_vocab_size = 10
    trg_vocab_size = 10
    model = Transformer(src_vocab_size, trg_vocab_size, src_pad_idx, trg_pad_idx).to(device)
    out = model(x, trg[:, :-1]) #target is shifted by 1 so it doesnt have <eos> token as we want it to predict. 9-1 = 8
    print(out[0,:,:]) #shape (batch, seq_length, trg_vocab_size) -> For each word in seq, output likelihood for each word in vocab