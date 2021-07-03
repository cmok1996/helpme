#Practical Machine Learning

library(caret)
library(kernlab)

data(spam) #4601 observations and 58 variables with labels spam or ham; columns represent frequency of word in the email
head(spam) #spam$type is a factor with levels 1-nonspam 2-spam

plot(density(spam$your[spam$type == "nonspam"]), col="blue", xlab = "Frequency of 'your", main = "")
lines(density(spam$your[spam$type == "spam"])) #most ham has 0 "your"
abline(v=0.5, col = "red") #draw cutoff line

prediction <- ifelse(spam$your>0.5, "spam", "nonspam")
confusion_matrix <- table(prediction, spam$type) / length(spam$type)
confusion_table <- confusion_matrix %>% as.data.frame() 
accuracy <- accuracy$Freq[1] + accuracy$Freq[4]

#Introductory example
inTrain <- createDataPartition(y=spam$type, p=0.75, list=F) #output indices
training <- spam[inTrain, ]
testing <- spam[-inTrain, ]
dim(training) #print the shape

set.seed(3323)
modelFit <- caret::train(type ~. , data=training, method = "glm") #y is type, input on all other variables; predict probability that it is a spam
modelFit
modelFit$finalModel #look at the coefficients
predictions <- predict(modelFit, newdata = testing) #output a factor vector with labels
confusionMatrix(predictions, testing$type)

#Data slicing
inTrain <- createDataPartition(y=spam$type, p=0.75, list=F) #output indices
training <- spam[inTrain, ]
testing <- spam[-inTrain, ]
dim(training) #print the shape

## k fold
nrow(spam) #4601
folds <- createFolds(y=spam$type, k=10, list=T, returnTrain = T) #create a list of train folds; returnTrain = F returns test set
sapply(folds, length) #check the length of each fold. Each fold contains 0.9*4601 training examples
folds[[1]][1:10] #first 10 elements of first fold

folds <- createResample(y=spam$type, times=10, list=T) #bootstrap, sample with replacement
folds[[1]][1:10] #might get repeated index

## timeseries
tme <- seq(1000) + 10
folds <- createTimeSlices(y=tme, initialWindow=20, horizon=10) #create a named list of length 2; blocked CV
names(folds) #train, test
folds$train[1] #1...20
folds$test[1] #21...30
folds$train[2] #2...21
folds$test[2] #22...31

##Plotting predictors
library(ISLR)
summary(Wage)
inTrain <- createDataPartition(y=Wage$wage, p=0.7, list=F) #output indices
training <- Wage[inTrain, ]
testing <- Wage[-inTrain, ]

featurePlot(x=training[, c("age", "education", "jobclass")], y=training$wage, plot="pairs")
ggplot(data = training) + geom_point(mapping = aes(x=age, y=wage, color=jobclass))

library(psych)
pairs.panels(training[, c("age", "education", "jobclass", "wage")], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

##Preprocess

inTrain <- createDataPartition(y=spam$type, p=0.7, list=F) #output indices
training <- spam[inTrain, ]
testing <- spam[-inTrain, ]
preObj <- preProcess(training[-58], method = c("center", "scale")) #normal standardization, scale all except the dependent variable; fit
trainCapAveS <- predict(preObj, training[,-58])$capitalAve #scale capitalAve column; transform
mean(trainCapAveS) #0
sd(trainCapAveS) #1

scaled <- function(x) {
  avg <- mean(x, na.rm=T)
  rescaled <- (x - avg) / sd(x, na.rm=T)  
  return(rescaled)
}
sapply(training[-58], scaled)

testCapAveS <- predict(preObj, testing[-58])$capitalAve #Normalize test set using train set
mean(testCapAveS) # non-zero, sd also non-1

set.seed(32343)
modelFit <- train(type ~., data=training, preProcess = c("center", "scale"), method="glm")

predObj <- preProcess(training[-58], method = c("BoxCox")) #make data more normal

## Impute data
### Make missing values
training$capAve <- training$capitalAve #create new variable
selectNA <- rbinom(dim(training)[1], size = 1, prob = 0.05) == 1 #binomial distribution takes 2 parameters, p and number of trials
training$capAve[selectNA] <- NA
sum(is.na(training$capAve)) #183 na values

### Impute and standardize
library(RANN)
preObj <- preProcess(training[, -58], method = c("medianImpute")) #impute and standardize
training$capAve <- predict(preObj, training[-58])$capAve
sum(is.na(training$capAve)) #0 na values

## standardize true values
preObj_truth <- preProcess(training[,-58], method = c("center", "scale"))
capAveTruth <- predict(preObj_truth, training[-58])$capitalAve
quantile((training$capAve - capAveTruth)[selectNA]) #imputed values are close to 0

#Feature selection

### One hot encoding
inTrain <- createDataPartition(y=Wage$wage, p=0.7, list=F) #output indices
training <- Wage[inTrain, ]
testing <- Wage[-inTrain, ]
dim(training) #print the shape

str(training$jobclass)
table(training$jobclass)
dummies <- dummyVars(wage ~ jobclass, data=training)
head(predict(dummies, newdata=training))

### removing zero covariates -> each feature should have some variation
nzv <- nearZeroVar(training, saveMetrics = T) #want to remove those nzv = T
nzv

### creating polynomial variables
library(splines)
bsBasis <- bs(training$age, df=3) #create df columns with each column representing the polynomial
bsBasis
lm1 <- lm(wage ~ bsBasis, data=training) #create linear regression
plot(training$age, training$wage, pch=19, cex=0.5) #true points, can see a curve relationship
points(training$age, predict(lm1, newdata=training), col="red") #predicted is not a linear line

#### Splines on the test set ->need to use train set
predict(bsBasis, age = testing$age)

### PCA
inTrain <- createDataPartition(y=spam$type, p=0.75, list=F) #output indices
training <- spam[inTrain, ]
testing <- spam[-inTrain, ]

M <- abs(cor(training[-58])) #caclulate correlation across all features except dependent variable
diag(M) <- 0 #set correlation with itself to 0 instead of 1
which(M>0.8, arr.ind = T) #output the row and col indices with pairwise correlation greater than 0.8
names(spam[c(32, 34, 40)])
plot(spam[,34], spam[,32])

smallSpam <- spam[,c(32,34)]
prComp <- prcomp(smallSpam)
plot(prComp$x[,1], prComp$x[,2])
prComp$rotation #look at loading scores of each pc

typeColor <- ((spam$type=="spam")*1 + 1)
prComp <- prcomp(log10(spam[,-58]+1))
plot(prComp$x[,1], prComp$x[,2], col=typeColor, xlab = "PC1", ylab = "PC2") #red colour represents spam, black is ham

preProc <- preProcess(log10(training[,-58] + 1), method="pca", pcaComp=2)
spamPC <- predict(preProc, log10(training[,-58]+1))
spamPC <- cbind(spamPC, type = training$type)
plot(spamPC[,1], spamPC[,2], col=typeColor)
modelFit <- train(type ~ ., method="glm", data=spamPC)
testPC <- predict(preProc,log10(testing[, -58]+1)) #use the parameters computed in training set
testPC <- cbind(testPC, testing$type)
confusionMatrix(testing$type, predict(modelFit, newdata= testPC))

modelFit <- train(training$type ~ ., method="glm", preProcess = "pca", data=training) #more comapct way
confusionMatrix(predict(modelFit, testing), testing$type) #(pred, actual)

#Fit linear model
inTrain <- createDataPartition(y=Wage$wage, p=0.7, list=F) #output indices
training <- Wage[inTrain, ]
testing <- Wage[-inTrain, ]
modFit <- train(wage ~ age + jobclass + education, method="lm", data=training) #jobclass and education are factor variables -> automatically perform one hot encoding
finMod <- modFit$finalModel
print(modFit)
plot(finMod, pch=19, cex=0.05) #diagnostic plots
qplot(finMod$fitted, finMod$residuals, color=race, data=training)
plot(finMod$residuals, pch=19)

#Trees
inTrain <- createDataPartition(y=iris$Species, p=0.7, list=F) #output indices
training <- iris[inTrain, ]
testing <- iris[-inTrain, ]
qplot(x=Sepal.Length, y=Sepal.Width, color = Species,data=training)
modFit <- train(Species ~ ., method = "rpart", data=training)
print(modFit$finalModel)
plot(modFit$finalModel, uniform=T, main="Classification Tree")

fancyRpartPlot(modFit$finalModel) #pretty dendogram
predict(modFit, newdata=testing)

#Random Forest
### 1. Bootstrap samples; 2. Bootstrap variables; 3.Vote
library(randomForest)
library(caret)
library(tidyverse)
modFit <- train(Species~., data = training, method = "rf", prox=T)
modFit
getTree(modFit$finalModel, k=2)
irisP <- classCenter(training[,c(3,4)], training$Species, modFit$finalModel$prox) %>% as.data.frame()
irisP$species <- rownames(irisP) #create column
p <- qplot(Petal.Width, Petal.Length, color=Species, data=training)
p + geom_point(aes(x=Petal.Width, y=Petal.Length, col=species), data=irisP, size=5, shape=4, stroke=2)
pred <- predict(modFit, testing)
testing$predicted_speicies <- predict(modFit, testing)
testing$predRight <- pred==testing$Species
table(pred, testing$Species)
confusionMatrix(predict(modFit, testing), testing$Species) #(pred, actual)
qplot(Petal.Width, Petal.Length, color=predRight, data=testing) #find the point that incorrectly label

inTrain <- createDataPartition(y=spam$type, p=0.75, list=F) #output indices
training <- spam[inTrain, ]
testing <- spam[-inTrain, ]
rfGrid <- expand.grid(mtry = c(10,20,30), #sqrt number of cols
                      min.node.size = c(5, 10, 20, 40),
                      splitrule = "gini")
mod_rf_50 <- train(type ~ . , data = training, method = "ranger",
                   num.trees = 50,
                   importance = "impurity",
                   tuneGrid = rfGrid,
                   trControl = trainControl("oob"))
print(mod_rf_50)
confusionMatrix(testing$type, predict(mod_rf_50, testing)) #94.5% accuracy
varImp(mod_rf_50) %>% plot(top = 10) #see importance; command only available if importance = not_none

var_importance <- mod_rf_50$finalModel$variable.importance %>%
  sort(decreasing = TRUE) %>% head(10) #ability to reduce impurity

#Boosting
## 1.Take lots of weak learners, 2.weigh them and add up (upweigh the misclassified points), 3.get stronger predictor
library(ISLR)

summary(Wage)
Wage <- subset(Wage, select = -c(logwage))
inTrain <- createDataPartition(y=Wage$wage, p=0.7, list=F) #output indices
training <- Wage[inTrain, ]
testing <- Wage[-inTrain, ]

ntrees <- seq(50,500, by = 50)
shrinkage <- 10^seq(-3,0, length = 10)
interaction.

modFit <- train(wage ~ ., method="gbm", data=training, 
                trControl = trainControl("cv", 5),
                tuneGrid = expand.grid(n.trees = ntrees, shrinkage = shrinkage,
                                       interaction.depth = 1, n.minobsinnode = 10), verbose = F)


rmse(testing$wage, predict(modFit, testing)) #33.32948
varImp(modFit$finalModel)

#SVM
inTrain <- createDataPartition(y=spam$type, p=0.75, list=F) #output indices
training <- spam[inTrain, ]
testing <- spam[-inTrain, ]

svm_gauss_grid <- expand.grid(sigma = c(1/16, 1/8, 1/4, 1, 2), #defines how far the influence of a single training example reaches. The higher the value, the closer the reach, decision boundary is less smooth (overfit)
                              C = c(0.25, 0.5, 1, 2))#inversely related to regularization. Smaller value leads to higher regularization

mod_svm_radial <- train(type ~ . , data = training, method = "svmRadial",
                        tuneGrid = svm_gauss_grid,
                        trControl = trainControl("cv", number = 5)) 

mod_svm_radial

mod_svm_radial %>%
  predict(test_data) %>%
  confusionMatrix(test_data$y)


#Regularization
inTrain <- createDataPartition(y=Wage$wage, p=0.7, list=F) #output indices
training <- Wage[inTrain, ]
testing <- Wage[-inTrain, ]


lambda <- 10^seq(-3, 0 , length = 20) #vector ranging from -3 to 0 of length 20
lambda

lasso <- train(
  wage ~.,data = training, method = "glmnet",
  trControl = trainControl("cv", number = 5),
  tuneGrid = expand.grid(alpha = 1, lambda = lambda), #parameters as defined by ?method
  preProcess = c("scale")
)

rmse <- function(pred, actual) {
  squared_error <- (pred-actual)^2
  root_mean_squared_error <- sqrt(mean(squared_error))
  return(root_mean_squared_error)
}

rmse(predict(lasso, newdata = testing), testing$wage) #rmse of 34.75

lasso_coefs <- coef(lasso$finalModel, lambda) %>%
  as.matrix %>% t %>% as_tibble %>%
  mutate(lambda = lambda)

lasso_coefs %>%
  pivot_longer(year:`health_ins2. No`, 
               names_to = "variable", values_to = "coef") %>%
  ggplot(aes(x = lambda, y = coef, group = variable, colour = variable)) +
  geom_line() + scale_x_log10() #plot coefficient value wrt to lambda
```

# print the coefficient
coef(lasso$finalModel, lasso$bestTune$lambda)


modFit <- train(wage ~ year + age + race + education, method="ridge", data=training,
                trControl = trainControl(method = 'cv', number = 5)) #lasso, ridge, relaxo
print(modFit$finalModel)

#Model Stacking
library(ISLR)

summary(Wage)
Wage <- subset(Wage, select = -c(logwage))
inBuild <- createDataPartition(y=Wage$wage, p=0.7, list=F) #output indices
testing <- Wage[-inBuild, ] #leave 30% to testing
buildData <- Wage[inBuild, ] 
inTrain <- createDataPartition(y=buildData$wage, p=0.7, list=F) #output indices
training <- buildData[inTrain, ]
validation <- buildData[-inTrain,]
dim(validation) #628,10
dim(training) #1474,10
dim(testing) #898,10

mod1 <- train(wage ~. , method="lm", data=training)
mod2 <- train(wage ~. , method = "rf", data=training, trControl = trainControl(method = "cv"),number=3)
pred1 <- predict(mod1, newdata = validation)
pred2 <- predict(mod2, validation)
validation <- cbind(validation, pred1, pred2)
qplot(pred1, pred2, color = wage, data=validation) #do not agree for higher wages

predDf <- data.frame(pred1, pred2, wage=validation$wage) %>% as_tibble()
combModFit <- train(wage ~. , method="gam", data = predDf) #generalized additive model
test_pred1 <- predict(mod1, newdata = testing)
test_pred2 <- predict(mod2, testing)
test_predDf <- data.frame(pred1 = test_pred1, pred2 = test_pred2, wage=testing$wage) %>% as_tibble()
test_comb_pred <- predict(combModFit, test_predDf)

rmse(testing$wage, test_pred1) #34.37
rmse(testing$wage, test_pred2) #35.71
rmse(testing$wage, test_comb_pred) #34.01

#Forecasting
library(quantmod)
from.dat <- as.Date("01/01/08", format = "%d/%m/%y")
to.dat <- as.Date("01/12/12", format = "%d/%m/%y")
getSymbols("GOOG", src = "yahoo", from=from.dat, to=to.dat)
mGoog <- to.monthly(GOOG) #convert to monthly
googOpen <- Op(mGoog) #just get opening information
ts1 <- ts(googOpen, frequency = 12, start = 2008) #set freq=12 because monthly
plot(ts1, xlab = "Years", ylab = "Google")
plot(decompose(ts1), xlab="Years")
ts1Train <- window(ts1, start=2008, end = 2012) #2012 jan
ts1Test <- window(ts1, start = 2012) #until Nov 2012


#Clustering with kmeans
inTrain <- createDataPartition(y=iris$Species, p=0.7, list=F) #output indices
training <- iris[inTrain, ]
testing <- iris[-inTrain, ]

t <- training[, -c(5,5)]
#scale
m <- apply(training_, 2, mean) #apply mean across column
s <- apply(training_, 2, sd)
training_ <- scale(training_ ,m,s) 

distance <- stats::dist(training_) #euclidean distance
print(distance[1:10], digits=3)

#Cluster dendogram with complete linkage
hc.c <- hclust(distance) #Each point is first its own cluster, then grouped together based on distance, until all points form 1 big cluster
plot(hc.c, hang=-1) #labels = df$col ->eg, company clustering; hang=-1 to align all the lines
hc.a <- hclust(distance, method="average") #complete linkage maximizes inter-cluster distance; single linkage minimizes intra_cluster distance; average is a compromise between complete and single
plot(hc.a)

##Membership
member.c <- cutree(hc.c, 3) #cut at 3rd level
member.a <- cutree(hc.a, 3)
  
table(member.c, member.a)

#kmeans

kmeans1 <- kmeans(subset(training, select = -c(Species)), centers=3)
training$clusters <- as.factor(kmeans1$cluster)
qplot(Petal.Width, Petal.Length, colour = clusters, data=training)
kmeans1$betweenss #average inter-cluster distance
kmeans1$withinss #vetor of within-cluster sum of squares
kmeans1$tot.withinss #total withinss
kmeans1$size #number of points per cluster
kmeans1$centers #Dataframe of all input variables with center values
table(kmeans1$cluster, training$Species)

## Build predictor for cluster
modFit <- train(clusters ~ ., data=subset(training, select=-c(Species)), method = "rpart")
table(predict(modFit, training), training$Species)
table(predict(modFit, testing), testing$Species)
