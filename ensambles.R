# Load libraries

library(caretEnsemble)
library(caret)
library(mlbench)

# Load the dataset
#data(Ionosphere)
DF <- read.csv("C:/Users/anton/Documents/Tesis/tomek_newData100AMD.csv")
dataset <- DF
#dataset <- dataset[,-1]
#dataset <- dataset[,-12]


for(i in 1:29)
  dataset[,i] <- as.numeric(dataset[,i])

dataset$GRUPO = as.factor(dataset$GRUPO)

# Example of Boosting Algorithms
control <- trainControl(method="repeatedcv", number=10, repeats=3)
seed <- 7
metric <- "Accuracy"
# C5.0
set.seed(seed)
fit.c50 <- train(GRUPO~., data=dataset, method="C5.0", metric=metric, trControl=control)
# Stochastic Gradient Boosting
set.seed(seed)
fit.gbm <- train(GRUPO~., data=dataset, method="gbm", metric=metric, trControl=control, verbose=FALSE)
# summarize results
boosting_results <- resamples(list(c5.0=fit.c50, gbm=fit.gbm))
summary(boosting_results)
dotplot(boosting_results)

# Example of Bagging algorithms
control <- trainControl(method="repeatedcv", number=10, repeats=3)
seed <- 7
metric <- "Accuracy"
# Bagged CART
set.seed(seed)
fit.treebag <- train(GRUPO~., data=dataset, method="treebag", metric=metric, trControl=control)
# Random Forest
set.seed(seed)
fit.rf <- train(GRUPO~., data=dataset, method="rf", metric=metric, trControl=control)
# summarize results
bagging_results <- resamples(list(treebag=fit.treebag, rf=fit.rf))
summary(bagging_results)
dotplot(bagging_results)

library(caretEnsemble)
# Example of Stacking algorithms
# create submodels
control <- trainControl(method="repeatedcv", number=10, repeats=3, savePredictions=TRUE, classProbs=TRUE)
algorithmList <- c('lda', 'rpart', 'glm', 'knn', 'svmRadial')
set.seed(7)
dataset$GRUPO<-make.names(dataset$GRUPO)
models <- caretList(GRUPO~ ., data=dataset, trControl=control, methodList=algorithmList)
results <- resamples(models)
summary(results) 
dotplot(results)

# correlation between results
modelCor(results)
splom(results)


library(diverse)
# stack using glm
#algorithmList <- c('lda', 'rpart', 'glm', 'knn', 'svmRadial',classProbs=TRUE)
#models <- caretList(GRUPO~ ., data=dataset, trControl=control, methodList=algorithmList)

set.seed(7)

stackControl <- trainControl(method="repeatedcv", number=10, repeats=3, savePredictions=TRUE, classProbs=TRUE)
set.seed(seed)
stack.glm <- caretStack(models, method="glm", metric="Accuracy", trControl=stackControl)
print(stack.glm)

stackControl <- trainControl(method="repeatedcv", number=10, repeats=3, savePredictions=TRUE, classProbs=TRUE,summaryFunction=twoClassSummary)
stack.glm2 <- caretStack(models, method="glm", metric="ROC", trControl=stackControl)
print(stack.glm2)


stackControl <- trainControl(method="repeatedcv", number=10, repeats=3, savePredictions=TRUE, classProbs=TRUE,summaryFunction=mnLogLoss)
stack.glm3 <- caretStack(models, method="glm", metric="logLoss", trControl=stackControl)
print(stack.glm3)


# stack using random forest
set.seed(seed)
stack.rf <- caretStack(models, method="rf", metric="Accuracy", trControl=stackControl)
print(stack.rf)

set.seed(seed)
stack.rf <- caretStack(models, method="rf", trControl=stackControl)
print(stack.rf)

# stack using random forest
set.seed(seed)
stack.svmRadial <- caretStack(models, method="svmRadial", metric="Accuracy", trControl=stackControl)
print(stack.svmRadial)

