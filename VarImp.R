# ensure results are repeatable
set.seed(7)
# load the library
library(mlbench)
library(caret)
library(pROC)

# load the dataset
Tabla   <- read.csv("C:/Users/anton/Documents/Tesis//BD_DEPURADA_AMD_R.csv")

for(i in 2:ncol(Tabla))
    Tabla[,i] <- as.numeric(Tabla[,i])

# Utilizo (factor) Para decir cual es la clase de la matriz
Tabla[,1] <- as.factor(Tabla[,1])

# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=5)

# train the model
model <- train(GRUPO~., data=Tabla, method="lvq", preProcess="scale", trControl=control)

# estimate variable importance
importance <- varImp(model, scale=FALSE)

# summarize importance
print(importance)

# plot importance
plot(importance)
