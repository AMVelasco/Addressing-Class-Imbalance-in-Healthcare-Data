# get data (N=25)
library(RCurl)
x <- read.csv("C:/Users/anton/Documents/Tesis/base_preeclampsia_R_preproc.csv")
#x<-x[,-1]
df <- data.frame(x)


for(i in 1:ncol(df))                          # Transformamos las columnas a valor numerico
  DF[,i] <- as.numeric(df[,i]) 
  
df$PREECLMP<- factor(df$PREECLMP)

# specify and run rfe
library(mlbench)
library(caret)
set.seed(1)
rfFuncs$summary <- twoClassSummary  
control <- rfeControl(functions=rfFuncs, 
                      method = "cv",
                      repeats =5, 
                      number = 10,
                      returnResamp="final", 
                      verbose = TRUE)
trainctrl <- trainControl(classProbs= TRUE,
                          summaryFunction = twoClassSummary)
result <- rfe(df[, 2:length(df)],         # features
              df[, 1],                    # classification
              sizes=2:30,             
              rfeControl=control,
              method="svmRadial",
              metric = "ROC",
              trControl = trainctrl)
result
print(results)
Resumen <- cbind(df[,1],  df[ ,predictors(result)])
write.csv(Resumen, "C:/Users/anton/Documents/Tesis/RFE_PREEC.csv",row.names=FALSE)
#capture.output(summary(results), file = "RFE_DMRE.txt")

plot(result, type=c("g", "o"))
