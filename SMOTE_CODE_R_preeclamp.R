library(performanceEstimation)
library(csv)
library(DMwR2)
library(imbalance)

original_RFE<- read.csv('C:/Users/anton/Documents/Tesis/results_REF_preec2_sin_toxemia.csv',header = TRUE, sep = ',')
data<-original_RFE[,c(1:18)]

for(i in 2:3)
  data[,i] <- as.factor(data[,i])

data$PREECLMP <- factor(data$PREECLMP)


table(data$PREECLMP)
imbalanceRatio(data, classAttr = "PREECLMP")
prop.table(table(data$PREECLMP))

#funciona igual que sin usar perc.under
newDataPREECLMP <- SMOTE(PREECLMP ~ ., data,perc.over = 400, perc.under=100)
table(newDataPREECLMP$PREECLMP)
prop.table(table(newDataPREECLMP$PREECLMP))
imbalanceRatio(newDataPREECLMP, classAttr = "PREECLMP")
write.csv(newDataPREECLMP, file = "C:/Users/anton/Documents/Tesis/newData400_100PREECLMP.csv")


newDataPREECLMP32 <- SMOTE(PREECLMP ~ ., data,perc.over = 200, perc.under=200)
table(newDataPREECLMP32$PREECLMP)
prop.table(table(newDataPREECLMP32$PREECLMP))
imbalanceRatio(newDataPREECLMP32, classAttr = "PREECLMP")
write.csv(newDataPREECLMP32, file = "C:/Users/anton/Documents/Tesis/newData32PREECLMP.csv")


#funciona igual que sin usar perc.under
newDataPREECLMP42 <- SMOTE(PREECLMP ~ ., data, perc.over = 400, perc.under=200)
#write.csv(newDataAMD42, file = "C:/Users/anton/Documents/Tesis/newDataPREECLMP400.csv")
table(newDataPREECLMP42$PREECLMP)
prop.table(table(newDataPREECLMP42$PREECLMP))
imbalanceRatio(newDataPREECLMP42, classAttr = "PREECLMP")

## now using SMOTE to create a more "balanced problem"
newDataAMD200 <- SMOTE(PREECLMP ~ ., data, perc.over = 200)
#write.csv(newDataAMD200, file = "C:/Users/anton/Documents/Tesis/newData200PREEC.csv")
table(newDataAMD200$PREECLMP)
imbalanceRatio(newDataAMD200, classAttr = "PREECLMP")
#Reportada en tesis
newDataAMD100 <- SMOTE(PREECLMP ~ ., data, perc.over = 100)
write.csv(newDataAMD100, file = "C:/Users/anton/Documents/Tesis/newData100PREEC.csv")
table(newDataAMD100$PREECLMP)
imbalanceRatio(newDataAMD100, classAttr = "PREECLMP")

par(mfrow = c(1, 2))
plot(data[, 1], pch = 19 + as.integer(data[, 2]),   main = "Original Data")
plot(newDataPREECLMP32[, 1], pch = 19 + as.integer(newDataPREECLMP32[,1]),  main = "SMOTE'd Data")



