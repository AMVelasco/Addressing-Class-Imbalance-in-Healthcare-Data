library(performanceEstimation)
library(csv)
library(DMwR)
library(imbalance)

original_RFE<- read.csv('C:/Users/anton/Documents/Tesis/BD_AMD_recortada.csv',header = TRUE, sep = ',')
data<-original_RFE[,c(1:16)]

data$GRUPO <- factor(data$GRUPO)
table(data$GRUPO)
imbalanceRatio(data, classAttr = "GRUPO")
prop.table(table(data$GRUPO))

#funciona igual que sin usar perc.under
newDataAMD1 <- SMOTE(GRUPO ~ ., data,perc.over = 200, perc.under=200)
table(newDataAMD1$GRUPO)
prop.table(table(newDataAMD1$GRUPO))
imbalanceRatio(newDataAMD1, classAttr = "GRUPO")

#funciona igual que sin usar perc.under
newDataAMD300 <- SMOTE(GRUPO ~ ., data, perc.over = 300, perc.under=200)
#write.csv(newDataAMD200, file = "C:/Users/anton/Documents/Tesis/newData300AMD.csv")
table(newDataAMD300$GRUPO)
prop.table(table(newDataAMD300$GRUPO))
imbalanceRatio(newDataAMD300, classAttr = "GRUPO")

## now using SMOTE to create a more "balanced problem"
newDataAMD200 <- SMOTE(GRUPO ~ ., data, perc.over = 200)
write.csv(newDataAMD200, file = "C:/Users/anton/Documents/Tesis/newData200AMD.csv")
table(newDataAMD200$GRUPO)
imbalanceRatio(newDataAMD200, classAttr = "GRUPO")

newDataAMD100 <- SMOTE(GRUPO ~ ., data, perc.over = 100)
write.csv(newDataAMD100, file = "C:/Users/anton/Documents/Tesis/newData100AMD.csv")
table(newDataAMD100$GRUPO)
imbalanceRatio(newDataAMD100, classAttr = "GRUPO")

#par(mfrow = c(1, 2))
#plot(data[, 1], pch = 19 + as.integer(data[, 2]),   main = "Original Data")
#plot(newDataAMD100[, 1], pch = 19 + as.integer(newDataAMD100[,1]),  main = "SMOTE'd Data")



