# EJERCICIO 1
Tabla <- read.csv("C:/Users/anton/Documents/Tesis/base_preeclampsia_R_preproc_2.csv")
#Tabla<-Tabla[,-1]
#Tabla<-read.csv('C:/Users/anton/Documents/Tesis/BD_DMRE_trabajo.csv')
require(randomForest)
fit=randomForest(factor(PREECLMP)~., data=Tabla)
library(caret)
varImp(fit)
par("mar")
par(mar=c(1,1,1,1))
dev.off()
par(cex=0.55)
varImpPlot(fit, type=2)

# EJERCICIO 2
par("mar")
par(mar=c(1,1,1,1))
dev.off()
importanceOrder=order(-fit$importance)
names=rownames(fit$importance)[importanceOrder][1:5]
par(mfrow=c(3,2), xpd=NA)


for (name in names)
   partialPlot(fit, Tabla, eval(name), main=name, xlab=name,ylim=c(-.2,.9))



# ARBOL DEFINITIVO
library(rpart)
library(rpart.plot)
fit <- rpart(factor(PREECLMP)~., data=Tabla, cp=0.017)
rpart.plot(fit,type=2,extra=106,cex= 0.68)
#rpart.plot(fit, type=2, extra = 106,fallen.leaves = FALSE,roundint =1)
#rpart.plot(fit, type=2, extra = 106)


# EJERCICIO 3
library(rpart)
fit=rpart(factor(PREECLMP)~., data=Tabla)
plot(fit)
text(fit)

# EJERCICIO 3
library(rpart)
par(mar=c(1,1,1,1))
dev.off()
fit=rpart(factor(PREECLMP)~., data=Tabla,control=rpart.control(minsplit=3,minbucket=2,cp=0))
par(cex=0.54)
plot(fit)
text(fit)

# EJERCICIO 4
tmp=rownames(fit$splits)
allVars=colnames(attributes(fit$terms)$factors)
rownames(fit$splits)=1:nrow(fit$splits)
splits=data.frame(fit$splits)
splits$var=tmp
splits$type=""
frame=as.data.frame(fit$frame)

index=0
for(i in 1:nrow(frame)){
   if(frame$var[i] != "<leaf>"){
   index=index + 1
   splits$type[index]="primary"
   if(frame$ncompete[i] > 0){
   for(j in 1:frame$ncompete[i]){
   index=index + 1
   splits$type[index]="competing"}}
   if(frame$nsurrogate[i] > 0){
   for(j in 1:frame$nsurrogate[i]){
      index=index + 1
      splits$type[index]="surrogate"}}}}
splits$var=factor(as.character(splits$var))
splits=subset(splits, type != "surrogate")
out=aggregate(splits$improve,
    list(Variable = splits$var),
    sum, na.rm = TRUE)
allVars=colnames(attributes(fit$terms)$factors)
 if(!all(allVars %in% out$Variable)){
  missingVars=allVars[!(allVars %in% out$Variable)]
  zeros=data.frame(x = rep(0, length(missingVars)), Variable = missingVars)
  out=rbind(out, zeros)}
out2=data.frame(Overall = out$x)
rownames(out2)=out$Variable
out2

VI_T=out2
barplot(unlist(VI_T/sum(VI_T)),names.arg=1:7)
barplot(t(VI_T/sum(VI_T)))

# EJERCICIO 5
library(earth)
marsModel <- earth(PREECLMP~ ., data=Tabla) # build model
ev <- evimp (marsModel) # estimate variable importance
plot(ev)

# EJERCICIO 5
library(Boruta)
# Decide if a variable is important or not using Boruta
boruta_output <- Boruta(PREECLMP~ ., data=na.omit(Tabla), doTrace=2)  # perform Boruta search
# Confirmed 10 attributes: Humidity, Inversion_base_height, Inversion_temperature, Month, Pressure_gradient and 5 more.
# Rejected 3 attributes: Day_of_month, Day_of_week, Wind_speed.
boruta_signif <- names(boruta_output$finalDecision[boruta_output$finalDecision %in% c("Confirmed", "Tentative")])  # collect Confirmed and Tentative variables
print(boruta_signif)  # significant variables
plot(boruta_output, cex.axis=.7, las=2, xlab="", main="Variable Importance")  # plot variable importance


