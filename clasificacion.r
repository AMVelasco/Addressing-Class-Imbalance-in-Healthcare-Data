library(readr)
library(caret)
library(bst)
library(plyr)
library(fastAdaboost)


DF <- read.csv("C:/Users/anton/Documents/Tesis/tomek_newData100AMD.csv") # Leemos la base de datos
        

DF <- DF[,c(6,3,4:8,9:34)]  # Descartamos la primera columna y ponemos PREECLMP como la clase
DF[is.na(DF)] <- 0         # Los valosres en blanco los pasamos a cero

# Cambiamos la estructura de la base de datos para poder trabajar con ella
DF = as.matrix(as.data.frame(lapply(DF, as.numeric)))  # https://emilkirkegaard.dk/en/?p=5412
DF = as.data.frame(DF)

DF <- subset(DF, DF$GRUPO== 0 | DF$GRUPO==1) # Seleccionamos las filas donde la clase es 1 o 2

for(i in 2:ncol(DF))                          # Transformamos las columnas a valor numerico
  DF[,i] <- as.numeric(DF[,i]) 
DF[,1] <- as.factor(DF[,1])                   # Transformamos la clase a tipo factor

Met<-"LogitBoost"                # Lista de los metodos que vamos a usar
Met[2]<-"svmLinear"
Met[3]<-"rf"
Met[4]<-"adaboost"
Met[5]<-"bagFDA"
Met[6]<-"bagEarth"
Met[7]<-"mlpWeightDecayML" #kknn"
Met[9]<-"mlpWeightDecayML"

Res <- ""

for(k in 1:7){
  # Inicia el cronometro
  t <- proc.time() 
  
  # Utilizo Cross-Validation con 10 particiones y repito el proceso 5 veces.
  control <- trainControl(method="repeatedcv", number=10, repeats=1)
  
  # Param 1: Todas las entradas - Param 2:La clase - Param 3: Nombre del metodo - Param 4: Tipo de repeticiones 
  modelo <- train(DF[,2:(ncol(DF))], DF[,1], method = Met[k], trControl = control)
  
  # Guardamos el tiempo que tarda en construir el modelo
  time = proc.time()-t
  
  com <-  confusionMatrix(modelo) # Matriz de confusion del modelo
  
  # Calculamos las metricas del modelo
  Acc <-  (com$table[1,1]+com$table[2,2])/(com$table[1,1]+com$table[1,2]+com$table[2,1]+com$table[2,2])
  Spe <-  com$table[1,1]/(com$table[1,1]+com$table[2,1])  
  Sen <-  com$table[2,2]/(com$table[1,2]+com$table[2,2]) 
  Pre <-  (com$table[1,1]+com$table[2,1])/(com$table[1,1]+com$table[1,2]+com$table[2,1]+com$table[2,2]) 
  PPV = (Sen * Pre)/((Sen*Pre) + ((1-Spe)*(1-Pre)))
  
  Res[k] <- paste(modelo$modelInfo$label, "@Acc@" , Acc, "@Sensitivity@", Sen , "@Specificity@", Spe, "@Prevalence@", Pre,
                    "@Pos Pred Value@", PPV , "@Balanced Accuracy@", ((Spe+Sen)/2) ,"@Configuraciones@", length(modelo$results$Accuracy), 
                    "@Time@" , time[3] , sep="")
  
  mostrar <- paste(Res[k]," ", k, sep="")   # Guardamos las soluciones en Res
  print(mostrar) # Imprimimos los valores separados por @, se puede separar facilemnte en excell por este separador.
  
}


