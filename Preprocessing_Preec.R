Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
Mode2 = function(x){ 
  ta = table(x)
  tam = max(ta)
  if (all(ta == tam))
    mod = NA
  else
    if(is.numeric(x))
      mod = as.numeric(names(ta)[ta == tam])
  else
    mod = names(ta)[ta == tam]
  return(mod)
}

library(readr)
DF <- read.csv("C:/Users/anton/Documents/Tesis/base_preeclampsia_R_preproc.csv")
DF <- DF[,c(2:26)]  # Descartamos la primera columna y ponemos PREECLMP como la clase
Original <- DF

for(k in 2:ncol(DF)){
  if(nrow(unique(DF[,k])) < 10)
    M <- as.numeric(Mode2(DF[,k]))
  else{
    auxcol <- as.data.frame(na.omit(DF[,k]))
    M <- mean(auxcol[,1])}
  
  for(r in 1:nrow(DF))
    if(is.na(DF[r,k]))
      DF[r,k] <- M    # Los valosres en blanco los pasamos a cero
  print(paste(k, " - ",nrow(unique(DF[,k])),  " - ", M, sep=""))
}

write.csv(DF, file = "C:/Users/anton/Documents/Tesis/BD_DMRE_trabajo.csv",row.names=FALSE)
write.csv(Original, file = "C:/Users/anton/Documents/Tesis/Original.csv",row.names=FALSE)
