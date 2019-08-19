# Process ----
require(XLConnect)
require(plyr)
require(dplyr)
source("C:/Users/IPOLIVERA/OneDrive - CGIAR/git_repos/ceptometer_processing/cept_proc_fun.R",echo = FALSE)

# Read_dataset
filesDir <- 'C:/Users/IPOLIVERA/Documents/scripts-io-files/Cep_HIBAPE40'
fileNames <- list.files(filesDir, pattern = ".xls$") #files to process

#Read data from excel file
fEndRows <- c(1251,1542)
outDfList <- as.list(NULL)
outIncDfList <-as.list(NULL)

for(k in 1:2){
wb <-loadWorkbook( paste(filesDir,fileNames,sep = "/")[k] )

data <- readWorksheet(wb,sheet=2,startCol=1,startRow=1,endCol=25,endRow=fEndRows[k]) #Read daset from worksheet

#select only useful columns
data <- subset(data,select = c("Record.Type","Date.and.Time","Annotation","Segment.1.PAR","Segment.2.PAR","Segment.3.PAR",
                               "Segment.4.PAR","Segment.5.PAR","Segment.6.PAR","Segment.7.PAR","Segment.8.PAR","Record.ID","Raw.Record.ID")) 

rownames(data) <- as.character(as.numeric(rownames(data))+1)

# Filter_by_dates and Trial 
annIndx <- data$Annotation[!is.na(data$Annotation)]
annBoundaries <- c(0,which(!is.na(data$Annotation)))
annBInFile <- data.frame(Annotation = annIndx, initB = annBoundaries[1:(length(annBoundaries)-1)]+1, finishB = annBoundaries[2:length(annBoundaries)])

toProcess <- annBInFile[ grepl("HIB",annBInFile$Annotation,ignore.case=TRUE), ]

outputDf <- data.frame(NULL,stringsAsFactors = FALSE)
  
incAtn <- as.data.frame(NULL) #inconsistent annotations output dataframe

for(k in 1:nrow(toProcess) ){
  subds <- data[toProcess$initB[k]:toProcess$finishB[k],] #One annotation
  row.names(subds) <- row.names(subds) #keep origin Idx as row names
  
  reL <- AnnotationProc(subds,nRecords=3,2:7) #

  if(length(reL)>2){
    outputDf <- rbind(outputDf,as.data.frame(reL))
  }
  else{
    incAtn <- rbind(incAtn,as.data.frame(reL))
  }

}
outDfList[[length(outDfList) + 1]] <- outputDf
outIncDfList[[length(outIncDfList) + 1]] <- incAtn

}

allPlots <- rbind(outDfList[[1]],outDfList[[2]])

allPlots <- allPlots %>% mutate(Plot = as.numeric(gsub("\\D","",Anotacion)))
#allPlots[(nrow(allPlots)+1),] <- list(NA,NA,NA,NA,NA,NA,227)
#Missed data, Plot 227 is NA, added correcting the sample order in the xlsx file
allPlots <- allPlots %>% arrange(Plot)
print(outIncDfList)

which(table(allPlots$Plot) > 1)

allPlots <- allPlots[-c(which(allPlots$Anotacion=='HIBAP34-')),]

allPlots <- allPlots[,c(2,4:ncol(allPlots))]

if(nrow(allPlots) != 450) {print(nrow(allPlots))}

random <- read.table("c:/Users/IPOLIVERA/Documents/scripts-io-files/Cep_HIBAPE40/HIBAP_Random.csv",header = TRUE,sep=',')
allPlots <- allPlots %>% mutate(Ent = random[,2],Rep= random[,3])
head(allPlots)

HIBAPE40_27 <- allPlots
# remove objects ----
remove(list = ls()[ls()!= "HIBAPE40_18"&ls()!= "HIBAPE40_27"])
#write.csv(allPlots,"C:/Users/IPOLIVERA/Documents/scripts-io-files/Cep_HIBAPE40/Procesado_18.csv",row.names = FALSE)

# Analysis -----
library(ggplot2)
getRepInOrder <- function(inputDs,rep){
  (inputDs %>% subset(Rep == rep) %>% arrange(Ent))
}

plotVarinOrder <- function(ds,var1){
  
  
  plot(1:150,getRepInOrder(ds,1)[,var1],col='black',xlim = c(0,450),ylim = c(min(ds[,var1]),max(ds[,var1])),ylab = var1,xlab='PLOT 1-450')
  points(151:300,getRepInOrder(ds,2)[,var1],col='red')
  points(301:450,getRepInOrder(ds,3)[,var1],col='blue')
}

plotCV <- function(ds,var1,retVal = FALSE){
  tmp <- cbind(getRepInOrder(ds,1)[,c(var1)],getRepInOrder(ds,2)[,c(var1)],getRepInOrder(ds,3)[,c(var1)])
  tmp <- apply(tmp,1,sd)/rowMeans(tmp)
  plot(tmp,col='black',xlim = c(0,150),ylim = c(min(tmp),max(tmp)),ylab = paste('CV_',var1,''),xlab='ENT 1-150')
  if(retVal){return(tmp)}
}

calcInter <- function(ds){
  data.frame(Plot = ds$Plot, Ent = ds$Ent, Rep = ds$Rep,
             LI = ((ds$ARRIBA-ds$REFLEJADO)-ds$ABAJO)/(ds$ARRIBA-ds$REFLEJADO)*100)
}

calcCorr <- function(ds,rep1,rep2,var1){
  diag(cor(getRepInOrder(ds,rep1)[,c(var1)],getRepInOrder(ds,rep2)[,c(var1)]))
}

plotSD <- function(ds,var1,retVal = FALSE){
  tmp <- cbind(getRepInOrder(ds,1)[,c(var1)],getRepInOrder(ds,2)[,c(var1)],getRepInOrder(ds,3)[,c(var1)])
  tmp <- apply(tmp,1,sd)
  plot(tmp,col='black',xlim = c(0,150),ylim = c(min(tmp),max(tmp)),ylab = paste('SD_',var1,''),xlab='ENT 1-150')
  if(retVal){return(tmp)}
}
# Plots ------
plotVarinOrder(HIBAPE40_18,'ARRIBA')
plotVarinOrder(HIBAPE40_18,'REFLEJADO')
plotVarinOrder(HIBAPE40_18,'ABAJO')

par(mfrow = c(3,1))
plotVarinOrder(HIBAPE40_27,'ARRIBA')
plotVarinOrder(HIBAPE40_27,'REFLEJADO')
plotVarinOrder(HIBAPE40_27,'ABAJO')
#Correations-----



#join random info with ceptometer data

#allPlots <- allPlots[,c(4,5,6,1,2,3)]

#Correlation ←
cor12 <- diag(cor(getRepInOrder(HIBAPE40_27,1)[,c("ARRIBA","REFLEJADO","ABAJO")],getRepInOrder(HIBAPE40_27,2)[,c("ARRIBA","REFLEJADO","ABAJO")]))
cor13 <- diag(cor(getRepInOrder(HIBAPE40_27,1)[,c("ARRIBA","REFLEJADO","ABAJO")],getRepInOrder(HIBAPE40_27,3)[,c("ARRIBA","REFLEJADO","ABAJO")]))
cor23 <- diag(cor(getRepInOrder(HIBAPE40_27,2)[,c("ARRIBA","REFLEJADO","ABAJO")],getRepInOrder(HIBAPE40_27,3)[,c("ARRIBA","REFLEJADO","ABAJO")]))

print(cor12)
print(cor13)
print(cor23)