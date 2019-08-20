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
allPlotsDfList <- as.list(NULL)

for(k in 1:2){
  wb <-loadWorkbook( paste(filesDir,fileNames,sep = "/")[k] )

  data <- readWorksheet(wb,sheet=2,startCol=1,startRow=1,endCol=25,endRow=fEndRows[k]) #Read daset from worksheet

  rownames(data) <- as.character(as.numeric(rownames(data))+1)

  procesed <- SubsetAnn(data,'HIB',nRecords = 3,segments = 1:8,asDf = TRUE,raw = FALSE,parBarStats = FALSE)
  
  allPlotsDfList[[length(allPlotsDfList) + 1]] <- procesed

}

# Collapse the output dataframes
allPlots <- rbind(allPlotsDfList[[1]],allPlotsDfList[[2]])
# Add one column with plot number
allPlots <- allPlots %>% mutate(Plot = as.numeric(gsub("\\D","",Anotacion)))
# Arrange dataset by plot in ascending order
allPlots <- allPlots %>% arrange(Plot)

# Check missed or duplicated plots
#Missed data, Plot 227 is NA, added correcting the sample order in the xlsx file
missedP <- (1:450)[!((1:450)%in% allPlots$Plot)]
duplicatedP <- allPlots$Plot %in% unique(allPlots$Plot[duplicated(allPlots$Plot)])
if ((length(missedP)>0) | length(duplicatedP)){
  cat("There are missed or duplicated Plots\n")
  print(missedP)
  print(allPlots[duplicatedP,] %>% arrange(Plot))
}
  
# Prepare_dataset-----
allPlots <- allPlots[-c(which(allPlots$Anotacion=='HIBAP34-')),]

# Read Ramdom config from file
random <- read.table("c:/Users/IPOLIVERA/Documents/scripts-io-files/Cep_HIBAPE40/HIBAP_Random.csv",header = TRUE,sep=',')
allPlots <- allPlots %>% mutate(Ent = random[,2],Rep= random[,3])
head(allPlots)

HIBAPE40_18 <- allPlots
# remove_objects_for_analysis----
remove(list = ls()[ls()!= "HIBAPE40_18"&ls()!= "HIBAPE40_18"&ls()!= "HIBAPE40_18"])
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
#plotVarinOrder(HIBAPE40_18,'ARRIBA')
#plotVarinOrder(HIBAPE40_18,'REFLEJADO')
#plotVarinOrder(HIBAPE40_18,'ABAJO')

#par(mfrow = c(3,1))
#plotVarinOrder(HIBAPE40_18,'ARRIBA')
#plotVarinOrder(HIBAPE40_18,'REFLEJADO')
#plotVarinOrder(HIBAPE40_18,'ABAJO')

calcCorr(HIBAPE40_18,1,2,"ARRIBA")

#Correlations-----

#Correlation 
cor12 <- diag(cor(getRepInOrder(HIBAPE40_18,1)[,c("ARRIBA","REFLEJADO","ABAJO")],getRepInOrder(HIBAPE40_18,2)[,c("ARRIBA","REFLEJADO","ABAJO")]))
cor13 <- diag(cor(getRepInOrder(HIBAPE40_18,1)[,c("ARRIBA","REFLEJADO","ABAJO")],getRepInOrder(HIBAPE40_18,3)[,c("ARRIBA","REFLEJADO","ABAJO")]))
cor23 <- diag(cor(getRepInOrder(HIBAPE40_18,2)[,c("ARRIBA","REFLEJADO","ABAJO")],getRepInOrder(HIBAPE40_18,3)[,c("ARRIBA","REFLEJADO","ABAJO")]))

print(cor12)
print(cor13)
print(cor23)
#Luz------
Luze40 <- read.table("C:/Users/IPOLIVERA/Documents/scripts-io-files/Cep_HIBAPE40/Luze40.csv",header=TRUE,sep=",")
