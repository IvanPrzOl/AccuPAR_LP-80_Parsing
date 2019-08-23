# Load_packages -----
library(XLConnect)
require(dplyr)
library(ggplot2)
source("cept_proc_fun.R",echo = FALSE) #load processsing functions

# Read_dataset -----
# Dir Name of the ceptometer file
f = "C:/Users/IPOLIVERA/Documents/scripts-io-files/Cep_HIBAPE40/HIBAP II Y18_E+40_CEPT 1.xls"
wb <-loadWorkbook(f) #load worbook
dataCept1 <- readWorksheet(wb,sheet=2,startCol=1,startRow=1,endCol=24,endRow=1251) #Read daset from worksheet 

f = "C:/Users/IPOLIVERA/Documents/scripts-io-files/Cep_HIBAPE40/HIBAP II Y18_E+40_CEPT 2.xls"
wb <-loadWorkbook(f) #load worbook
dataCept2 <- readWorksheet(wb,sheet=2,startCol=1,startRow=1,endCol=24,endRow=1542) #Read daset from worksheet 

#Subset annotation from the dataset, if r
processedC1 <- SubsetAnn(dataCept1,'HIB',nRecords = 3,segments = 1:8,asDf = TRUE,raw = FALSE,parBarStats = TRUE)


processedC1 <- processedC1 %>% subset(Anotacion!='HIBAP34-')
rownames(processedC1) <- NULL
processedC2 <- SubsetAnn(dataCept2,'HIB',nRecords = 3,segments = 1:8,asDf = TRUE,raw = FALSE,parBarStats = TRUE)
rownames(processedC2) <- NULL

random <- read.table("c:/Users/IPOLIVERA/Documents/scripts-io-files/Cep_HIBAPE40/HIBAP_Random.csv",header = TRUE,sep=',')
allPlots <- rbind(processedC1,processedC2)
allPlots$Label <- ordered(allPlots$Label,levels=c("ARRIBA","REFLEJADO","ABAJO"))
allPlots <- allPlots %>% arrange(Plot) %>% arrange(Label)
allPlots$Ent = random$Ent
allPlots$Rep = random$Rep


getLI <- function(df,seg=1:8){
  #new column with mean of each row
  #df$mean <- rowMeans(df[,paste("Segment.",seg,".PAR",sep="")])
  df$mean <- apply(df[paste("Segment.",seg,".PAR",sep="")],1,median)
  A <- subset(df,Label=="ARRIBA")$mean
  B <- subset(df,Label=="REFLEJADO")$mean
  C <- subset(df,Label=="ABAJO")$mean
  LI <- data.frame(Plot = subset(allPlotsOrderByEnt,Label=="ARRIBA")$Plot,
                   Entry = subset(allPlotsOrderByEnt,Label=="ARRIBA")$Ent,
                   Rep = subset(allPlotsOrderByEnt,Label=="ARRIBA")$Rep,
                   LI=((A-B)-C)/(A-B) * 100,
                   ARRIBA = A,
                   REFLEJADO = B,
                   ABAJO = C)
  return(LI)
}

allPlotsOrderByEnt <- allPlots %>%arrange(Ent) %>% arrange(Rep)
LI <- getLI(allPlotsOrderByEnt,1:8)

LIByRep <- data.frame(LI_1 = subset(LI,Rep==1)$LI,LI_2=subset(LI,Rep==2)$LI,LI_3=subset(LI,Rep==3)$LI)
ARRIBAByRep <- data.frame(ARRRIBA_1 = subset(LI,Rep==1)$ARRIBA,ARRIBA_2=subset(LI,Rep==2)$ARRIBA,ARRIBA_3=subset(LI,Rep==3)$ARRIBA)

cor(LIByRep)
cor(ARRIBAByRep)

#====
lab <- "ARRIBA"
segmentsC1 <- allPlotsOrderByEnt %>% subset(Label == lab & Rep == 1) %>% select(paste("Segment.",as.character(1:8),".PAR",sep=""))
segmentsC2 <- allPlotsOrderByEnt %>% subset(Label == lab & Rep == 2) %>% select(paste("Segment.",as.character(1:8),".PAR",sep=""))
segmentsC3 <- allPlotsOrderByEnt %>% subset(Label == lab & Rep == 3) %>% select(paste("Segment.",as.character(1:8),".PAR",sep=""))

#image(as.matrix(segmentsC1))
#image(as.matrix(segmentsC2))
#image(as.matrix(segmentsC3))

plot(1:8,segmentsC1[1,],type = 'l',ylim = c(min(segmentsC1),max(segmentsC1)))
apply(segmentsC1,1,lines)

plot(1:8,segmentsC2[1,],type = 'l',ylim = c(min(segmentsC2),max(segmentsC2)))
apply(segmentsC2,1,lines)

plot(1:8,segmentsC3[1,],type = 'l',ylim = c(min(segmentsC3),max(segmentsC3)))
apply(segmentsC3,1,lines)
