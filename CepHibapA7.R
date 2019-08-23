# Read data ----
require(XLConnect)
require(plyr)
require(dplyr)
source("cept_proc_fun.R",echo = FALSE)

# Read_dataset
filesDir <- 'C:/Users/IPOLIVERA/Documents/scripts-io-files/Cep_HIBAPE40'
fileNames <- 'hibapA7.xlsx'

wb <-loadWorkbook( paste(filesDir,fileNames,sep = "/") )
  
data <- readWorksheet(wb,sheet=1,startCol=1,startRow=1,endCol=26,endRow=4036) #Read daset from worksheet
  
rownames(data) <- as.character(as.numeric(rownames(data))+1)

#process-----  
processed <- SubsetAnn(data,'HIB',nRecords = 7,segments = 1:8,asDf = TRUE,raw = FALSE)

# Check missed or duplicated plots
#Missed data, Plot 227 is NA, added correcting the sample order in the xlsx file
missedP <- (1:450)[!((1:450)%in% processed$Plot)]
duplicatedP <- processed$Plot %in% unique(processed$Plot[duplicated(processed$Plot)])

if ((length(missedP)>0) | length(duplicatedP)){
  cat("There are missed or duplicated Plots\n")
  print(missedP)
  print(processed[duplicatedP,] %>% arrange(Plot))
  print(processed[processed$Plot > 450,])
}