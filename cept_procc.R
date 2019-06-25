# Load_packages -----
library(XLConnect)
require(plyr)
require(dplyr)

# Read_dataset -----
dropboxDir <- 'C:\\Users\\IPOLIVERA\\Dropbox\\CIMMYT\\Datos_Cept_Jaz\\'
fNames <- list.files(dropboxDir, pattern = "\\R.xlsx$") #files to process

f = fNames[1] #Select one file

wb <- loadWorkbook(paste(dropboxDir,f,sep="")) #load excel Workbook 

data <- readWorksheet(wb,1) #Read daset from worksheet

#delete non used columns
data <- subset(data,select = c("Record.Type","Date.and.Time","Annotation","Segment.1.PAR","Segment.2.PAR","Segment.3.PAR",
                               "Segment.4.PAR","Segment.5.PAR","Segment.6.PAR","Segment.7.PAR","Segment.8.PAR","Record.ID","Raw.Record.ID")) 