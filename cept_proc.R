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

# Process_by_dates ------
#trunc(date,"hours")
source("cept_proc_fun.R",echo = FALSE) #load processsing functions 

unqDates <- unique((as.Date(data$Date.and.Time))) #year-month-day #Dates in file

outDfList <- as.list(NULL)
outIncDfList <-as.list(NULL)

for(d in as.character(unqDates)){
  #Output data frame structure
  outputDf <- data.frame(OriginIdx = as.numeric(),
                         Fecha = as.character(),
                         Anotacion = as.numeric(),
                         ARRIBA = as.numeric(),
                         REFLEJADO = as.numeric(),
                         ESPIGA = as.numeric(),
                         HB = as.numeric(),
                         H2 = as.numeric(),
                         H3 = as.numeric(),
                         ABAJO = as.numeric(), 
                         stringsAsFactors = FALSE)
  
  incAtn <- as.data.frame(NULL) #inconsistent annotations output dataframe

  subds <- (data %>% filter(as.Date(Date.and.Time) == d)) #all annotations in one day
  row.names(subds) <- row.names(data[as.Date(data$Date.and.Time) == d,]) #keep origin Idx as row names
  anB <- c(0,which(!is.na(subds$Annotation))) #Annotation boundaries
  
  for( k in 1:(length(anB)-1)){ #Process each annotation
    reL <- AnnotationProc(subds[(anB[k]+1):anB[k+1],]) #Length >2 elements is ok
    
    if(length(reL)>2){outputDf[nrow(outputDf)+1,] <- reL}
    else{incAtn <- rbind(incAtn,reL)}
  }
  # Format data frame
  outputDf <- outputDf %>% mutate(No.Plot = as.numeric(gsub("[A-Za-z\']","",outputDf$Anotacion)))
  outputDf <- outputDf %>% arrange(No.Plot)
  outputDf <- outputDf[,c(1:3,ncol(outputDf),4:(ncol(outputDf)-1))]
  # print(outputDf)
  
  # save_to_excel
  owb <- loadWorkbook("C:/Users/IPOLIVERA/Documents/git_repos/Outputs/ceptometro_procesado.xlsx",create = TRUE)
  createSheet(owb,name = d)
  writeWorksheet(object = owb,data = outputDf, sheet = d)
  saveWorkbook(owb)
  
  oiwb <- loadWorkbook("C:/Users/IPOLIVERA/Documents/git_repos/Outputs/ceptometro_no_procesado.xlsx",create = TRUE)
  createSheet(oiwb,name = d)
  writeWorksheet(object = oiwb,data = incAtn, sheet = d)
  saveWorkbook(oiwb)
  outDfList[[length(outDfList) + 1]] <- outputDf
  outIncDfList[[length(outIncDfList) + 1]] <- incAtn
#
  #print(outDfList)
  #print(outIncDfList)
}