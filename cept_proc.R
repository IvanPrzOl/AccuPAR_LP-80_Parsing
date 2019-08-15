# Load_packages -----
library(XLConnect)
require(plyr)
require(dplyr)
source("cept_proc_fun.R",echo = FALSE) #load processsing functions

#---- Specify wich data extract
config = data.frame(TrialNameIWIS = "HIBAP",
                    Etapa = "veg",
                    fechaInicio = as.POSIXct("1990/01/01"),
                    FechaFin = as.POSIXct("1990/01/01)"))


# Read_dataset -----
dropboxDir <- 'C:\\Users\\IPOLIVERA\\Dropbox\\CIMMYT\\Datos_Cept_Jaz\\'
fNames <- list.files(dropboxDir, pattern = "\\R.xlsx$") #files to process

f = fNames[1] #Select one file
f = '~/scripts-io-files/04-03-19 CEPT 1.xls'

#wb <- loadWorkbook(paste(dropboxDir,f,sep="")) #load excel Workbook 
wb <-loadWorkbook(f)

data <- readWorksheet(wb,2) #Read daset from worksheet

#select only useful columns
data <- subset(data,select = c("Record.Type","Date.and.Time","Annotation","Segment.1.PAR","Segment.2.PAR","Segment.3.PAR",
                               "Segment.4.PAR","Segment.5.PAR","Segment.6.PAR","Segment.7.PAR","Segment.8.PAR","Record.ID","Raw.Record.ID")) 

rownames(data) <- as.character(as.numeric(rownames(data))+1)

# Filter_by_dates and Trial ------
annIndx <- data$Annotation[!is.na(data$Annotation)]
annBoundaries <- c(0,which(!is.na(data$Annotation)))
annBInFile <- data.frame(Annotation = annIndx,
  initB = annBoundaries[1:(length(annBoundaries)-1)]+1,
  finishB = annBoundaries[2:length(annBoundaries)])

#unqDates <- unique((as.Date(data$Date.and.Time))) #year-month-day #Dates in file

toProcess <- annBInFile[ grepl("HIB",annBInFile$Annotation,ignore.case=TRUE), ]

#outDfList <- as.list(NULL)
#outIncDfList <-as.list(NULL)

outputDf <- data.frame(NULL,stringsAsFactors = FALSE)
  
incAtn <- as.data.frame(NULL) #inconsistent annotations output dataframe

for(k in 1:nrow(toProcess) ){
  #Output data frame structure


  subds <- data[toProcess$initB[k]:toProcess$finishB[k],] #One annotation
  row.names(subds) <- row.names(subds) #keep origin Idx as row names
  
  reL <- AnnotationProc(subds)
    
  if(length(reL)>2){
    outputDf <- rbind(outputDf,as.data.frame(reL))
  }
  else{
    incAtn <- rbind(incAtn,as.data.frame(reL))
  }

  # Format data frame
  #outputDf <- outputDf %>% mutate(No.Plot = as.numeric(gsub("[A-Za-z\']","",outputDf$Anotacion)))
  #outputDf <- outputDf %>% arrange(No.Plot)
  #outputDf <- outputDf[,c(1:3,ncol(outputDf),4:(ncol(outputDf)-1))]
  
  
  # save_to_excel
  #owb <- loadWorkbook("C:/Users/IPOLIVERA/Documents/git_repos/Outputs/ceptometro_procesado.xlsx",create = TRUE)
  #createSheet(owb,name = k)
  #writeWorksheet(object = owb,data = outputDf, sheet = k)
  #saveWorkbook(owb)
  #
  #oiwb <- loadWorkbook("C:/Users/IPOLIVERA/Documents/git_repos/Outputs/ceptometro_no_procesado.xlsx",create = TRUE)
  #createSheet(oiwb,name = k)
  #writeWorksheet(object = oiwb,data = incAtn, sheet = k)
  #saveWorkbook(oiwb)
  #outDfList[[length(outDfList) + 1]] <- outputDf
  #outIncDfList[[length(outIncDfList) + 1]] <- incAtn
#
  #print(outDfList)
  #print(outIncDfList)
}
print(outputDf)