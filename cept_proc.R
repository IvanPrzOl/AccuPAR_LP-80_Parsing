# Load_packages -----
library(XLConnect)
require(dplyr)
source("cept_proc_fun.R",echo = FALSE) #load processsing functions

# Read_dataset -----
# Dir Name of the ceptometer file
f = '~/scripts-io-files/04-03-19 CEPT 1.xls'

wb <-loadWorkbook(f) #load worbook

data <- readWorksheet(wb,2) #Read daset from worksheet 

#Subset annotation from the dataset, if r
procesed <- SubsetAnn(data,'SYN',nRecords = 3,segments = 1:8,asDf = TRUE,raw = FALSE,parBarStats = FALSE)
rownames(procesed) <- NULL

print(procesed)

