#'Function to process one single annotation
#' @importFrom stats sd median

ProcSingleAnn <- function(antn,segments = 1:8,raw=FALSE,parBarStats=FALSE,rOrder,trialName){
  #fill anotation cells
  antn$Annotation <- antn$Annotation[!is.na(antn$Annotation)]
  #remove last row
  antn <- antn[-nrow(antn),]
  obs <- nrow(antn)
  #delete non-used columns
  antn <- antn[,-c(4:12,21:23)]
  if (raw){
    if(!is.na(trialName)){
            sep_annotation <- stringi::stri_match(antn$Annotation[obs],regex = sprintf('(%s)(\\d*)(.*)',trialName) )
            sep_annotation[sep_annotation == ""] <- NA
            antn$Trial.name <- sep_annotation[2] 
            antn$Plot <- sep_annotation[3]
            antn$Plot.obs <- sep_annotation[4]
    }
    if (!is.na(rOrder)){
      if(!(rOrder == paste(substr(antn$Record.Type,1,1) ,collapse = ''))){
        print ( sprintf('plot %s cannot be extracted',antn$Annotation[obs]) )
        print(paste(substr(antn$Record.Type,1,1) ,collapse = ''))
        return(NULL)
      }
    }
    unique_records <- make.unique(antn$Record.Type,sep='')
    record_order <- as.integer(gsub('\\D','',unique_records))
    record_order[is.na(record_order)] <- 0
    record_order <- record_order + 1
    antn$Record.Type <- apply(cbind(seq(1:obs),antn$Record.Type,deparse.level = 0),1,function(z) paste(z[1],z[2],sep=''))
    
    return(antn)
  
  }
  else{
    # Process function
    return(NULL)
  }
}

#'Process the ceptometer file as a dataframe
#'
#'@param df A dataframe readed from the ceptometer output file
#'@param tName A string in regex format indicating the name of the annotation(s) to be extracted
#'@param nRecords Num of Records per annotation
#'@param segments A vector indicating the PAR bar segments selected
#'@param raw Raw outputdata
#'@param parBarStats include some stastistics of the PAR bar in the output
#'@return A data frame containing a subset of the annotations
#'@export
CeptProc <- function(df,segments=1:8,raw=FALSE,parBarStats = FALSE,nRecords = NA ,tName = NA){
  # Get the annotation boundaries
  annBndIdx <- c(0,which(!is.na(df$Annotation)))
  #annBnd <- data.frame(Annotation = df$Annotation[!is.na(df$Annotation)],initB = annBndIdx[1:(length(annBndIdx)-1)]+1,finishB = annBndIdx[2:length(annBndIdx)])
  annBnd <- cbind(df$Annotation[!is.na(df$Annotation)], annBndIdx[1:(length(annBndIdx)-1)]+1, annBndIdx[2:length(annBndIdx)], deparse.level = 0)

  if (!is.na(tName) && any(grepl(tName,annBnd[,1],ignore.case=TRUE))){
    annBnd <- annBnd[ grepl(tName,annBnd[,1],ignore.case=TRUE), ]
  }
  else{
    return(NULL)
  }

  separatedAnn <- apply(annBnd,1,function(x,y) y[x[2]:x[3],],df)

  return ( do.call('rbind',lapply(separatedAnn,ProcSingleAnn,segments,raw,parBarStats,nRecords,tName)) )

}

#'Calculate some stats of the PAR from the raw anotation
#'@param parMat a Matrix of the PAR bar segments
#'@param strat a vector with the name of each strata, i.e ARRIBA,REFLEC, etc.
#'@return A list with the calculated params
getPARBarStats <- function(parMat,strat){
  parSD <- apply(parMat,1,sd)
  names(parSD) <- paste("SD_",strat,sep="")
  parMin <- apply(parMat,1,min)
  names(parMin) <- paste("Min_",strat,sep="")
  parMax <- apply(parMat,1,max)
  names(parMax) <- paste("Max_",strat,sep="")

  return(c(parSD,parMin,parMax))
}

#'Calculate PAR bar statistics for each record's raw data
#'@param rawRecords a dataframe returned by ProcSingleAnn setting the raw flag as TRUE
#'@param rejectOutliers a boolean flag to reject outliers in each record and calcalate the stats
#'@return a dataframe with the raw data and the calculated statistics
getPARBarStatsRaw <- function(rawRecords,rejectOutliers = FALSE){
  if(rejectOutliers){
  rawRecords <- recordsOutlierReject(rawRecords)
  }
  cbind(rawRecords,Median = apply(rawRecords[,paste("Segment.",1:8,".PAR",sep="")],1,median,na.rm=TRUE),
        Mean = apply(rawRecords[,paste("Segment.",1:8,".PAR",sep="")],1,mean,na.rm=TRUE),
        Max = apply(rawRecords[,paste("Segment.",1:8,".PAR",sep="")],1,max,na.rm=TRUE),
        Min = apply(rawRecords[,paste("Segment.",1:8,".PAR",sep="")],1,min,na.rm=TRUE),
        Std = apply(rawRecords[,paste("Segment.",1:8,".PAR",sep="")],1,sd,na.rm=TRUE),
        CV = apply(rawRecords[,paste("Segment.",1:8,".PAR",sep="")],1,sd,na.rm=TRUE)/apply(rawRecords[,paste("Segment.",1:8,".PAR",sep="")],1,mean,na.rm=TRUE)*100)
}

#'Reject outliers in each record
#'@param records a dataframe returned by ProcSingleAnn setting the raw flag as TRUE
#'@return a dataframe without outliers in each record
recordsOutlierReject <- function(records){
  for (k in 1:nrow(records)){
    bar <- as.numeric(records[k,paste("Segment.",1:8,".PAR",sep="")])
    limSup <- mean(bar) + 1*sd(bar)
    limInf <- mean(bar) - 1*sd(bar)

  if(records[k,'Label'] == 'ABAJO'){
    bar[bar>limSup | bar<limInf] <-NA
    records[k,paste("Segment.",1:8,".PAR",sep="")] <- bar
  }else{
    bar[bar>limSup | bar<limInf] <-NA
    records[k,paste("Segment.",1:8,".PAR",sep="")] <- bar
  }
  }
  return(records)
}
