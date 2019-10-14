#'Function to process one single annotation
#'@param antn One Annotation Records
#'@param nRecords Num of Records per annotation
#'@param segments A vector indicating the PAR bar segments selected
#'@param raw Raw outputdata
#'@param parBarStats include some stastistics of the PAR bar in the output
#'@return a dataframe with the means of each record or raw data if selected, if the annotation is not 
#'consistent, returns null
AnnProc <- function(antn,nRecords = 3,segments = 1:8,raw=FALSE,parBarStats=FALSE){ 
  recordOrder <- list("3"=c("ABV","BLW","ABV"),
                      "7"=c("ABV","BLW","ABV","ABV","ABV","ABV","ABV"))
  labelMeans <- list("3" = c("ARRIBA","REFLEJADO","ABAJO"),
                      "7" = c("ARRIBA","REFLEJADO","ESPIGA","HB","H2","H3","ABAJO"))
  # Select PAR segments to process, this can be an optinal parameter
  parSegments <- paste("Segement.",1:8,".PAR",sep="")[segments] 

  #Check consistency of the input data
  #Return a list with the origin index and the annotation name
  obs <- nrow(antn)
  if((obs-1) < nRecords){ #missed data, 
    return(NULL)#list(originIdx = as.numeric(row.names(antn))[obs],Anotacion = antn$Annotation[obs]))
  }
  else if(any(!(antn$Record.Type[(obs-nRecords):(obs-1)] == recordOrder[[as.character(nRecords)]]))){ #Wrong record order
    return(NULL)#list(originIdx = as.numeric(row.names(antn))[obs],Anotacion = antn$Annotation[obs]))
  }
  else if(raw){
    out <- list(Fecha = antn$Date.and.Time[(obs-nRecords):(obs-1)],
                Anotacion = antn$Annotation[obs],
                Label = labelMeans[[as.character(nRecords)]],
                antn[(obs-nRecords):(obs-1),parSegments],
                Plot = as.numeric(gsub("\\D","",antn$Annotation[obs])))
    return(out)
  }
  parMatrix = antn[(obs-nRecords):(obs-1),parSegments] #Numeric values of PAR segments
  parMeans = apply(as.matrix(parMatrix),1,mean)#rowMeans(parMatrix)
  names(parMeans) <- labelMeans[[as.character(nRecords)]] #Average in each strata
  
  #Output list structure
  out <- (c(list(originIndex = as.numeric(row.names(antn))[obs], 
                Fecha = trunc(antn$Date.and.Time[obs],"mins"),
                Anotacion = antn$Annotation[obs],
                Plot = as.numeric(gsub("\\D","",antn$Annotation[obs]))),parMeans))
  if(parBarStats) {out <- c(out,getPARBarStats(parMatrix,labelMeans[[as.character(nRecords)]]))}
  return(out)
}

#'Extract one annotation from dataframe given the annotation boundaries
#'This fucntion calls directly the AnnProc function
#'@param bdn A dataframe containing the name annotation names and their boundaries
#'@param df ceptometer file dataframe
#'@param antn One Annotation Records
#'@param nRecords Num of Records per annotation
#'@param segments A vector indicating the PAR bar segments selected
#'@param raw Raw outputdata
#'@param parBarStats include some stastistics of the PAR bar in the output
#'@return a dataframe defined by AnnProc function
SubsetByBnd <- function(bnd,df,nRecords,segments,raw,parBarStats){ 
  return( as.data.frame( AnnProc(df[bnd[2]:bnd[3],],nRecords,segments,raw,parBarStats)) )
}

#'Subsetting one or more annotations from the ceptometer file given as a dataframe
#'
#'@param df A dataframe readed from the ceptometer output file
#'@param tName A string in regex format indicating the name of the annotation(s) to be extracted
#'@param nRecords Num of Records per annotation
#'@param segments A vector indicating the PAR bar segments selected
#'@param asDf function output as dataframe or list of dataframes
#'@param raw Raw outputdata
#'@param parBarStats include some stastistics of the PAR bar in the output
#'@return A data frame containing a subset of the annotations
#'
SubsetAnn <- function(df,tName,nRecords=3,segments=1:8,asDf=TRUE,raw=FALSE,parBarStats = FALSE){
  # Get the annotation boundaries 
  annBndIdx <- c(0,which(!is.na(df$Annotation))) 
  annBnd <- data.frame(Annotation = df$Annotation[!is.na(df$Annotation)],initB = annBndIdx[1:(length(annBndIdx)-1)]+1,finishB = annBndIdx[2:length(annBndIdx)])


  annBnd <- annBnd[ grepl(tName,annBnd$Annotation,ignore.case=TRUE), ]
  
  if(asDf){ return (do.call('rbind',apply(annBnd,1,SubsetByBnd,df,nRecords,segments,raw,parBarStats))) }

  else{ apply(annBnd,1,SubsetByBnd,df,nRecords,segments,raw,parBarStats) } 
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
#'@param rawRecords a dataframe returned by AnnProc setting the raw flag as TRUE
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
#'@param records a dataframe returned by AnnProc setting the raw flag as TRUE
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