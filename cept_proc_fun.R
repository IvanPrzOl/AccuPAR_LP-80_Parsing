#Function to process one single annotation, the number of records may vary
#by default it process strata measurements, it means 7 records per annotation in
#'ABV','BLW','ABV','ABV','ABV','ABV','ABV' order
#return a list -> OriginIdx,fecha,plot,Arriba,Reflec,Spg,HB,H2,H3,Abajo
#if the annotation isn't consistent, return OriginIdx,Plot
AnnotationProc <- function(antn,nRecords = 7,segments = 1:8){ 
  recordOrder <- list("3"=c("ABV","BLW","ABV"),
                      "7"=c("ABV","BLW","ABV","ABV","ABV","ABV","ABV"))
  labelMeans <- list("3" = c("ARRIBA","REFLEJADO","ABAJO"),
                      "7" = c("ARRIBA","REFLEJADO","ESPIGA","HB","H2","H3","ABAJO"))

  #Check consistency if the input data
  #Return a list with the origin index and the annotation name
  obs <- nrow(antn) 
  if(obs < nRecords){ #missed data, 
    return(list(originIdx = as.numeric(row.names(antn))[obs],Anotacion = antn$Annotation[obs]))
  }
  else if(any(!(antn$Record.Type[(obs-nRecords):(obs-1)] == recordOrder[[as.character(nRecords)]]))){ #Wrong record order
    return(list(originIdx = as.numeric(row.names(antn))[obs],Anotacion = antn$Annotation[obs]))
  }
  
  # Select PAR segments to process, this can be an optinal parameter
  parSegments = c("Segment.1.PAR","Segment.2.PAR","Segment.3.PAR","Segment.4.PAR",
                    "Segment.5.PAR","Segment.6.PAR","Segment.7.PAR","Segment.8.PAR")[segments]

  parMatrix = antn[(obs-nRecords):(obs-1),parSegments] #Numeric values of PAR segments
  parMeans = rowMeans(parMatrix)
  names(parMeans) <- labelMeans[[as.character(nRecords)]] #Average in each strata
  
  parSD <- apply(parMatrix,1,sd)
  names(parSD) <- c("SD_ARRIBA","SD_REFLEC","SD_ABAJO")

  parMin <- apply(parMatrix,1,min)
  names(parMin) <- c("Min_ARRIBA","Min_REFLEC","Min_ABAJO")
  parMax <- apply(parMatrix,1,max)
  names(parMax) <- c("Max_ARRIBA","Max_REFLEC","Max_ABAJO")
  LI <- ((parMeans["ARRIBA"]-parMeans["REFLEJADO"])-parMeans["ABAJO"])/(parMeans["ARRIBA"]-parMeans["REFLEJADO"])*100
  names(LI) <- "LI"

  #Output list structusubre
  out <- (c(list(originIndex = as.numeric(row.names(antn))[obs], 
                Fecha = trunc(antn$Date.and.Time[obs],"mins"),
                Anotacion = antn$Annotation[obs]),
         parMeans,parSD,parMin,parMax,LI))
  return(out)
}