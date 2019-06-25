#Function to process one single annotation, the number of records may vary
#by default it process strata measurements, it means 7 records per annotation in
#'ABV','BLW','ABV','ABV','ABV','ABV','ABV' order
#return a list -> OriginIdx,fecha,plot,Arriba,Reflec,Spg,HB,H2,H3,Abajo
#if the annotation isn't consistent, return OriginIdx,Plot
AnnotationProc <- function(antn,nRecords = 7, rOrder = c('ABV','BLW','ABV','ABV','ABV','ABV','ABV')){ 

  obs <- nrow(antn) 
  if(obs < nRecords){ #missed data
    return(list(originIdx = as.numeric(row.names(antn[obs,]) ),Anotacion = antn$Annotation[obs]))
  }
  else if(any(!(antn$Record.Type[(obs-nRecords):(obs-1)] == rOrder))){ #Wrong record order
    return(list(originIdx = as.numeric(row.names(antn[obs,]) ),Anotacion = antn$Annotation[obs]))
  }
  
  # Select PAR segments to process, this can be an optinal parameter
  parSegments = c("Segment.1.PAR","Segment.2.PAR","Segment.3.PAR","Segment.4.PAR",
                    "Segment.5.PAR","Segment.6.PAR","Segment.7.PAR","Segment.8.PAR")
  parMatrix = antn[(obs-nRecords):(obs-1),parSegments] #Numeric values of PAR segments
  parMeans = rowMeans(parMatrix)
  names(parMeans) <- c("ARRIBA","REFLEJADO","ESPIGA","HB","H2","H3","ABAJO") #Average in each strata
  
  #Output list structure
  ot <- (c(list(originIdx = as.numeric(row.names(antn[obs,])), 
                Fecha = format.POSIXct(antn$Date.and.Time[obs],"%Y-%m-%d %I:%M"),
                Anotacion = antn$Annotation[obs]),
         parMeans))
  return(ot)
}