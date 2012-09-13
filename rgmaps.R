library(XML)
options(stringsAsFactors=FALSE)

baseurl <- "http://maps.googleapis.com/maps/api/distancematrix/xml?"

#Return the distance of a trip from the origin to the destination.
getGMdistance <- function(origin, destination, mode="driving", language="en", units="metric" ){
  
  #Fix Origin and Destinations
  origin <- paste("origins=",gsub(" ", "+", origin),sep="")
  destination <- paste("destinations=",gsub(" ", "+", destination),sep="")
  
  url <- paste(baseurl, origin,"&",destination,"&mode=",mode,"&language=",language,"&units=",units,"&sensor=false",sep="")
  doc <- xmlTreeParse(url, useInternal=TRUE)
  distance <- NA
  try(distance <- xmlValue((doc["//distance/value"])[[1]]),silent=TRUE)
 # if (!is.numeric(distance)) distance <- NA
  return(distance)
  }



#Return the duration (in seconds) of a trip from the origin to the destination.
getGMduration <- function(origin, destination, mode="driving", language="en", units="metric" ){
  
  #Fix Origin and Destinations
  origin <- paste("origins=",gsub(" ", "+", origin),sep="")
  destination <- paste("destinations=",gsub(" ", "+", destination),sep="")
  
  url <- paste(baseurl, origin,"&",destination,"&mode=",mode,"&language=",language,"&units=",units,"&sensor=false",sep="")
  doc <- xmlTreeParse(url, useInternal=TRUE)
  duration <- xmlValue((doc["//duration/value"])[[1]])
  return(duration)
}



# Return the name of the place as google interprets it.
getGMname <- function(text){
  mode="driving"; language="en"; units="metric"; 
  #Fix Origin and Destinations
  origin <- paste("origins=",gsub(" ", "+", text),sep="")
  destination <- paste("destinations=",gsub(" ", "+", "34000000 brazil"),sep="")
  
  url <- paste(baseurl, origin,"&",destination,"&mode=",mode,"&language=",language,"&units=",units,"&sensor=false",sep="")
  doc <- xmlTreeParse(url, useInternal=TRUE)
  name <- xmlValue((doc["//origin_address"])[[1]])
  return(name)
}

# Same as getGMname but for lists.
getGMnames <- function(list){
  #default parameters
  mode="driving"; language="en"; units="metric"
  
  #create origin and destination objects
  origins <- paste("origins=",paste(sub(pattern=" ", replacement="+", x=list), collapse="|"),sep="")
  destinations <- paste("destinations=",paste(sub(pattern=" ", replacement="+", x=list), collapse="|"),sep="")
  
  #generate the xml address
  url <- paste(baseurl, origins,"&",destinations,"&mode=",mode,"&language=",language,"&units=",units,"&sensor=false",sep="")
  
  #parse xml url
  doc <- xmlParse(url, getDTD=TRUE)
  
  #get all nodes with the origin_names
  names <- doc["//origin_address"]
  
  #replace names original names by the google names
  for(i in seq_along(names)){
    list[i] <- xmlValue(names[[i]])
  }
  return(list)
}

###########################################################################
# Generate a data frame with the distances between all places in the list #
###########################################################################
getGMdistancematrix <- function(list, mode="driving", language="en", units="metric"){
  
  #default parameters
  #mode="driving"; language="en"; units="metric"
  
  #create output object
  output <- as.data.frame(matrix(ncol=length(list),nrow=(length(list))))
  
  #create origin and destination objects
  origins <- paste("origins=",paste(sub(pattern=" ", replacement="+", x=list), collapse="|"),sep="")
  destinations <- paste("destinations=",paste(sub(pattern=" ", replacement="+", x=list), collapse="|"),sep="")
  
  ############################
  # generate the xml address #
  ############################
  
  url <- paste(baseurl, origins,"&",destinations,"&mode=",mode,"&language=",language,"&units=",units,"&sensor=false",sep="")
  #parse the xml 
  doc <- xmlParse(url, getDTD=TRUE)
 
  status <- xmlValue(doc["//DistanceMatrixResponse/status"][[1]])
  if(status!="OK") print(status)
  
  ##############################
  # rename matrix col and rows #
  ##############################
  
  # get the names
  names <- doc["//origin_address"]
  
  #replace names original names by the google names
  for(i in seq_along(names)){
    list[i] <- xmlValue(names[[i]])
  }
  colnames(output) <- list
  rownames(output) <- list
  
  ##########################
  # Fill the output matrix #
  ##########################
  
  #extract row nodes
  rows <- doc["//row"]
  #parse each row 
  for (r in seq_along(rows)){
    row <- rows[[r]]
    #parse each col in the row
    for(c in seq_along(names(row))){
      col <- row[[c]]
      #get distance value
      output[r,c] <- xmlValue(col[[3]][[1]])
    }
  }
  return(output)
}