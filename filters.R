
tamahagane.apply.filter.csvVersion<- function(filePath, columNames, filterType, Values)
{
  converted.Dataset <- read.csv(filePath)
  cols<- fromJSON(columNames)
  converted.filterType <- fromJSON(filterType)
  converted.Values <- fromJSON(Values)
  
  j<-1
  while(j<=length(converted.Values))
  {
    if(converted.filterType[j] == "STARTWITH" | converted.filterType[j] == "ENDWITH")
    {
      obj.start <- paste("^", converted.Values[j] , sep = "")
     obj.end <- paste(converted.Values[j] , "$", sep = "")
    }
    
    j<- j+1
  }

  i<- 1
  while(i <= length(converted.Values) )
  {
    if(converted.filterType[i] == "EQUAL"){
      converted.Dataset <-  subset(converted.Dataset, converted.Dataset[cols[i]] == converted.Values[i])
    }
    else if(converted.filterType[i] == "NOTEQUAL"){
      converted.Dataset <-  subset(converted.Dataset, converted.Dataset[cols[i]] != converted.Values[i])
    } 
    else if(converted.filterType[i] == "ISIN"){
      converted.Dataset <-  subset(converted.Dataset, converted.Dataset[cols[i]] == converted.Values[i])
    }
    else if(converted.filterType[i] == "ISNOTIN"){
      converted.Dataset <-  subset(converted.Dataset, converted.Dataset[cols[i]] != converted.Values[i])
    }
    else if(converted.filterType[i] == "LESSTHAN"){
      converted.Dataset <-  subset(converted.Dataset, converted.Dataset[cols[i]] < converted.Values[i])
    }
    else if(converted.filterType[i] == "GREATERTHAN"){
      converted.Dataset <-  subset(converted.Dataset, converted.Dataset[cols[i]] > converted.Values[i])
    }
    else if(converted.filterType[i] == "STARTWITH"){
      converted.Dataset <- converted.Dataset[grep(obj.start, converted.Dataset[,cols[i]]) ,]
    }
    else if(converted.filterType[i] == "ENDWITH"){
      converted.Dataset <- converted.Dataset[grep(obj.end, converted.Dataset[,cols[i]]) ,]
    }
    
    i<- i+1
  }
  
  if(nrow(converted.Dataset) == 0){
      
    return("No Records Found !!")
  }
  else{
  return(converted.Dataset)
  }
}

