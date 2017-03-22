library(jsonlite)
library(lubridate)

tamahagane.sort.Date.csvVersion <- function(inputFilePath, columName, sortType, outputFilePath)
{
  Dataset <- read.csv(inputFilePath)
  columName <- fromJSON(columName)
  sortType <- fromJSON(sortType)
  
  check  <- tryCatch({
    !all(is.na(as.Date(Dataset[, columName],format="%d/%m/%Y")))
  }, error=function(e){NULL})

  if(!is.null(check) && check!= FALSE)
  {
    Dataset[columName] <- dmy(Dataset[, columName])
    count.misssing.values  <- colSums(is.na(Dataset[columName]))
    if(count.misssing.values >= 1)
    {
      return( write.table( "Date Formate is Not Correct !!", file = outputFilePath, sep = ",", quote = FALSE, row.names = FALSE, col.names = FALSE))
    }
    else
    {
      
      if(sortType=="ASC" || sortType=="")
      {
        return (write.csv(Dataset[order(Dataset[, columName]) ,], file = outputFilePath, quote = FALSE, row.names = FALSE) )
      }
      else(sortType=="DESC")
      {
        return (write.csv(Dataset[rev(order(Dataset[, columName]))  ,] , file = outputFilePath, quote = FALSE, row.names = FALSE))
      }
        
      }
    
  }
  else if (is.null(check) || check== FALSE){
    
    if(sortType=="ASC" || sortType=="")
    {
      return (write.csv(Dataset[order(Dataset[, columName]) ,] , file = outputFilePath, quote = FALSE, row.names = FALSE))
    }
    else(sortType=="DESC")
    {
      return (write.csv(Dataset[rev(order(Dataset[, columName]))  ,], file = outputFilePath, quote = FALSE, row.names = FALSE))
    }
  }
}
