library(jsonlite)
library(plyr)
tamahagane.apply.join <- function(inputFilePath.1, inputFilePath.2, joinType, outputFilePath)
{
  x <- read.csv(inputFilePath.1)
  y <- read.csv(inputFilePath.2)
  joinType <- fromJSON(joinType)
  result <- join(x, y, type= joinType)
  result[is.na(result)] <- "?"
  if(nrow(result) == 0){
      
    return( write.table( "No Records Found !!", file = outputFilePath, sep= ",", quote = FALSE, row.names = FALSE, col.names = FALSE))
  }
  else{
    return(write.csv(result, file = outputFilePath, quote = FALSE, row.names = FALSE) )
  }
}
