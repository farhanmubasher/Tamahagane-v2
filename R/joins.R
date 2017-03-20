library(plyr)
tamahagane.apply.join<- function(filePath.1,filePath.2,joinType)
{
  x <- read.csv(filePath.1)
  y <- read.csv(filePath.2)
  joinType <- fromJSON(joinType)
  result <- join(x, y, type= joinType)
  result[is.na(result)] <- "?"
  if(nrow(result) == 0){

    return("No Records Found !!")
  }
  else{
    return(result)
  }
}
