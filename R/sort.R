
library(lubridate)

tamahagane.apply.sort<- function(filePath, columName, sortType)
{
  Dataset <- read.csv(filePath)
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
      return ("Date Format is not Correct")
    }
    else
    {

      if(sortType=="ASC" || sortType=="")
      {
        return (Dataset[order(Dataset[, columName]) ,])
      }
      else(sortType=="DESC")
      {
        return (Dataset[rev(order(Dataset[, columName]))  ,])
      }

    }

  }

  else if (is.null(check) || check== FALSE){

    if(sortType=="ASC" || sortType=="")
    {
      return (Dataset[order(Dataset[, columName]) ,])
    }
    else(sortType=="DESC")
    {
      return (Dataset[rev(order(Dataset[, columName]))  ,])
    }
  }


}
