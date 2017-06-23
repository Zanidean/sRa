#'FOIP your data!
#'@description Really can be useful for protecting privacy, but have to be aware of how to use it safely.
#'@param column column to subset on
#'@param dataframe data to be foiped

#'
#'@examples foipR(program, survey)
#'
#'@export foipR
foipR <- function(column, dataframe){
  df <- dataframe
  dfsum <- dplyr::select_(dataframe, as.name(column))
  dfsum <- as.data.frame(table(dfsum[[column]]), responseName="Count")
  dfsum <- dfsum[dfsum$Count >=5,]
  df <- dataframe[df[[column]] %in% dfsum$Var1,]
  return(df)
}
