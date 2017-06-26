#'Wrapper for summary(as.factor(Vec))
#'@description Just provides a simple frequency table of a vector
#'@param vector column to subset on

#'
#'@examples smry(df$Program)
#'
#'@export smry
smry <- function(vector){
  print(summary(as.factor(vector)))
}
