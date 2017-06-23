#'Generate a Simpson Diversity Index Number
#'
#'@param column Column which contains the count/tally of people from each country.
#'@param data Dataset
#'@param type The type of Simpson index you'd like to generate. Reciprocal is 1/Pa, Dominance is 1-Pa, Blank is just the Simpson Index
#'
#'@examples SDI("Unique Student Static", EnrolmentData, type = "reciprocal")
#'
#'@export SDI
SDI <- function(col, data, type){
  data$number2 <- data[col]*(data[col]-1)
  Numer <- sum(data$number2)
  Denom <- sum(data[col])*(sum(data[col])-1)
  if(type == "dominance"){
    DI <- 1 - (Numer/Denom)}
  else if(type == "reciprocal"){
    DI <- 1/(Numer/Denom)
  }
  else if(type == "SI"){
    DI <- Numer/Denom
  }
  return(DI)
}
