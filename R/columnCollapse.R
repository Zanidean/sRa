#'Collapse responses from a multiple response into one column
#'
#'@param columns columns which contain a string to be collapse
#'@param colname column name to output to
#'@param dataframe Dataframe to be collapsed
#'
#'@examples columnCollapse("city moved to", "cities moved to", GradSurvey)
#'
#'@export columnCollapse
columnCollapse <- function(columns, colname, dataframe){
  df <- tidyr::unite(dataframe, output, contains(columns), sep="#", remove=T)
  df$output <- gsub("NA", "", df$output)
  df$output <- gsub("#", "", df$output)
  names(df)[names(df) == "output"] <- colname
  return(df)
}
