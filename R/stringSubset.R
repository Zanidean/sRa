#'Subset a dataframe by a string
#'@description Use when you want to use a regex search to filter an element by a string.
#'Can be slow using regex, so try to avoid using it inside loops.
#'@param list List of strings you'd like to subset a dataset by
#'@param column Column you'd like to search for the strings in
#'@param data Data you'd like to subset
#'
#'@examples stringSubset(list, column, data)
#'
#'@export stringSubset
stringSubset<- function(list, column, data){
  for(i in list){
    df2 <- data[stringr::str_detect(data[[column]], stringr::regex(paste(list, collapse = "|"),
                                                                   ignore_case = TRUE)),]
    return(df2)
  }
}
