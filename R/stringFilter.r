#'Filter cells containing a string from a dataset
#'
#'@param list List of strings you'd like to filter out of a dataset
#'@param column Column you'd like to search for the strings in
#'@param data Data you'd like to filter
#'
#'@examples stringFilter(list, column, data)
#'
#'@export stringFilter
stringFilter <- function(list, column, data){
    for(i in list){
      df2 <- data[!stringr::str_detect(data[[column]], stringr::regex(paste(list, collapse = "|"),
                                                                     ignore_case = TRUE)),]
      return(df2)
    }
  }

