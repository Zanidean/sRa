#'Count the number of occurances of strings in a column
#'@description Not super useful, but there when you need it.
#'@param list The list of strings you want to count and put into dataframe
#'@param column The columnname you'd like to search
#'@param data Dataset to be searched
#'
#'@examples multipleResponseCountList(c("medicine hat", "lethbridge", "calgary"),
#'          "Cities", "Bathroom Habits", "Bathroom Use Count")
#'@export multipleResponseCountList
multipleResponseCountList<- function(list, column, data){
  for(i in list){
    df <- data[stringr::str_detect(data[[column]], stringr::regex(paste(list, collapse = "|"),
                                                        ignore_case = TRUE)),]
    df <- as.data.frame(table(df[[column]]), responseName = "Count")
    df <- df[df$Count > 0,]
    df <- dplyr::rename(df, column = Var1)
    df1 <- sum(df$Count)
    return(df)
  }
}
