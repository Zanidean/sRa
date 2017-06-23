#'Generate a different dataframe for every cohort year
#'@description Not totally useful, but there if you need it.
#'@param Range Range of years in dataset
#'@param CohortCol Column which contains the indentifier of the cohort
#'@param YearCol Column which contains the year
#'@param RetentionStatus The column which contains the retention status of the individual
#'@param dataframe The data itself
#'@param interval The distance from the cohort year you'd like to look at. (ex. look at the retention status for
#'        those cohort year 2008 in the year 2012, interval = 4)
#'@examples cohortSeperator(2008:2014,
#'                          CohortCol = "Cohort Year",
#                           YearCol = "Year",
#'                          RetentionStatus = "RetentionStatus",
#'                          dat, interval = 1)
#'@export cohortSeperator
cohortSeperator <- function(Range, CohortCol, YearCol, RetentionStatus, dataframe, interval){
  for(i in Range){
    df <- dataframe
    df <- df[df[[CohortCol]] == i & df[[YearCol]] == i+interval,]
    df <- dplyr::select(df, RetentionStatus)
    df <- as.data.frame(table(df$RetentionStatus), responseName = "Count")
    df$CohortYear <- i
    df$Year <- i+interval
    df$Interval <- interval
    assign(paste("Cohort", i, "(",interval, ")", sep=""), df, envir = .GlobalEnv)
  }
}

