#'Generate the XMR data for any time-series data. BUT DO NOT USE THIS
#'@description Will be used to calculate XMR data.
#'
#'@param df The dataframe containing the time-series data. Must be in long format.
#'At least 1 variable for time and one variable for measure.
#'@param measure The column containing the measure. Must be in numeric format.
#'@param interval The interval you'd like to use to calculate the averages. Defaults to 5.
#'@examples dat.xmr <- xmR(dat, "Measure", 5)
#'dat.xmr <- dat %>% group_by(., Program, Variable) %>% do(xmR2(., "Measure"))
#'
#'@export xmR
xmR <- function(df, measure, interval) {

  if (missing(interval)) {interval <- 5}
  interval <- round2(interval, 0)
  df$Order <- seq(1, nrow(df), 1)


  if ((nrow(df)) >= interval) {
    df$`Central Line` <- mean(df[[measure]][1:interval])
    original_cnt <- mean(df[[measure]][1:interval])
    #moving range
    df$`Moving Range` <- NA
    for (i in 1:(nrow(df) - 1)) {
      df$`Moving Range`[i + 1] <- abs(df[[measure]][i] - df[[measure]][i + 1])
    }
    df$`Average Moving Range` <- mean(df$`Moving Range`[2:(1 + interval)])
    df$`Average Moving Range`[1] <- NA
    original_avg_mving_rng <- mean(df$`Average Moving Range`[1:interval], na.rm = T)

    ###central line recalculate

    #longrun - over
    #condition
    df_sub <- df %>%
      filter(., .[[measure]] > `Central Line`, Order > interval) %>%
      arrange(., Order)
    #print(df_sub)
    #get row to only include measures to recalculate
    if (nrow(df_sub) >= 8) {
      df_sub <- df_sub %>%
        mutate(., Num = Order - lead(Order, 1),
               Num = Num * -1)

      print(df_sub)
      #select row
      df_sub <- df[df[[measure]] %in% df_sub[[measure]], ]
      df_sub_length <- nrow(df_sub)
      if (df_sub_length >= interval) {
        start <- min(df_sub$Order, na.rm = T)
        lastrow <- max(df_sub$Order, na.rm = T)
        new_cnt <- mean(df_sub[[measure]][start:lastrow], na.rm = T)
        new_av_mv_rng <- df_sub$`Moving Range`
        new_av_mv_rng <- new_av_mv_rng[!is.na(new_av_mv_rng)]
        new_av_mv_rng <- new_av_mv_rng[2:length(new_av_mv_rng)]
        new_av_mv_rng <- mean(new_av_mv_rng)
        df$`Average Moving Range`[start:lastrow] <- new_av_mv_rng
        df$`Central Line`[start:lastrow] <- new_cnt
      }
    }
  }
 return(df)
}
