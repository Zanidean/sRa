#'Generate the XMR data for any time-series data. BUT DO NOT USE THIS
#'@description Will be used to calculate XMR data.
#'
#'@param df The dataframe containing the time-series data. Must be in long format.
#'At least 1 variable for time and one variable for measure.
#'@param measure The column containing the measure. Must be in numeric format.
#'@param interval The interval you'd like to use to calculate the averages. Defaults to 5.
#'@param recalc Logical if you'd like it to recalculate bounds. Defaults to False.

#'@examples dat.xmr <- xmR(dat, "Measure", 5)
#'dat.xmr <- dat %>% group_by(., Program, Variable) %>% do(xmR(., "Measure"))
#'
#'@export xmR
xmR <- function(df, measure, interval, recalc) {
  #limits
  limits <- function(df){
    df$`Lower Natural Process Limit` <-
      df$`Central Line` - (df$`Average Moving Range` * 2.66)
    df$`Lower Natural Process Limit`[1] <- NA
    df$`Lower Natural Process Limit` <-
      ifelse(df$`Lower Natural Process Limit` <= 0,
             0,
             df$`Lower Natural Process Limit`)
    df$`Upper Natural Process Limit` <-
      df$`Central Line` + (df$`Average Moving Range` * 2.66)
    df$`Upper Natural Process Limit`[1] <- NA
    return(df)
  }
  
  if (missing(interval)) {interval <- 5}
  interval <- round2(interval, 0)
  df$Order <- seq(1, nrow(df), 1)
  
  if (missing(recalc)) {recalc = F}
  
  if ((nrow(df)) >= interval) {
    
    if(recalc == F){
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
    df <- limits(df)
    }
    
    if(recalc == T){
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
      df <- limits(df)
    ###central line recalculate
    #longrun - over
      df_sub <- df %>%
        filter(., .[[measure]] > `Central Line` & Order > interval) %>%
        arrange(., Order)
      if (nrow(df_sub) >= 8) {
        df_sub <- df_sub %>%
          mutate(., Num = Order - lead(Order, 1),
                 Num = Num * -1) %>% 
          filter(., Num == 1)
        df_sub_length <- nrow(df_sub)
        if (df_sub_length >= interval) {
          start <- min(df_sub$Order, na.rm = T)
          lastrow <- max(df_sub$Order, na.rm = T)
          
          new_cnt <- mean(df_sub[[measure]][df_sub$Order %in% c(start:(start+4))], na.rm = T)
          
          new_mv_rng <- df_sub$`Moving Range`
          new_mv_rng <- new_mv_rng[!is.na(new_mv_rng)]
          new_av_mv_rng <- new_mv_rng[2:length(new_mv_rng)]
          new_av_mv_rng <- mean(new_mv_rng)
          df$`Average Moving Range`[start:lastrow] <- new_av_mv_rng
          df$`Central Line`[start:lastrow] <- new_cnt
          print("Recalc Over")
        }
      }
        
        df$`Central Line`[(nrow(df)-3):nrow(df-3)] <- df$`Central Line`[(nrow(df)-4)]
        df$`Average Moving Range`[(nrow(df)-3):nrow(df-3)] <- df$`Average Moving Range`[(nrow(df)-4)]
        df <- limits(df)
    }
      
  }
 return(df)
}
