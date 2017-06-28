#'Generate the XMR data for any time-series data. BUT DO NOT USE THIS
#'@description Will be used to calculate XMR data.
#'
#'@param df The dataframe containing the time-series data. 
#'Must be in long format.
#'At least 1 variable for time and one variable for measure.
#'@param measure The column containing the measure. Must be in numeric format.
#'@param interval The interval you'd like to use to calculate the averages. 
#'Defaults to 5.
#'@param recalc Logical if you'd like it to recalculate bounds. 
#'Defaults to False.

#'@examples dat.xmr <- xmR(dat, "Measure", 5)
#'dat.xmr <- dat %>% 
#'           group_by(., Program, Variable) %>% 
#'           do(xmR(., measure = "Retention Rate", interval = 5, recalc = T))
#'
#'@export xmR
xmR <- function(df, measure, interval, recalc) {
  
  if (missing(interval)) {interval <- 5}
  if (missing(recalc)) {recalc = F}
  interval <- round2(interval, 0)
  df$Order <- seq(1, nrow(df), 1)
  points <- seq(1,interval,1)
  
  
  starter <- function(data){
  data$`Central Line` <- mean(data[[measure]][1:interval])
  original_cent <- mean(data[[measure]][1:interval])
  #moving range
  data$`Moving Range` <- NA
  for (i in 1:(nrow(data) - 1)) {
    data$`Moving Range`[i + 1] <- abs(data[[measure]][i] - data[[measure]][i + 1])
  }
  data$`Average Moving Range` <- mean(data$`Moving Range`[2:(1 + interval)])
  data$`Average Moving Range`[1] <- NA
  original_avg_mving_rng <- mean(data$`Average Moving Range`[1:interval], na.rm = T)
  data <- limits(data)
  return(data)
  }
  
  #limits calculator
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
  
  #credit to Marc Schwartz for subsetting runs
  run_subset <- function(subset, order){
    breaks <- c(0, which(diff(subset[[order]]) != 1), length(subset[[order]])) 
    d <- sapply(seq(length(breaks) - 1), function(i) subset[[order]][(breaks[i] + 1):breaks[i+1]]) 

    if(is.matrix(d)){d <- split(d, rep(1:ncol(d), each = nrow(d)))}
    #print(d)
    if(length(d) > 1) {
      rns <- c()
      idx <- c()
      for(i in 1:length(d)){
        a <- length(d[[i]])
        rns <- c(rns, a)
        idx <- c(idx, i)
      }
      runs <- data.frame(idx, rns)
      idx <- unique(runs$idx[runs$rns == max(runs$rns)])
      run <- d[idx]
      #print(run[[1]])
      #print(subset)
      subset <- subset[subset[[order]] %in% run[[1]],]
    } else {
      #print(d)
      subset <- subset[subset[[order]] %in% d[[1]],]
      #print(subset)
    }
  return(subset)
  }
  
  #recalculator
  recalculator <- function(subset, order, length, message){
    if (nrow(subset) >= length) {
      start <- min(subset[[order]], na.rm = T)
      end <- start+4
      lastrow <- max(df$Order, na.rm = T)
      new_cnt <- mean(subset[[measure]][1:5], na.rm = T)
      new_mv_rng <- subset$`Moving Range`[1:5]
      new_av_mv_rng <- mean(new_mv_rng)
      print(new_av_mv_rng)
      df$`Average Moving Range`[start:lastrow] <- new_av_mv_rng
      df$`Central Line`[start:lastrow] <- new_cnt
      print(message)
      df <- limits(df)
      points <- c(points, c(start:end))
      return(df)
    } else {return(df)}
  }


  if ((nrow(df)) >= interval) {
    #if no recalculation of limits is desired
    if(recalc == F){df <- starter(df)}
    
    #if recalculation of limits desired
    if(recalc == T){
    #calculate inital values
    df <- starter(df)

    ###longrun - over
      df_sub <- df %>%
        filter(., .[[measure]] > `Central Line` & !(Order %in% points)) %>%
        arrange(., Order)
      if(nrow(df_sub) >= 8){
        df_sub <- run_subset(df_sub, "Order")
        df <- recalculator(df_sub, "Order", 8, "Over")
      }


  
        
        
        
    df$`Central Line`[(nrow(df)-3):nrow(df-3)] <- 
      df$`Central Line`[(nrow(df)-4)]
    df$`Average Moving Range`[(nrow(df)-3):nrow(df-3)] <-
      df$`Average Moving Range`[(nrow(df)-4)]
    df <- limits(df)
    }
      
  }
 return(df)
}
