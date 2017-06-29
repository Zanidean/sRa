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
xmR <- function(dataframe, measure, interval, recalc) {
  
  if (missing(interval)) {interval <- 5}
  if (missing(recalc)) {recalc = F}

  df <- as.data.frame(dataframe, stringsAsFactors = F)
  interval <- round2(interval, 0)
  df$Order <- seq(1, nrow(df), 1)
  
  df[[measure]] <- as.numeric(as.character(df[[measure]]))
  
  points <- seq(1,interval,1)
  
  #starting conditions
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
  
  #run subsetter
  run_subset <- function(subset, order){
    #subset[[order]] <- as.integer(subset[[order]])
    breaks <- c(0, which(diff(subset[[order]]) != 1), length(subset[[order]])) 
    d <- sapply(seq(length(breaks) - 1), function(i) subset[[order]][(breaks[i] + 1):breaks[i+1]]) 
    if(is.matrix(d)){d <- split(d, rep(1:ncol(d), each = nrow(d)))}
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
      subset <- subset[subset[[order]] %in% run[[1]],]
    } else {
      subset <- subset[subset[[order]] %in% d[[1]],]
    }
  return(subset)
  }
  
  #recalculator
  recalculator <- function(dat, subset, order, length, message){
    if (length == 8){
      int <- 5
      } else if (length == 4){
      int <- 3
      }
    if (nrow(subset) >= length) {
      start <- min(subset[[order]], na.rm = T)
      if(length == 8){end <- start+4} else if(length == 4){end <- start+3}
      lastrow <- max(dat$Order, na.rm = T)
      new_cnt <- mean(subset[[measure]][1:int], na.rm = T)
      new_mv_rng <- subset$`Moving Range`[1:int]
      new_av_mv_rng <- mean(new_mv_rng, na.rm = T)
      dat$`Average Moving Range`[start:lastrow] <- new_av_mv_rng
      dat$`Central Line`[start:lastrow] <- new_cnt
      print(message)
      dat <- limits(dat)
      points <- c(points, c(start:end))
      points <- c(min(points):max(points))
      assign("points", points, envir = parent.frame())
      return(dat)
    } else {return(dat)}
  }

  #runs application
  runs <- function(dat, run = c("short", "long"), side = c("upper", "lower")){
    if(run == "short"){l <- 4} else if (run == "long"){l <- 8}
      if(side == "upper" && run == "long"){
        dat_sub <- dat %>%
          filter(., .[[measure]] > `Central Line` & !(Order %in% points)) %>%
          arrange(., Order)
      } else if (side == "lower" && run == "long"){
        dat_sub <- dat %>%
          filter(., .[[measure]] < `Central Line` & !(Order %in% points)) %>%
          arrange(., Order)
      }
      if(side == "upper" && run == "short"){
        dat_sub <- dat %>%
          filter(., (abs(.[[measure]] - `Central Line`) > 
                       abs(.[[measure]] - `Upper Natural Process Limit`))) %>%
          filter(., !(Order %in% points)) %>%
          arrange(., Order)
      } else if (side == "lower" && run == "short"){
        dat_sub <- dat %>%
          filter(., (abs(.[[measure]] - `Central Line`) > 
                       abs(.[[measure]] - `Lower Natural Process Limit`))) %>%
          filter(., !(Order %in% points)) %>%
          arrange(., Order)
      }
    dat_sub <- run_subset(dat_sub, "Order")
    rep <- nrow(dat_sub)
    while(rep >= l){
      mess <- paste0(run, ": ", side)
      dat <- recalculator(dat, dat_sub, "Order", l, mess)
      assign("points", points, envir = parent.frame())
      dat_sub <- dat %>%
        filter(., .[[measure]] > `Central Line` & !(Order %in% points)) %>%
        arrange(., Order)
      dat_sub <- run_subset(dat_sub, "Order")
      rep <- nrow(dat_sub)
    } 
    return(dat)
  }
  
  if ((nrow(df)) >= interval) {
    #if no recalculation of limits is desired
    if(recalc == F){df <- starter(df)}
    #if recalculation of limits desired
    if(recalc == T){
    #calculate inital values
      df <- starter(df)
      df <- runs(df, "long", "upper")
      df <- runs(df, "short", "upper")
      df <- runs(df, "long", "lower")
      df <- runs(df, "short", "lower")
      
    }
    df$`Central Line`[(nrow(df)-3):nrow(df)] <- 
      df$`Central Line`[(nrow(df)-4)]
    df$`Average Moving Range`[(nrow(df)-3):nrow(df)] <-
      df$`Average Moving Range`[(nrow(df)-4)]
    df <- limits(df)
  }
 return(df)
}
