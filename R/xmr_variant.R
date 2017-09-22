#'Generate the XMR data for any time-series data.
#'@description Will be used to calculate XMR data using the "wecca" rules. 
#'
#'@param df The dataframe containing the time-series data. 
#'Must be in tidy format.
#'At least 1 variable for time and one variable for measure.
#'@param measure The column containing the measure. Must be in numeric format.
#'@param interval The interval you'd like to use to calculate the averages. 
#'Defaults to 5.
#'@param recalc Logical if you'd like it to recalculate bounds. Defaults to False
#'
#'
#'Long Runs - 8 consecutive points above or below the central line and after the initial 5 points. If a long run is present, then use the first 5 points to recalculathe new bounds, after which these points are never to be used again. These newly calculated bounds begin at the first point in the long run and extend to the end of the chart, unless re-calculated after the end of the run. If the long run is longer than 8 points, it should still be enveloped by the bounds of normal variation to be re-calculated.
#'
#'
#'Short Runs - 2 of 3 points that are closer to either bound than they are to the central line and after the initial 5 points. If a short run is made by points not previously used in a calculation, then use those points to calculate new bounds and exclude them from any further calculations. A single point may be used in two calculations if Like long runs, these newly calculated bounds begin at the first point in the short run, and extend to the end of the chart unless re-calculated after the end of the run.
#'
#'@examples 
#'
#'dat.xmr <- xmR_variant(dat, "Measure", 5)
#'dat.xmr <- dat %>% 
#'           group_by(., Program, Variable) %>% 
#'           do(xmR_variant(., measure = "Retention Rate", 
#'           interval = 5, recalc = T))           
#'xmR_chart(., "Time", "Measure", "Facet")
#'
#'@export xmR_variant
xmR_variant <- function(df, measure, interval, recalc, testing) {
  
  if (missing(interval)) {interval <- 5}
  if (missing(recalc)) {recalc <- F}
  if (missing(testing)) {testing <- F}
  
  round2 <- function(x, n) {
    posneg = sign(x)
    z = abs(x)*10^n
    z = z + 0.5
    z = trunc(z)
    z = z/10^n
    z*posneg
  }
  
  interval <- round2(interval, 0)
  df$Order <- seq(1, nrow(df), 1)
  points <- seq(1,interval,1)
  
  #starting conditions
  starter <- function(dat){
    original_cent <- mean(dat[[measure]][1:interval])
    dat$`Central Line` <- original_cent
    #moving range
    dat$`Moving Range` <- abs(dat[[measure]] - lag(dat[[measure]], 1))
    for (i in 1:(nrow(dat) - 1)) {
      dat$`Moving Range`[i + 1] <- abs(dat[[measure]][i] - dat[[measure]][i + 1])
    }
    dat$`Average Moving Range` <- mean(dat$`Moving Range`[2:(interval)])
    dat$`Average Moving Range`[1] <- NA
    original_avg_mving_rng <- mean(dat$`Average Moving Range`[1:interval], na.rm = T)
    dat <- limits(dat)
    return(dat)
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
  
  #testing for shortruns
  shortruns <- function(df, side, points){
    if(side == "upper"){
      df$Test <- NA 
      df$Test <- ifelse(
        (abs(df[[measure]] - df$`Central Line`) > 
           abs(df[[measure]] - df$`Upper Natural Process Limit`) & 
           !(df$Order %in% points)
        ),"1", "0")
      return(df)
    }
    if(side == "lower"){
      df$Test <- NA 
      df$Test <- ifelse(
        (abs(df[[measure]] - df$`Central Line`) > 
           abs(df[[measure]] - df$`Lower Natural Process Limit`) & 
           !(df$Order %in% points)
        ),"1", "0")
      return(df)
    }
  }
  
  #run subsetters
  shortrun_subset <- function(df, test, order, measure, points, int){
    int <- int
    subsets <- c()
    value <- "1"
    run <- 3
    percentage <- run*.66
    
    for(i in int:nrow(df)){
      pnts <- i:(i+3)
      #if(max(pnts) > max(df[[order]])){pnts <- min(pnts):max(df[[order]])}
      q <- df[[test]][df[[order]] %in% pnts]
      r <- as.data.frame(table(q))
      if(!any(is.na(q) == T) && (value %in% r$q)){
        #switch this statements to show run must be 4 long
        #if(sum(r$Freq) == run && r$Freq[r$q == value] >= percentage && 
        if(r$Freq[r$q == value] >= percentage && 
           !(pnts %in% points)){
          subset <- df[df[[order]] %in% pnts,]
          df <- df[!(df[[order]] %in% pnts),]
          subsets <- rbind(subsets, subset)
        }
      }
    }
    return(subsets[1:3,])
  }
  
  run_subset <- function(subset, order, df, type, side, points){
    if(missing(type)){type <- "long"}
    if(missing(subset)){subset <-  df}
    if(type == "long"){
      breaks <- c(0, which(diff(subset[[order]]) != 1), length(subset[[order]])) 
      
      d <- sapply(seq(length(breaks) - 1), 
                  function(i) subset[[order]][(breaks[i] + 1):breaks[i+1]]) 
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
      } 
      else {subset <- subset[subset[[order]] %in% d[[1]],]}
    }
    if(type == "short" && side == "upper"){
      df <- shortruns(df, "upper", points)
      subset <- shortrun_subset(df, "Test", "Order", measure, points, interval)
    }
    if(type == "short" && side == "lower"){
      df_subset <- shortruns(df, "lower", points)
      subset <- shortrun_subset(df_subset, "Test", "Order", measure, points, interval)
    }
    return(subset)
  }
  
  #recalculator
  recalculator <- function(dat, subset, order, length, message){
    if(length == 8){
      int <- 5
      subset$Test <- 1
    } else if (length == 3){
      int <- 3
    }
    if(nrow(subset) >= length){
      start <- min(subset[[order]], na.rm = T)
      if(length == 8){end <- start+3} 
      else if(length == 3){end <- start+1}
      lastrow <- max(dat[[order]], na.rm = T)
      if (length == 8){
        new_cnt <- mean(subset[[measure]][1:int], na.rm = T)
        new_mv_rng <- subset$`Moving Range`[1:int]
        new_av_mv_rng <- mean(new_mv_rng, na.rm = T)
        dat$`Average Moving Range`[start:lastrow] <- new_av_mv_rng
        dat$`Central Line`[start:lastrow] <- new_cnt
        dat <- limits(dat)
        calcpoints <- start:end
        points <- c(points, calcpoints)
        points <- c(min(points):max(points))
        assign("points", points, envir = parent.frame())
        assign("calcpoints", calcpoints, envir = parent.frame())
        return(dat)
      } else if (length == 3){
        new_cnt <- mean(subset[[measure]][subset$Test == 1], na.rm = T)
        new_mv_rng <- subset$`Moving Range`
        new_av_mv_rng <- mean(new_mv_rng, na.rm = T)
        start <- min(subset[[order]][subset$Test == 1], na.rm = T)
        end <- max(subset[[order]][subset$Test == 1], na.rm = T)
        dat$`Average Moving Range`[start:lastrow] <- new_av_mv_rng
        dat$`Central Line`[start:lastrow] <- new_cnt
        dat <- limits(dat)
        calcpoints <- start:end
        #points <- c(points, calcpoints)
        #points <- c(min(points):max(points))
        assign("points", points, envir = parent.frame())
        assign("calcpoints", calcpoints, envir = parent.frame())
        return(dat)
      }
      
    } else {return(dat)}
  }
  
  #runs application
  runs <- function(dat, run = c("short", "long"), side = c("upper", "lower")){
    if(run == "short"){l <- 3} else if (run == "long"){l <- 8}
    
    #upper longruns
    if(side == "upper" && run == "long"){
      dat_sub <- dat %>%
        filter(., .[[measure]] > `Central Line` & !(Order %in% points)) %>%
        arrange(., Order)
      dat_sub <- run_subset(dat_sub, "Order")
      rep <- nrow(dat_sub)
      while(rep >= l){
        mess <- paste0(run, ": ", side)
        dat <- recalculator(dat, dat_sub, "Order", l, mess)
        assign("points", points, envir = parent.frame())
        if(testing == T){
          print(mess) 
          print(calcpoints)
        }
        dat_sub <- dat %>%
          filter(., .[[measure]] > `Central Line` & !(Order %in% points)) %>%
          arrange(., Order)
        dat_sub <- run_subset(dat_sub, "Order")
        rep <- nrow(dat_sub)
      } 
    } 
    
    #lower longruns
    else if(side == "lower" && run == "long"){
      dat_sub <- dat %>%
        filter(., .[[measure]] < `Central Line` & !(Order %in% points)) %>%  
        arrange(., Order)
      dat_sub <- run_subset(dat_sub, "Order")
      rep <- nrow(dat_sub)
      while(rep >= l){
        mess <- paste0(run, ": ", side)
        dat <- recalculator(dat, dat_sub, "Order", l, mess)
        assign("points", points, envir = parent.frame())
        if(testing == T){
          print(mess) 
          print(calcpoints)
        }
        dat_sub <- dat %>%
          filter(., .[[measure]] < `Central Line` & !(Order %in% points)) %>%
          arrange(., Order)
        dat_sub <- run_subset(dat_sub, "Order")
        rep <- nrow(dat_sub)
      }
    }
    
    #upper shortruns
    else if(side == "upper" && run == "short"){
      dat_sub <- run_subset(order = "Order", 
                            df = dat, 
                            type = "short", 
                            side = "upper", 
                            points = points)
      rep <- nrow(dat_sub)
      while(!is.null(rep) && !is.na(rep)){
        mess <- paste0(run, ": ", side)
        dat <- recalculator(dat, dat_sub, "Order", l, mess)
        assign("points", points, envir = parent.frame())
        if(testing == T){
          print(mess) 
          print(calcpoints)
        }
        dat_sub <- run_subset(order = "Order", 
                              df = dat, 
                              type = "short",
                              side = "upper", 
                              points = points)
        rep <- nrow(dat_sub)
        #print(dat_sub)
      }
    } 
    
    ##lower shortrun
    else if(side == "lower" && run == "short"){
      dat_sub <- run_subset(order = "Order", 
                            df = dat, 
                            type = "short", 
                            side = "lower", 
                            points = points)
      rep <- nrow(dat_sub)
      while(!is.null(rep) && !is.na(rep)){
        mess <- paste0(run, ": ", side)
        dat <- recalculator(dat, dat_sub, "Order", l, mess)
        assign("points", points, envir = parent.frame())
        if(testing == T){
          print(mess) 
          print(calcpoints)
        }
        dat_sub <- run_subset(order = "Order", 
                              df = dat, 
                              type = "short", 
                              side = "lower", 
                              points = points)
        rep <- nrow(dat_sub)
      }
    } 
    return(dat)
  }
  if((nrow(df)) >= interval){
    #if no recalculation of limits is desired
    if(recalc == F){df <- starter(df)}
    #if recalculation of limits desired
    if(recalc == T){
      #calculate inital values
      df <- starter(df)
      df <- runs(df, "short", "upper")
      df <- runs(df, "short", "lower")
      df <- runs(df, "long", "upper")
      df <- runs(df, "long", "lower")
      df <- runs(df, "short", "upper")
      df <- runs(df, "short", "lower")
      
      
      
    }
    df <- limits(df)
    #rounding
    df$`Central Line` <- round2(df$`Central Line`, 3)
    df$`Moving Range` <- round2(df$`Moving Range`, 3)
    df$`Average Moving Range` <- round2(df$`Average Moving Range`, 3)
    df$`Lower Natural Process Limit` <-
      round2(df$`Lower Natural Process Limit`, 3)
    df$`Upper Natural Process Limit` <-
      round2(df$`Upper Natural Process Limit`, 3)
  }
  if ((nrow(df)) < interval) {
    df$`Central Line` <- NA
    df$`Moving Range` <- NA
    df$`Average Moving Range` <- NA
    df$`Lower Natural Process Limit` <- NA
    df$`Upper Natural Process Limit` <- NA
  }
  return(df)
}
