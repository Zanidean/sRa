#'Generate the XMR chart for XMR data
#'@description Useful for diagnostics on xmR, and just visualizing the data.
#'
#'@param dataframe Output from xmR()
#'@param measure Measure
#'@param facetvar The column containing the factor you'd like to facet on.
#'@examples dat.xmr <- xmR(dat, "Measure", 5)
#'dat.xmr <- dat %>% 
#'             group_by(., Program, Variable) %>% 
#'             do(xmR(., "Measure")) %>% 
#'             do(xmR_Chart(., "Variable"))
#'
#'@export xmR_chart
xmR_chart <- function(dataframe, time, measure, facetvar){
  
  whitetheme <- theme_bw() + 
    theme(strip.background = element_rect(fill = NA, linetype = 0), 
          panel.border = element_rect(color = NA), 
          panel.spacing.y = unit(4, "lines"), 
          panel.spacing.x = unit(2, "lines"), 
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.text.y =  element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.y = element_blank(), 
          axis.ticks.x = element_blank(),
          text = element_text(family = "sans"),
          axis.text.x = element_text(colour = "#000000", size = 8),
          axis.title.x = element_text(size = 9, face = "bold"))
  
  if(missing(facetvar)){
    plot <- ggplot(dataframe,
                   aes(dataframe[[time]])) +
      geom_line(aes(y = `Central Line`),
                size = 0.5, linetype = "dotted", na.rm = T) +
      geom_line(aes(y = `Lower Natural Process Limit`), color = "#d02b27",
                size = 0.5, linetype = "dashed", na.rm = T) +
      geom_line(aes(y = `Upper Natural Process Limit`), color = "#d02b27",
                size = 0.5, linetype = "dashed", na.rm = T) +
      geom_line(aes(y = dataframe[[measure]])) + 
      geom_point(aes(y = dataframe[[measure]]), size = 2, color = "#000000") +
      geom_point(aes(y = dataframe[[measure]]), size = 1.25, color = "#7ECBB5") +
      geom_text(aes(y = dataframe[[measure]]), label = round(dataframe[[measure]]), vjust = -1, size = 2.8) + 
      guides(colour=FALSE) + 
      labs(x = time, y = measure) +
      whitetheme
    return(plot)
  } else {
      plot <- ggplot(dataframe,
                   aes(dataframe[[time]], group = dataframe[[facetvar]])) +
        geom_line(aes(y = `Central Line`),
                  size = 0.5, linetype = "dotted", na.rm = T) +
        geom_line(aes(y = `Lower Natural Process Limit`), color = "#d02b27",
                  size = 0.5, linetype = "dashed", na.rm = T) +
        geom_line(aes(y = `Upper Natural Process Limit`), color = "#d02b27",
                  size = 0.5, linetype = "dashed", na.rm = T) +
        geom_line(aes(y = dataframe[[measure]])) + 
        geom_point(aes(y = dataframe[[measure]]), size = 2, color = "#000000") +
        geom_point(aes(y = dataframe[[measure]]), size = 1.25, color = "#7ECBB5") +
        geom_text(aes(y = dataframe[[measure]]), label = round(dataframe[[measure]]), vjust = -1, size = 2.8) + 
        guides(colour=FALSE) +  
        facet_wrap(~dataframe[[facetvar]], scales = "free") + 
        labs(x = time, y = measure) +
        whitetheme
  return(plot)
  }
}
