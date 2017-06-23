#'Generate the XMR chart for XMR data
#'@description Useful for generating the static XMR data for XMR charts. Output can be piped into xmR_chart. Does not recalculate the data for long and short runs....yet.
#'
#'@param dataframe Output from xmR()
#'@param facetvar The column containing the factor you'd like to facet on.
#'@examples dat.xmr <- xmR(dat, "Measure", 5)
#'dat.xmr <- dat %>% group_by(., Program, Variable) %>% do(xmR(., "Measure")) %>% do(xmR_Chart(., "Variable"))
#'
#'@export xmR_chart
xmR_chart <- function(dataframe, facetvar){
  whitetheme <- theme_bw() +
    theme(
      strip.background = element_rect(fill = NA, linetype = 0),
      panel.border = element_rect(color = NA),
      panel.spacing.y = unit(4,"lines"),
      panel.spacing.x = unit(2,"lines"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
      #strip.text = element_text(size=18)
    )
  if(missing(facetvar)){
    plot <- ggplot(dataframe,
                   aes(Year)) +
      geom_line(aes(y=Measure)) + geom_point(aes(y=Measure, color = "#CF2A26")) +
      geom_line(aes(y=`Central Line`), size=1, linetype = "dotted",na.rm = T) +
      geom_line(aes(y=`Lower Natural Process Limit`, color = "#7ECBB5"), size=1, linetype = "dashed",na.rm = T) +
      geom_line(aes(y=`Upper Natural Process Limit`, color = "#7ECBB5"), size=1, linetype = "dashed",na.rm = T) +
      guides(colour=FALSE) + whitetheme
  return(plot)
  } else {
      plot <- ggplot(dataframe,
                   aes(Year, group = dataframe[[facetvar]])) +
      geom_line(aes(y=Measure)) + geom_point(aes(y=Measure, color = "#CF2A26")) +
      geom_line(aes(y=`Central Line`), size=1, linetype = "dotted",na.rm = T) +
      geom_line(aes(y=`Lower Natural Process Limit`, color = "#7ECBB5"), size=1, linetype = "dashed",na.rm = T) +
      geom_line(aes(y=`Upper Natural Process Limit`, color = "#7ECBB5"), size=1, linetype = "dashed",na.rm = T) +
      guides(colour=FALSE) + facet_wrap(~dataframe[[facetvar]], scales = "free") + whitetheme
  return(plot)
  }
}
