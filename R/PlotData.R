#' Creates Frequency Profiles and plots it.
#' 
#' @param data.freqprof data formated into \code{freqprof} class.
#' @param yAxis a string giving the legend of the y-axis.
#' @param xAxisUnits a string indicating which unit has been used. By default,
#'   "sec".
#' @param panel.in a boolean indicating if the first panel has to be plotted.
#' @param panel.out a boolean indicating if the third panel has to be plotted
#' @param gg if TRUE, will use the 'ggplot2' package. By default, gg = FALSE.
#' @param multiPlot if TRUE, will plot each behavior in a distinct panel. By
#'   default, multiPlot = FALSE.
#' @param tick.every the spacing between each tick. By default, N/30 where N is
#'   the number of time units.
#' @param label.every label every X ticks, where X = label.every. By default,
#'   label.every = 3.
#' @return The function plots the data but does not return anything.
#' @export
#' @examples
#' plot.freqprof(freqprof(import.data()))
plot.freqprof = function(data.freqprof,
                         yAxis=NULL,
                         xAxisUnits = "sec",
                         panel.in = T,
                         panel.out = T,
                         gg = F,
                         multiPlot = F,
                         tick.every = round(length(data.freqprof$data$time)/31),
                         label.every = 3){
  # extract relevant data from data.freqprof
  res <- data.freqprof$data
  panels = res$panels
  res = res[,-2]
  freqprof <- res[,-1]
  t <- res$time
  
  window = data.freqprof$window
  step = data.freqprof$step
  resolution = data.freqprof$resolution
  type = data.freqprof$type
  
  # panels limits
  x.panel.left = max(which(data.freqprof$data$panels==1)) * resolution
  x.panel.right = min(which(data.freqprof$data$panels==3)) * resolution
  
  # x-axis limits
  xmin = ifelse(test = panel.in,yes = min(t),no = x.panel.left)
  xmax = ifelse(test = panel.out,yes = max(t),no = x.panel.right)
  
  # plotting results
  if(is.null(yAxis)){
    yAxis = switch(type,
                   sum = 'Moving sum',
                   proportion = 'Moving proportion')
  }
  
  # colour-blind friendly palette
  cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  
  if(gg){
    require(ggplot2)
    require(reshape2)
    require(grid)
    
    res.melt <- melt(res, id = "time")
    
    # Graphing function
    p = ggplot(res.melt,
               aes(x=time, y = value, colour = variable, group = variable)) + 
      geom_line(size = .8) +
      labs(title = "Frequency Profile") +
      xlab(paste('Time (',resolution * step,' ',xAxisUnits,')',sep="")) +
      ylab(yAxis) +
      scale_x_continuous(limits = c(xmin,xmax),
                         minor_breaks = round(seq(xmin, xmax, by=tick.every)),
                         breaks = round(seq(xmin, xmax, by=tick.every*label.every))) +
      scale_color_discrete(name="Behavior") +
      theme(axis.text = element_text(size = 12, colour = "#3f3f3f"),
            axis.title.x = element_text(size = 15, face = "bold", vjust = -.5),
            axis.title.y = element_text(size = 15, face = "bold", vjust = 1.5),
            title = element_text(size = 17, face = "bold", vjust = 2),
            legend.text = element_text(size = 12),
            panel.background = element_rect(fill = '#f6f6f6'),
            panel.grid.major = element_line(colour = "#e9e9e9"),
            panel.grid.minor = element_line(colour = "#e9e9e9"),
            axis.line = element_line(color = "#a8a8a8"),
            axis.ticks = element_line(colour = "black", size = 1),
            axis.ticks.length = unit(-0.25, "cm"),
            axis.ticks.margin = unit(0.5, "cm"))
    
    if(panel.in){
      p = p + geom_vline(xintercept = x.panel.left)
    }
    
    if(panel.out){
      p = p + geom_vline(xintercept = x.panel.right) 
    }
    
    if (multiPlot) {
      p = p + facet_grid(variable ~ .) + theme(legend.position = "none")
    }
    
    print(p)
  } else{
    
    # no ggplot
    
    if(is.null(ncol(freqprof))){
      # case of only one column selected
      freqprof = as.data.frame(freqprof)
    }
    
    if(multiPlot) {
      plotBehavior = function(j){
        plot(t,
             freqprof[,j],
             type='l',
             col=cbbPalette[(j %% length(cbbPalette))+1],
             ylim = c(0,max(freqprof)),
             xlim = c(xmin,xmax),
             xlab = '',
             ylab = '',
             xaxt = 'n')
        n.minor = length(ax <- seq(from = xmin,to = xmax,by = tick.every))
        axis(1,at = ax, labels = F)
        lab = ax[seq(1,n.minor,by=label.every)]
        axis(1,at = lab, labels = T)
        if(panel.in){
          abline(v = x.panel.left)
        } 
        if(panel.out){
          abline(v = x.panel.right)
        }
      }
      
      par(mfrow = c(ncol(freqprof),1))
      par(cex = .6)
      par(mar = c(2,2,2,2), oma = c(4, 4, 0.5, 0.5))
      
      for(j in 1:ncol(freqprof)) plotBehavior(j)
      
      mtext(paste('Time (',resolution * step,' ',xAxisUnits,')',sep=""), side = 1, outer = TRUE,cex = 0.7, line = 2.2)
      mtext(yAxis, side = 2, outer = TRUE,cex = 0.7, line = 2.2)
    }
    else {
      plot(t,
           freqprof[,1],
           type = 'l',
           col = cbbPalette[1],
           ylim = c(0,max(freqprof)),
           xlim = c(xmin,xmax),
           ylab = yAxis,
           xlab = paste('Time (',resolution,' ',xAxisUnits,')',sep=""),
           xaxt = 'n')
      n.minor = length(ax <- seq(from = xmin,to = xmax,by = tick.every))
      axis(1,at = ax, labels = F)
      lab = ax[seq(1,n.minor,by=label.every)]
      axis(1,at = lab, labels = T)
      
      if(ncol(freqprof)>1){
        for(j in 2:ncol(freqprof)){
          lines(t,
                freqprof[,j],
                type='l',
                col=cbbPalette[(j %% length(cbbPalette))+1])
        }
      }
      if(panel.in){
        abline(v = x.panel.left)
      } 
      if(panel.out){
        abline(v = x.panel.right)
      }
    }
    
  }
}
