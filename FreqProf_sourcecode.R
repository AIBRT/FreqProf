# Import data pop up
import.data = function(filename = file.choose()){
  # this function reads a file, whose extension is either csv, bin or fpw,
  # and imports it as a data.frame
  
  file.extension = tolower(substr(filename,nchar(filename)-2,nchar(filename)))
  
  data = switch(file.extension,
                csv = read.csv(filename),
                bin = read.bin(filename),
                fpw = read.fpw(filename))
  
  if(is.null(data)){
    stop("file extension must be either csv, fpw, or bin")  
  }
  
  return(data)
}

# Convert data to moving sum/prop
freqprof = function(data.behavior,
                    window=round(.25*nrow(data.behavior)),
                    step=1,
                    resolution=1,
                    which = c('sum','proportion')){
  # selecting the appropriate moving function, according to the variable 'which'
  # by default, which is 'sum'
  if(length(which)>1) which = 'sum'
  
  if(!(which %in% c('sum','proportion'))){
    stop("possible values for variable 'which' are c('sum','proportion').")
  }
  
  # computing frequency profile
  freqprof = as.data.frame(apply(data.behavior,MARGIN = 2,FUN = function(x){movfun(x,n=window,s=step,r=resolution,fun=which)$movfun}))
  res = cbind(data.frame(time = (0 : (nrow(freqprof)-1) ) * resolution,
                         panels = movfun(data.behavior[,1],n=window,s=step,r=resolution,fun=which)$panels),
              freqprof)
  
  return(structure(list(window = window,
                        step = step,
                        resolution = resolution,
                        raw.data = data.behavior,
                        type = which,
                        data = res),
                   class = "freqprof"))
}

# Create Frequency Profiles
plot.freqprof = function(data.freqprof,yAxis=NULL,xAxisUnits = "sec", panel.in = T, panel.out = T, gg = F, multiPlot = F){
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
      scale_x_continuous(limits = c(xmin,xmax)) +
      scale_color_discrete(name="Behavior") +
      theme(axis.text = element_text(size = 12, colour = "#3f3f3f"),
            axis.title.x = element_text(size = 15, face = "bold", vjust = -.5),
            axis.title.y = element_text(size = 15, face = "bold", vjust = 1.5),
            title = element_text(size = 17, face = "bold", vjust = 2),
            legend.text = element_text(size = 12),
            panel.background = element_rect(fill = '#f6f6f6'),
            panel.grid.major = element_line(colour = "#e9e9e9"),
            axis.line = element_line(color = "#a8a8a8"))
    
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
    if(multiPlot) {
      plotBehavior = function(j){
        plot(t,
             freqprof[,j],
             type='l',
             col=cbbPalette[(j %% length(cbbPalette))+1],
             ylim = c(0,max(freqprof)),
             xlim = c(xmin,xmax),
             xlab = '',
             ylab = '')
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
           xlab = paste('Time (',resolution,' ',xAxisUnits,')',sep=""))
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


# Useful functions --------------------------------------------------------

radj <- function(x, r) {
  # x is data
  # r is resolution
  adj <- rep(NA, floor( (length(x)/r) ))
  
  for (j in 1:length(adj)){
    adj[j] <- sum(x[(1+(j-1)*r):(j*r)])
    adj[j] <- ifelse(adj[j] > 0, 1, 0)
  }
  
  return(adj)
}

movfun = function(x,n,s,r,fun){
  # x is data
  # n is window length
  # s is step
  
  if (r > 1){
    x <- radj(x, r)
  }
  
  fun = switch(fun,
               sum = sum,
               proportion = function(y){sum(y)/n})
  
  res = rep(NA,floor( (length(x)+n-1)/s ) )
  
  panels = rep(2,floor( (length(x)+n-1)/s ) )
  
  for(j in 1:length(res)){
    res[j] = fun(x[max(1,j*s-n+1):min(j*s,length(x))])
    
    if( (j*s-n+1) < 1 ) panels[j] = 1
    if( (j*s) > length(x) ) panels[j] = 3
  }
  
  return( list(movfun = c(0,res,0), panels = c(1,panels,3) ) )
}

read.bin = function(filename){
  # this function reads the data in the file "filename",
  # which is supposed to be a .bin file
  # it returns a data.frame giving the raw data
  
  # scanning the file, line by line
  file.scan = scan(file = filename,
                   what = "character",
                   sep = "\n",
                   quiet = T)
  
  # looking for the line that starts with an asterisk
  N = which(sapply(X = 1:length(file.scan),FUN = function(x){substr(file.scan[x],1,1)=="*"}))
  file.data = file.scan[(N+2):length(file.scan)]
  # removing "<" at the end of line
  for(j in 1:length(file.data)){
    file.data[j] = substr(file.data[j],1,nchar(file.data[j])-1)
  }
  
  # converting these strings into a matrix
  result = matrix(data = NA,nrow = nchar(file.data[1]),ncol = length(file.data))
  for(j in 1:dim(result)[2]){
    result[,j] = sapply(X = 1:nchar(file.data[j]),FUN = function(x){as.numeric(substr(file.data[j],x,x))})
  }
  
  return(as.data.frame(result))
}

read.fpw = function(filename){
  # this function reads the data in the file "filename",
  # which is supposed to be a .fpw file
  # it returns a data.frame giving the raw data
  
  file.scan = scan(file = filename,
                   what = "character",
                   sep = "\n",
                   quiet = T)
  # looking for the line that indicates "[DATA]"
  N = which(sapply(X = 1:length(file.scan),FUN = function(x){file.scan[x]=="[DATA]"}))
  
  # converting these strings into a data.frame
  result = read.table(file = filename,skip=N+2,sep=" ",strip.white=F)
  # as the lines begin with a " ", the first column is read as NA
  result = result[,2:ncol(result)]
  names(result) = paste("V",1:ncol(result),sep="")
  
  return(result)
}

print.freqprof = function(data.freqprof){
  print(data.freqprof$data)
}

head.freqprof = function(data.freqprof){
  head(data.freqprof$data)
}

tail.freqprof = function(data.freqprof){
  tail(data.freqprof$data)
}

