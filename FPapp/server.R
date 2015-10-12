library(shiny)
source("../FreqProf_sourcecode.R")

shinyServer(function(input, output, session) {
  
  getDataFromShiny = function(inFile){
    
    if (is.null(inFile))
      return(NULL)
    
    # reading a file, whose extension is either csv, bin or fpw,
    # and importing it as a data.frame
    filename = inFile$name
    
    file.extension = tolower(substr(filename,nchar(filename)-2,nchar(filename)))
    
    data.behavior = switch(file.extension,
                           csv = read.csv(inFile$datapath),
                           bin = read.bin(inFile$datapath),
                           fpw = read.fpw(inFile$datapath))
    
    if(is.null(data.behavior)) stop("file extension must be either csv, fpw, or bin")
    
    return(data.behavior)
  }
  
  output$distPlot <- renderPlot({  
    
    data.behavior = getDataFromShiny(input$file)
    if(is.null(data.behavior)) return(NULL)
    
    data.freqprof = freqprof(data.behavior,
                             window = input$window,
                             step = input$step,
                             resolution = input$resolution,
                             which = input$which)
    plot.freqprof(data.freqprof,
                  gg=input$ggplot,
                  panel.in = input$panel.in,
                  panel.out = input$panel.out,
                  multiPlot = input$multiplot,
                  xAxisUnits = input$units)
  })
  
  observe({
    data.behavior = getDataFromShiny(input$file)
    if(is.null(data.behavior)) return(NULL)
    
    window = round(.25*nrow(data.behavior))
    updateSliderInput(session, "window", value = window,
                      min = 1, max = 4*window, step = round(window/20))
  })
  
  output$downloadData <- downloadHandler(
    filename = "freqprof.csv",
    content = function(file) {
      data.behavior = getDataFromShiny(input$file)
      if(is.null(data.behavior)) return(NULL)
      
      data.freqprof = freqprof(data.behavior,
                               window = input$window,
                               step = input$step,
                               resolution = input$resolution,
                               which = input$which)
      
      # which panels will be downloaded?
      panels = c(2)
      if(input$panel.in) panels = c(1,panels)
      if(input$panel.out) panels = c(panels,3)
      
      write.csv(data.freqprof$data[ data.freqprof$data$panels %in% panels, ], file,row.names=F)
    }
  )
  
  output$downloadPlotPDF <- downloadHandler(
    filename = function() { paste0("ShinyPlot.pdf") },
    content = function(file) {
      pdf(file,width = 10)
      data.behavior = getDataFromShiny(input$file)
      data.freqprof = freqprof(data.behavior,
                               window = input$window,
                               step = input$step,
                               resolution = input$resolution,
                               which = input$which)
      plot.freqprof(data.freqprof,
                    gg=input$ggplot,
                    panel.in = input$panel.in,
                    panel.out = input$panel.out,
                    multiPlot = input$multiplot,
                    xAxisUnits = input$units)
      dev.off()
      
      if (file.exists(paste0(file, ".pdf")))
        file.rename(paste0(file, ".pdf"), file)
    }) 
  
  output$downloadPlotPNG <- downloadHandler(
    filename = function() { paste0("ShinyPlot.png") },
    content = function(file) {
      png(file,width = 800)
      data.behavior = getDataFromShiny(input$file)
      data.freqprof = freqprof(data.behavior,
                               window = input$window,
                               step = input$step,
                               resolution = input$resolution,
                               which = input$which)
      plot.freqprof(data.freqprof,
                    gg=input$ggplot,
                    panel.in = input$panel.in,
                    panel.out = input$panel.out,
                    multiPlot = input$multiplot,
                    xAxisUnits = input$units)
      dev.off()
      
      if (file.exists(paste0(file, ".png")))
        file.rename(paste0(file, ".png"), file)
    }) 
  
})


