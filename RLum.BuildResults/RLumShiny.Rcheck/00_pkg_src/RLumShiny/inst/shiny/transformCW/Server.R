## Server.R
library(Luminescence)
library(RLumShiny)
library(shiny)

data("ExampleData.CW_OSL_Curve", envir = environment())
data <- ExampleData.CW_OSL_Curve

## MAIN FUNCTION
shinyServer(function(input, output, session) {
  
  # RECEIVE USER DATA ----
  datGet<- reactive({
    inFile<- input$file
    if(is.null(inFile)) 
      return(NULL) 
    
    t <- tryCatch(read.table(file = inFile$datapath,
                             sep = input$sep, 
                             quote = "", 
                             header = input$headers),
                  error = function(e) {
                    return(NULL)
                  })
    
    if (is.null(t))
      return(NULL)
    
    if (ncol(t) == 1)
      return(NULL)
    
    return(t)
  })
  
  # TRANSFORM DATA
  observe({
    if (!is.null(datGet()))
      data <- datGet()
    
    P <- input$p
    delta <- input$delta
    
    # validate method parameters
    if (is.na(input$delta)) {
      updateNumericInput(session, "delta", value = 1)
      delta <- 1
    }
    else if (input$delta < 1) {
      updateNumericInput(session, "delta", value = 1)
      delta <- 1
    }
    
    # validate method parameters
    if (is.na(input$p)) {
      updateNumericInput(session, "p", value = 1)
      P <- 1
    }
    else if (input$p < 1) {
      updateNumericInput(session, "p", value = 1)
      P <- 1
    }

      
    args <- list(data)
    if (input$method == "CW2pHMi")
      if (delta >= 1)
        args <- append(args, delta)
    if (input$method == "CW2pLMi" || input$method == "CW2pPMi")
      if (P >= 1)
        args <- append(args, P)
    
    tdata <<- do.call(input$method, args)
  })
  
  output$main_plot <- renderPlot({
    
    # be reactive on method changes
    datGet()
    input$method
    input$delta
    input$p
    
    pargs <- list(tdata[ ,1], tdata[ ,2], 
                  log = paste0(ifelse(input$logx, "x", ""), ifelse(input$logy, "y", "")),
                  main = input$main,
                  xlab = input$xlab,
                  ylab = input$ylab,
                  cex = input$cex,
                  type = input$type,
                  pch = ifelse(input$pch != "custom", as.integer(input$pch) - 1, input$custompch),
                  col = ifelse(input$color != "custom", input$color, input$jscol1))
    
    do.call(plot, pargs)
    
    output$exportScript <- downloadHandler(
      filename = function() { paste(input$filename, ".", "txt", sep="") },
      content = function(file) {
        write.table(tdata, file, sep = ",", quote = FALSE, row.names = FALSE)
      },#EO content =,
      contentType = "text"
    )#EndOf::dowmloadHandler()
    
    
    # nested downloadHandler() to print plot to file
    output$exportFile <- downloadHandler(
      filename = function() { paste(input$filename, ".", input$fileformat, sep="") },
      content = function(file) {
        
        # determine desired fileformat and set arguments
        if(input$fileformat == "pdf") {
          pdf(file, 
              width = input$imgwidth, 
              height = input$imgheight, 
              paper = "special",
              useDingbats = FALSE, 
              family = input$fontfamily)
        }
        if(input$fileformat == "svg") {
          svg(file, 
              width = input$imgwidth, 
              height = input$imgheight, 
              family = input$fontfamily)
        }
        if(input$fileformat == "eps") {
          postscript(file, 
                     width = input$imgwidth, 
                     height = input$imgheight, 
                     paper = "special", 
                     family = input$fontfamily)
        }
        
        # plot curve 
        do.call(plot, args = pargs)
        
        dev.off()
      },#EO content =,
      contentType = "image"
    )#EndOf::dowmloadHandler()
  })
  
  output$dataset <- renderDataTable({
    # be reactive on method changes
    datGet()
    input$method
    input$delta
    input$p
    
    if (exists("tdata")){
      tdata
    }
  })
  

})##EndOf::shinyServer(function(input, output)