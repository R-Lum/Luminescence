## Server.R
library(Luminescence)
library(shiny)

# load example data
data(ExampleData.DeValues)
data <- ExampleData.DeValues$CA1

## MAIN FUNCTION
shinyServer(function(input, output, session) {
  
  # check and read in file (DATA SET 1)
  datGet<- reactive({
    inFile<- input$file1
    if(is.null(inFile)) return(NULL) # if no file was uploaded return NULL
    return(read.table(file = inFile$datapath, # inFile[1] contains filepath 
                      sep = input$sep, 
                      quote = "", 
                      header = input$headers)) # else return file
  })
  
  # check and read in file (DATA SET 2)
  datGet2<- reactive({
    inFile2<- input$file2
    if(is.null(inFile2)) return(NULL) # if no file was uploaded return NULL
    return(read.table(file = inFile2$datapath, # inFile[1] contains filepath 
                      sep = input$sep, 
                      quote = "", 
                      header = input$headers)) # else return file
  })
  
  
  # dynamically inject sliderInput for central value
  output$centValue<- renderUI({
    
    # check if file is loaded
    # # case 1: yes -> slinderInput with custom values
    if(!is.null(datGet())) {
      if(!is.null(datGet2())) {
        centValue.data<- rbind(datGet(),datGet2())
        sliderInput(inputId = "centValue", 
                    label = "Central Value",
                    min = min(centValue.data[,1])*0.9, 
                    max = max(centValue.data[,1])*1.1,
                    value = mean(centValue.data[,1]))
      } else {
        data<- datGet()
        sliderInput(inputId = "centValue", 
                    label = "Central Value",
                    min = min(data[,1])*0.9, 
                    max = max(data[,1])*1.1,
                    value = mean(data[,1]))
      }
      
    }
    else { #case 2: no -> sliderInput for example data
      sliderInput(inputId = "centValue", 
                  label = "Central Value", 
                  min = min(data[,1])*0.9, 
                  max = max(data[,1])*1.1,
                  value = mean(data[,1]), 
                  step = 1, round = 0)
    }
  })## EndOf::renderUI()
  
  
  # dynamically inject sliderInput for z-axis range
  output$xlim<- renderUI({
    
    # check if file is loaded
    # # case 1: yes -> slinderInput with custom values
    if(!is.null(datGet())) {
      if(!is.null(datGet2())) {
        
        xlim.data<- rbind(datGet(),datGet2())
        
        if(input$logz == TRUE) {
          sd<- xlim.data[,2] / xlim.data[,1]
        } else {
          sd<- xlim.data[,2] 
        }
        prec<- 1/sd
        
        sliderInput(inputId = "xlim", 
                    label = "Range x-axis",
                    min = 0, 
                    max = max(prec)*2,
                    value = c(0, max(prec)*1.05), round=FALSE, step=0.0001)
      } else {
        data<- datGet()
        
        if(input$logz == TRUE) {
          sd<- data[,2] / data[,1]
        } else {
          sd<- data[,2] 
        }
        prec<- 1/sd
        
        sliderInput(inputId = "xlim", 
                    label = "Range x-axis",
                    min = 0, 
                    max = max(prec)*2,
                    value = c(0, max(prec)*1.05), round=FALSE, step=0.0001)
      }
    }
    else { #case 2: no -> sliderInput for example data
      if(input$logz == TRUE) {
        sd<- data[,2] / data[,1]        
      } else {
        sd<- data[,2] 
        
      }
      prec<- 1/sd
      
      sliderInput(inputId = "xlim", 
                  label = "Range x-axis",
                  min = 0, 
                  max = max(prec)*2,
                  value = c(0, max(prec)*1.05), round=FALSE, step=0.0001)
    }
  })## EndOf::renderUI()
  
  
  # dynamically inject sliderInput for z-axis range
  output$zlim<- renderUI({
    
    # check if file is loaded
    # # case 1: yes -> slinderInput with custom values
    if(!is.null(datGet())) {
      if(!is.null(datGet2())) {
        zlim.data<- rbind(datGet(),datGet2())
        sliderInput(inputId = "zlim",
                    label = "Range z-axis", 
                    min = min(zlim.data[,1])*0.25,
                    max = max(zlim.data[,1])*1.75,
                    value = c(min(zlim.data[,1])*0.8, max(zlim.data[,1])*1.2))
      } else {
        data<- datGet()
        sliderInput(inputId = "zlim",
                    label = "Range z-axis", 
                    min = min(data[,1])*0.25,
                    max = max(data[,1])*1.75,
                    value = c(min(data[,1])*0.8, max(data[,1])*1.2))
      }
    }
    else { #case 2: no -> sliderInput for example data
      sliderInput(inputId = "zlim", 
                  label = "Range z-axis",
                  min = min(data[,1])*0.25, 
                  max = max(data[,1])*1.75,
                  value = c(min(data[,1])*0.8, max(data[,1]))*1.2,
                  step = 1, round = 0)
    }
  })## EndOf::renderUI()
  
  # render Radial Plot
  output$main_plot <- renderPlot({
    
    # refresh plot on button press
    input$refresh
    
    # progress bar
    progress<- Progress$new(session, min = 0, max = 5)
    progress$set(message = "Calculation in progress",
                 detail = "Retrieve data")
    on.exit(progress$close())
    
    # make sure that input panels are registered on non-active tabs.
    # by default tabs are suspended and input variables are hence
    # not available
    outputOptions(x = output, name = "zlim", suspendWhenHidden = FALSE)
    outputOptions(x = output, name = "centValue", suspendWhenHidden = FALSE)
    outputOptions(x = output, name = "xlim", suspendWhenHidden = FALSE)
    
    # check if file is loaded and overwrite example data
    if(!is.null(datGet())) {
      data<- datGet()
    }
    if(!is.null(datGet2())) {
      data2<- datGet2()
    }
    
    if(is.null(datGet()) == FALSE && is.null(datGet2()) == FALSE) {
      data<- datGet()
      data2<- datGet2()
      data<- list(data, data2)
    }
    
    progress$set(value = 1)
    progress$set(message = "Calculation in progress",
                 detail = "Get values")
    
    # check if any summary stats are activated, else NA
    if (input$summary) {
      summary<- input$stats
    } else {
      summary<- NA
    }
    
    # if custom datapoint color get RGB code from separate input panel
    if(input$color == "custom") {
      color<- input$rgb
    } else {
      color<- input$color
    }
    
    if(!is.null(datGet2())) {
      # if custom datapoint color get RGB code from separate input panel
      if(input$color2 == "custom") {
        color2<- input$rgb2
      } else {
        color2<- input$color2
      }
    } else {
      color2<- adjustcolor("white", alpha.f = 0)
    }
    
    # if custom datapoint style get char from separate input panel
    if(input$pch == "custom") {
      pch<- input$custompch
    } else {
      pch<- as.integer(input$pch)-1 #-1 offset in pch values
    }
    
    # if custom datapoint style get char from separate input panel
    if(input$pch2 == "custom") {
      pch2<- input$custompch2
    } else {
      pch2<- as.integer(input$pch2)-1 #-1 offset in pch values
    }
    
    # workaround to initialize plotting after app startup
    if(is.null(input$centValue)) {
      centValue<- 3000
    } else {
      centValue<- input$centValue
    }
    
    # update progress bar
    progress$set(value = 2)
    progress$set(message = "Calculation in progress",
                 detail = "Combine values")
    
    # create numeric vector of lines
    line<-  as.numeric(c(input$line1, input$line2,
                         input$line3, input$line4,
                         input$line5, input$line6,
                         input$line7, input$line8))
    
    # create char vector of line colors
    line.col<-  c(input$colline1, input$colline2,
                  input$colline3, input$colline4,
                  input$colline5, input$colline6,
                  input$colline7, input$colline8)
    
    # create char vector of line labels
    line.label<- c(input$labline1, input$labline2,
                   input$labline3, input$labline4,
                   input$labline5, input$labline6,
                   input$labline7, input$labline8)
    
    
    # update progress bar
    progress$set(value = 3)
    progress$set(message = "Calculation in progress",
                 detail = "Get values")
    
    # if custom bar color get RGB from separate input panel or "none"
    if(input$bar == "custom") {
      bar.col<- adjustcolor(col = input$rgbBar, 
                            alpha.f = input$alpha.bar/100)
    } else {
      if(input$bar == "none") {
        bar.col<- input$bar
      } else {
        bar.col<- adjustcolor(col = input$bar, 
                              alpha.f = input$alpha.bar/100)
      }
    }
    
    # if custom bar color get RGB from separate input panel or "none"
    # SECONDARY DATA SET
    if(input$bar2 == "custom") {
      bar.col2<- adjustcolor(col = input$rgbBar2, 
                             alpha.f = input$alpha.bar/100)
    } else {
      if(input$bar2 == "none") {
        bar.col2<- input$bar
      } else {
        bar.col2<- adjustcolor(col = input$bar2, 
                               alpha.f = input$alpha.bar/100)
      }
    }
    
    # if custom grid color get RGB from separate input panel or "none"
    if(input$grid == "custom") {
      grid.col<- adjustcolor(col = input$rgbGrid, 
                             alpha.f = input$alpha.grid/100)
    } else {
      if(input$grid == "none") {
        grid.col<- input$grid
      } else {
        grid.col<- adjustcolor(col = input$grid, 
                               alpha.f = input$alpha.grid/100)
      }
    }
    
    # update progress bar
    progress$set(value = 4)
    progress$set(message = "Calculation in progress",
                 detail = "Almost there...")
    
    # workaround: if no legend wanted set label to NA and hide 
    # symbol on coordinates -999, -999
    if(input$showlegend == FALSE) {
      legend<- c(NA,NA)
      legend.pos<- c(-999,-999)
    } else {
      if(!is.null(datGet2()))
      {
        legend<- c(input$legendname, input$legendname2)
        legend.pos<- input$legend.pos
      } else {
        legend<- c(input$legendname, "")
        legend.pos<- input$legend.pos
      }
      
    }
    
    # validate(need()) makes sure that all data are available to
    # renderUI({}) before plotting and will wait until there
    validate(
      need(expr = input$centValue, message = 'Waiting for data... Please wait!'),
      need(expr = input$zlim, message = 'Waiting for data... Please wait!')
    )
    
    progress$set(value = 5)
    progress$set(message = "Calculation in progress",
                 detail = "Ready to plot")
    
    # plot radial Plot
    args <- list(data = data, 
                 xlim = input$xlim, 
                 zlim = input$zlim, 
                 xlab = c(input$xlab1, input$xlab2), 
                 ylab = input$ylab,
                 zlab = input$zlab,
                 y.ticks = input$yticks,
                 grid.col = grid.col,
                 bar.col = c(bar.col, bar.col2),
                 pch = c(pch,pch2),
                 col = c(color,color2),
                 line = line,
                 line.col = line.col,
                 line.label = line.label,
                 main = input$main,
                 cex = input$cex,
                 mtext = input$mtext,
                 log.z = input$logz, 
                 stats = input$statlabels, 
                 plot.ratio = input$curvature, 
                 summary = summary, 
                 summary.pos = input$sumpos, 
                 legend = legend, 
                 legend.pos = legend.pos,
                 na.rm = input$naExclude, 
                 central.value = input$centValue, 
                 centrality = input$centrality,
                 lwd = c(input$lwd, input$lwd2),
                 lty = c(as.integer(input$lty), as.integer(input$lty2)))
    
    
    do.call(plot_RadialPlot, args = args)
    
    # prepare code as text output
    if (is.null(input$sep)) 
      updateRadioButtons(session, "fileformat", selected = "\t")
    
    if(input$sep == "\t")
      verb.sep<-  "\\t"
    else
      verb.sep<- input$sep
    
    str1 <- paste("data <- read.delim(file, header = ",input$headers, ", sep= '", verb.sep,"')",
                  sep = "")
    
    if(!is.null(datGet2())) {
      str2 <- "file2<- file.choose()"
      str3 <- paste("data2 <- read.delim(file2, header = ",input$headers, ", sep= '", verb.sep,"')",
                    sep= "")
      str4 <- "data<- list(data, data2)"
      str1 <- paste(str1, str2, str3, str4, sep = "\n")
    }
    
    header <- paste("# To reproduce the plot in your local R environment",
                    "# copy and run the following code to your R console.",
                    "library(Luminescence)",
                    "file<- file.choose()",
                    str1,
                    "\n",
                    sep = "\n")
    
    names <- names(args)
    
    verb.arg <- paste(mapply(function(name, arg) {
      if (all(inherits(arg, "character")))
        arg <- paste0("'", arg, "'")
      if (length(arg) > 1)
        arg <- paste0("c(", paste(arg, collapse = ", "), ")")
      if (is.null(arg))
        arg <- "NULL"
      paste(name, "=", arg) 
    }, names[-1], args[-1]), collapse = ",\n")
    
    funCall <- paste0("plot_RadialPlot(data = data,\n", verb.arg, ")")
    
    code.output <- paste0(header, funCall, collapse = "\n")
    
    # nested renderText({}) for code output on "R plot code" tab
    output$plotCode<- renderText({
      
      code.output
      
    })##EndOf::renderText({})
    
    
    output$exportScript <- downloadHandler(
      filename = function() { paste(input$filename, ".", "R", sep="") },
      content = function(file) {
        write(code.output, file)
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
        
        # plot radial Plot 
        do.call(plot_RadialPlot, args = args)
        
        dev.off()
      },#EO content =,
      contentType = "image"
    )#EndOf::dowmloadHandler()
  })##EndOf::renderPlot({})
  
  # renderTable() that prints the data to the second tab
  output$dataset<- renderDataTable(
    options = list(pageLength = 10, autoWidth = FALSE),
    callback = "function(table) {
    table.on('click.dt', 'tr', function() {
    $(this).toggleClass('selected');
    Shiny.onInputChange('rows',
    table.rows('.selected').data().toArray());
    });
}",
{
  if(!is.null(datGet())) {
    data<- datGet()
    colnames(data)<- c("De","De error")
    data
  } else {
    colnames(data)<- c("De","De error")
    data
  }
  })##EndOf::renterTable()

# renderTable() that prints the secondary data to the second tab
output$dataset2<- renderDataTable(
  options = list(pageLength = 10, autoWidth = FALSE),
  callback = "function(table) {
  table.on('click.dt', 'tr', function() {
  $(this).toggleClass('selected');
  Shiny.onInputChange('rows',
  table.rows('.selected').data().toArray());
  });
  }",
{
  if(!is.null(datGet2())) {
    data<- datGet2()
    colnames(data)<- c("De","De error")
    data
  } else {
  }
  })##EndOf::renterTable()
  
  
  # renderTable() to print the results of the
  # central age model (CAM)
  output$CAM<- renderDataTable(
    options = list(pageLength = 10, autoWidth = FALSE),
{
  if(!is.null(datGet())) {
    if(!is.null(datGet2())) {
      data<- list(datGet(), datGet2())
    } else {
      data<- list(datGet())
    }
  } else {
    data<- list(data)
  }
  t<- as.data.frame(matrix(nrow = length(data), ncol = 7))
  colnames(t)<- c("Data set","n", "log data", "Central dose", "SE abs.", "OD (%)", "OD error (%)")
  res<- lapply(data, function(x) { calc_CentralDose(x, verbose = FALSE, plot = FALSE) })
  for(i in 1:length(res)) {
    t[i,1]<- ifelse(i==1,"pimary","secondary")
    t[i,2]<- length(res[[i]]@data$data[,1])
    t[i,3]<- res[[i]]@data$args$log
    t[i,4:7]<- round(res[[i]]@data$summary[1:4],2)
  }
  t
})##EndOf::renterTable()
  
})##EndOf::shinyServer(function(input, output)