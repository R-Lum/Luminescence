##############################################################################
###                        MAIN PROGRAM                                    ###
##############################################################################
function(input, output, session) {
  
  # input data (with default)
  values <- reactiveValues(data_primary =  ExampleData.DeValues$BT998[7:11,],
                           data_secondary =  setNames(as.data.frame(matrix(NA_real_, nrow = 5, ncol = 2)), c("x", "y")))
  
  # check and read in file (DATA SET 1)
  observeEvent(input$file1, {
    inFile<- input$file1
    
    if(is.null(inFile)) 
      return(NULL) # if no file was uploaded return NULL
    
    values$data_primary <- fread(file = inFile$datapath, data.table = FALSE) # inFile[1] contains filepath 
  })
  
  # check and read in file (DATA SET 2)
  observeEvent(input$file2, {
    inFile<- input$file2
    
    if(is.null(inFile)) 
      return(NULL) # if no file was uploaded return NULL
    
    values$data_secondary <- fread(file = inFile$datapath, data.table = FALSE) # inFile[1] contains filepath 
  })
  
  ### GET DATA SETS
  Data<- reactive({

    data <- list(values$data_primary, values$data_secondary)
    data <- lapply(data, function(x) { 
      x_tmp <- x[complete.cases(x), ]
      if (nrow(x_tmp) == 0) return(NULL)
      else return(x_tmp)
      })
    data <- data[!sapply(data, is.null)]
    data <- lapply(data, function(x) setNames(x, c("Dose", "Error")))
    
    return(data)
  })
  
  output$table_in_primary <- renderRHandsontable({
    rhandsontable(values$data_primary, 
                  height = 300, 
                  colHeaders = c("Dose", "Error"), 
                  rowHeaders = NULL)
  })
  
  observeEvent(input$table_in_primary, {
    
    # Workaround for rhandsontable issue #138 
    # https://github.com/jrowen/rhandsontable/issues/138
    df_tmp <- input$table_in_primary
    row_names <-  as.list(as.character(seq_len(length(df_tmp$data))))
    
    df_tmp$params$rRowHeaders <- row_names
    df_tmp$params$rowHeaders <- row_names
    df_tmp$params$rDataDim <- as.list(c(length(row_names),
                                        length(df_tmp$params$columns)))
    
    if (df_tmp$changes$event == "afterRemoveRow")
      df_tmp$changes$event <- "afterChange"
    
    if (!is.null(hot_to_r(df_tmp)))
      values$data_primary <- hot_to_r(df_tmp)
  })
  
  
  output$table_in_secondary <- renderRHandsontable({
    
    rhandsontable(values$data_secondary, 
                  height = 300,
                  colHeaders = c("Dose", "Error"), 
                  rowHeaders = NULL)
  })
  
  observeEvent(input$table_in_secondary, {
    
    # Workaround for rhandsontable issue #138 
    # https://github.com/jrowen/rhandsontable/issues/138
    # See detailed explanation above
    df_tmp <- input$table_in_secondary
    row_names <-  as.list(as.character(seq_len(length(df_tmp$data))))
    df_tmp$params$rRowHeaders <- row_names
    df_tmp$params$rowHeaders <- row_names
    df_tmp$params$rDataDim <- as.list(c(length(row_names),
                                        length(df_tmp$params$columns)))
    if (df_tmp$changes$event == "afterRemoveRow")
      df_tmp$changes$event <- "afterChange"
    
    if (!is.null(hot_to_r(df_tmp)))
      values$data_secondary <- hot_to_r(df_tmp)
    
  })
  
  
  output$xlim<- renderUI({
    
    data<- Data()

    n <- max(sapply(data, nrow))
    
    sliderInput(inputId = "xlim", label = "Range x-axis", 
                min = 0, max = n*2, 
                value = c(1, n+1))
  })
  
  observe({
    updateTextInput(session, inputId = "xlab", 
                    value = if(input$preheat==TRUE){"Preheat Temperature [\u00B0C]"}else{"# Aliquot"})
  })
  
  #### PLOT ####
  output$main_plot <- renderPlot({
    input$refresh
    
    data<- Data()
    
    
    outputOptions(x = output, name = "xlim", suspendWhenHidden = FALSE)
    validate(
      need(expr = input$xlim, message = 'Waiting for data... Please wait!')
    )
    
    
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
    
    
    # if custom datapoint color get RGB code from separate input panel
    if(input$color == "custom") {
      color<- input$rgb
    } else {
      color<- input$color
    }
    
    if(length(data) > 1) {
      # if custom datapoint color get RGB code from separate input panel
      if(input$color2 == "custom") {
        color2<- input$rgb2
      } else {
        color2<- input$color2
      }
    } else {
      if(input$preheat == TRUE) {
        color2<- color
      } else {
      color2<- "white"
      }
    }
    
    if(length(data)==1){
      given.dose<- input$dose
      legend<- input$legendname
    } else {
      given.dose<- c(input$dose, input$dose2)
      legend<- c(input$legendname, input$legendname2)
    }
    
    # save all arguments in a list
    args<- list(values = data, 
                error.range = input$error,
                given.dose = given.dose,
                summary = input$stats,
                summary.pos = input$sumpos,
                boxplot = input$boxplot,
                legend = legend,
                legend.pos = input$legend.pos,
                main = input$main,
                mtext = input$mtext,
                col = c(color, color2),
                pch = c(pch, pch2),
                xlab = input$xlab,
                ylab = input$ylab,
                xlim = input$xlim,
                ylim = input$ylim,
                cex = input$cex)
    
    if(input$preheat == TRUE) {
      
      n<- length(data[[1]][,1])
      ph<- c(input$ph1, input$ph2, input$ph3, input$ph4, input$ph5, input$ph6, input$ph7, input$ph8)
      ph<- ph[1:n]
      
      args<- c(args, "preheat" = NA)
      args$preheat<- ph
      
      args$pch<- rep(args$pch, n)
      args$col<- rep(args$col, n)
      
    }
    
    # plot DRT Results
    do.call(what = plot_DRTResults, args = args)
    
    
    # prepare code as text output
    str1 <- "data <- data.table::fread(file, data.table = FALSE)"
    
    if(!all(is.na(unlist(values$data_secondary)))) {
      str2 <- "file2 <- file.choose()"
      str3 <- "data2 <- data.table::fread(file2, data.table = FALSE)"
      str4 <- "data <- list(data, data2)"
      str1 <- paste(str1, str2, str3, str4, sep = "\n")
    }
    
    header <- paste("# To reproduce the plot in your local R environment",
                    "# copy and run the following code to your R console.",
                    "library(Luminescence)",
                    "file <- file.choose()",
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
    
    funCall <- paste0("plot_DRTResults(values = data,\n", verb.arg, ")")
    
    code.output <- paste0(header, funCall, collapse = "\n")
    
    # nested renderText({}) for code output on "R plot code" tab
    output$plotCode<- renderText({
      
      code.output
      
    })##EndOf::renderText({})
    
    
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
        
        # plot Abanico Plot 
        do.call(what = plot_DRTResults, args = args)
        
        dev.off()
      },#EO content =,
      contentType = "image"
    )#EndOf::dowmloadHandler()
    
  })
  
  # renderTable() that prints the data to the second tab
  output$dataset<- renderDataTable(
    options = list(pageLength = 10, autoWidth = FALSE),
    callback = "function(table) {
  table.on('click.dt', 'tr', function() {
  $(this).toggleClass('selected');
  Shiny.onInputChange('rows',
  table.rows('.selected').data().toArray());
  });}",
{
  data<- Data()
  colnames(data[[1]])<- c("De", "De error")
  data[[1]]
})##EndOf::renterTable()


# renderTable() that prints the data to the second tab
output$dataset2<- renderDataTable(
  options = list(pageLength = 10, autoWidth = FALSE),
  callback = "function(table) {
  table.on('click.dt', 'tr', function() {
  $(this).toggleClass('selected');
  Shiny.onInputChange('rows',
  table.rows('.selected').data().toArray());
  });}",
{
  data<- Data()
  if(length(data)>1) {
    colnames(data[[2]])<- c("De", "De error")
    data[[2]]
  }
})##EndOf::renterTable()

}
