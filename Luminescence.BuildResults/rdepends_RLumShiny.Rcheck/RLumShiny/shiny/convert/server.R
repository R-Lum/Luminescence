## Server.R
## MAIN FUNCTION
function(input, output, session) {
  
  
  # input data (with default)
  values <- reactiveValues(data = NULL, 
                           data_filtered = NULL, 
                           positions = NULL, 
                           types = NULL)
  
  session$onSessionEnded(function() {
    stopApp()
  })
  
  # check and read in file (DATA SET 1)
  observeEvent(input$import, {
    inFile<- input$file
    
    if(is.null(inFile)) 
      return(NULL)
    
    # 1. Risoe .bin(x)
    if (tools::file_ext(inFile$name) == "bin" || tools::file_ext(inFile$name) == "binx") {
      
      # rename temp file
      file <- paste0(inFile$datapath, ".", tools::file_ext(inFile$name))
      file.rename(inFile$datapath, file)
      
      # import the file
      values$data <- read_BIN2R(file, fastForward = TRUE, verbose = FALSE)
      values$data_filtered <- values$data
      
      # set some diagnostic values
      values$positions <- unique(sapply(values$data, function(x) { x@records[[1]]@info$POSITION }))
      values$types <- unique(sapply(values$data[[1]]@records, function(x) { x@recordType }))
    }
    
  })
  
  output$positions <- renderUI({
    if (!is.null(values$positions))
      checkboxGroupInput("positions", "Positions", 
                         choiceNames = as.character(values$positions), choiceValues = 1:length(values$positions), 
                         selected = 1:length(values$positions),
                         inline = TRUE)
  })
  
  output$curveTypes <- renderUI({
    if (!is.null(values$types))
      checkboxGroupInput("curveTypes", "Curve types", 
                         choices = values$types, selected = values$types)
  })
  
  
  ## FILTER ----
  observe({
    if (is.null(values$data))
      return(NULL)
    
    data_filtered <- values$data[as.numeric(input$positions)]
    
    values$data_filtered <- lapply(data_filtered, function(x) {
      subset(x, recordType %in% input$curveTypes)
    })
    
  })
  
  ## --------------------- OUTPUT ------------------------------------------- ##
  output$positionTabs <- renderUI({
    
    if (is.null(values$data_filtered))
      return(NULL)
    
    tabs <- lapply(values$positions[as.numeric(input$positions)], function(pos) {
      tabPanel(pos,
               plotOutput(paste0("pos", pos))) 
    })
    do.call(tabsetPanel, c(id = "tab", tabs))
    
  })

  observe({
    
    input$tab
    values$data
    values$data_filtered
    input$curveTypes
    
    if (is.null(values$data_filtered) || length(values$data_filtered) == 0)
      return(NULL)
    
    pos <- which(unique(sapply(values$data_filtered, function(x) { x@records[[1]]@info$POSITION })) == input$tab)
    print(pos)
    
    if (length(pos) > 0)
      updateCheckboxGroupInput(session, "curves", 
                               choices = 1:length(values$data_filtered[[pos]]),
                               selected = 1:length(values$data_filtered[[pos]]),
                               inline = TRUE)
  })
  
  observeEvent(input$export, {
    if (is.null(values$data_filtered))
      return(NULL)
    
    do.call(input$targetFile, values$data_filtered)
  })
  
  observe({
    pos_sel <- values$positions[as.numeric(input$positions)]
    pos_sel_index <- which(values$positions %in% pos_sel)
    
    for (i in 1:length(pos_sel))
      
      # Explanation on local({}):
      # https://gist.github.com/wch/5436415/
      local({
        local_i <- i
        output[[paste0("pos", pos_sel[local_i])]] <- renderPlot({
          
          if (is.null(values$data_filtered[[local_i]])) {
            plot(0, type = "n", axes = FALSE, ann = FALSE)
            return(NULL)
          } else {
            plot(values$data_filtered[[local_i]], combine = TRUE)
          }
          
        })
      })
  })
  
  
  
}##EndOf::function(input, output)