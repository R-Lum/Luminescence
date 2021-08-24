## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Title:   Filter_app
## Authors: Urs Tilmann Wolpert, Department of Geography, Justus-Liebig-University Giessen
##          Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France)
## Contact: urs.t.wolpert@geogr.uni-giessen.de
## Date:    Thu June 22 2017
##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
shinyServer(function(input, output, session) {

  session$onSessionEnded(function() {
    stopApp()
  })
  
  #check for own set filter dataset
  output$filters <- renderUI({

    if(!is.null(input$own_file)){

      ##rename file in path
      file.rename(
        from = input$own_file$datapath,
        to = paste0(input$own_file$datapath,input$own_file$name))

      ##set new datapath
      database_path <<- paste0(input$own_file$datapath,input$own_file$name)

      ##set new filter list ... and do not filter them, we have no idea
      filters <- readxl::excel_sheets(database_path)

      ##update input for optical density
      updateSelectInput(session, "opticaldensity", choices = filters)

    }

    ##create chooser output
    RLumShiny:::chooserInput("filterInput", "Filters available:", "Filters chosen:", filters, c(),
                 multiple = TRUE,  size = 5)

  })

  # Transmission: Prepare data + plot
  output$filterPlot <- renderPlot({

    if (length(input$filterInput$right) != 0) {
      data <- lapply(input$filterInput$right, function(x) {
        as.matrix(readxl::read_excel(
          path = database_path,
          sheet = x,
          skip = 14
        ))

      })

   filter.matrix <- plot_FilterCombinations(filters = data,
                              xlim = input$range,
                              main = input$main,
                              legend = input$legend,
                              legend.text = input$filterInput$right,
                              interactive = FALSE)
      if(input$stimulationInput == "NA"){
        NA}
      if(input$stimulationInput == "violett"){
        rect(402, 0, 408, 1, col = "purple", lty = 0)}
      if(input$stimulationInput == "green"){
        rect(505, 0, 545, 1, col = "green", lty = 0)}
      if(input$stimulationInput == "blue"){
        rect(455, 0, 462, 1, col = "blue", lty = 0)}
      if(input$stimulationInput == "infrared"){
        rect(847, 0, 853, 1, col = "red", lty = 0)}
     if(input$stimulationInput == "custom"){
       rect(input$stimulationInput_custom_centre - input$stimulationInput_custom_width/2, 0,
            input$stimulationInput_custom_centre + input$stimulationInput_custom_width/2, 1,
            col = input$rec_colour, lty = 0)}

    }
  })

  # Optical Density: Prepare data + plot
  output$densityPlot <- renderPlot({
    data <- as.matrix(readxl::read_excel(
        path = database_path,
        sheet = input$opticaldensity,
        skip = 14
      ))
  data[is.na(data[,3]),3] <- max(data[,3],na.rm = TRUE)
     plot(data[,c(1,3)], type = "l",
          xlim = input$rangeOD,
          xlab = "Wavelength [nm]",
          ylab = "Optical Density [a. u.]",
          main = input$mainOD)

  })
  # Metadata
  output$metadata <- renderTable({
    if (length(input$filterInput$right) != 0) {
      data <- lapply(input$filterInput$right, function(x) {
        data <- as.data.frame(t(readxl::read_excel(
          path = database_path,
          sheet = x,
          col_names = FALSE,
          n_max = 7)),
          stringsAsFactors = FALSE)

        ##change column names & remove unwanted characters
        colnames(data) <- gsub(pattern = ":", replacement = "", x = as.character(data[1,]), fixed = TRUE)

        ##remove first row
        data <- data[-1,]

        ##remove NA values
        data <- data[!sapply(data[,1],is.na),]

        ##remove row with "BACK to Filterlist"
        data <- data[!grepl(pattern = "Back to Filterlist", x = data[,1]), ]

      })


      data.table::rbindlist(data)



    }
  })

  # Transmission: plot download
  output$exportPlot <- downloadHandler(
    filename = function() {
      paste(input$filename, ".pdf", sep = "")
    },
    content = function(file) {
      pdf(file,
          width = input$widthInput,
          height = input$heightInput,
          paper = "special")
      if (length(input$filterInput$right) != 0) {
        data <- lapply(input$filterInput$right, function(x) {
          as.matrix(readxl::read_excel(
            path = database_path,
            sheet = x,
            skip = 14))
        })
        plot_FilterCombinations(filters = data,
                                d = input$thicknessInput,
                                P = input$reflectionInput,
                                xlim = input$range,
                                main = input$main,
                                legend = input$legend,
                                legend.text = input$filterInput$right,
                                interactive = FALSE)
        if(input$stimulationInput == "NA"){
          NA}
        if(input$stimulationInput == "violett"){
          rect(402, 0, 408, 1, col = "purple", lty = 0)}
        if(input$stimulationInput == "green"){
          rect(505, 0, 545, 1, col = "green", lty = 0)}
        if(input$stimulationInput == "blue"){
          rect(455, 0, 462, 1, col = "blue", lty = 0)}
        if(input$stimulationInput == "infrared"){
          rect(847, 0, 853, 1, col = "red", lty = 0)}


      }
      dev.off()
    }
  )
 # Transmission: data-table download
  output$exportTable <- downloadHandler(
    filename = function(){
      paste(input$filename, ".csv", sep = "")
    },
    content = function(file) {
      if (length(input$filterInput$right) != 0) {
        data <- lapply(input$filterInput$right, function(x) {
          as.matrix(readxl::read_excel(
            path = database_path,
            sheet = x,
            skip = 14
          ))

        })

        filter.matrix <- plot_FilterCombinations(filters = data,
                                                 xlim = input$range,
                                                 main = input$main,
                                                 legend = input$legend,
                                                 legend.text = input$filterInput$right,
                                                 interactive = FALSE)

        write.csv(filter.matrix$filter_matrix, file)
      }
    }
  )

 # Optical Density: plot download
  output$exportPlotOD <- downloadHandler(
    filename = function() {
      paste(input$filenameOD, ".pdf", sep = "")
    },
    content = function(file) {
      pdf(file,
          width = input$widthInputOD,
          height = input$heightInputOD,
          paper = "special")
      data <- as.matrix(readxl::read_excel(
        path = database_path,
        sheet = input$opticaldensity,
        skip = 14
      ))
      data[is.na(data[,3]),3] <- max(data[,3],na.rm = TRUE)
        plot(data[,c(1,3)], type = "l",
             xlim = input$rangeOD,
             xlab = "Wavelength [nm]",
             ylab = "Optical Density [a. u.]",
             main = input$mainOD)

      dev.off()
    }
  )

  # Optical Density: data table download
  output$exportTableOD <- downloadHandler(
    filename = function(){
      paste(input$filenameOD, ".csv", sep = "")
    },
    content = function(file) {
      data <- as.matrix(readxl::read_excel(
        path = database_path,
        sheet = input$opticaldensity,
        skip = 14
      ))

        write.csv(data, file)

    }
  )


 # Download Filterdatabase Master File
output$MasterFile <- downloadHandler(
  filename = "Filterdatabase.xlsx",
  content = function(file){
    file.copy(database_path, file)

  })

  ##set warning for template data
    ##Transmission Tab
    output$warningtext <- renderUI({

        if(is.null(input$own_file)){
         if (grepl(pattern = "template", x = database_path, fixed = TRUE)){
            div(
              "Attention: Template data set. No real filter data!",
              style = "color:red; font-size:15px",
              align = "center"
            )
         }

        }else{
          div(paste0("Using custom filter database: ", basename(database_path)), align = "center")

         }
      })

    ##Optical Density Tab
    output$warningtextOD <- renderUI({

      if(is.null(input$own_file)){
        if (grepl(pattern = "template", x = database_path, fixed = TRUE)){
          div(
            "Attention: Template data set. No real filter data!",
            style = "color:red; font-size:15px",
            align = "center"
          )
        }

      }else{
        div(paste0("Using custom filter database: ", basename(database_path)), align = "center")
    }
    })


}
)

