## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Title:   RCarb Shiny App
## Authors: Sebastian Kreutzer, IRAMAT-CRP2A, Université Bordeaux Montaigne (France)
## Contact: sebastian.kreutzer@u-bordeaux-montainge.fr
## Initial date: 2018-10-14
##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
shinyServer(function(input, output, session) {
    # Initialisation ------------------------------------------------------------------------------
    ##we run RCarb one time and create the table we need
    df <- RCarb::model_DoseRate(data = Example_Data[1,], n.MC = NULL, plot = FALSE, verbose = FALSE)
    df <- df[-1,]

    ##make table reactive
    values <- reactiveValues(
        df = df
    )

    ##render table
    output$df <- renderRHandsontable({
        rhandsontable(
            data = values$df,debug = TRUE,
            selectCallback = TRUE,
            readOnly = FALSE,
            customOpts = list(
                          csv = list(name = "Download to CSV",
                                     callback = htmlwidgets::JS(
                                         "function (key, options) {
                         var csv = csvString(this, sep=',', dec='.');

                         var link = document.createElement('a');
                         link.setAttribute('href', 'data:text/plain;charset=utf-8,' +
                           encodeURIComponent(csv));
                         link.setAttribute('download', 'data.csv');

                         document.body.appendChild(link);
                         link.click();
                         document.body.removeChild(link);
                       }")))) %>%
            hot_table(highlightCol = TRUE, highlightRow = TRUE, allowRowEdit = TRUE)


    })

    #feedback changes in the table
    observe({
        if(!is.null(input$df)){
            values$df <- hot_to_r(input$df)
        }


    })

    # Load example data ---------------------------------------------------------------------------
    observeEvent(input$load_example, {
        m <- matrix(NA, nrow = 2, ncol = length(colnames(values$df)) - ncol(Example_Data))
        temp <- cbind(Example_Data[c(1,14),], as.data.frame(m, stringsAsFactors = FALSE))
        colnames(temp) <- colnames(values$df)
        values$df <- temp

    })
    #
    #

    # File import ---------------------------------------------------------------------------------
    observeEvent(input$load_file, {
        ##check whether this is empty
        if(is.null(input$file$datapath)){
            return(NULL)
        }

        ##import
        temp <- read.table(
            file = input$file$datapath,
            header = as.logical(input$import_header),
            sep = input$import_sep)

        ##check input
        if (ncol(Example_Data) != ncol(temp) &&
             !all(colnames(Example_Data) == colnames(temp))) {
            showModal(modalDialog(
                title = "Important message",
                "Your input CSV-file does not appear to be correctly formated!
                Please try again or use the input template!",
                easyClose = TRUE
            ))
        return(NULL)
        }

        ##limit to the first columns
        m <- matrix(NA, nrow = nrow(temp), ncol = length(colnames(values$df)) - ncol(Example_Data))
        temp <- cbind(temp[,1:29], as.data.frame(m, stringsAsFactors = FALSE))
        colnames(temp) <- colnames(values$df)

        ##write into table
        values$df <- temp

    })

    # Calculation ---------------------------------------------------------------------------------
    observeEvent(input$run_calculation, {

     ##check input and return null if needed
     if(nrow(values$df) == 0){
         message("Input data has 0 rows, nothing was done!")
         return(NULL)
     }

     ##get temp dir
     temp_dir <- tempdir()

     ##run with progressbar
     withProgress(
         message = "Running calculations ...", min = 0, max = nrow(values$df), {

        ##run calculation and create plots
        for(i in 1:nrow(values$df)){
             incProgress(i)
             temp_files[[i]] <<- paste0(temp_dir,"/SAMPLE_",i,".png")
             png(file = temp_files[[i]], bg = "transparent", width = 800, height = 400, res = 100)
             values$df[i,] <- RCarb::model_DoseRate(
                 data =  values$df[i,1:29],
                 length_step = input$length_step,
                 max_time = input$max_time,
                 n.MC = input$n.MC,
                 verbose = TRUE,
                 plot = TRUE,
                 mfrow = c(1,2)
                 )
             dev.off()
         }

     })#end progressbar

     ##show first plot
     output$plot <- renderImage({
             ##grep correct aliquot
             temp_aliquot <- paste0("SAMPLE_1.png")

             ##set filename
             filename <- temp_files[[grep(pattern = temp_aliquot, x = temp_files,fixed = TRUE)]]

             #Return a list containing the filename and alt text
             list(src = filename,
                  alt = paste("Image number", temp_aliquot))

         }, deleteFile = FALSE)

    })


    # Graphical output ----------------------------------------------------------------------------
    observeEvent(input$df_select, {
        if(is.null(temp_files))
            return(NULL)

        ##grep correct aliquot
        temp_aliquot <- paste0("SAMPLE_",input$df_select$select$r,".png")

        ##return NULL if it does not exist
        if(length(grep(pattern = temp_aliquot, x = temp_files,fixed = TRUE)) == 0)
            return(NULL)

        ##render image
        output$plot <- renderImage({

            ##set filename
            filename <- temp_files[[grep(pattern = temp_aliquot, x = temp_files, fixed = TRUE)]]

            #Return a list containing the filename and alt text
            list(src = filename,
                 alt = paste("Image number", temp_aliquot))

        }, deleteFile = FALSE)

    })


    # Download for template -----------------------------------------------------------------------
    output$download_template <- downloadHandler(
        filename = "RCarb_InputTemplate.csv",
        content = function(file){

            ##use the internal function from RCarb
            RCarb::write_InputTemplate(file = file)
        },
        contentType = "text/csv"
    )


    # Render static pages -------------------------------------------------------------------------
    output$about <- renderUI({
        HTML(markdown::markdownToHTML(knit('static/about.Rmd', quiet = TRUE, output = tempfile()),
                                      fragment.only = TRUE))
    })

    output$news <- renderUI({
        HTML(markdown::markdownToHTML(knit('static/news.Rmd', quiet = TRUE, output = tempfile()),
                                      fragment.only = TRUE))
    })

})#EOF


