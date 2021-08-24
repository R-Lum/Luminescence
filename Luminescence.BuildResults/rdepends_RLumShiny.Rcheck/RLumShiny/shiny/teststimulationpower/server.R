## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Title:   Test Stimulation Power App
## Authors: Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France)
## Contact: sebastian.kreutzer@u-bordeaux-montainge.fr
## Initial date:    2017-11-22
##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
shinyServer(function(input, output, session) {

    # TABPANEL Import ----------------------------------------------------------------------------
    ##reset
    observeEvent(input$ClearButton, {
        file_data <- NULL
        file_info <- NULL

        ##clear plot and text
        output$curves <- renderPlot({NULL})
        output$text <- renderText({NULL})
        output$df <- renderDataTable(NULL)
        updateRadioButtons(session, inputId = "SelectedCurves",
                           choices = "none")

    })

    ##import data
    observeEvent(input$file_data, {

          ##data import
          file_data <<- read_XSYG2R(
            file = as.list(input$file_data$datapath),
            fastForward = TRUE,
            verbose = FALSE
          )

          ##import info
          file_info <<- read_XSYG2R(
            file = as.list(input$file_data$datapath),
            fastForward = TRUE,
            verbose = FALSE,
            import = FALSE
          )


          ##update radio buttons with the curve types
          display_curves <- unique(unlist(lapply(file_data, names)))
          display_curves <- display_curves[grepl(x = display_curves, pattern = "SL (NA)", fixed = TRUE)]

          ##update ratio buttons
          updateRadioButtons(session, inputId = "SelectedCurves",
                             choices = display_curves,
                             selected = display_curves[1]
          )

          ##grep wanted curves
          curves <-
            get_RLum(file_data,
                     recordType = display_curves[1],
                     curveType = "measured",
                     drop = FALSE)

          ##sort out all heating curves
          curves <- lapply(curves, function(c){
             if(length(c@records) == 0)
                 return(NULL)

            records <- lapply(c@records, function(r){
              if(r@info$stimulator == "heating element"){
                return(NULL)
              }else{
                r
              }

            })

            ##remove NULL data
            records <- records[!sapply(records, is.null)]

            ##construct new RLum.Analysis object
            set_RLum(class = "RLum.Analysis", records = records)

          })

         ##remove NULL from list
         curves_rm <- !sapply(curves, is.null)
         file_info <- file_info[curves_rm,]
         curves <- curves[curves_rm]

         ##get structure and set slider
         xrange <- range(structure_RLum(merge_RLum(curves))[,c("x.min", "x.max")])
         yrange <- range(structure_RLum(merge_RLum(curves))[,c("y.min", "y.max")])
         updateSliderInput(session, inputId = "xrange",
                           value = xrange,
                           min = min(xrange), max = max(xrange))
         updateSliderInput(session, inputId = "yrange",
                           value = yrange,
                           min = min(yrange), max = max(yrange))

         ##create plot
         output$curves <- renderPlot({
           records <- Luminescence:::.unlist_RLum(get_RLum(curves))
           plot_RLum(
             set_RLum("RLum.Analysis", records = records),
             xlab = "Stimulation time [s]",
             ylab = "Stimulation power [mW/cm^2]",
             xlim = input$xrange,
             ylim = input$yrange,
             log = paste0(input$xaxislog, input$yaxislog),
             main = "Control Plot",
             legend = FALSE,
             col = rgb(0,0,0,.8),
             mtext = paste(length(records), "curves are displayed"),
             combine = TRUE)

         })

        ##create table with affected values
        df <- as.data.frame(t(vapply(1:length(curves), function(x){
          y_values <- structure_RLum(curves[[x]])[["y.max"]]
          pos <- curves[[x]]@records[[1]]@info[["position"]]
          test <- which(y_values < max(y_values) * 0.95)
          if(length(test) == 0){
            return(c(NA_character_,NA_character_,NA_character_, NA_character_))
          }else{
            return(c("",x,pos,paste(test, collapse = ",")))
          }

        }, character(4))))

        ##remove NA
        df[,1] <- file_info$name
        df <- na.exclude(df)

        if(nrow(df) > 0){
          colnames(df) <- c("FILE","ALQ", "POSITION", "ID AFFECTED CURVE(S)")
          rownames(df) <- NULL
          output$df <- renderDataTable(df)
          output$text <- renderText({"Stimulation power mismatch detected!"})

        }else{
          output$text <- renderText({"Everything looks OK"})

        }

    })

    ##modify curves
    observeEvent(input$Display, {
     if(input$SelectedCurves != "none"){
        ##grep wanted curves
        curves <-
            get_RLum(file_data,
                     recordType = input$SelectedCurves,
                     curveType = "measured",
                     drop = FALSE)

        ##sort out all heating curves
        curves <- lapply(curves, function(c){
            if(length(c@records) == 0)
                return(NULL)

            records <- lapply(c@records, function(r){
                if(r@info$stimulator == "heating element"){
                    return(NULL)
                }else{
                    r
                }

            })

            ##remove NULL data
            records <- records[!sapply(records, is.null)]

            ##construct new RLum.Analysis object
            set_RLum(class = "RLum.Analysis", records = records)

        })


        ##remove NULL from list
        curves_rm <- !sapply(curves, is.null)
        file_info <- file_info[curves_rm,]
        curves <- curves[curves_rm]

        ##update slider
        xrange <- range(structure_RLum(merge_RLum(curves))[,c("x.min", "x.max")])
        yrange <- range(structure_RLum(merge_RLum(curves))[,c("y.min", "y.max")])
        updateSliderInput(session, inputId = "xrange",
                          value = xrange,
                          min = min(xrange), max = max(xrange))
        updateSliderInput(session, inputId = "yrange",
                          value = yrange,
                          min = min(yrange), max = max(yrange))

        ##create plot
        output$curves <- renderPlot({
            records <- Luminescence:::.unlist_RLum(get_RLum(curves))
            plot_RLum(
                set_RLum("RLum.Analysis", records = records),
                xlab = "Stimulation time [s]",
                ylab = "Stimulation power [mW/cm^2]",
                main = "Control Plot",
                xlim = input$xrange,
                ylim = input$yrange,
                log = paste0(input$xaxislog, input$yaxislog),
                legend = FALSE,
                col = rgb(0,0,0,.8),
                mtext = paste(length(records), "curves are displayed"),
                combine = TRUE)

        })

        ##create table with affected values
        df <- as.data.frame(t(vapply(1:length(curves), function(x){
            y_values <- structure_RLum(curves[[x]])[["y.max"]]
            test <- which(y_values < max(y_values) * 0.95)
            if(length(test) == 0){
                return(c(NA_character_,NA_character_,NA_character_))
            }else{
                return(c("",x,paste(test, collapse = ",")))
            }

        }, character(3))))

        ##remove NA
        df[,1] <- file_info$name
        df <- na.exclude(df)

        if(nrow(df) > 0){
            colnames(df) <- c("FILE","ALQ", "ID AFFECTED CURVE(S)")
            rownames(df) <- NULL
            output$df <- renderDataTable(df)
            output$text <- renderText({"Stimulation power mismatch detected!"})

        }else{
            output$text <- renderText({"Everything looks OK"})

        }

     }

    })

    # Static pages --------------------------------------------------------------------------------
    output$about <- renderUI({
        HTML(markdown::markdownToHTML(knit('static/about.Rmd', quiet = TRUE, output = tempfile()), fragment.only = TRUE))
    })


})

