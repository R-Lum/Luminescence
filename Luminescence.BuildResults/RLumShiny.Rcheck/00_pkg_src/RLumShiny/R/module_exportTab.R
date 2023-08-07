exportCodeHandler <- function(input, output, session, code) {
  
  output$exportScript <- downloadHandler(
    filename = function() { paste(input$filename, ".", "R", sep="") },
    content = function(file) {
      write(code, file)
    },#EO content =,
    contentType = "text"
  )#EndOf::dowmloadHandler()
  
}

exportPlotHandler <- function(input, output, session, fun, args) {
  
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
      
      # plot  
      do.call(fun, args)
      
      dev.off()
    },#EO content =,
    contentType = "image"
  )#EndOf::dowmloadHandler()
}


exportTab <- function(id, filename) {
  
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tabPanel("Export",
           radioButtons(inputId = ns("fileformat"), 
                        label = "Fileformat", 
                        selected = "pdf",
                        choices = c("PDF   (Portable Document Format)" = "pdf",
                                    "SVG   (Scalable Vector Graphics)" = "svg",
                                    "EPS   (Encapsulated Postscript)" = "eps")),
           textInput(inputId = ns("filename"), 
                     label = "Filename", 
                     value = filename),
           fluidRow(
             column(width = 6,
                    numericInput(inputId = ns("imgheight"),
                                 label =  "Image height", 
                                 value = 7)
             ),
             column(width = 6,
                    numericInput(inputId = ns("imgwidth"),
                                 label = "Image width", 
                                 value = 7)
             )
           ),
           selectInput(inputId = ns("fontfamily"), 
                       label = "Font", 
                       selected = "Helvetica",
                       choices = c("Helvetica" = "Helvetica",
                                   "Helvetica Narrow" = "Helvetica Narrow",
                                   "Times" = "Times",
                                   "Courier" = "Courier",
                                   "Bookman" = "Bookman",
                                   "Palatino" = "Palatino")),
           tags$hr(),
           downloadButton(outputId = ns("exportFile"), 
                          label = "Download plot"),
           
           tags$hr(),
           helpText("Additionally, you can download a corresponding .R file that contains",
                    "a fully functional script to reproduce the plot in your R environment!"),
           
           downloadButton(outputId = ns("exportScript"), 
                          label = "Download R script")
           
  )
}