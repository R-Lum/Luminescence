#' Create a JSColor picker input widget
#' 
#' Creates a JSColor (Javascript/HTML Color Picker) widget to be used in shiny applications. 
#' 
#' @param inputId [`character`] (**required**): 
#' Specifies the input slot that will be used to access the value.
#' 
#' @param label [`character`] (*optional*): 
#' Display label for the control, or NULL for no label.
#' 
#' @param value [`character`] (*optional*): 
#' Initial RGB value of the color picker. Default is black ('#000000').
#' 
#' @param position [`character`] (*with default*): 
#' Position of the picker relative to the text input ('bottom', 'left', 'top', 'right').
#' 
#' @param color [`character`] (*with default*): 
#' Picker color scheme ('transparent' by default). Use RGB color coding ('000000').
#' 
#' @param mode [`character`] (*with default*): 
#' Mode of hue, saturation and value. Can either be 'HSV' or 'HVS'.
#' 
#' @param slider [`logical`] (*with default*): 
#' Show or hide the slider.
#' 
#' @param close [`logical`] (*with default*): 
#' Show or hide a close button.
#'  
#' @seealso Other input.elements: [`animationOptions`], [`sliderInput`]; 
#' [`checkboxGroupInput`]; [`checkboxInput`]; [`dateInput`]; 
#' [`dateRangeInput`]; [`fileInput`]; [`numericInput`]; 
#' [`passwordInput`]; [`radioButtons`]; [`selectInput`], 
#' [`selectizeInput`]; [`submitButton`]; [`textInput`]
#'  
#' @examples 
#' # html code
#' jscolorInput("col", "Color", "21BF6B", slider = FALSE)
#' 
#' # example app
#' \dontrun{
#' shinyApp(
#' ui = fluidPage(
#'   jscolorInput(inputId = "col", label = "JSColor Picker", 
#'                value = "21BF6B", position = "right", 
#'                mode = "HVS", close = TRUE),
#'   plotOutput("plot")
#' ),
#' server = function(input, output) {
#'   output$plot <- renderPlot({
#'     plot(cars, col = input$col, cex = 2, pch = 16)
#'  })
#' })
#' }
#' @import shiny
#' 
#' @md
#' @export
jscolorInput <- function(inputId, label, value, position = 'bottom', color = 'transparent', mode = 'HSV', slider = TRUE, close = FALSE) {
  tagList(        
    singleton(tags$head(tags$script(src = "RLumShiny/jscolor_inputBinding.js"))),
    singleton(tags$head(tags$script(src = "RLumShiny/jscolor/jscolor.js"))),
    if (missing(label)) { tags$p(" ") } else if (!is.null(label)) { tags$p(label) },
    tags$input(id = inputId, 
               value = ifelse(!missing(value), value, "#000000"), 
               class = sprintf("color {hash:true, pickerPosition:'%s', pickerBorderColor:'transparent', pickerFaceColor:'%s', pickerMode:'%s', slider:%s, pickerClosable:%s} shiny-bound-input", position, color, mode, tolower(slider), tolower(close)), 
               onchange = sprintf("$('#%s').trigger('afterChange')", inputId)),
    tags$script(sprintf("$('#%s').trigger('afterChange')", inputId))
  )
}