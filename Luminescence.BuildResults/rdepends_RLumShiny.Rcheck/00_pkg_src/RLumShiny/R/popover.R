#' Create a bootstrap button with popover
#' 
#' Add small overlays of content for housing secondary information.
#' 
#' @param title [`character`] (**required**): 
#' Title of the button.
#' 
#' @param content [`character`] (**required**): 
#' Text to be displayed in the popover.
#' 
#' @param header [`character`] (*optional*): 
#' Optional header in the popover.
#' 
#' @param html [`logical`] (*with default*): 
#' Insert HTML into the popover.
#' 
#' @param class [`logical`] (*with default*):
#' Bootstrap button class (e.g. "btn btn-danger"). 
#' 
#' @param placement  [`character`] (*with default*): 
#' How to position the popover - top | bottom | left | right | auto. 
#' When "auto" is specified, it will dynamically reorient the popover. 
#' For example, if placement is "auto left", the popover will display to the 
#' left when possible, otherwise it will display right.
#' 
#' @param trigger  [`character`] (*with default*): 
#' How popover is triggered - click | hover | focus | manual.
#' 
#' @examples 
#' # html code
#' popover("title", "Some content")
#' 
#' # example app
#' \dontrun{
#' shinyApp(
#' ui = fluidPage(
#'   jscolorInput(inputId = "col", label = "JSColor Picker", 
#'                value = "21BF6B", position = "right", 
#'                mode = "HVS", close = TRUE),
#'   popover(title = "Help!", content = "Call 911"),
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
popover <- function(
  title, 
  content, 
  header = NULL, 
  html = TRUE,
  class = "btn btn-default",                    
  placement = c('right', 'top', 'left', 'bottom'),
  trigger = c('click', 'hover', 'focus', 'manual')) {
  
  tagList(
    singleton(
      tags$head(
        tags$script("$(function() { $(\"[data-toggle='popover']\").popover(); })")
      )
    ),
    tags$a(
      tabindex = "0", href = NULL, role = "button", class = class, `data-toggle` = "popover",
      title = header, `data-content` = content, `data-animation` = TRUE, html = html,
      `data-placement` = match.arg(placement, several.ok=TRUE)[1],
      `data-trigger` = match.arg(trigger, several.ok=TRUE)[1],
      title
    )
  )
}
# helpPopup Button by Winston Chang from RStudio:
# https://gist.github.com/jcheng5/5913297
# Documentation: http://getbootstrap.com/javascript/#popovers-usage