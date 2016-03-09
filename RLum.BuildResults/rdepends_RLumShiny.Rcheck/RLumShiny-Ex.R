pkgname <- "RLumShiny"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "RLumShiny-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('RLumShiny')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("app_RLum")
### * app_RLum

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: app_RLum
### Title: Run Luminescence shiny apps
### Aliases: app_RLum

### ** Examples

## Not run: 
##D # Plotting apps
##D app_RLum("abanico")
##D app_RLum("histogram")
##D app_RLum("KDE")
##D app_RLum("radialplot")
##D app_RLum("doserecovery")
##D 
##D # Further apps
##D app_RLum("cosmicdose")
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("app_RLum", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("jscolorInput")
### * jscolorInput

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: jscolorInput
### Title: Create a JSColor picker input widget
### Aliases: jscolorInput

### ** Examples

# html code
jscolorInput("col", "Color", "21BF6B", slider = FALSE)

# example app
## Not run: 
##D shinyApp(
##D ui = fluidPage(
##D   jscolorInput(inputId = "col", label = "JSColor Picker",
##D                value = "21BF6B", position = "right",
##D                mode = "HVS", close = TRUE),
##D   plotOutput("plot")
##D ),
##D server = function(input, output) {
##D   output$plot <- renderPlot({
##D     plot(cars, col = input$col, cex = 2, pch = 16)
##D  })
##D })
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("jscolorInput", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("popover")
### * popover

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: popover
### Title: Create a bootstrap button with popover
### Aliases: popover

### ** Examples

# html code
popover("title", "Some content")

# example app
## Not run: 
##D shinyApp(
##D ui = fluidPage(
##D   jscolorInput(inputId = "col", label = "JSColor Picker",
##D                value = "21BF6B", position = "right",
##D                mode = "HVS", close = TRUE),
##D   popover(title = "Help!", content = "Call 911"),
##D   plotOutput("plot")
##D ),
##D server = function(input, output) {
##D   output$plot <- renderPlot({
##D     plot(cars, col = input$col, cex = 2, pch = 16)
##D  })
##D })
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("popover", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("tooltip")
### * tooltip

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: tooltip
### Title: Create a bootstrap tooltip
### Aliases: tooltip

### ** Examples

# javascript code
tt <- tooltip("elementId", "This is a tooltip.")
str(tt)

# example app
## Not run: 
##D shinyApp(
##D ui = fluidPage(
##D   jscolorInput(inputId = "col", label = "JSColor Picker",
##D                value = "21BF6B", position = "right",
##D                mode = "HVS", close = TRUE),
##D   tooltip("col", "This is a JScolor widget"),
##D 
##D   checkboxInput("cbox", "Checkbox", FALSE),
##D   tooltip("cbox", "This is a checkbox"),
##D 
##D   checkboxGroupInput("cboxg", "Checkbox group", selected = "a",
##D                      choices = c("a" = "a",
##D                                  "b" = "b",
##D                                  "c" = "c")),
##D   tooltip("cboxg", "This is a <b>checkbox group</b>", html = TRUE),
##D 
##D   selectInput("select", "Selectinput", selected = "a", choices = c("a"="a", "b"="b")),
##D   tooltip("select", "This is a text input field", attr = "for", placement = "right"),
##D 
##D   passwordInput("pwIn", "Passwordinput"),
##D   tooltip("pwIn", "This is a password input field"),
##D 
##D   plotOutput("plot")
##D ),
##D server = function(input, output) {
##D   output$plot <- renderPlot({
##D     plot(cars, col = input$col, cex = 2, pch = 16)
##D  })
##D })
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("tooltip", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
