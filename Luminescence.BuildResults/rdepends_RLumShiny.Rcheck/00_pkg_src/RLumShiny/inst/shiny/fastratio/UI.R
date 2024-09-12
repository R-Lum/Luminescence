## UI.R
function(request) {
  fluidPage(
    titlePanel(NULL, windowTitle = "RLumShiny - Fast Ratio"),
    sidebarLayout(
      # 2- width = 5 -> refers to twitters bootstrap grid system
      # where the the maximum width is 12 that is to be shared between all
      # elements
      sidebarPanel(width = 5,
                   # include a tabs in the input panel for easier navigation
                   tabsetPanel(id = "tabs", type = "pill", selected = "Data",
                               # Tab 1: Data input
                               tabPanel("Data",
                                        
                                        # informational text
                                        div(align = "center", h5("Data upload")),
                                        # file upload button (data set 1)
                                        fileInput(inputId = "file", 
                                                  label = strong("Primary data set"),
                                                  accept="text/plain, .csv, text/csv"),
                                        # rhandsontable input/output
                                        fluidRow(
                                          column(width = 6,
                                                 rHandsontableOutput(outputId = "table_in_primary")
                                          ),
                                          column(width = 6)
                                        )
                                        
                               ),##EndOf::Tab_1
                               
                               tabPanel("Method",
                                        div(align = "center", h5("Input data preprocessing")),
                                        sliderInput(inputId = "deadchannels", "Dead channels", value = c(1, 1000), min = 1, max = 1000, step = 1, 
                                                    dragRange = TRUE),
                                        
                                        div(align = "center", h5("Simulation source")),
                                        
                                        # Stimulation power and wavelength
                                        fluidRow(
                                          column(width = 6,
                                                 numericInput(inputId = "stimpow", 
                                                              label = "Irradiance (W/cm^2)", 
                                                              value = 30.6, 
                                                              min = 0.1, 
                                                              step = 0.1)
                                          ),
                                          column(width = 6,
                                                 numericInput(inputId = "wavelength", 
                                                              label = "Wavelength (nm)", 
                                                              value = 470, 
                                                              min = 1, 
                                                              step = 1)
                                          )
                                        ),
                                        
                                        div(align = "center", h5("Photoionisation cross-sections (cm^2)")),
                                        # Photoionisation cross-sections
                                        fluidRow(
                                          column(width = 6,
                                                 HTML("Fast component"),
                                                 fluidRow(
                                                   column(width = 6,
                                                          numericInput(inputId = "cs1base", 
                                                                       label = "Base value", 
                                                                       value = 2.60, 
                                                                       min = 0.01, 
                                                                       step = 0.1)
                                                   ),
                                                   column(width = 6,
                                                          numericInput(inputId = "cs1exp", 
                                                                       label = "Exponent", 
                                                                       value = 17, 
                                                                       min = 1, 
                                                                       step = 1)
                                                   )
                                                 )
                                          ),
                                          column(width = 6,
                                                 HTML("Medium component"),
                                                 fluidRow(
                                                   column(width = 6,
                                                          numericInput(inputId = "cs2base", 
                                                                       label = "Base value", 
                                                                       value = 4.28, 
                                                                       min = 0.01, 
                                                                       step = 0.01)
                                                   ),
                                                   column(width = 6,
                                                          numericInput(inputId = "cs2exp", 
                                                                       label = "Exponent", 
                                                                       value = 18, 
                                                                       min = 1, 
                                                                       step = 1)
                                                   )
                                                 )
                                          )
                                        ),
                                        div(align = "center", h5("Channels")),
                                        # L1
                                        checkboxInput(inputId = "overrideL1", "Override channel for L1", value = FALSE),
                                        conditionalPanel("input.overrideL1 == true", 
                                                         # TODO: call updateSlider in Server.R to update max range
                                                         sliderInput(inputId = "L1", "Channel L1", value = 1, min = 1, max = 1000, step = 1)),
                                        # L2
                                        checkboxInput(inputId = "overrideL2", "Override channel for L2", value = FALSE),
                                        conditionalPanel("input.overrideL2 == true", 
                                                         # TODO: call updateSlider in Server.R to update max range
                                                         sliderInput(inputId = "L2", "Channel L2", value = 50, min = 1, max = 1000, step = 1)),
                                        # L3
                                        checkboxInput(inputId = "overrideL3", "Override channels for L3", value = FALSE),
                                        conditionalPanel("input.overrideL3 == true", 
                                                         # TODO: call updateSlider in Server.R to update max range
                                                         sliderInput(inputId = "L3", "Channel L3", value = c(400, 600), min = 1, max = 1000, step = 1, 
                                                                     dragRange = TRUE)),
                                        
                                        div(align = "center", h5("% of signal remaining")),
                                        fluidRow(
                                          column(width = 6,
                                                 numericInput(inputId = "x", label = "...from the fast component", value = 1, min = 0.1, max = 100, step = 0.1)
                                          ),
                                          column(width = 6,
                                                 numericInput(inputId = "x1", label = "...from the medium component", value = 0.1, min = 0.1, max = 100, step = 0.1)
                                          )
                                        )
                                        
                                        
                               ),
                               
                               tabPanel("Experimental",
                                        div(align = "center", h5("Curve fitting")),
                                        
                                        checkboxInput(inputId = "fitCWsigma", label = "Calculate and use photoionisaton cross-sections",
                                                      value = FALSE),
                                        checkboxInput(inputId = "fitCWcurve", label = "Derive fast ratio from fitted OSL curve",
                                                      value = FALSE) 
                                        
                                        ),
                               
                               tabPanel("Plot", 
                                        div(align = "center", h5("Title")),
                                        
                                        textInput(inputId = "main", 
                                                  label = "Title", 
                                                  value = "Fast Ratio"),
                                        
                                        radioButtons("type", "Type", selected = "b", inline = TRUE,
                                                     choices = c("Line" = "l",
                                                                 "Points" = "p",
                                                                 "Line+Points" = "b")),
                                        
                                        fluidRow(
                                          column(width = 6,
                                                 selectInput(inputId = "pch",
                                                             label = "Style",
                                                             selected = "1",
                                                             choices = c("Square"= "1",
                                                                         "Circle"="2",
                                                                         "Triangle point up"="3",
                                                                         "Plus"="4",
                                                                         "Cross"="5",
                                                                         "Diamond"="6",
                                                                         "Triangle point down"="7",
                                                                         "Square cross"="8",
                                                                         "Star"="9",
                                                                         "Diamond plus"="10",
                                                                         "Circle plus"="11",
                                                                         "Triangles up and down"="12",
                                                                         "Square plus"="13",
                                                                         "Circle cross"="14",
                                                                         "Square and Triangle down"="15",
                                                                         "filled Square"="16",
                                                                         "filled Circle"="17",
                                                                         "filled Triangle point up"="18",
                                                                         "filled Diamond"="19",
                                                                         "solid Circle"="20",
                                                                         "Bullet (smaller Circle)"="21",
                                                                         "Custom"="custom"))
                                          ),
                                          column(width = 6,
                                                 # show only if custom symbol is desired
                                                 conditionalPanel(condition = "input.pch == 'custom'",
                                                                  textInput(inputId = "custompch", 
                                                                            label = "Insert character", 
                                                                            value = "?"))
                                          )
                                        ),
                                        fluidRow(
                                          column(width = 6,
                                                 selectInput(inputId = "color", label = "Datapoint color",
                                                             choices = list("Black" = "black",
                                                                            "Grey" = "grey50",
                                                                            "Red" = "#b22222", 
                                                                            "Green" = "#6E8B3D", 
                                                                            "Blue" = "#428bca",
                                                                            "Custom" = "custom"))
                                          ),
                                          column(width = 6,
                                                 # show only if custom color is desired
                                                 conditionalPanel(condition = "input.color == 'custom'",
                                                                  HTML("Choose a color<br>"),
                                                                  jscolorInput(inputId = "jscol1"))
                                          )
                                        ),
                                        
                                        br(),
                                        div(align = "center", h5("Scaling")),
                                        sliderInput(inputId = "cex", 
                                                    label = "Scaling factor",
                                                    min = 0.5, max = 2, 
                                                    value = 1.0, step = 0.1)
                               ),##EndOf::Tab_3
                               
                               # Tab 4: modify axis parameters
                               tabPanel("Axis",
                                        div(align = "center", h5("X-axis")),
                                        checkboxInput(inputId = "logx",
                                                      label = "Logarithmic x-axis",
                                                      value = FALSE),
                                        textInput(inputId = "xlab", 
                                                  label = "Label x-axis",
                                                  value = "t (s)"),
                                        # inject sliderInput from Server.R
                                        br(),
                                        div(align = "center", h5("Y-axis")),
                                        checkboxInput(inputId = "logy",
                                                      label = "Logarithmic y-axis",
                                                      value = FALSE),
                                        textInput(inputId = "ylab", 
                                                  label = "Label y-axis (left)",
                                                  value = "Signal (cts)")
                               ),##EndOf::Tab_4
                               RLumShiny:::exportTab("export", filename = "fast ratio"),
                               RLumShiny:::aboutTab("about", "fastratio")
                   )##EndOf::tabsetPanel
      ),##EndOf::sidebarPanel
      
      # 3 - output panel
      mainPanel(width = 7,
                # insert css code inside <head></head> of the generated HTML file:
                # allow open dropdown menus to reach over the container
                tags$head(tags$style(type="text/css",".tab-content {overflow: visible;}")),
                tags$head(includeCSS("www/style.css")),
                # divide output in separate tabs via tabsetPanel
                fluidRow(
                  tabsetPanel(
                    tabPanel("Results", 
                             plotOutput(outputId = "main_plot", height = "500px"),
                             htmlOutput(outputId = "results")
                             ),
                    tabPanel("R code", verbatimTextOutput("plotCode"))
                  )
                )
      )##EndOf::mainPanel
    ),##EndOf::sideBarLayout
    bookmarkButton()
  )##EndOf::fluidPage
}