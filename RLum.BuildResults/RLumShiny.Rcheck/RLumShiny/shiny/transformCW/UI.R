## UI.R
library(RLumShiny)

pageWithSidebar(  
  # 1 - title = NULL -> Panel will not be shown
  headerPanel(title = NULL),
  
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
                                              accept="text/plain"),
                                    tooltip(refId = "file1", text = tags$img(src='file_structure.png', width='250px')),
                                    # informational text
                                    div(align = "center", h5("Settings")),
                                    fluidRow(
                                      column(width = 6,
                                             # logical: should NA values be excluded?
                                             checkboxInput(inputId = "naExclude", 
                                                           label = "Exclude NA values",
                                                           value = TRUE),
                                             tooltip(refId = "naExclude", text = "Exclude NA values from the data set prior to any further operations.")
                                      ),
                                      column(width = 6,
                                             # logical: file contains headers?
                                             checkboxInput(inputId = "headers", 
                                                           label = "File contains headers", 
                                                           value = FALSE),
                                             tooltip(refId = "headers", text = tags$img(src='file_containsHeader.png', width='250px'))
                                      )
                                    ),
                                    # char: columns separated by tab, space, comma
                                    radioButtons("sep", "Separator", selected = "\t", inline = TRUE,
                                                 c("Tab" = "\t",
                                                   "Space" = " ",
                                                   "Comma" = ",",
                                                   "Semicolon" = ";")),
                                    tooltip(refId = "sep", text = tags$img(src='file_sep.png', width='400px'), placement = "auto left")
                           ),##EndOf::Tab_1
                           
                           tabPanel("Method",
                                    hr(),
                                    div(align = "center", h5("Transformation settings")),
                                    radioButtons("method", "Method", selected = "CW2pHMi", 
                                                 choices = c("Hyperbolic" = "CW2pHMi",
                                                             "Linear" = "CW2pLM",
                                                             "Linear (interpolated)" = "CW2pLMi",
                                                             "Parabolic" = "CW2pPMi")
                                    ),
                                    conditionalPanel(condition = "input.method == 'CW2pHMi'",
                                                     numericInput("delta", "Delta", value = 1, min = 0)),
                                    conditionalPanel(condition = "input.method == 'CW2pLMi' || input.method == 'CW2pPMi'",
                                                      numericInput("p", "P", value = 1, min = 0))
                           ),
                           
                           tabPanel("Plot", 
                                    div(align = "center", h5("Title")),
                                    
                                    textInput(inputId = "main", 
                                              label = "Title", 
                                              value = "CW Curve Transfomation"),
                                    
                                    radioButtons("type", "Type", selected = "l", inline = TRUE,
                                                 choices = c("Line" = "l",
                                                             "Points" = "p")),
                                    
                                    fluidRow(
                                      column(width = 6,
                                             selectInput(inputId = "pch",
                                                         label = "Style",
                                                         selected = "17",
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
                                                  value = TRUE),
                                    textInput(inputId = "xlab", 
                                              label = "Label x-axis",
                                              value = "t [s]"),
                                    # inject sliderInput from Server.R
                                    br(),
                                    div(align = "center", h5("Y-axis")),
                                    checkboxInput(inputId = "logy",
                                                  label = "Logarithmic y-axis",
                                                  value = FALSE),
                                    textInput(inputId = "ylab", 
                                              label = "Label y-axis (left)",
                                              value = "pseudo OSL [cts/s]")
                           ),##EndOf::Tab_4
                           
                           # Tab 10: save plot as pdf, wmf or eps
                           tabPanel("Export",
                                    radioButtons(inputId = "fileformat", 
                                                 label = "Fileformat", 
                                                 selected = "pdf",
                                                 choices = c("PDF   (Portable Document Format)" = "pdf",
                                                             "SVG   (Scalable Vector Graphics)" = "svg",
                                                             "EPS   (Encapsulated Postscript)" = "eps")),
                                    textInput(inputId = "filename", 
                                              label = "Filename", 
                                              value = "transformed CW"),
                                    fluidRow(
                                      column(width = 6,
                                             numericInput(inputId = "imgheight",
                                                          label =  "Image height", 
                                                          value = 7)
                                      ),
                                      column(width = 6,
                                             numericInput(inputId = "imgwidth",
                                                          label = "Image width", 
                                                          value = 7)
                                      )
                                    ),
                                    selectInput(inputId = "fontfamily", 
                                                label = "Font", 
                                                selected = "Helvetica",
                                                choices = c("Helvetica" = "Helvetica",
                                                            "Helvetica Narrow" = "Helvetica Narrow",
                                                            "Times" = "Times",
                                                            "Courier" = "Courier",
                                                            "Bookman" = "Bookman",
                                                            "Palatino" = "Palatino")),
                                    tags$hr(),
                                    downloadButton(outputId = "exportFile", 
                                                   label = "Download plot"),
                                    tags$hr(),
                                    helpText("The transformed CW curve data can be downloaded as a comma separated 
                                             ASCII file."),
                                    
                                    downloadButton(outputId = "exportScript", 
                                                   label = "Download transformed data")
                           ),##EndOf::Tab_8
                           
                           # Tab 10: further information
                           tabPanel("About",
                                    hr(),
                                    div(align = "center",
                                        # HTML code to include a .png file in the tab; the image file must be in
                                        # a subfolder called "wwww"
                                        img(src="RL_Logo.png", height = 100, width = 100, alt = "R.Lum"),
                                        p("Links:"),
                                        a(href = "http://www.r-luminescence.de", "R.Luminescence project page", target="_blank"),
                                        br(),
                                        a(href = "https://forum.r-luminescence.de", "Message board", target="_blank"),
                                        br(),
                                        a(href = "http://zerk.canopus.uberspace.de/R.Lum", "Online application", target="_blank"),
                                        br(),hr(),
                                        img(src='GitHub-Mark-32px.png', width='32px', height='32px'),
                                        br(),
                                        a(href = "https://github.com/tzerk/RLumShiny/tree/master/inst/shiny/transformCW", "See the code at GitHub!", target="_blank")
                                    )#/div
                           )##EndOf::Tab_9
               )##EndOf::tabsetPanel
  ),##EndOf::sidebarPanel
  
  
  # 3 - output panel
  mainPanel(width = 7,
            # insert css code inside <head></head> of the generated HTML file:
            # allow open dropdown menus to reach over the container
            tags$head(tags$style(type="text/css",".tab-content {overflow: visible;}")),
            tags$head(includeCSS("www/style.css")),
            # divide output in separate tabs via tabsetPanel
            tabsetPanel(
              tabPanel("Plot", plotOutput(outputId = "main_plot", height = "500px")),
              tabPanel("Output table", fluidRow(column(width = 12, dataTableOutput("dataset"))))
            )###EndOf::tabsetPanel
  )##EndOf::mainPanel
)##EndOf::shinyUI(pageWithSidebar)