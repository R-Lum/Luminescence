## UI.R
function(request) {
  fluidPage(
    titlePanel(NULL, windowTitle = "RLumShiny - transformCW"),
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
                                        checkboxInput(inputId = "showCW", 
                                                      label = "Show CW-OSL curve",
                                                      value = TRUE),
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
                                        textInput(inputId = "ylab1", 
                                                  label = "Label y-axis (left)",
                                                  value = "pseudo OSL [cts/s]"),
                                        textInput(inputId = "ylab2", 
                                                  label = "Label y-axis (right)",
                                                  value = "CW-OSL [cts/s]")
                               ),##EndOf::Tab_4
                               
                               RLumShiny:::exportTab("export", filename = "transformCW"),
                               RLumShiny:::aboutTab("about", "transformCW")
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
                  tabPanel("Output table", fluidRow(column(width = 12, dataTableOutput("dataset")))),
                  tabPanel("R code", verbatimTextOutput("plotCode"))
                )###EndOf::tabsetPanel
      )##EndOf::mainPanel
    ),##EndOf::sideBarLayout
    bookmarkButton(),
    downloadButton("exportScript", "Download transformed data", class="btn btn-success")
  )##EndOf::fluidPage
}