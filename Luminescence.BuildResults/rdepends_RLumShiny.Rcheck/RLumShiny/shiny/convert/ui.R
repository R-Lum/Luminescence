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
                                                  label = strong("Measurement file"),
                                                  accept="application/octet-stream, .bin, .binx"),
                                        # import
                                        actionButton(inputId = "import", label = "Import", class = "btn btn-success"),
                                        tags$hr(),
                                        # dynamic elements depending on input file
                                        fluidRow(
                                          column(width = 6,
                                                 uiOutput("positions")
                                                 
                                          ),
                                          column(width = 6,
                                                 uiOutput("curveTypes")
                                          )
                                        )
                               ),##EndOf::Tab_1
                               
                               tabPanel("Curves",
                                        div(align = "center", h5("(De)select individual curves")),
                                        checkboxGroupInput("curves", "Curves")
                               ),##EndOf::Tab_4
                               
                               tabPanel("Export",
                                        selectInput("targetFile", label = "Export to...", 
                                                    choices = list(".bin(x)" = "write_R2BIN",
                                                                   ".csv" = "write_RLum2CSV")),
                                        actionButton("export", "Download file", class = "btn btn-success")
                                        )
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
                  uiOutput("positionTabs")
                )
      )##EndOf::mainPanel
    ),##EndOf::sideBarLayout
    bookmarkButton()
  )##EndOf::fluidPage
}