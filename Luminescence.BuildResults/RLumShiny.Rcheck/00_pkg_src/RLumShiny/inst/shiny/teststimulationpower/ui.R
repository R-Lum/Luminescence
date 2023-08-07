## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Title:   Test Stimulation Power App
## Authors: Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France)
## Contact: sebastian.kreutzer@u-bordeaux-montainge.fr
## Initial date: 2017-11-22
##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
fluidPage(
  tags$head(tags$style(".centerAlign{text-align: center;}")),
  tags$head(
    tags$style(HTML('#Display{background-color:lightgreen}'))
  ),
 titlePanel("Test Stimulation Power"),
   sidebarLayout(
     sidebarPanel(

    # PANEL File input ----------------------------------------------------------------------------
    tabsetPanel(
         tabPanel("File input", icon = icon("upload", lib = "glyphicon"),
                  br(),
                  fileInput("file_data",
                            accept = "*.xsyg",
                            label = "Select XSYG-files containing your measurement data...",
                            multiple = TRUE),
                  radioButtons("SelectedCurves", "Stimulation curves to display ...", "none"),
                  div(
                    actionButton(inputId = "Display", "Update curves!", col = "green", icon = icon("fas fa-sync")),
                    actionButton(inputId = "ClearButton", "Reset!")
                    , align = "center")

          ),

    # PANEL Modify plot ---------------------------------------------------------------------------
    tabPanel("Modify plot", icon = icon("equalizer", lib = "glyphicon"),
       h4("x-axis"),
       sliderInput("xrange", "",
                   value = c(0,0),
                   min = min(xrange), max = max(xrange)),
       div(
       radioButtons("xaxislog", label = "",
                    selected = "", inline = TRUE,
                    choiceNames = c("linear", "log"),
                    choiceValues = c("", "x")),
       align = "center"
       ),
       hr(),
       h4("y-axis"),
       sliderInput("yrange", "",
                   value = c(0,0),
                   min = min(yrange), max = max(yrange)),
       div(
         radioButtons("yaxislog", label = "",
                      selected = "", inline = TRUE,
                      choiceNames = c("linear", "log"),
                      choiceValues = c("", "y")),
         align = "center"
       )
       ),id = "InputPanel",

    # PANEL About ---------------------------------------------------------------------------------
    tabPanel("About", icon = icon("info-sign", lib = "glyphicon"),
            uiOutput('about'),
            id = "AboutPanel")
       ), #end tabsetPanel
     br(),
     fluidRow(
       column(width = 12,
        wellPanel(
         h4(
         textOutput("text")
         ), class = 'centerAlign'))),
     tags$hr(style="border-color: darkred;"),
     p(HTML("This software comes WITHOUT ANY WARRANTY."), align = "center")
    ),

  # MAINPANEL -----------------------------------------------------------------------------------
  mainPanel(
      plotOutput("curves"),
      DT::dataTableOutput("df")

    )

  )##sidebarLayout
)##fluidPage


