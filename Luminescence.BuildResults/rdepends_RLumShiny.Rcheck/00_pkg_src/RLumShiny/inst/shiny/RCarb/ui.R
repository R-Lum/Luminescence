## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Title:   RCarb Shiny App -ui.R
## Authors: Sebastian Kreutzer, IRAMAT-CRP2A, Université Bordeaux Montaigne (France)
## Contact: sebastian.kreutzer@u-bordeaux-montainge.fr
## Initial date: 2018-10-14
##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
shinyUI(
    navbarPage(
        title = HTML(paste0("RCarb App")),
        windowTitle = "RCarb App",
        footer = HTML(
            "<hr>
            <div align = 'center'><small>This software comes WITHOUT ANY WARRANTY.</small>
            </div>"),
    #
    #
    # PANEL - Analysis-----------------------------------------------------------------------------
    tabPanel(title = "Import - Analysis",
        sidebarLayout(
          sidebarPanel(
            tabsetPanel(
              tabPanel(title = "Import data",
              br(),
              fileInput("file",
                        accept = "*.csv",
                        label = "Select CSV-file with your data ...",
                        multiple = FALSE),
              div(
                radioButtons("import_header", label = "1st row is column header?",
                             choiceNames = c("yes", "no"), choiceValues = list(TRUE, FALSE), inline = TRUE),
                selectInput("import_sep", label = "Column separator", choices = c(",",";","&","$")),
                actionButton("load_file", label = "Load from file ...", icon("import", lib = "glyphicon"),
                             style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                actionButton("load_example", label = "Load example data"),
              align = 'center')
              ),
              tabPanel(title = "Run calculation",
               fluidRow(
                 column(6,
                  selectInput("conversion_factors", label = "Dose rate converison factors",
                              choices = Reference_Data$DR_conv_factors$REFERENCE),
                  numericInput(
                    inputId = "length_step", label = "Step length", value = 1, min = 1, max = 100, width = "100%"),
                  numericInput(
                    inputId = "max_time", label = "Max. time", value = 500, min = 1, max = 500, width = "100%")

                 ),
                 column(6,
                   numericInput(
                     inputId = "n.MC", label = "MC runs", value = 100, min = 1, max = 10000, width = "100%")
                 )
                ),
                div(
                 actionButton("run_calculation",
                               label = "Run calculation",
                               icon = icon("play-circle"),
                               style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
                  ), align = "center")
                ),##tabPanel
              tabPanel(
                title = "Input template",
                br(),
                div(
                  downloadButton("download_template",label = "Download input CSV-file template",
                                 icon = icon("download")),
                  align = "center"
                )
              )
             )##end TabsetPanel
          ),##end sidebarPanel
          mainPanel(
            rHandsontableOutput("df", height = "250px"),
            div(align = "center",
                plotOutput(outputId = "plot")
            )
          )##mainPanel
        ),##sidebarLayout
    icon = icon("dashboard", lib = "glyphicon")
    ),##tabPanel
    #
    #
    # PANEL - News ------------------------------------------------------------------------------
    tabPanel("News",
             fluidRow(
                 column(10, offset = 1,
                        uiOutput('news')
                 )
             ),icon = icon("list-alt", lib = "glyphicon")
    ),#news
    # PANEL - About ------------------------------------------------------------------------------
    tabPanel("About",
             fluidRow(
                 column(10, offset = 1,
                        uiOutput('about')
                 )
             ),icon = icon("info-sign", lib = "glyphicon")
    )#About
  )##navbarPage
)##EOF


