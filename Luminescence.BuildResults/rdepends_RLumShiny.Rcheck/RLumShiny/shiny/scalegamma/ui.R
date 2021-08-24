## UI.R
function(request) {
  fluidPage(
    titlePanel(NULL, windowTitle = "RLumShiny - Scale Gamma Dose"),
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
                                        rHandsontableOutput(outputId = "table_in_primary"),
                                        helpText(HTML(paste0(
                                          tags$b("NOTE: "), "The uploaded file must have exactly 12 columns (<i>see pre-loaded data set above</i>). ",
                                          "Only one value in 'Sample offset (cm)' allowed, which indicates the position of the sample in a layer, measured from the bottom of respective layer.",
                                          "<br>Right-click on the table to add or remove rows. Copy-paste is supported.")
                                        ))
                                        
                               ),##EndOf::Tab_1
                               
                               tabPanel("Settings",
                                        tags$br(),
                                        selectInput(inputId = "frac_dose", "Fractional gamma dose table", 
                                                    choices = c("Aitken 1985" = "Aitken1985")
                                        ),
                                        selectInput(inputId = "conv_fac", "Conversion Factors", 
                                                    choices = c(
                                                      "Cresswell et al. 2019" = "Cresswelletal2019",
                                                      "Liritzis et al. 2013" = "Liritzisetal2013",
                                                      "Guerin et al. 2011" = "Guerinetal2011",
                                                      "Adamiec & Aitken 1998" = "AdamiecAitken1998"
                                                    )
                                        )
                               ),
                               
                               RLumShiny:::exportTab("export", filename = "scalegammadose"),
                               RLumShiny:::aboutTab("about", "scalegammadose")
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
                  tabPanel("Plot",
                           htmlOutput("error"),
                           plotOutput(outputId = "main_plot", height = "500px"),
                           htmlOutput(outputId = "console")),
                  tabPanel("Infinite matrix \u1E0A\u03B3", dataTableOutput("df_inf")),
                  tabPanel("Scaled \u1E0A\u03B3", dataTableOutput("df_scaled")),
                  tabPanel("R code", verbatimTextOutput("plotCode"))
                )###EndOf::tabsetPanel
      )##EndOf::mainPanel
    ),##EndOf::sideBarLayout
    bookmarkButton()
  )##EndOf::fluidPage
}

