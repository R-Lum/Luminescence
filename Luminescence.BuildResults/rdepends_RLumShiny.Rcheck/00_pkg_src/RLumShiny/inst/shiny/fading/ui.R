## UI.R
function(request) {
  fluidPage(
    titlePanel(NULL, windowTitle = "RLumShiny - Fading"),
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
                                        hr(),
                                        helpText(HTML(paste(
                                          tags$b("PLEASE NOTE:"), "Estimation of the g-value and correcting for fading
                                          using the approach after Huntley & Lamothe (2001) is computationally expensive,
                                          which is why the number of Monte Carlo simulations is fixed to 100 and 1000,
                                          respectively. Consider running the code given in the R code panels with a higher
                                          number of MC iterations in a local R environment."
                                          )))
                               ),##EndOf::Tab_1
                               # Tab 2: Fading correction
                               tabPanel("Age correction",
                                        div(align = "center", h5("Fading correction after Huntley & Lamothe (2001)")),
                                        hr(),
                                        fluidRow(
                                          column(width = 6,
                                                 numericInput(inputId = "age_faded", HTML("Age (ka)", "<em>(faded)</em>"), 
                                                              min = 0, step = 1, value = 10)),
                                          column(width = 6,
                                                 numericInput(inputId = "age_error_faded", "Age error",
                                                              min = 0, step = 1, value = 1))
                                        ),
                                        checkboxInput(inputId = "override_gval", "Manual g-value", FALSE),
                                        conditionalPanel("input.override_gval == true",
                                                         fluidRow(
                                                           column(width = 6,
                                                                  numericInput(inputId = "g_value", "g-value (%/decade)", 
                                                                               min = 0, step = 0.01, value = 5.18)),
                                                           column(width = 6,
                                                                  numericInput(inputId = "g_value_error", "g-value error",
                                                                               min = 0, step = 0.01, value = 0.75))
                                                         ),
                                                         helpText(HTML(
                                                           "T<sub>c</sub> = time in seconds between irradiation and the 
                                                           prompt measurement (cf. Huntley & Lamothe 2001). </br></br>",
                                                           "T<sub>c, g-value</sub> =  the time in seconds between irradiation
                                                           and the prompt measurement used for estimating the g-value. 
                                                           If the g-value was normalised to, e.g., 2 days, this time in
                                                           seconds (i.e., 172800) should be given here. 
                                                           This time should be identical to t<sub>c</sub>, which is usually case for 
                                                           g-values obtained using the SAR method and g-values that had 
                                                           been not normalised to 2 days.")),
                                                         fluidRow(
                                                           column(width = 6,
                                                                  numericInput(inputId = "tc", HTML("T<sub>c</sub>"), 
                                                                               min = 0, step = 1, value = 378)),
                                                           column(width = 6,
                                                                  numericInput(inputId = "tc_gval", HTML("T<sub>c, g-value</sub>"),
                                                                               min = 0, step = 1, value = 172800))
                                                         )
                                                         
                                        )
                                        
                               ),
                               
                               # Tab 4: modify axis parameters
                               
                               RLumShiny:::exportTab("export", filename = "analyseFading"),
                               RLumShiny:::aboutTab("about", "fading")
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
                           plotOutput(outputId = "main_plot", height = "450px"),
                           fluidRow(
                             column(6, htmlOutput(outputId = "results")),
                             column(6, htmlOutput(outputId = "results_corr"))
                             )),
                  tabPanel("R code (g-value)", verbatimTextOutput("plotCode")),
                  tabPanel("R code (age correction)", verbatimTextOutput("corrCode"))
                )###EndOf::tabsetPanel
      )##EndOf::mainPanel
    ),##EndOf::sideBarLayout
    bookmarkButton()
  )##EndOf::fluidPage
}