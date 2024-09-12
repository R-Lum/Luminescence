## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Title:   Filter_app
## Authors: Urs Tilmann Wolpert, Department of Geography, Justus-Liebig-University Giessen
##          Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France)
## Contact: urs.t.wolpert@geogr.uni-giessen.de
## Date:    Thu June 22 2017
##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
shinyUI(
 navbarPage("Filter_app",
  tabPanel("Transmission",
   sidebarLayout(
    sidebarPanel(
        # tabs on sidebar panel
        tabsetPanel(type = "pills", selected = "Data",
            # Tab 1: Data Transmission
            tabPanel("Data",
                  tags$hr(),
                  strong("Select filters"),
                  uiOutput(outputId = "filters"),
                  tags$hr(),
                  radioButtons(
                    "stimulationInput",
                    label = "Show stimulation wavelength",
                    choices =
                      c("None" = "NA",
                        "Violet: 405 \u0394 3 nm " = "violett",
                        "Blue: 458 \u0394 3 nm" = "blue",
                        "Green: 525 \u0394 20 nm" = "green",
                        "Infrared: 850 \u0394 3 nm" = "infrared",
                        "Custom wavelength" = "custom"
                        )
                    ),
                    fluidRow(
                      column(width = 10,
                          inputPanel(
                             numericInput("stimulationInput_custom_centre",
                                          label = "Centre", value = 470, width = 150, min = 2, max = 1000),

                             numericInput("stimulationInput_custom_width",
                                          label = "Width", value = 20, width = 150, min = 1, max = 1000),

                             RLumShiny:::jscolorInput("rec_colour", label = "Colour")))

                    )


            ), # End Tab 1

            # Tab 2: Plot Options Transmission
            tabPanel("Plot Options",
                     tags$hr(),
                     textInput("main",
                               label = "Plot title",
                               value = "Filter Combinations"),
                     tags$hr(),
                     sliderInput("range",
                                 "Wavelength range",
                                 min = 200,
                                 max = 1000,
                                 value = c(200, 1000)),
                     checkboxInput(inputId = "net_transmission",
                                   label = "Show net transmisison",
                                   value = TRUE),
                     checkboxInput(inputId = "legend",
                                   label = "Show legend",
                                   value = TRUE)


            ), # End Tab 2

            # Tab 3: Export plots + datatable Transmission
            tabPanel("Export",
                     tags$hr(),
                  textInput(
                    "filename",
                    label = "Filename",
                    value = "Enter filename..."),
                  tags$hr(),
                  fluidRow(
                          column(6,
                           numericInput(
                             "widthInput",
                             label = "Image width",
                             value = 7
                             )),
                          column(6,
                          numericInput(
                             "heightInput",
                             label = "Image height",
                             value = 7
                             ))),
                  downloadButton("exportPlot", label = "Download plot as PDF"),
                  tags$hr(),
                  downloadButton("exportTable", label = "Download raw data as CSV")
            ) # End Tab 3



            )),



    mainPanel(uiOutput(outputId = "warningtext"),
              plotOutput("filterPlot"),
              tableOutput("metadata")
              )
   )
  ),
  tabPanel("Optical Density",
   sidebarLayout(
     sidebarPanel(
       # tabs on sidebar Panel
       tabsetPanel(type = "pills", selected = "Data & Plot Options",
                   # Tab 1: Data Optical Density
                   tabPanel("Data & Plot Options",
                            tags$hr(),
                            selectInput("opticaldensity", label = "Select filters",
                                        choices = filters),
                            tags$hr(),
                            textInput("mainOD",
                                      label = "Plot title",
                                      value = "Filter"),
                            sliderInput("rangeOD",
                                        "Wavelength range",
                                        min = 200,
                                        max = 1000,
                                        value = c(200, 1000))



                   ), # End Tab 1
                   # Tab 2: Plot Options Optical Density
                   tabPanel("Export",
                            tags$hr(),
                            textInput(
                              "filenameOD",
                              label = "Filename",
                              value = "Enter filename..."),
                            fluidRow(
                              column(width = 6,
                                     numericInput(
                                       "widthInputOD",
                                       label = "Image width",
                                       value = 7)
                              ),
                              column(width = 6,
                                     numericInput(
                                       "heightInputOD",
                                       label = "Image height",
                                       value = 7)
                              )
                            ),
                            downloadButton("exportPlotOD", label = "Download plot as PDF"),
                            tags$hr(),
                            downloadButton("exportTableOD", label = "Download raw data as CSV")
        )
       )
     ),
    mainPanel(
      uiOutput(outputId = "warningtextOD"),
      plotOutput("densityPlot")

    )
   )
  ),

  tabPanel("Advanced",
           fileInput("own_file", accept = "*.xlsx", label = "Upload individual filter data"),
           helpText("A '.xlsx' file containing one's individual filter data can be temporarily uploaded here."),
           helpText(strong("Note to keep the exact same data structure as in the template '.xlsx' file, which can be downloaded below.")),
           tags$hr(),
           downloadButton("MasterFile",label = "Download Filterdatabase"),
           br(),
           br(),
           helpText("The currently used '.xlsx' file of the app (template or individual) can be downloaded here.")


           ),

  tabPanel("About",
           h5("App version"),
           p("0.2.1 (2021-02-24)"),
           h5("Authors"),
           p("Urs Tilmann Wolpert, Department of Geography, Justus-Liebig-University Giessen (Germany)"),
           p("Sebastian Kreutzer, Geography & Earth Sciences, Aberystwyth University (United Kingdom)"),
           h5("Contact"),
           p("urs.t.wolpert@geogr.uni-giessen.de"),
           tags$hr(),
           p("This application was developed in framework of an
             internship at the IRAMAT-CRP2A at the Université Bordeaux Montaigne, France."),
           p(strong("Due to legal restrictions the app itself comes without any filter data.")),
           br(),
           h5("License"),
           p("This program is free software: you can redistribute it and/or
             modify it under the terms of the GNU General Public License as
             published by the Free Software Foundation, either version 3 of
             the License, or any later version."),
           p("This program is distributed in the hope that it will be useful
             , but WITHOUT ANY WARRANTY; without even the implied warranty
             of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the",
             a("GNU General Public License", href = "https://github.com/LaikaNo2/Filter_app/blob/master/LICENSE"),
             "for more details."),
           p("The 'chooser.R' R-script and 'chooser-binding.js' Java Script used in this program are taken from the",
             a("shiny-example", href = "https://github.com/rstudio/shiny-examples"),
             "repository under the", a("MIT License", href = "https://github.com/rstudio/shiny-examples/blob/master/LICENSE"), ".")
           )
)
)

