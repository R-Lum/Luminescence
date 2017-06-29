#' RLumShiny Dashboard Addin
#' 
#' RLumShiny dashboard
#'  
#' @export
RLumShinyAddin <- function() {
  
  ## GLOBAL --------------------------------------------------------------------
  # List of applications available in RLumShiny
  applications <- list(
    
    "abanico" = list(title = "Abanico Plot",
                     keyword = "abanico",
                     category = "plot",
                     description = "A plot which allows comprehensive presentation of data precision and its dispersion around a central value as well as illustration of a kernel density estimate, histogram and/or dot plot of the dose values."),
    
    "cosmic" = list(title = "Cosmic Dose Rate",
                    keyword = "cosmicdose",
                    category = "calc",
                    description = "This function calculates the cosmic dose rate taking into account the soft- and hard-component of the cosmic ray flux and allows corrections for geomagnetic latitude, altitude above sea-level and geomagnetic field changes."),
    
    "kde" = list(title = "Kernel Density Estimate Plot",
                 keyword = "kde",
                 category = "plot",
                 description = "Plot a kernel density estimate of measurement values in combination with the actual values and associated error bars in ascending order."),
    
    "doserecovery" = list(title = "Dose Recovery Test",
                          keyword = "doserecovery",
                          category = "plot",
                          description = "The function provides a standardised plot output for dose recovery test measurements."),
    
    "radialplot" = list(title = "Radial Plot",
                        keyword = "radialplot",
                        category = "plot",
                        description = "A Galbraith's radial plot is produced on a logarithmic or a linear scale."),
    
    "histogram" = list(title = "Histogram",
                       keyword = "histogram",
                       category = "plot",
                       description = "Function plots a predefined histogram with an accompanying error plot as suggested by Rex Galbraith at the UK LED in Oxford 2010."),
    
    "transformCW" = list(title = "Transform CW-OSL curves",
                         keyword = "transformCW",
                         category = "misc",
                         description = "Transform a conventionally measured continuous-wave (CW) OSL-curve to a pseudo parabolic/hyperbolic/linearly modulated curve."),
    
    "filter" = list(title = "Filter Combinations",
                    keyword = "filter",
                    category = "misc",
                    description = "Plot filter combinations along with the (optional) net transmission window.")
    
  )
  
  # HELPER FUNCTIONS ------------------
  split_by_category <- function(x) {
    
    # get unique categories
    categories <- unique(sapply(x, function(el) el$category))
    
    # for each unique category...
    lst <- lapply(categories, function(cat) {
      
      # ...get application
      lst.sub <- lapply(x, function(el) {
        if (el$category == cat)
          return(el)
      })
      
      # remove NULL objects (ie. apps not within the category)
      lst.sub[!sapply(lst.sub, is.null)]
    })
    
    # append category names
    names(lst) <- categories
    
    return(lst)
  }
  
  
  ## HEADER ----------------------------------------------------------------------
  header <- dashboardHeader(
    title = tags$p(style = "color:white; font-family:verdana;","RLumShiny"),
    tags$li(class = "dropdown", tags$a(href = "https://github.com/tzerk/RLumShiny", icon("github"))),
    tags$li(class = "dropdown", tags$a(href = "https://twitter.com/RLuminescence", icon("twitter"))),
    tags$li(class = "dropdown", tags$a(href = "https://forum.r-luminescence.de/", icon("comments-o")))
  )#EndOf:Header
  
  
  ## SIDEBAR ---------------------------------------------------------------------
  sidebar <- dashboardSidebar(
    
    sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                      label = "Search..."),
    
    # tabNames must have the categorial value (see globals.R)
    sidebarMenu(id = "sidebar",
                menuItem("Dashboard", icon = icon("dashboard"), tabName = ""),
                menuItem("Plotting", icon = icon("bar-chart"), tabName = "plot"),
                menuItem("Calculation", icon = icon("calculator"), tabName = "calc"),
                menuItem("Miscellaneous", icon = icon("cogs"), tabName = "misc")
    ),
    tags$hr(),
    tags$div(align = "left",
             tags$p(style = "color: grey; margin-left: 10px; margin-right: 40px; font-size: 80%;",
                    attributes(unclass(citation("RLumShiny"))[[1]])$textVersion)
             )
    
  )#EndOf:Sidebar
  
  
  ## BODY ------------------------------------------------------------------------
  body <- dashboardBody(
    
    ## custom CSS for shiny(dashboard) elements
    # info-box
    tags$head(tags$style(HTML('.info-box {min-height: 210px;} .info-box-icon {height: 100px; line-height: 100px;}'))),
    # background of the dashboard body
    tags$head(tags$style(HTML('.content-wrapper {height: 1400px;}'))),
    
    # JavaScript code executed when clicking a href link; it will initialise 
    # the input$linkClicked variable that can be used within the server logic
    tags$script(HTML("
function clickFunction(link){
alert('The following application will now be started: ' + link + '\\n\\nNote: This window will become unresponsive. \\nDo not close until done with the application!'); 
Shiny.onInputChange('linkClicked', link);
}
")),
    
    # The whole dashboard body is generated dynamically in the server logic
    uiOutput("body")
  )#EndOf:Body
  
  
  ## RENDER PAGE -----------------------------------------------------------------
  ui <- dashboardPage(header, sidebar, body)
  
  
  ## SERVER LOGIC ----------------------------------------------------------------
  server <- function(input, output, session) {
    
    # FILTER -----------------------------------
    get_Items <- reactive({
      
      matches <- sapply(applications, function(el) {
        # filter by search name & category
        grepl(input$searchText, el$title, ignore.case = TRUE) &
          grepl(input$sidebar, el$category) 
      })
      
      # split by category (globals.R)
      split_by_category(applications[matches])
    })
    
    
    # BODY -------------------------------------
    output$body <- renderUI({
      
      # get (filtered) list of available applications
      items <- get_Items()
      
      # create infoBoxes for each application
      mainbody <- Map(function(apps, cat) {
        
        category <- switch(cat,
                           "plot" = "Plotting",
                           "calc" = "Calculation",
                           "misc" = "Miscellaneous",
                           "stat" = "Statistics")
        
        color <- switch(cat,
                        "plot" = "red",
                        "calc" = "light-blue",
                        "misc" = "green",
                        "stat" = "black")
        
        icon <- switch(cat,
                       "plot" = icon("bar-chart"),
                       "calc" = icon("calculator"),
                       "misc" = icon("cogs"),
                       "stat" = icon("superscript"))
        
        # all applications of a particular category are wrapped around with
        # with collapsible box
        box(title = category,
            collapsible = TRUE,
            width = 12, height = "100%",
            # embed infoboxes for all applications of a category
            Map(function(app, id) {
              
              div(
                infoBox(title = HTML("<b>", app$title, "</b><hr>"), 
                        fill = TRUE,
                        subtitle = app$description, 
                        color = color,
                        icon = icon,
                        href = "#"),
                onclick = paste0("clickFunction('", app$keyword,"'); return false;"))
              
            }, apps, 1:length(apps)))
      }, items, names(items))
      
      return(mainbody)
    })
    
    
    ## Start application
    # workaround: clicking on any of the infoboxes causes the gadget to
    # terminate, which triggers the custom onSessionEnded callback. 
    # We have to terminate the gadget first to make room for starting 
    # another shiny instance, i.e., the chosen app
    observeEvent(input$linkClicked, {
      stopApp(NULL)
    })
    session$onSessionEnded(function() {
      isolate({
        if (!is.null(input$linkClicked))
          app_RLum(input$linkClicked)
        })
    })
    
  }#EndOf:ServerLogic
  
  viewer <- dialogViewer("RLumShiny Dashboard", width = 1400, height = 800)
  
  runGadget(ui, server, viewer = viewer)
}