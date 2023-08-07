aboutTab <- function(id, subdir) {
  
  # Create a namespace function using the provided id
  ns <- NS(id)
  
    tabPanel("About",
             hr(),
             div(align = "center",
                 # HTML code to include a .png file in the tab; the image file must be in
                 # a subfolder called "wwww"
                 img(src="RL_Logo.png", height = 100, width = 100, alt = "R.Lum"),
                 p("Links:"),
                 a(href = "http://www.r-luminescence.de", "R.Luminescence project page", target="_blank"),
                 br(),
                 a(href = "http://rlum.geographie.uni-koeln.de/", "Online application", target="_blank"),
                 br(),hr(),
                 img(src='GitHub-Mark-32px.png', width='32px', height='32px'),
                 br(),
                 a(href = paste0("https://github.com/tzerk/RLumShiny/tree/master/inst/shiny/", subdir), "See the code at GitHub!", target="_blank")
             )#/div
    )
    
  
}
