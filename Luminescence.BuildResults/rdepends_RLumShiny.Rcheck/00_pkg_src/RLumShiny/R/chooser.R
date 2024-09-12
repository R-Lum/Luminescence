## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 'chooser.R' taken from the shiny-examples repository
## (https://github.com/rstudio/shiny-examples) under the MIT License
##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
chooserInput <- function(inputId, leftLabel, rightLabel, leftChoices, rightChoices,
                         size = 5, multiple = FALSE) {

  shiny::registerInputHandler("shinyjsexamples.chooser", function(data, ...) {
    if (is.null(data))
      NULL
    else
      list(left=as.character(data$left), right=as.character(data$right))
  }, force = TRUE)

  leftChoices <- lapply(leftChoices, tags$option)
  rightChoices <- lapply(rightChoices, tags$option)

  if (multiple)
    multiple <- "multiple"
  else
    multiple <- NULL

  tagList(
    singleton(tags$head(
      tags$head(tags$script(src = "RLumShiny/chooser_inputBinding.js")),
      tags$style(type="text/css",
                 HTML(".chooser-container { display: inline-block; }")
      )
    )),
    div(id=inputId, class="chooser",
        div(class = "chooser-container chooser-left-container",
            tags$select(class="left", size=size, multiple=multiple, leftChoices)
        ),
        div(class = "chooser-container chooser-center-container",
            icon("fas fa-arrow-alt-circle-right", "right-arrow fa-2x"),
            tags$br(),
            icon("fas fa-arrow-alt-circle-left", "left-arrow fa-2x")
        ),
        div(class = "chooser-container chooser-right-container",
            tags$select(class="right", size=size, multiple=multiple, rightChoices)
        )
    )
  )
}
