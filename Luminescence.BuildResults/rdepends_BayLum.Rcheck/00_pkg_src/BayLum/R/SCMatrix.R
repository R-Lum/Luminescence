#' @title Construct the Stratigraphic Constrain Matrix Interactively
#'
#' @description
#
#' This function helps to define the stratigraphic relation between samples using questions.
#' The output of this function can be used in the function [AgeS_Computation].
#'
#' @details
#' The function will ask if sample `i` is younger than sample `j` to construct
#' the stratigraphic constrain matrix.
#'
#' @param DATA `BayLum.list` (*with default*): Object of class `BayLum.list`, if provided
#' the other parameters are not any longer mandatory.
#'
#' @param Nb_sample [integer] (**required**): the sample number, if `DATA` is provided,
#' the input is not required
#'
#' @param SampleNames [character] (**required**): sample names, if `DATA` is provided,
#' the input is not required
#'
#' @return Returns a [matrix] that summarise the ordered relation between samples.
#' This matrix can be integrate in [AgeS_Computation] function.
#' We refer to detail on [AgeS_Computation] for more information concerning this matrix.
#'
#' @seealso [AgeS_Computation]
#'
#' @author Claire Christophe, Anne Philippe, Guillaume Guérin, Sebastian Kreutzer
#'
#' @examples
#' \dontrun{
#' SCMatrix(
#'  Nb_sample = 2,
#'  SampleNames = c("sample1","sample2"))
#' }
#'
#' @md
#' @export
SCMatrix <- function(
    DATA = NULL,
    Nb_sample,
    SampleNames
){
  # set up connection, default to stdin() if not set
  con <- getOption("SCMatrix.con", stdin())

  ## treat input if DATA is provided
  if (!is.null(DATA) &&
      !is.null(attr(DATA, which = "originator")) &&
      attr(DATA, which = "originator") == "create_DataFile") {

    if(missing(Nb_sample))
      Nb_sample <- DATA$Nb_sample

    if(missing(SampleNames))
      SampleNames <- DATA$SampleNames

  }

  ## set matrix
  StratiConstraints <- matrix(data = 0, ncol = Nb_sample, nrow = (Nb_sample+1))
  StratiConstraints[1,1] <- 1

  ## start interactive session
  cli::cat_line("[SCMatrix()]")
  cli::cat_rule("(interactive session - start)")
  for(i in 2:Nb_sample){
    StratiConstraints[1,i] <- 1
    for(j in 1:(i-1)){
      cli::cat_bullet(
        paste0("Is sample <", SampleNames[j], "> younger than sample <", SampleNames[i],"> (y/n)?"),
        col = "red")

      ## make selection
      R <- switch(
        readLines(con, n = 1),
        "y" = 1,
        "yes" = 1,
        "1" = 1,
        "n" = 0,
        "no" = 0,
        "0" = 0,
        stop("[SCMatrix()] Answer not supported!", call. = FALSE)
      )

      StratiConstraints[(j+1),i] <- R
    }
  }
  cli::cat_rule("(interactive session - end)")

  ## output
  return(StratiConstraints)
}

