#' Solving Ordinary Differential Equations to Unterstand Luminescence
#'
#' A collection of function to simulate luminescence signals in the mineral quartz based on
#' published models.
#'
#' \tabular{ll}{ Package: \tab RLumModel\cr Type: \tab Package\cr Version:
#' \tab 0.2.1\cr Date: \tab 2017-XX-XX \cr License: \tab GPL-3\cr }
#'
#' @name RLumModel-package
#' @docType package
#' @author \bold{Authors}
#'
#' \tabular{ll}{Johannes Friedrich \tab University of Bayreuth, Germany \cr
#' Sebastian Kreutzer \tab IRAMAT-CRP2A, Universite Bordeaux Montaigne, France\cr
#' Christoph Schmidt \tab University of Bayreuth, Germany
#'}
#'
#' \bold{Supervisor}
#'
#' Christoph Schmidt, University of Bayreuth, Germany\cr
#' Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne, France
#'
#' \bold{Support contact}
#'
#' \email{developers@@model.r-luminescence.de}\cr
#'
#' \bold{Project source code repository}\cr
#' \url{https://github.com/R-Lum/RLumModel}\cr
#'
#' \bold{Related package projects}\cr
#' \url{http://www.r-luminescence.de}\cr
#' \url{https://cran.r-project.org/package=Luminescence}\cr
#' \url{http://shiny.r-luminescence.de}\cr
#' \url{https://cran.r-project.org/package=RLumShiny}\cr
#'
#' \bold{Package maintainer}
#'
#' Johannes Friedrich, University of Bayreuth, Germany
#' \cr \email{johannes.friedrich@@uni-bayreuth.de}
#'
#' \bold{Acknowledgement}
#'
#'  The work of Johannes Friedrich is gratefully supported by the DFG in framework of the project
#'  'Modelling quartz luminescence signal dynamics relevant for dating and dosimetry' (SCHM 305114-1).
#'
#' @keywords package
#'
#' @import Luminescence deSolve methods utils
#' @importFrom stats setNames
#' @importFrom Rcpp evalCpp
#' @useDynLib RLumModel
NULL


#' Example data (TL curve) simulated with parameter set from Pagonis 2007
#'
#' @format A RLum.Analysis object containing one TL curve as RLum.Data.Curve.
#'
#' @references
#'
#' Pagonis, V., Chen, R., Wintle, A.G., 2007: Modelling thermal transfer in optically
#' stimulated luminescence of quartz. Journal of Physics D: Applied Physics 40, 998-1006.
#'
#' @source \bold{model_LuminescenceSignals()}
#'
#' @note This example has only one record (TL). The used sequence was
#' sequence <- list(IRR = c(temp = 20, dose = 10, DoseRate = 1),
#'                  TL = c(temp_begin = 20, temp_end = 400, heating_rate = 5))
#'
#' @keywords datasets
#' @docType data
#' @aliases model.output
#' @section Function version: 0.1.1
#' @author Johannes Friedrich, University of Bayreuth (Germany)
#' @examples
#'
#' data("ExampleData.ModelOutput", envir = environment())
#'
#' TL_curve <- get_RLum(model.output, recordType = "TL$", drop = FALSE)
#'
#' ##plot TL curve
#' plot_RLum(TL_curve)
#'
#' TL_concentrations <- get_RLum(model.output, recordType = "(TL)", drop = FALSE)
#' plot_RLum(TL_concentrations)
#'
#'
#' @name ExampleData.ModelOutput
NULL
