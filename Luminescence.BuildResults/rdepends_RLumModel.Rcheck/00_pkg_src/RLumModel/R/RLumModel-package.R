#' Solving Ordinary Differential Equations to Understand Luminescence
#'
#' \if{html}{
#' \figure{RLumModel_Logo.png}{options: width="50"}\cr
#' }
#'
#' A collection of function to simulate luminescence signals in the mineral quartz based on
#' published models.
#'
#' @name RLumModel-package
#' @docType package
#' @details **Authors**
#'
#' \tabular{ll}{
#' Johannes Friedrich \tab University of Bayreuth, Germany \cr
#' Sebastian Kreutzer \tab Geography & Earth Sciences, Aberystwyth University, United Kingdom\cr
#' Christoph Schmidt \tab University of Bayreuth, Germany
#'}
#'
#' **Supervisor**
#'
#' Christoph Schmidt, University of Bayreuth, Germany\cr
#' Sebastian Kreutzer,Geography & Earth Sciences, Aberystwyth University, United Kingdom
#'#'
#' **Project source code repository**
#' - https://github.com/R-Lum/RLumModel
#'
#' **Related package projects**
#' - http://r-luminescence.de
#' - https://CRAN.r-project.org/package=Luminescence
#' - https://CRAN.r-project.org/package=RLumShiny
#'
#' **Package maintainer**
#'
#' Johannes Friedrich, University of Bayreuth, Germany
#' \cr \email{johannes.friedrich@@posteo.de}
#'
#' **Acknowledgement**
#'
#'  The work of Johannes Friedrich is gratefully supported by the DFG in framework of the project
#'  'Modelling quartz luminescence signal dynamics relevant for dating and dosimetry' (SCHM 305114-1).
#'
#' @keywords package
#' @md
#' @import Luminescence deSolve methods utils
#' @importFrom stats setNames
#' @importFrom tools file_ext
#' @importFrom Rcpp evalCpp
#' @importFrom grDevices rgb
#' @importFrom graphics axis lines mtext par rect
#' @useDynLib RLumModel, .registration = TRUE
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
