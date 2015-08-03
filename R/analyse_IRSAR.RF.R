#' Analyse IRSAR RF measurements
#'
#' Function to analyse IRSAR RF measurements on K-feldspar samples, performed
#' using the protocol according to Erfurt et al. (2003)
#'
#' The function performs an IRSAR analysis described for K-feldspar samples by
#' Erfurt et al. (2003) assuming a negligible sensitivity change of the RF
#' signal.\cr \bold{General Sequence Structure} (according to Erfurt et al.
#' (2003)) \enumerate{ \item Measuring IR-RF intensity of the natural dose for
#' a few seconds (\eqn{D_{natural}}) \item Bleach the samples under solar
#' conditions for at least 30 min without changing the geometry \item Waiting
#' for at least one hour \item Regeneration of the IR-RF signal to at least the
#' natural level \item Fitting data with a stretched exponential function \item
#' Calculate the the palaeodose \eqn{D_{e}} using the parameters from the
#' fitting } \bold{Function Used For The Fitting} (according to Erfurt et al.
#' (2003))\cr \deqn{\phi(D) = \phi_{0}-\Delta\phi(1-exp(-\lambda*D))^\beta}
#' with \eqn{\phi(D)} the dose dependent IR-RF flux, \eqn{\phi_{0}} the inital
#' IR-RF flux, \eqn{\Delta\phi} the dose dependent change of the IR-RF flux,
#' \eqn{\lambda} the exponential parameter, \eqn{D} the dose and \eqn{\beta}
#' the dispersive factor.\cr\cr To obtain the palaeodose \eqn{D_{e}} the
#' function is changed to:\cr \deqn{D_{e} = ln(-(\phi(D) -
#' \phi_{0})/(-\lambda*\phi)^{1/\beta}+1)/-\lambda}\cr The fitting is done
#' using the \code{port} algorithm of the \code{\link{nls}} function.\cr
#'
#' Two methods are supported to obtain the De:\cr
#'
#' \bold{\code{method = "FIT"}}\cr
#'
#' The principle is described above and follows the orignal suggestions by
#' Erfurt et al., 2003. For the fitting the mean count value of the RF_nat curve is used.
#'
#' \bold{\code{method = "SLIDE"}}\cr
#'
#' For this method the natural curve is slided along the x-axis until
#' congruence with the regenerated curve is reached. Instead of fitting this
#' allows to work with the original data without the need of any physical
#' model. This approach was introduced for RF curves by Buylaert et al., 2012
#' and Lapp et al., 2012.
#'
#' Here the sliding is done by searching for the minimum of the squared residuals.
#'
#' //WILL BE ADDED TODO
#'
#' \bold{Error estimation}\cr
#'
#' For \bold{\code{method = "FIT"}} the asymmetric error range is taken from
#' the standard deviation of the natural signal.\cr
#'
#' For \bold{\code{method = "SLIDE"}} an
#'
#' //WILL BE ADDED TODO
#'
#' @param object \code{\linkS4class{RLum.Analysis}} (\bold{required}): input
#' object containing data for protocol analysis. Generally the function expects two curves.
#' (1) RF_nat, (2) RF_reg
#'
#' @param sequence.structure \code{\link{vector}} \link{character} (with
#' default): specifies the general sequence structure. Allowed steps are
#' \code{NATURAL}, \code{REGENERATED}. In addition any other character is
#' allowed in the sequence structure; such curves will be ignored during the analysis.
#'
#' @param RF_nat.lim \code{\link{vector}} (with default): set minimum and maximum
#' channel range for natural signal fitting and sliding. If only one value is provided this
#' will be treated as minium value and the maximum limit will be added automatically.
#'
#' @param RF_reg.lim \code{\link{vector}} (with default): set minimum and maximum
#' channel range for regenerated signal fitting and sliding. If only one value is provided this
#' will be treated as minium value and the maximum limit will be added automatically.
#'
#' @param method \code{\link{character}} (with default): setting method applied
#' for the data analysis. Possible options are \code{"FIT"} or \code{"SLIDE"}.
#'
#' @param rejection.criteria \code{\link{list} (with default)}: set rejection
#' criteria. Supported criteria are: \code{curves_ratio}, \code{residuals_slope} (only for
#' \code{method = "SLIDE"}) and \code{curves_bounds} (see Details for further information)
#'
#' @param fit.trace \code{\link{logical}} (with default): trace fitting (for
#' debugging use)
#'
#' @param n.MC \code{\link{numeric}} (with default): set number of Monte
#' Carlo runs for start parameter estimation (\code{method = "FIT"}) or
#' error estimation (\code{method = "SLIDE"}). Note: Large values will
#' significantly increase the computation time
#'
#' @param slide.show_density \code{\link{logical}} (with default): enable or
#' disable KDE for MC run results. If the distribution is too narrow nothing is shown
#'
#' @param txtProgressBar \code{\link{logical}} (with default): enables \code{TRUE} or
#' disables \code{FALSE} the progression bar during MC runs
#'
#' @param plot \code{\link{logical}} (with default): plot output (\code{TRUE}
#' or \code{FALSE})
#'
#' @param \dots further arguments that will be passed to the plot output.
#' Currently supported arguments are \code{main}, \code{xlab}, \code{ylab},
#' \code{xlim}, \code{ylim}, \code{log}, \code{legend.pos} (passes argument to x,y in
#' \code{\link[graphics]{legend}})
#'
#'
#' @return A plot (optional) and an \code{\linkS4class{RLum.Results}} object is
#' returned. The slot data contains the following elements: \cr
#'
#' $ De.values: \code{\link{data.frame}} table with De and corresponding values\cr
#' ..$ De : \code{numeric} \cr
#' ..$ De.lower : \code{numeric} \cr
#' ..$ De.upper : \code{numeric}c \cr
#' ..$ De.status  : \code{character} \cr
#' ..$ RF_nat.lim  : \code{charcter} \cr
#' ..$ RF_reg.lim : \code{character} \cr
#' $ De.RC : \code{\link{data.frame}} table with rejection criteria \cr
#' $ fit : {\code{\link{nls}} \code{nlsModel} object} \cr
#' $ slide : \code{\link{list}} data from the sliding process\cr
#' $ call : \code{\link[methods]{language-class}}: the orignal function call \cr
#'
#' The output (\code{De.values}) should be accessed using the
#' function \code{\link{get_RLum}}
#'
#' @note This function assumes that there is no sensitivity change during the
#' measurements (natural vs. regenerated signal), which is in contrast to the
#' findings from Buylaert et al. (2012).\cr
#'
#'
#' @section Function version: 0.4.0
#'
#' @author Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France)
#'
#' @seealso \code{\linkS4class{RLum.Analysis}},
#' \code{\linkS4class{RLum.Results}}, \code{\link{get_RLum}},
#' \code{\link{nls}}
#'
#'
#' @references Buylaert, J.P., Jain, M., Murray, A.S., Thomsen, K.J., Lapp, T.,
#' 2012. IR-RF dating of sand-sized K-feldspar extracts: A test of accuracy.
#' Radiation Measurements 44 (5-6), 560-565. doi: 10.1016/j.radmeas.2012.06.021
#'
#' Erfurt, G., Krbetschek, M.R., 2003. IRSAR - A single-aliquot
#' regenerative-dose dating protocol applied to the infrared radiofluorescence
#' (IR-RF) of coarse- grain K-feldspar. Ancient TL 21, 35-42.
#'
#' Erfurt, G., 2003. Infrared luminescence of Pb+ centres in potassium-rich
#' feldspars. physica status solidi (a) 200, 429-438.
#'
#' Erfurt, G., Krbetschek, M.R., 2003. Studies on the physics of the infrared
#' radioluminescence of potassium feldspar and on the methodology of its
#' application to sediment dating. Radiation Measurements 37, 505-510.
#'
#' Erfurt, G., Krbetschek, M.R., Bortolot, V.J., Preusser, F., 2003. A fully
#' automated multi-spectral radioluminescence reading system for geochronometry
#' and dosimetry. Nuclear Instruments and Methods in Physics Research Section
#' B: Beam Interactions with Materials and Atoms 207, 487-499.
#'
#' Lapp, T., Jain, M., Thomsen, K.J., Murray, A.S., Buylaert, J.P., 2012. New
#' luminescence measurement facilities in retrospective dosimetry. Radiation
#' Measurements 47, 803-808. doi:10.1016/j.radmeas.2012.02.006
#'
#' Trautmann, T., 2000. A study of radioluminescence kinetics of natural
#' feldspar dosimeters: experiments and simulations. Journal of Physics D:
#' Applied Physics 33, 2304-2310.
#'
#' Trautmann, T., Krbetschek, M.R., Dietrich, A., Stolz, W., 1998.
#' Investigations of feldspar radioluminescence: potential for a new dating
#' technique. Radiation Measurements 29, 421-425.
#'
#' Trautmann, T., Krbetschek, M.R., Dietrich, A., Stolz, W., 1999. Feldspar
#' radioluminescence: a new dating method and its physical background. Journal
#' of Luminescence 85, 45-58.
#'
#' Trautmann, T., Krbetschek, M.R., Stolz, W., 2000. A systematic study of the
#' radioluminescence properties of single feldspar grains. Radiation
#' Measurements 32, 685-690.
#'
#'
#' @keywords datagen
#'
#'
#' @examples
#'
#' ##load data
#' data(ExampleData.RLum.Analysis, envir = environment())
#'
#' ##perform analysis
#' temp <- analyse_IRSAR.RF(object = IRSAR.RF.Data)
#'

analyse_IRSAR.RF<- function(
  object,
  sequence.structure = c("NATURAL", "REGENERATED"),
  RF_nat.lim,
  RF_reg.lim,
  method = "FIT",
  rejection.criteria,
  fit.trace = FALSE,
  n.MC = 10,
  slide.show_density = FALSE,
  txtProgressBar = TRUE,
  plot = TRUE,
  ...
){

  ##===============================================================================================#
  ## INTEGRITY TESTS AND SEQUENCE STRUCTURE TESTS
  ##===============================================================================================#

  ##MISSING INPUT
  if(missing("object")){
    stop("[analyse_IRSAR.RF()] No input 'object' set!")
  }

  ##INPUT OBJECTS
  if(!is(object, "RLum.Analysis")){
    stop("[analyse_IRSAR.RF()] Input object is not of type 'RLum.Analysis'!")
  }

  ##CHECK OTHER ARGUMENTS
  assertive::assert_is_character(sequence.structure)
  assertive::assert_is_logical(plot)
  assertive::assert_is_logical(txtProgressBar)


  ##SELECT ONLY MEASURED CURVES
  ## (this is not really necessary but rather user friendly)
  if(!length(suppressWarnings(get_RLum(object, curveType= "measured"))) == 0){
    object <- get_RLum(object, curveType= "measured", keep.object = TRUE)

  }

  ##INVESTIGATE SEQUENCE OBJECT STRUCTURE

    ##grep object strucute
    temp.sequence.structure <- structure_RLum(object)

    ##set structure values
    temp.sequence.structure$protocol.step <-
      rep(sequence.structure, length_RLum(object))[1:length_RLum(object)]

    ##check if the first curve is shorter than the first curve
    if (temp.sequence.structure[1,"n.channels"] > temp.sequence.structure[2,"n.channels"]) {
      stop(
        "[analyse_IRSAR.RF()] Number of data channels in RF_nat > RF_reg. This is not supported!"
      )

   }

  ##===============================================================================================#
  ## SET CURVE LIMITS
  ##===============================================================================================#
  ##the setting here will be valid for all subsequent operations

    ##01
    ##first get allowed curve limits, this makes the subsequent checkings easier and the code
    ##more easier to read
    RF_nat.lim.default <- c(1,max(
      subset(
        temp.sequence.structure,
        temp.sequence.structure$protocol.step == "NATURAL"
      )$n.channels
    ))

    RF_reg.lim.default <- c(1,max(
      subset(
        temp.sequence.structure,
        temp.sequence.structure$protocol.step == "REGENERATED"
      )$n.channels
    ))


    ##02 - check boundaris
    ##RF_nat.lim
    if (missing(RF_nat.lim)) {
      RF_nat.lim <- RF_nat.lim.default

    }else {
      ##this allows to provide only one boundary and the 2nd will be added automatically
      if (length(RF_nat.lim) == 1) {
        RF_nat.lim <- c(RF_nat.lim, RF_nat.lim.default[2])

      }

      if (min(RF_nat.lim) < RF_nat.lim.default[1] |
          max(RF_nat.lim) > RF_nat.lim.default[2]) {
        RF_nat.lim <- RF_nat.lim.default

        warning(paste0(
          "RF_nat.lim out of bounds, reset to: RF_nat.lim = c(",
          paste(range(RF_nat.lim), collapse = ":")
        ),")")
      }

    }

    ##RF_reg.lim
    ##
    if (missing(RF_reg.lim)) {
      RF_reg.lim <- RF_reg.lim.default

    }else {
      ##this allows to provide only one boundary and the 2nd will be added automatically
      if (length(RF_reg.lim) == 1) {
        RF_reg.lim <- c(RF_reg.lim, RF_reg.lim.default[2])

      }

      if (min(RF_reg.lim) < RF_reg.lim.default[1] |
          max(RF_reg.lim) > RF_reg.lim.default[2]) {
        RF_reg.lim <- RF_reg.lim.default

        warning(paste0(
          "RF_reg.lim out of bounds, reset to: RF_reg.lim = c(",
          paste(range(RF_reg.lim), collapse = ":")
        ),")")

      }
    }

    ##check if intervalls make sense at all
    if(length(RF_reg.lim[1]:RF_reg.lim[2]) < RF_nat.lim[2]){
      RF_reg.lim[2] <- RF_reg.lim[2] + abs(length(RF_reg.lim[1]:RF_reg.lim[2]) - RF_nat.lim[2]) + 1

      warning(paste0("Length intervall RF_reg.lim < length RF_nat. Reset to RF_reg.lim = c(",
                     paste(range(RF_reg.lim), collapse=":")),")")

    }


  ##===============================================================================================#
  ## SET PLOT PARAMETERS
  ##===============================================================================================#

  ##get channel resolution (should be equal for all curves, but if not the mean is taken)
  resolution.RF <- round(mean((temp.sequence.structure$x.max/temp.sequence.structure$n.channels)),digits=1)

  plot.settings <- list(
    main = "IR-RF",
    xlab = "Time/s",
    ylab = paste0("IR-RF/(cts/", resolution.RF," s)"),
    log = "",
    cex = 1,
    legend.pos = "top"
    ##xlim and ylim see below as they has to be modifid differently
  )

  ##modify list if something was set
  plot.settings <- modifyList(plot.settings, list(...))


  ##=============================================================================#
  ## ANALYSIS
  ##=============================================================================#

  ##grep first regenerated curve
  RF_reg <- as.data.frame(object@records[[
    temp.sequence.structure[temp.sequence.structure$protocol.step=="REGENERATED","id"]]]@data)

  RF_reg<- as.data.frame(object@records[[2]]@data)
  RF_reg.x <- RF_reg[RF_reg.lim[1]:RF_reg.lim[2],1]
  RF_reg.y <- RF_reg[RF_reg.lim[1]:RF_reg.lim[2],2]


  ##grep values from natural signal
  RF_nat <- as.data.frame(object@records[[
    temp.sequence.structure[temp.sequence.structure$protocol.step=="NATURAL","id"]]]@data)

  ##limit values to fit range (at least to the minimum)
  RF_nat.limited<- RF_nat[min(RF_nat.lim):max(RF_nat.lim),]

  ##calculate some useful parameters
  RF_nat.mean <- mean(RF_nat.limited[,2])
  RF_nat.sd <- sd(RF_nat.limited[,2])

  RF_nat.error.lower <- RF_nat.mean + RF_nat.sd
  RF_nat.error.upper <- RF_nat.mean - RF_nat.sd

  ##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
  ##METHOD FIT
  ##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
  if(method == "FIT"){
    ## REGENERATED SIGNAL
    # set function for fitting ------------------------------------------------

    fit.function <- as.formula(y~phi.0-(delta.phi*((1-exp(-lambda*x))^beta)))

    ##stretched expontial function according to Erfurt et al. (2003)
    ## + phi.0 >> initial IR-RF flux
    ## + delta.phi >> dose dependent change of the IR-RF flux
    ## + lambda >> exponential parameter
    ## + beta >> dispersive factor

    # set start parameter estimation ------------------------------------------

    fit.parameters.start <- c(
      phi.0 = max(RF_reg.y),
      lambda = 0.0001,
      beta = 1,
      delta.phi = 2 * (max(RF_reg.y) - min(RF_reg.y))
    )

    # start nls fitting -------------------------------------------------------

    ##Monte Carlo approach for fitting
    fit.parameters.results.MC.results <- data.frame()

    ##produce set of start paramters
    phi.0.MC <- rep(fit.parameters.start["phi.0"], n.MC)
    lambda.MC <- seq(0.0001, 0.001, by=(0.001-0.0001)/n.MC) ##TODO
    beta.MC <- rep(fit.parameters.start["beta"], n.MC)
    delta.phi.MC <- rep(fit.parameters.start["delta.phi"], n.MC)

    ##start fitting loop for MC runs
    for(i in 1:n.MC){

        fit.MC <- try(nls(
          fit.function,
          trace = FALSE,
          data = data.frame(x = RF_reg.x, y = RF_reg.y),
          algorithm = "port",
          start = list(
            phi.0 = phi.0.MC[i],
            delta.phi = delta.phi.MC[i],
            lambda = lambda.MC[i],
            beta = beta.MC[i]
          ),
          nls.control(
            maxiter = 100,
            warnOnly = FALSE,
            minFactor = 1 / 1024
          ),
          lower = c(
            phi.0 = .Machine$double.xmin,
            delta.phi = .Machine$double.xmin,
            lambda = .Machine$double.xmin,
            beta = .Machine$double.xmin
          ),
          upper = c(
            phi.0 = max(RF_reg.y),
            delta.phi = max(RF_reg.y),
            lambda = 1,
            beta = 100
          )
        ),
        silent = TRUE
        )

      if(inherits(fit.MC,"try-error") == FALSE) {
        temp.fit.parameters.results.MC.results <- coef(fit.MC)

        fit.parameters.results.MC.results[i,"phi.0"] <-
          temp.fit.parameters.results.MC.results["phi.0"]
        fit.parameters.results.MC.results[i,"lambda"] <-
          temp.fit.parameters.results.MC.results["lambda"]
        fit.parameters.results.MC.results[i,"delta.phi"] <-
          temp.fit.parameters.results.MC.results["delta.phi"]
        fit.parameters.results.MC.results[i,"beta"] <-
          temp.fit.parameters.results.MC.results["beta"]

      }
    }

    ##FINAL fitting after successful MC
    if(length(na.omit(fit.parameters.results.MC.results)) != 0){

      ##choose median as final fit version
      fit.parameters.results.MC.results <- sapply(na.omit(fit.parameters.results.MC.results), median)

      ##try final fitting
      fit <- try(nls(
        fit.function,
        trace = fit.trace,
        data = data.frame(x = RF_reg.x, y = RF_reg.y),
        algorithm = "port",
        start = list(
          phi.0 = fit.parameters.results.MC.results["phi.0"],
          delta.phi = fit.parameters.results.MC.results["delta.phi"],
          lambda = fit.parameters.results.MC.results["lambda"],
          beta = fit.parameters.results.MC.results["beta"]
        ),
        nls.control(
          maxiter = 500,
          warnOnly = FALSE,
          minFactor = 1 / 4096
        ),
        lower = c(
          phi.0 = .Machine$double.xmin,
          delta.phi = .Machine$double.xmin,
          lambda = .Machine$double.xmin,
          beta = .Machine$double.xmin
        ),
        upper = c(
          phi.0 = max(RF_reg.y),
          delta.phi = max(RF_reg.y),
          lambda = 1, beta = 100
        )
      ),
      silent = FALSE
      )
    }else{

      fit <- NA
      class(fit) <- "try-error"

    }

    # get parameters ----------------------------------------------------------
    # and with that the final De

    if (!inherits(fit,"try-error")) {
      fit.parameters.results <- coef(fit)

    }else{
      fit.parameters.results <- NA

    }

    ##calculate De value
    if (!is.na(fit.parameters.results[1])) {
      De <- suppressWarnings(round(log(
        -((RF_nat.mean - fit.parameters.results["phi.0"]) /
            -fit.parameters.results["delta.phi"]
        ) ^ (1 / fit.parameters.results["beta"]) + 1
      ) /
        -fit.parameters.results["lambda"], digits =
        2))

      De.lower <- suppressWarnings(round(log(
        -((RF_nat.error.lower - fit.parameters.results["phi.0"]) /
            -fit.parameters.results["delta.phi"]
        ) ^ (1 / fit.parameters.results["beta"]) + 1
      ) /
        -fit.parameters.results["lambda"],digits = 2))

      De.upper <- suppressWarnings(round(log(
        -((RF_nat.error.upper - fit.parameters.results["phi.0"]) /
            -fit.parameters.results["delta.phi"]
        ) ^ (1 / fit.parameters.results["beta"]) + 1
      ) /
        -fit.parameters.results["lambda"],digits = 2))

    }else{
      De <- NA
      De.lower <- NA
      De.upper <- NA

    }
  }

  ##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
  ##METHOD SLIDE - ANALYSIS
  ##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
  else if(method == "SLIDE"){

    ##convert to matrix (in fact above the matrix data were first transfered to data.frames ... here
    ##we correct this ... again)  ##TODO
    RF_nat.limited <- as.matrix(RF_nat.limited)
    RF_reg.limited <- matrix(c(RF_reg.x, RF_reg.y), ncol = 2)
    RF_nat <- as.matrix(RF_nat)

    ##DEFINE FUNCTION FOR SLIDING
    ##FIND MINIMUM - this is done in a function so that it can be further used for MC simulations
    sliding <- function(RF_nat,
                        RF_nat.limited,
                        RF_reg.limited,
                        numerical.only = FALSE){


      ##(0) set objects ... nomenclature as used in Frouin et al., please note that here the index
      ##is used instead the real time values
      t_max.id <- nrow(RF_reg.limited)
      t_max_nat.id <- nrow(RF_nat.limited)
      t_min.id <- 1
      t_min <- RF_nat.limited[1,1]

      ##(1) calculate sum of residual squares using internal Rcpp function

        #pre-allocate object
        temp.sum.residuals <- vector("numeric", length = t_max.id - t_max_nat.id)

        ##calculate sum of squared residuals ... for the entire set
        temp.sum.residuals <- .analyse_IRSARRF_SRS(RF_reg.limited[,2], RF_nat.limited[,2])

      #(2) get minimum value (index and time value)
      t_n.id <- which.min(temp.sum.residuals)
      temp.sliding.step <- RF_reg.limited[t_n.id] - t_min

      ##(3) slide curve graphically ... full data set we need this for the plotting later
      RF_nat.slided <- matrix(data = c(RF_nat[,1] + temp.sliding.step, RF_nat[,2]), ncol = 2)
      t_n <- RF_nat.slided[1,1]

      ##(4) get residuals
      residuals <- RF_nat.limited[,2] - RF_reg.limited[t_n.id:(t_n.id+length(RF_nat.limited[,2])-1), 2]

      ##(4.1) calculate De from the first channel
      De <- round(t_n, digits = 2)
      temp.trend.fit <- NA

     ##(5) calculate trend fit
     temp.trend.fit <- coef(lm(y~x, data.frame(x = RF_nat.limited[,1], y = residuals)))


      ##return values and limited if they are not needed
      if (numerical.only == FALSE) {
        return(
          list(
            De = De,
            residuals = residuals,
            trend.fit = temp.trend.fit,
            RF_nat.slided = RF_nat.slided,
            t_n.id = t_n.id
          )
        )
      }else{
        return(list(De))
      }

    }##end of function sliding()


    ##PERFORM sliding and overwrite values
    slide <-  sliding(
      RF_nat = RF_nat,
      RF_nat.limited = RF_nat.limited,
      RF_reg.limited = RF_reg.limited,
    )

      ##write results in variables
      De <- slide$De
      residuals <- slide$residuals
      RF_nat.slided <-  slide$RF_nat.slided


    # ERROR ESTIMATION
    # MC runs for error calculation ---------------------------------------------------------------

      ##set residual matrix for MC runs, i.e. set up list of pseudo RF_nat curves as function
      slide.MC.list <- lapply(1:n.MC,function(x) {
        cbind(
          RF_nat.limited[,1],
          (RF_reg.limited[slide$t_n.id:(slide$t_n.id + nrow(RF_nat.limited)-1) ,2]
           + sample(residuals, size = nrow(RF_nat.limited), replace = TRUE)
           )
        )
       })

     ##predefine vector
     De.MC <- vector(length = n.MC)

     if(txtProgressBar){
     ##terminal output fo MC
     cat("\n\t Run Monte Carlo loops for error estimation\n")

      ##progress bar
      pb<-txtProgressBar(min=0, max=n.MC, initial=0, char="=", style=3)
     }

       for (i in 1:n.MC) {
         temp.slide.MC <- sliding(
           RF_nat = RF_nat,
           RF_reg.limited = RF_reg.limited,
           RF_nat.limited = slide.MC.list[[i]],
           numerical.only = TRUE
         )

         De.MC[i] <- temp.slide.MC[[1]]

         ##update progress bar
         if (txtProgressBar) {
           setTxtProgressBar(pb, i)
         }

       }

      ##close
      if(txtProgressBar){close(pb)}

      ##calculate absolute deviation between De and the here newly calculated De.MC
      ##this is, e.g. ^t_n.1* - ^t_n in Frouin et al.
      De.diff <- diff(x = c(De, De.MC))
      De.lower <- De - quantile(De.diff, 0.975)
      De.upper <- De - quantile(De.diff, 0.025)

  }else{

    warning("Analysis skipped: Unknown method or threshold of rejection criteria reached.")

  }

  ##===============================================================================================#
  ## REJECTION CRITERIA
  ##===============================================================================================#
  ## Rejection criteria are intentionally evaluated after all the calculations have been done as
  ## it should be up to the user to decide whether a value should be taken into account or not.

  ##(0)
  ##set default values and overwrite them if there was something new
  ##set defaults
  RC <- list(
    curves_ratio = 1.01,
    residuals_slope = 5,
    curves_bounds = as.integer(max(RF_reg.x))
  )

  ##modify default values
  if(!missing(rejection.criteria)){RC <- modifyList(RC, rejection.criteria)}

  ##(1) check if RF_nat > RF_reg, considering the fit range
  RC.curves_ratio <- sum(RF_nat.limited[,2])/sum(RF_reg.y)
  RC.curves_ratio.status <- ifelse(RC.curves_ratio >= RC$curves_ratio, "FAILED", "OK")

  ##(2) check slop of the residuals using a linear fit
  if(exists("slide")){
    RC.residuals_slope <- abs(slide$trend.fit[2])
    RC.residuals_slope.status <- ifelse(RC.residuals_slope >= RC$residuals_slope, "FAILED", "OK")

  }else{
    RC.residuals_slope <- NA
    RC.residuals_slope.status <- "OK"

  }

  ##(99) check whether after sliding the
  if(exists("slide")){
    RC.curves_bounds <- max(RF_nat.slided[,1])
    RC.curves_bounds.status <- ifelse(RC.curves_bounds >= max(RF_reg.x), "FAILED", "OK")

  }else if(exists("fit")){
    RC.curves_bounds <- De.upper
    RC.curves_bounds.status <- ifelse(RC.curves_bounds  >= max(RF_reg.x), "FAILED", "OK")

  }else{
    RC.curves_bounds <- NA
    RC.curves_bounds.status <- "OK"

  }

  ##Combine everthing in a data.frame
  RC.data.frame <- data.frame(
      CRITERIA = c(names(RC)),
      THRESHOLD = unlist(RC),
      VALUE = c(RC.curves_ratio, RC.residuals_slope,RC.curves_bounds),
      STATUS = c(RC.curves_ratio.status, RC.residuals_slope.status, RC.curves_bounds.status),
    row.names = NULL,
    stringsAsFactors = FALSE
  )

  ##set De.status to indicate whether there is any problem with the De according to the rejection
  ##criteria
  if ("FAILED" %in% RC.data.frame$STATUS) {
    De.status <- "FAILED"
  }else{
    De.status <- "OK"
  }

  ##===============================================================================================#
  ## PLOTTING
  ##===============================================================================================#
  if(plot){

    ##grep par default
    def.par <- par(no.readonly = TRUE)

    ##get internal colour definition
    col <- get("col", pos = .LuminescenceEnv)

    ##set plot frame, if a method was choosen
    if(method == "SLIDE" | method == "FIT"){

      layout(matrix(c(1,2),2,1,byrow=TRUE),c(2), c(1.3,0.4), TRUE)
      par(oma=c(1,1,1,1), mar=c(0,4,3,0), cex = plot.settings$cex)

    }

    ##here control xlim and ylim behaviour
    ##xlim
    xlim  <- if ("xlim" %in% names(list(...))) {
      list(...)$xlim
    } else
    {
      if (plot.settings$log == "x" | plot.settings$log == "xy") {
        c(min(temp.sequence.structure$x.min),max(temp.sequence.structure$x.max))

      }else{
        c(0,max(temp.sequence.structure$x.max))

      }

    }

    ##ylim
    ylim  <- if("ylim" %in% names(list(...))) {list(...)$ylim} else
    {c(min(temp.sequence.structure$y.min), max(temp.sequence.structure$y.max))}


    ##open plot area
    plot(
      NA,NA,
      xlim = xlim,
      ylim = ylim,
      xlab = ifelse(method != "SLIDE" &
                      method != "FIT", plot.settings$xlab," "),
      xaxt = ifelse(method != "SLIDE" & method != "FIT","s","n"),
      yaxt = "n",
      ylab = plot.settings$ylab,
      main = plot.settings$main,
      log = plot.settings$log,

    )

    if(De.status == "FAILED"){
      mtext(text = "RC criteria exceeded, check De results!", side = 3, outer = TRUE, col = "red")
      warning("RC criteria exceeded, check De results!")

    }

      ##use scientific format for y-axis
      labels <- axis(2, labels = FALSE)
      axis(side = 2, at = labels, labels = format(labels, scientific = TRUE))

      ##(1) plot points that have been not selected
      points(RF_reg[-(min(RF_reg.lim):max(RF_reg.lim)),1:2], pch=3, col=col[19])

      ##(2) plot points that has been used for the fitting
      points(RF_reg.x,RF_reg.y, pch=3, col=col[10])

      ##show natural points if no analysis was done
      if(method != "SLIDE" & method != "FIT"){

        ##add points
        points(RF_nat, pch = 20, col = "grey")
        points(RF_nat.limited, pch = 20, col = "red")

        ##legend
        legend(plot.settings$legend.pos, legend=c("RF_nat","RF_reg"),
               pch=c(19,3), col=c("red", col[10]),
               horiz=TRUE, bty = "n", cex=.9)


      }

    ## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
    ## PLOT - METHOD FIT
    ## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
    if(method == "FIT"){

      ##dummy to cheat R CMD check
      x<-NULL; rm(x)

      ##plot fitted curve
      curve(fit.parameters.results["phi.0"]-
              (fit.parameters.results["delta.phi"]*
                 ((1-exp(-fit.parameters.results["lambda"]*x))^fit.parameters.results["beta"])),
            add=TRUE,
            from = RF_reg[min(RF_reg.lim), 1],
            to = RF_reg[max(RF_reg.lim), 1],
            col="red")

        ##plotting to show the limitations if RF_reg.lim was chosen
        ##show fitted curve GREY (previous red curve)
        curve(fit.parameters.results["phi.0"]-
                (fit.parameters.results["delta.phi"]*
                   ((1-exp(-fit.parameters.results["lambda"]*x))^fit.parameters.results["beta"])),
              add=TRUE,
              from = min(RF_reg[, 1]),
              to = RF_reg[min(RF_reg.lim), 1],
              col="grey")

        ##show fitted curve GREY (after red curve)
        curve(fit.parameters.results["phi.0"]-
                (fit.parameters.results["delta.phi"]*
                   ((1-exp(-fit.parameters.results["lambda"]*x))^fit.parameters.results["beta"])),
              add=TRUE,
              from = RF_reg[max(RF_reg.lim), 1],
              to = max(RF_reg[, 1]),
              col="grey")

      ##add points
      points(RF_nat, pch = 20, col = col[19])
      points(RF_nat.limited, pch = 20, col = col[2])

      ##legend
      legend(plot.settings$legend.pos, legend=c("RF_nat","RF_reg"),
             pch=c(19,3), col=c("red", col[10]),
             horiz=TRUE, bty = "n", cex=.9)

      ##plot range choosen for fitting
      abline(v=RF_reg[min(RF_reg.lim), 1], lty=2)
      abline(v=RF_reg[max(RF_reg.lim), 1], lty=2)

      ##plot De if De was calculated
      if(is.na(De) == FALSE & is.nan(De) == FALSE){

        lines(c(0,De.lower), c(RF_nat.error.lower,RF_nat.error.lower), lty=2, col="grey")
        lines(c(0,De), c(RF_nat.mean,RF_nat.mean), lty=2, col="red")
        lines(c(0,De.upper), c(RF_nat.error.upper,RF_nat.error.upper), lty=2, col="grey")

        lines(c(De.lower, De.lower),
              c(0,RF_nat.error.lower), lty=2, col="grey")
        lines(c(De,De), c(0, RF_nat.mean), lty=2, col="red")
        lines(c(De.upper, De.upper),
              c(0,RF_nat.error.upper), lty=2, col="grey")

      }

      ##Insert fit and result
      if(is.na(De) != TRUE & (is.nan(De) == TRUE |
                                     De > max(RF_reg.x) |
                                     De.upper > max(RF_reg.x))){

        try(mtext(side=3, substitute(D[e] == De,
                                     list(De=paste(
                                       De," (",De.lower," ", De.upper,")", sep=""))),
                  line=0, cex=0.8, col="red"), silent=TRUE)

        De.status <- "VALUE OUT OF BOUNDS"

      } else{

        if ("mtext" %in% names(list(...))) {
          mtext(side = 3, list(...)$mtext)
        }else{
          try(mtext(
            side = 3,
            substitute(D[e] == De,
                       list(
                         De = paste(De," [",De.lower," ; ", De.upper,"]", sep =
                                           "")
                       )),
            line = 0,
            cex = 0.7
          ),
          silent = TRUE)
        }

        De.status <- "OK"
      }


      ##==lower plot==##
      par(mar=c(4.2,4,0,0))

      ##plot residuals
      if(is.na(fit.parameters.results[1])==FALSE){

        plot(RF_reg.x,residuals(fit),
             xlim=c(0,max(temp.sequence.structure$x.max)),
             xlab=plot.settings$xlab,
             yaxt = "n",
             type="p",
             pch=20,
             col="grey",
             ylab="E",
             log="")

        ##add 0 line
        abline(h=0)
      }else{
        plot(NA,NA,
             xlim=c(0,max(temp.sequence.structure$x.max)),
             ylab="E",
             xlab=plot.settings$xlab,
             ylim=c(-1,1)
        )
        text(x = max(temp.sequence.structure$x.max)/2,y=0, "Fitting Error!")
      }
    }

    ## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
    ## PLOT - METHOD SLIDE
    ## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
    else if(method == "SLIDE"){

      ##(0) density plot
      if (slide.show_density) {

        ##showing the density makes only sense when we see at least 10 data points
        if (length(unique(De.MC)) >= 10) {
          ##normal De
          density.mean.MC <- density(De.MC)

          if (plot.settings$log == "y" | plot.settings$log == "xy") {
            temp.scale.ratio <-
              abs(((unique(
                max(RF_nat.limited[,2])
              ) - par("usr")[3]) / 1.75 + par("usr")[3]) /
                unique(max(density.mean.MC$y)))

          }else{
            temp.scale.ratio <-
              ((unique(max(
                RF_nat.limited[,2]
              )) - par("usr")[3]) / 2 + par("usr")[3]) /
              unique(max(density.mean.MC$y))
          }

          polygon(density.mean.MC$x,
                  density.mean.MC$y * temp.scale.ratio,
                  col = rgb(0,0,1,0.5))

        }else{

          warning("Narrow density distribution, no density distribution plotted!")

        }

      }

      ##(1) plot unused points in grey ... unused points are points outside of the set limit
      points(
        matrix(RF_nat.slided[-(min(RF_nat.lim):max(RF_nat.lim)),1:2], ncol = 2),
        pch = 21, col = col[19]
      )

      ##(2) add used points
      points(RF_nat.slided[min(RF_nat.lim):max(RF_nat.lim),], pch = 21, col = col[2],
             bg = col[2])

      ##(3) add line to show the connection between the first point and the De
      lines(x = c(RF_nat.slided[1,1], RF_nat.slided[1,1]),
            y = c(.Machine$double.xmin,RF_nat.slided[1,2]),
            lty = 2,
            col = col[2]
      )

      ##(4) add arrow at the lowest y-coordinate possible to show the sliding
      if (plot.settings$log != "y" & plot.settings$log != "xy") {
        shape::Arrows(
          x0 = 0,
          y0 = ylim[1],
          y1 = ylim[1],
          x1 = RF_nat.slided[1,1],
          arr.type = "triangle",
          arr.length = 0.5,
          code = 2,
          col = col[2],
          arr.adj = 1,
          arr.lwd = 1
        )
      }
      ##uncomment here to see all the RF_nat curves produced by the MC runs
      ##could become a polygone for future versions
      ##lapply(1:n.MC, function(x){lines(slide.MC.list[[x]], col = rgb(0,0,0, alpha = 0.2))})

      ##plot range choosen for fitting
      abline(v=RF_reg[min(RF_reg.lim), 1], lty=2)
      abline(v=RF_reg[max(RF_reg.lim), 1], lty=2)

        legend(plot.settings$legend.pos, legend=c("RF_nat","RF_reg"),
               pch=c(19,3), col=c("red", col[10]),
               horiz=TRUE, bty = "n", cex=.9)


      ##write information on the De in the plot
      if("mtext" %in% names(list(...))) {

        mtext(side = 3, list(...)$mtext)

      }else{

          try(mtext(side=3,
                    substitute(D[e] == De, list(De=paste0(De," [", De.lower, " ; ", De.upper, "]"))),
                    line=0,
                    cex=0.7),
              silent=TRUE)

      }

      ##==lower plot==##
      ##RESIDUAL PLOT
      par(mar=c(4,4,0,0))

        plot(NA,NA,
             ylim = range(residuals),
             xlim=xlim,
             xlab=plot.settings$xlab,
             type="p",
             pch=1,
             col="grey",
             ylab="E",
             yaxt = "n",
             log=ifelse(plot.settings$log == "y" | plot.settings$log == "xy", "", plot.settings$log)
            )

        ##add axis for 0 ... means if the 0 is not visible there is labelling
        axis(side = 4, at = 0, labels = 0)

        ##add residual indicator (should circle around 0)
        col.ramp <- colorRampPalette(c(col[19], "white", col[19]))
        col.polygon <- col.ramp(100)

        if (plot.settings$log != "x") {
          shape::filledrectangle(
            mid = c((xlim[2]) + (par("usr")[2] - xlim[2]) / 2,
                    max(residuals) - diff(range(residuals)) / 2),
            wx = par("usr")[2] - xlim[2],
            wy = diff(range(residuals)),
            col = col.polygon
          )

        }
        ##add 0 line
        abline(h=0, lty = 3)

        ##0-line indicator and arrows if this is not visible
        ##red colouring here only if the 0 point is not visible to avoid too much colouring
        if(max(residuals) < 0 &
           min(residuals) < 0) {
          shape::Arrowhead(
            x0 =   xlim[2] + (par("usr")[2] - xlim[2]) / 2,
            y0 = max(residuals),
            angle = 270,
            lcol = col[2],
            arr.length = 0.4, arr.type = "triangle",
            arr.col = col[2]
          )

        }else if (max(residuals) > 0 & min(residuals) > 0) {
          shape::Arrowhead(
            x0 =   xlim[2] + (par("usr")[2] - xlim[2]) / 2,
            y0 = min(residuals),
            angle = 90,
            lcol = col[2],
            arr.length = 0.4, arr.type = "triangle",
            arr.col = col[2]
          )


        }else{
          points(xlim[2], 0, pch = 3)

        }


      ##add residual points
      points(RF_nat.slided[c(min(RF_nat.lim):max(RF_nat.lim)),1], residuals,
               pch = 20, col = col[19])

      ##add vertical line to mark De (t_n)
      abline(v = De, lty = 2, col = col[2])

      ##add numeric value of De ... t_n
      axis(side = 1, at = De, labels = De, cex.axis = 0.8*plot.settings$cex,
             col = "blue", padj = -1.55,)

    }

    #reset par to default
    par(def.par)


  }#endif::plot
  ##=============================================================================#
  ## RETURN
  ##=============================================================================#

  ##catch up worst case scenarios ... means something went wrong
  if(!exists("De")){De  <- NA}
  if(!exists("De.MC")){De.MC  <- NA}
  if(!exists("De.lower")){De.lower  <- NA}
  if(!exists("De.upper")){De.upper  <- NA}
  if(!exists("De.status")){De.status  <- NA}
  if(!exists("fit")){fit  <- list()}
  if(!exists("slide")){slide <- list()}

  ##combine values for De into a data frame
  De.values <- data.frame(
      De = De,
      De.lower = De.lower,
      De.upper = De.upper,
      De.status = De.status,
      RF_nat.lim = paste(RF_nat.lim, collapse = ":"),
      RF_reg.lim = paste(RF_reg.lim, collapse = ":"),
    row.names = NULL,
    stringsAsFactors = FALSE
  )

  ##produce results object
  newRLumResults.analyse_IRSAR.RF <- set_RLum(class = "RLum.Results",
                                              data = list(
                                                De.values = De.values,
                                                De.RC = RC.data.frame,
                                                De.MC = De.MC,
                                                fit = fit,
                                                slide = slide,
                                                call = sys.call()
                                              ))

  return(newRLumResults.analyse_IRSAR.RF)

}
