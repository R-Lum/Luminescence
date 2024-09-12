#' Obtain the equivalent dose using the approach by Woda and Fuchs 2008
#'
#' The function generates a histogram-like reorganisation of the data, to
#' assess counts per bin. The log-transformed counts per bin are used to
#' calculate the second derivative of the data (i.e., the curvature of the
#' curve) and to find the central value of the bin hosting the distribution
#' maximum. A normal distribution model is fitted to the counts per bin
#' data to estimate the dose distribution parameters. The uncertainty of the
#' model is estimated based on all input equivalent doses smaller that of the
#' modelled central value.
#'
#' @param data [data.frame] [vector], or [RLum.Results-class] object (**required**):
#' for [data.frame]: either two columns: De (`values[,1]`) and De error
#' (`values[,2]`), or one: De (`values[,1]`). If a numeric vector or a
#' single-column data frame is provided, De error is set to `NA`.
#' For plotting multiple data sets, these must be provided as `list`
#' (e.g. `list(dataset1, dataset2)`).
#'
#' @param breaks [numeric]:
#' Either number or locations of breaks. See `[hist]` for details.
#' If missing, the number of breaks will be estimated based on the bin width
#' (as function of median error).
#'
#' @param plot [logical] (*with default*):
#' enable plot output.
#'
#' @param ... Further plot arguments passed to the function.
#'
#' @section Function version: 0.2.0
#'
#' @author Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany),\cr
#' Michael Dietze, GFZ Potsdam (Germany)
#'
#' @seealso [calc_FuchsLang2001], [calc_CentralDose]
#'
#' @references
#' Woda, C., Fuchs, M., 2008. On the applicability of the leading edge method to
#' obtain equivalent doses in OSL dating and dosimetry. Radiation Measurements 43, 26-37.
#'
#' @examples
#'
#' ## read example data set
#' data(ExampleData.DeValues, envir = environment())
#'
#' results <- calc_WodaFuchs2008(
#'   data = ExampleData.DeValues$CA1,
#'    xlab = expression(paste(D[e], " [Gy]"))
#'  )
#'
#' @md
#' @export
calc_WodaFuchs2008 <- function(
  data,
  breaks = NULL,
  plot = TRUE,
  ...
) {

  ##TODO
  # - complete manual
  # - add statistics to the plot
  # - check whether this makes sense at all ...

  ## check data and parameter consistency -------------------------------------

    if(is(data, "RLum.Results") == FALSE &
         is(data, "data.frame") == FALSE &
         is.numeric(data) == FALSE) {

      .throw_warning("Input data must be one of 'data.frame', 'RLum.Results' ",
                     "or 'numeric', NULL returned")
      return(NULL)

    } else {

      if(is(data, "RLum.Results") == TRUE) {
        data <- tryCatch(get_RLum(data, "data"),
                         error = function(e) get_RLum(data))
      }

      ## if data is a numeric vector or a single-column data frame,
      ## append a second column of NAs
      if (NCOL(data) < 2) {
        data <- cbind(data, NA)
      }

      ## with just one data point, it's possible to cause nls() to hang
      if (nrow(data) < 2) {
        .throw_error("Insufficient number of data points")
      }
    }

  ## read additional arguments

  if("trace" %in% names(list(...))) {

    trace <- list(...)$trace
  } else {

    trace <- FALSE
  }

  ## calculations -------------------------------------------------------------

  ## estimate bin width based on Woda and Fuchs (2008)
  if (all(is.na(data[, 2]))) {
    message("[calc_WodFuchs2008()] No errors provided. Bin width set ",
            "by 10 percent of input data")
    bin_width <- median(data[,1] / 10,
                        na.rm = TRUE)
  } else {

    bin_width <- median(data[,2],
                        na.rm = TRUE)
  }

  ## optionally estimate class breaks based on bin width
  if(is.null(breaks)) {
    n_breaks <- diff(range(data[, 1], na.rm = TRUE)) / bin_width
  } else {
    n_breaks <- breaks
  }

  if (n_breaks <= 3) {
    .throw_warning("Fewer than 4 bins produced, 'breaks' set to 4")
    n_breaks = 4
  }

  ## calculate histogram
  H <- hist(x = data[,1],
            breaks = n_breaks,
            plot = FALSE)

  ## extract values from histogram object
  H_c <- H$counts
  H_m <- H$mids

  ## log counts
  counts_log <- log(H_c)

  ## estimate curvature
  curvature <- (counts_log[1] - counts_log[2]) /
    (counts_log[1] - counts_log[3])

  ## do some other black magic
  class_center <- H$mids[H_c == max(H_c)]

  ## optionally print warning
  if(length(class_center) != 1) {
    .throw_warning("More than one maximum, fit may be invalid")
    class_center <- class_center[1]
  }

  ## fit normal distribution to data
  fit <-	nls(H_c ~ (A / sqrt(2 * pi * sigma^2)) *
               exp(-(H_m - class_center)^2 / (2 * sigma^2)),
             start = c(A = mean(H_m),
                       sigma = bin_width),
             control = c(maxiter = 5000),
             algorithm = "port",
             trace = trace)

  ## extract fitted parameters
  A <- coef(fit)["A"]
  sigma <- coef(fit)["sigma"]

  ## estimate dose
  D_estimate <- as.numeric(x = class_center - sigma)

  ## count number of values smaller than center class
  count_ID <- length(which(H_m <= class_center))

  ## extract H_m values smaller than center class
  H_m_smaller <- H_m[1:count_ID]

  ## calculate uncertainty according to Woda and Fuchs (2008)
  s <- round(sqrt(sum((H_m_smaller - D_estimate)^2) / (count_ID - 1)),
             digits = 2)

  ## plot output --------------------------------------------------------------
  if(plot) {

    ##define plot settings
    plot_settings <- list(
      xlab = expression(paste(D[e], " [s]")),
      ylab = "Frequency",
      xlim = range(data[,1], na.rm = TRUE) + c(-10, 20),
      ylim = NULL,
      main = expression(paste(D[e]," applying Woda and Fuchs (2008)")),
      sub = NULL
    )

    plot_settings <- modifyList(x = plot_settings, val = list(...), keep.null = TRUE)

    plot(
      x = H,
      xlab = plot_settings$xlab,
      ylab = plot_settings$ylab,
      xlim = plot_settings$xlim,
      ylim = plot_settings$ylim,
      main = plot_settings$main,
      sub = plot_settings$sub
    )

    ## add curve with normal distribution
    x <- 0
    rm(x)
    curve((A / sqrt(2 * pi * sigma^2)) * exp(-(x- class_center)^2 / (2 * sigma^2)),
          add = TRUE,
          to = class_center,
          col = "red"
          )
  }

  ## return output ------------------------------------------------------------
  return(set_RLum(
    class = "RLum.Results",
    data = list(
      D_estimate = data.frame(
        DP = D_estimate,
        DP.ERROR = s,
        CLASS_CENTER = class_center,
        BIN_WIDTH = bin_width,
        SIGMA = sigma,
        A = A,
        row.names = NULL
      ),
      breaks = H$breaks
    ),
    info = list(call = sys.call())
  ))
}
