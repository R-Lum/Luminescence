#' Plot filter combinations along with net transmission window
#'
#' The function allows to plot transmission windows for different filters. Missing data for specific
#' wavelenghts are automatically interpolated for the given filter data using the function \code{\link{approx}}.
#' With that a standardised output is reached and a net transmission window can be shown.\cr
#'
#' \bold{How to provide input data?}\cr
#'
#' CASE 1\cr
#'
#' The function expects that all filter values are either of type \code{matrix} or \code{data.frame}
#' with two columns. The first columens contains the wavelength, the second the relative transmission
#' (but not in percentage, i.e. the maximum transmission can be only become 1).
#'
#' In this case only the transmission window is show as provided. Changes in filter thickness and
#' relection factor are not considered. \cr
#'
#' CASE 2\cr
#'
#' The filter data itself are provided as list element containing a \code{matrix} or \code{data.frame}
#' and additional information on the thickness of the filter, e.g., \code{list(filter1 = list(filter_matrix, d = 2))}.
#' The given filter data are always considered as standard input and the filter thickness value
#' is taken into account by
#'
#' \deqn{Transmission = Transmission^(d)}
#'
#' with d given in the same dimension as the original filter data.\cr
#'
#' CASE 3\cr
#'
#' Same as CASE 2 but additionally a reflection factor P is provided, e.g.,
#' \code{list(filter1 = list(filter_matrix, d = 2, P = 0.9))}. The final transmission
#' becomes:
#'
#' \deqn{Transmission = Transmission^(d) * P}\cr
#'
#' \bold{Advanced plotting parameters}\cr
#'
#' The following further non-common plotting parameters can be passed to the function:\cr
#'
#' \tabular{lll}{
#' \bold{Argument} \tab \bold{Datatype} \tab \bold{Description}\cr
#' \code{legend} \tab \code{logical} \tab enable/disable legend \cr
#' \code{legend.pos} \tab \code{character} \tab change legend position (\code{\link[graphics]{legend}}) \cr
#' \code{legend.text} \tab \code{character} \tab same as the argument \code{legend} in (\code{\link[graphics]{legend}}) \cr
#' \code{net_transmission.col} \tab \code{col} \tab colour of net transmission window polygon \cr
#' \code{grid} \tab \code{list} \tab full list of arguments that can be passd to the function \code{\link[graphics]{grid}}
#' }
#'
#' For further modifications standard additional R plot functions are recommend, e.g., the legend
#' can be fully customised by disabling the standard legend and use the function \code{\link[graphics]{legend}}
#' instead.
#'
#'
#' @param filters \code{\link{list}} (\bold{required}): a named list of filter data for each filter to be shown.
#' The filter data itself should be either provided as \code{\link{data.frame}} or \code{\link{matrix}}.
#' (for more options s. Details)
#'
#' @param wavelength_range \code{\link{numeric}} (with default): wavelength range used for the interpolation
#'
#' @param show_net_transmission \code{\link{logical}} (with default): show net transmission window
#' as polygon.
#'
#' @param plot \code{\link{logical}} (with default): enables or disables the plot output
#'
#' @param \dots further arguments that can be passed to control the plot output. Suppored are \code{main},
#' \code{xlab}, \code{ylab}, \code{xlim}, \code{ylim}, \code{type}, \code{lty}, \code{lwd}. For non common plotting
#' parameters see the details section.
#'
#' @return Returns an S4 object of type \code{\linkS4class{RLum.Results}}.
#'
#' \bold{@data}
#' \tabular{lll}{
#' \bold{Object} \tab \bold{Type} \bold{Description} \cr
#'  net_transmission_window \tab \code{matrix} \tab the resulting net transmission window \cr
#'  filter_matrix \tab \code{matrix} \tab the filter matrix used for plotting
#'
#' }
#'
#' \bold{@info}
#' \tabular{lll}{
#' \bold{Object} \tab \bold{Type} \bold{Description} \cr
#' call \tab \code{call} \tab the original function call
#'
#' }
#'
#' @section Function version: 0.1.0
#'
#' @author Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montagine (France)\cr
#'
#' @seealso \code{\linkS4class{RLum.Results}}, \code{\link{approx}}
#'
#' @keywords datagen aplot
#'
#' @examples
#'
#' ## (For legal reasons no real filter data are provided)
#'
#' ## Create filter sets
#' filter1 <- density(rnorm(100, mean = 450, sd = 20))
#' filter1 <- matrix(c(filter1$x, filter1$y/max(filter1$y)), ncol = 2)
#' filter2 <- matrix(c(200:799,rep(c(0,0.8,0),each = 200)), ncol = 2)
#'
#' ## Example 1 (standard)
#' plot_FilterCombinations(filters = list(filter1, filter2))
#'
#' ## Example 2 (with d and P value and name for filter 2)
#' results <- plot_FilterCombinations(
#' filters = list(filter_1 = filter1, Rectangle = list(filter2, d = 2, P = 0.6)))
#' results
#'
#'
#' @export
plot_FilterCombinations <- function(
  filters,
  wavelength_range = 200:1000,
  show_net_transmission = TRUE,
  plot = TRUE,
  ...) {
  # Integrity tests -----------------------------------------------------------------------------

  #check filters
  if (!is(filters, "list")) {
    stop("[plot_FilterCombinations()] 'filters' should be of type 'list'")

  }

  #input should either data.frame or matrix
  lapply(filters, function(x) {
    if (!is(x, "data.frame") & !is(x, "matrix") & !is(x, "list")) {
      stop(
        paste(
          "[plot_FilterCombinations()] input for filter",
          x,
          "is not of type 'matrix', 'data.frame' or 'list'!"
        )
      )

    }


  })

  #check for named list, if not set names
  if (is.null(names(filters))) {
    names(filters) <- paste("Filter ", 1:length(filters))

  }


  # Data Preparation ----------------------------------------------------------------------------

  ##check if filters are provided with their tickness, if so correct
  ##transmission for this ... relevant for glass filters
  filters <- lapply(filters, function(x) {
    if (is(x, "list")) {

      ##correction for the transmission accounting for filter tickness, the
      ##provided thickness is always assumed to be 1
      if(length(x) > 1){
        x[[1]][, 2] <- x[[1]][, 2] ^ (x[[2]])

      }else{
        return(x[[1]])

      }

      ##account for potentially provided transmission relexion factor
      if(length(x) > 2){
       x[[1]][,2] <-  x[[1]][,2] * x[[3]]
       return(x[[1]])

      }else{
       return(x[[1]])

      }

    } else{
      return(x)

    }

  })

  #check if there are transmission values greater than one, this is not possible
  lapply(filters, function(x) {
    if (max(x[, 2], na.rm = TRUE) > 1.01) {
      stop("[plot_FilterCombinations()] transmission values > 1 found. Check your data.")

    }

  })

  ##combine everything in a matrix using approx for interpolation
  filter_matrix <- vapply(filters, function(x) {
    approx(x = x[, 1], y = x[, 2], xout = wavelength_range)$y

  }, FUN.VALUE = vector(mode = "numeric", length = length(wavelength_range)))

  ##calculate transmission window
  filter_matrix <- cbind(filter_matrix)
  net_transmission_window <- matrix(
    c(wavelength_range, matrixStats::rowMins(filter_matrix)),
    ncol = 2)

  ##set rownames of filter matrix
  rownames(filter_matrix) <- wavelength_range

  ##set column names for filter matrix
  colnames(filter_matrix) <- names(filters)

  # Plotting ------------------------------------------------------------------------------------

  if (plot) {
    ##set plot settings
    plot_settings <- list(
      main = "Filter Combination",
      xlab = "Wavelength [nm]",
      ylab = "Transmission [a.u.]",
      xlim = range(wavelength_range),
      ylim = c(0, 1),
      legend.pos = "topleft",
      lty = 1,
      lwd = 1,
      col = 1:length(filters),
      grid = expression(nx = 10, ny = 10),
      legend = TRUE,
      legend.text = colnames(filter_matrix),
      net_transmission.col = "grey"

    )

    ##modify settings on request
    plot_settings <- modifyList(plot_settings, list(...))

    ##plot induvidal filters
    graphics::matplot(
      x = wavelength_range,
      y = filter_matrix,
      type = "l",
      main = plot_settings$main,
      xlab = plot_settings$xlab,
      ylab = plot_settings$ylab,
      xlim = plot_settings$xlim,
      ylim = plot_settings$ylim,
      lty = plot_settings$lty,
      lwd = plot_settings$lwd,
      col = plot_settings$col

    )

    if (!is.null(plot_settings$grid)) {
      graphics::grid(eval(plot_settings$grid))

    }

    ##show effective transmission, which is the minimum for each row
    if (show_net_transmission) {
      polygon(
        x = c(wavelength_range, rev(wavelength_range)),
        y = c(net_transmission_window[, 2],
              rep(0, length(wavelength_range))),
        col = plot_settings$net_transmission.col,
        border = NA
      )

    }

    #legend
    if (plot_settings$legend) {
      legend(
        plot_settings$legend.pos,
        legend = plot_settings$legend.text,
        col = plot_settings$col,
        lty = plot_settings$lty,
        bty = "n"
      )
    }


  }


  # Produce output object -----------------------------------------------------------------------
  return(set_RLum(
    class = "RLum.Results",
    data = list(
      net_transmission_window = net_transmission_window,
      filter_matrix = filter_matrix

    ),
    info = list(call = sys.call())
  ))



}
