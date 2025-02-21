#' @title Plot filter combinations along with the (optional) net transmission window
#'
#' @description
#' The function allows to plot transmission windows for different filters. Missing data for specific
#' wavelengths are automatically interpolated for the given filter data using the function [approx].
#' With that a standardised output is reached and a net transmission window can be shown.
#'
#' **Calculations**
#'
#' **Net transmission window**
#'
#' The net transmission window of two filters is approximated by
#'
#' \deqn{T_{final} = T_{1} * T_{2}}
#'
#' **Optical density**
#'
#' \deqn{OD = -log10(T)}
#'
#' **Total optical density**
#'
#' \deqn{OD_{total} = OD_{1} +  OD_{2}}
#'
#' Please consider using own calculations for more precise values.
#'
#' **How to provide input data?**
#'
#' *CASE 1*
#'
#' The function expects that all filter values are either of type `matrix` or `data.frame`
#' with two columns. The first columns contains the wavelength, the second the relative transmission
#' (but not in percentage, i.e. the maximum transmission can be only become 1).
#'
#' In this case only the transmission window is show as provided. Changes in filter thickness and
#' reflection factor are not considered.
#'
#' *CASE 2*
#'
#' The filter data itself are provided as list element containing a `matrix` or
#' `data.frame` and additional information on the thickness of the filter, e.g.,
#' `list(filter1 = list(filter_matrix, d = 2))`.
#' The given filter data are always considered as standard input and the filter thickness value
#' is taken into account by
#'
#' \deqn{Transmission = Transmission^(d)}
#'
#' with d given in the same dimension as the original filter data.
#'
#' *CASE 3*
#'
#' Same as CASE 2 but additionally a reflection factor P is provided, e.g.,
#' `list(filter1 = list(filter_matrix, d = 2, P = 0.9))`.
#' The final transmission becomes:
#'
#' \deqn{Transmission = Transmission^(d) * P}
#'
#' **Advanced plotting parameters**
#'
#' The following further non-common plotting parameters can be passed to the function:
#'
#' \tabular{lll}{
#' **`Argument`** \tab **`Datatype`** \tab **`Description`**\cr
#' `legend` \tab `logical` \tab enable/disable legend \cr
#' `legend.pos` \tab `character` \tab change legend position ([graphics::legend]) \cr
#' `legend.text` \tab `character` \tab same as the argument `legend` in ([graphics::legend]) \cr
#' `net_transmission.col` \tab `col` \tab colour of net transmission window polygon \cr
#' `net_transmission.col_lines` \tab `col` \tab colour of net transmission window polygon lines \cr
#' `net_transmission.density` \tab  `numeric` \tab specify line density in the transmission polygon \cr
#' `grid` \tab `list` \tab full list of arguments that can be passed to the function [graphics::grid]
#' }
#'
#' For further modifications standard additional R plot functions are recommend, e.g., the legend
#' can be fully customised by disabling the standard legend and use the function [graphics::legend]
#' instead.
#'
#'
#' @param filters [list] (**required**):
#' a named list of filter data for each filter to be shown.
#' The filter data itself should be either provided as [data.frame] or [matrix]
#' (see details for more options).
#'
#' @param wavelength_range [numeric] (*with default*):
#' wavelength range used for the interpolation
#'
#' @param show_net_transmission [logical] (*with default*):
#' show net transmission window as polygon.
#'
#' @param interactive [logical] (*with default*):
#' enable/disable interactive plots.
#'
#' @param plot [logical] (*with default*):
#' enable/disable the plot output.
#'
#' @param ... further arguments that can be passed to control the plot output.
#' Supported are `main`, `xlab`, `ylab`, `xlim`, `ylim`, `type`, `lty`, `lwd`.
#' For non common plotting parameters see the details section.
#'
#' @return Returns an S4 object of type [RLum.Results-class].
#'
#' **@data**
#'
#' \tabular{lll}{
#'  **`Object`** \tab **`Type`** **`Description`** \cr
#'  `net_transmission_window` \tab `matrix` \tab the resulting net transmission window \cr
#'  `OD_total` \tab `matrix` \tab the total optical density\cr
#'  `filter_matrix` \tab `matrix` \tab the filter matrix used for plotting
#' }
#'
#' **@info**
#'
#' \tabular{lll}{
#' **Object** \tab **Type** **Description** \cr
#' `call` \tab [call] \tab the original function call
#' }
#'
#' @section Function version: 0.3.2
#'
#' @author Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#'
#' @seealso [RLum.Results-class], [approx]
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
#' ## Example 3 show optical density
#' plot(results$OD_total)
#'
#' \dontrun{
#' ##Example 4
#' ##show the filters using the interactive mode
#' plot_FilterCombinations(filters = list(filter1, filter2), interactive = TRUE)
#'
#' }
#'
#'
#' @md
#' @export
plot_FilterCombinations <- function(
  filters,
  wavelength_range = 200:1000,
  show_net_transmission = TRUE,
  interactive = FALSE,
  plot = TRUE,
  ...
) {
  .set_function_name("plot_FilterCombinations")
  on.exit(.unset_function_name(), add = TRUE)

  ## Integrity checks -------------------------------------------------------

  .validate_class(filters, "list")
  .validate_not_empty(filters, "list")

  #input should either data.frame or matrix
  lapply(filters, function(x) {
    .validate_class(x, c("data.frame", "matrix", "list"),
                    name = "All elements of 'filters'")
  })

  #check for named list, if not set names
  if (is.null(names(filters))) {
    names(filters) <- paste("Filter ", seq_along(filters))
  }


  ## Data preparation -------------------------------------------------------

  ## check if filters are provided with their thickness, if so correct
  ## transmission for this ... relevant for glass filters
  filters <- lapply(filters, function(x) {
    if (is(x, "list")) {

      ## correction for the transmission accounting for filter thickness,
      ## the provided thickness is always assumed to be 1
      if(length(x) > 1){
        x[[1]][, 2] <- x[[1]][, 2] ^ (x[[2]])

      }else{
        return(x[[1]])
      }

      ## account for potentially provided transmission reflection factor
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
      .throw_error("Transmission values > 1 found, check your data")
    }
  })

  ##combine everything in a matrix using approx for interpolation
  filter_matrix <- vapply(filters, function(x) {
    approx(x = x[, 1], y = x[, 2], xout = wavelength_range)$y

  }, FUN.VALUE = vector(mode = "numeric", length = length(wavelength_range)))

  ##calculate transmission window
  filter_matrix <- cbind(filter_matrix)
  net_transmission_window <- matrix(
    c(wavelength_range, matrixStats::rowProds(filter_matrix)),
    ncol = 2)

  ##add optical density to filter matrix

  ##calculate OD
  OD <- -log10(filter_matrix)

  ##calculate  total OD
  OD_total <- cbind(wavelength_range, matrixStats::rowSums2(OD))

  ##add to matrix
  filter_matrix <- cbind(filter_matrix, OD)

  ##set rownames of filter matrix
  rownames(filter_matrix) <- wavelength_range

  ##set column names for filter matrix
  colnames(filter_matrix) <- c(names(filters), paste0(names(filters), "_OD"))


  # Plotting ------------------------------------------------------------------------------------
  if (plot) {

    ##(1) ... select transmission values
    filter_matrix_transmisison <- filter_matrix[,!grepl(pattern = "OD", x = colnames(filter_matrix)), drop = FALSE]

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
      legend.text = colnames(filter_matrix_transmisison),
      net_transmission.col = rgb(0,0.7,0,.2),
      net_transmission.col_lines = "grey",
      net_transmission.density = 20
    )

    ##modify settings on request
    plot_settings <- modifyList(plot_settings, list(...))

    if(interactive){
      .require_suggested_package("plotly", "Producing interactive plots")

      ##create basic plot
      p <-
        plotly::plot_ly(x = wavelength_range,
                        y = filter_matrix[,1],
                        type = "scatter",
                        name = colnames(filter_matrix_transmisison)[1],
                        mode = "lines")

        ##add further filters
        if (ncol(filter_matrix_transmisison) > 1) {
          for (i in 2:ncol(filter_matrix_transmisison)) {
            p <- plotly::add_trace(p,
                        y = filter_matrix[, i],
                        name = colnames(filter_matrix_transmisison)[i],
                        mode = 'lines')
          }
        }


      ##add polygon
      ##replace all NA vaules with 0, otherwise it looks odd
      net_transmission_window[is.na(net_transmission_window)] <- 0

      p <-  plotly::add_polygons(p,
                        x = c(wavelength_range, rev(wavelength_range)),
                        y = c(net_transmission_window[, 2], rep(0, length(wavelength_range))),
                        name = "net transmission"
                        )

      ##change graphical parameters
      p <-  plotly::layout(
        p = p,
        xaxis = list(
          title = plot_settings$xlab
        ),
        yaxis = list(
          title = plot_settings$ylab
        ),
        title = plot_settings$main
      )

      print(p)
      on.exit(return(p), add = TRUE)


    }else{
      ##plot induvidal filters
      graphics::matplot(
        x = wavelength_range,
        y = filter_matrix_transmisison,
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

        ##replace all NA vaules with 0, otherwise it looks odd
        net_transmission_window[is.na(net_transmission_window)] <- 0

        polygon(
          x = c(wavelength_range, rev(wavelength_range)),
          y = c(net_transmission_window[, 2],
                rep(0, length(wavelength_range))),
          col = plot_settings$net_transmission.col,
          border = NA,
        )
        polygon(
          x = c(wavelength_range, rev(wavelength_range)),
          y = c(net_transmission_window[, 2],
                rep(0, length(wavelength_range))),
          col = plot_settings$net_transmission.col_lines,
          border = NA,
          density = plot_settings$net_transmission.density
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
  }


  # Produce output object -----------------------------------------------------------------------
  invisible(set_RLum(
    class = "RLum.Results",
    data = list(
      net_transmission_window = net_transmission_window,
      OD_total = OD_total,
      filter_matrix = filter_matrix

    ),
    info = list(call = sys.call())
  ))
}
