#' Plot comparison of CW-OSL component photoionisation cross sections of different models
#'
#' This function takes the `output.complex = TRUE` output of [fit_OSLcurve] and draws the
#' photoionisation cross sections of different models in relation to each other.
#' If a stimulation wavelength between 465 and 480 nm was chosen,
#' the photoionisation cross sections are also set in relation to literature values
#' from Singarayer and Bailey (2003), Jain et al. (2003) and Durcan and Duller (2011).
#'
#' The photoionisation cross section ranges of the reference components are defined as following:
#'
#'  \tabular{lll}{
#'  **Component** \tab **Lower limit (cm^2)** \tab **Upper limit (cm^2)**\cr
#'  Ultrafast \tab 1e-16 \tab 1e-15 \cr
#'  Fast \tab 1.9e-17 \tab 3.1e-17 \cr
#'  Medium \tab 3e-18 \tab 9e-18 \cr
#'  Slow1 \tab 1e-18 \tab 1.85e-18 \cr
#'  Slow2 \tab 1.1e-19 \tab 4e-19 \cr
#'  Slow3 \tab 1e-20 \tab 4.67e-20 \cr
#'  Slow4 \tab 1e-21 \tab 1e-20
#' }
#'
#'
#' @param fit.list [list] (**required**):
#' Output object of [fit_OSLcurve]. The object must be created with the setting `output.complex = TRUE`.
#'
#' @param stimulation.intensity [numeric] (*optional*):
#' Intensity of optical stimulation in *mW / cm²*. Used to calculate the photoionisation cross sections.
#' If not given, the input value for [fit_OSLcurve] is used
#'
#' @param stimulation.wavelength [numeric] (*optional*):
#' Wavelength of optical stimulation in nm. Used to calculate the photoionisation cross sections.
#' If not given, the input value for [fit_OSLcurve] is used
#'
#' @param K.selected [numeric] (*optional*):
#' Draws a red rectangle around the `K = K.selected` row, thus highlighting the model of choice.
#'
#' @param title [character] (*with default*):
#' Plot title. Set `title = NULL` for no title.
#'
#' @param hide.plot [logical] (*with default*):
#' If true, plot is not drawn but can still be saved as file or caught by `A <- plot_PhotoCrosssections(...)`.
#' If caught, the plot can be drawn manually for example by using [gridExtra::grid.arrange].
#'
#' @param filename [character] (*optional*):
#' File name or path to save the plot as image. If just a file name is given, the image is
#' saved in the working directory. The image type is chosen by the file ending. Both, vector images
#' as well as pixel images are possible. Allowed are `.pdf`, `.eps`, `.svg` (vector graphics),
#' `.jpg`, `.png`, `.bmp` (pixel graphics) and more, see [ggplot2::ggsave].
#'
#' @return
#' An invisible [ggplot2::ggplot] object containing the diagram will returned. "Invisible" means, the no value
#' will be returned (e.g. no console printout) if the function is not assigned to a variable via `<-`.
#' If the function is assigned, the returned object can be further manipulated by [ggplot2-package] methods
#' or manually drawn by various functions like for example [gridExtra::grid.arrange].
#'
#' @section Last updates:
#'
#' 2020-11-04, DM: Added roxygen documentation
#'
#' @author
#' Dirk Mittelstraß, \email{dirk.mittelstrass@@luminescence.de}
#'
#' Please cite the package the following way:
#'
#' Mittelstraß, D., Schmidt, C., Beyer, J., Heitmann, J. and Straessner, A.:
#' R package OSLdecomposition: Automated identification and separation of quartz CW-OSL signal components, *in preparation*.
#'
#' @seealso [fit_OSLcurve], [RLum.OSL_global_fitting]
#'
#' @references
#'
#' Durcan, J.A., Duller, G.A.T., 2011. The fast ratio: A rapid measure for testing the dominance of the fast component in the initial OSL signal from quartz. Radiation Measurements 46, 1065–1072.
#'
#' Jain, M., Murray, A.S., Bøtter-Jensen, L., 2003. Characterisation of blue-light stimulated luminescence components in different quartz samples: implications for dose measurement. Radiation Measurements 37, 441–449.
#'
#' Singarayer, J.S., Bailey, R.M., 2003. Further investigations of the quartz optically stimulated luminescence components using linear modulation.
#' Radiation Measurements, Proceedings of the 10th international Conference on Luminescence and Electron-Spin Resonance Dating (LED 2002) 37, 451–458.
#'
#'
#' @examples
#'
#' # Set some arbitrary decay parameter for a dim CW-OSL measurement of quartz
#' name <- c("fast", "slow")
#' lambda <- c(2, 0.02)
#' n <- c(1e6, 5e7)
#'
#' # Build a component table
#' components <- data.frame(name, lambda, n)
#'
#' # Simulate the CW-OSL curve and add some signal noise
#' curve <- simulate_OSLcomponents(components, simulate.curve = TRUE, add.poisson.noise = TRUE)
#'
#' # Perform nonlinear regression at the simulated curve
#' fit_results <- fit_OSLcurve(curve, K.max = 2, output.complex = TRUE)
#'
#' # Plot the fitting iterations and set them into context
#' plot_PhotoCrosssections(fit_results)
#'
#'
#' @md
#' @export
plot_PhotoCrosssections <- function(
  fit.list,
  stimulation.intensity = NULL,
  stimulation.wavelength = NULL,
  K.selected = NULL,
  title = NULL,
  hide.plot = FALSE,
  filename = NULL
){

  # Changelog:
  # * 2019-10-08 Separated from fit_OSLcurve()
  # * 2020-04-09 Changed from decay rates to photoionisation cross-sections; cleaned code
  # * 2020-09-13 Added: K.selected, hide.plot, filename and auto-finding simulation parameters
  # * 2020-11-04, DM: Added roxygen documentation; Removed library(XXX)
  #
  # ToDo:
  # * Sometimes the red rectangle is not drawn (fit_OSLcurve(Batagai, K.max = 6)). Why?
  # * add literature values for other than just 470nm values
  # * Add argument ggsave.control() to give direct control about image saving

  ggplot2::theme_set(ggplot2::theme_bw())

  # Solve a devtools::check problem were it complains about ggplot2 syntax ("no visible binding for global variable")
  lambda <- lambda.low <- lambda.up <- NULL

  if (is.null(stimulation.intensity)) stimulation.intensity <- fit.list$parameters$stimulation.intensity

  if (is.null(stimulation.wavelength)) stimulation.wavelength <- fit.list$parameters$stimulation.wavelength

  plot_data <- fit.list$plot.data
  x <- nrow(fit.list$F.test)

  # Calc photon energy: E = h*v  [W*s^2 * s^-1 = W*s = J]
  E <-6.62606957e-34 * 299792458 * 10^9 / stimulation.wavelength

  # Calc photon flux of stimulation light: Flux = I / E  [W/cm^2 / W*s = 1/s*cm^2]
  Flux <- stimulation.intensity / (E * 1000)

  # Transform decay rates into photoionisation cross-sections
  plot_data$lambda <- plot_data$lambda / Flux
  plot_data$lambda.low <- plot_data$lambda.low / Flux
  plot_data$lambda.up <- plot_data$lambda.up / Flux

  # Plot literature values just in case of blue light stimulation
  plot_literature <- FALSE
  if ((stimulation.wavelength >= 465) && (stimulation.wavelength <= 480)) plot_literature <- TRUE

  if(plot_literature) {

    # set drawing borders for fast component (1:2) and medium component (3:4)
    y.components <- c(1.9e-17, 3.1e-17,
                      3e-18, 9e-18,
                      1e-18, 1.85e-18,
                      1.1e-19, 4e-19,
                      1e-20, 4.67e-20)
    y.means <- c((y.components[1] - y.components[2])/ log(y.components[1] / y.components[2]),
                 (y.components[3] - y.components[4])/ log(y.components[3] / y.components[4]),
                 (y.components[5] - y.components[6])/ log(y.components[5] / y.components[6]),
                 (y.components[7] - y.components[8])/ log(y.components[7] / y.components[8]),
                 (y.components[9] - y.components[10])/ log(y.components[9] / y.components[10]))

     # Increase number of plotting rows
    x <- x + 1

    # Durcan and Duller 2011
    plot_data <- rbind(plot_data,
                       data.frame(lambda = c(2.6e-17, 4.28e-18, 1.09e-18, 3.04e-19, 3.39e-20, 9.06e-21),
                                  lambda.low = c(2.54e-17, 3.93e-18, 1.04e-18, 2.58e-19, 2.73e-20, 8.31e-21),
                                  lambda.up = c(2.66e-17, 4.62e-18, 1.14e-18, 3.5e-19, 4.03e-20, 9.81e-21),
                                  name = factor("Durcan & Duller (2011)"),
                                  x = x))
    x <- x + 1

    # Jain et al. 2003
    plot_data <- rbind(plot_data,
                       data.frame(lambda = c(2.9e-16, 2.32e-17, 5.59e-18, 1.33e-18, 2.08e-19, 2.06e-20, 2.76e-21),
                                  lambda.low = c(2.9e-16,2.16e-17, 5.15e-18, 1.07e-18, 1.62e-19, 1.9e-20, 2.59e-21),
                                  lambda.up = c(2.9e-16,2.48e-17, 6.03e-18, 1.59e-18, 2.54e-19, 2.22e-20, 2.93e-21),
                                  name = factor("Jain et al. (2003)"),
                                  x = x))
    x <- x + 1

    # Singarayer and Bailey 2003
    plot_data <- rbind(plot_data,
                       data.frame(lambda = c(2.5e-17, 5.9e-18, 2.1e-19, 1.2e-20, 1.9e-21),
                                  lambda.low = c(2.2e-17, 3.9e-18, 1.6e-19, 1.0e-20, 0.7e-21),
                                  lambda.up = c(2.8e-17, 7.9e-18, 2.6e-19, 1.4e-20, 4.7e-21),
                                  name = factor("Singarayer & Bailey (2003)"),
                                  x = x))
  }

  # set x-Axis
  reverselog_trans <- function(base = exp(1)) {
    trans <- function(x) -log(x, base)
    inv <- function(x) base^(-x)
    scales::trans_new(paste0("reverselog-", format(base)), trans, inv,
                      scales::log_breaks(base = base),
                      domain = c(1e-100, Inf))
    }

  breaks <- 10^(-25:-15)
  minor_breaks <- rep(1:9, 21)*(10^rep(-25:-15, each=9))
  xmax <- x

  # set plotting band width
  ymin <- min(plot_data$lambda.low) / 1.5
  ymax <- max(plot_data$lambda.up) * 1.5

  ##### build F-test plot #####

  # Draw decay rates from F-table as diagram
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = x,
                             y = lambda)) +
    ggplot2::geom_point(size = 2.5) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = lambda.low, ymax = lambda.up), width = .3) +
    ggplot2::scale_y_continuous(trans = reverselog_trans(10),
                       limits = c(ymax, ymin),
                       breaks = breaks,
                       minor_breaks = minor_breaks) +
    ggplot2::scale_x_reverse(labels = levels(plot_data$name),
                    breaks = 1:xmax,
                    minor_breaks = 1:(xmax + 1) - 0.5)

  # Draw component definition ranges from literature
  if(plot_literature) {p <- p +
    ggplot2::annotate("rect", xmin = 0.5, xmax = xmax + 1.5,  #################
             ymin = y.components[1], ymax = y.components[2], alpha = 0.1, fill = "red3") +
    ggplot2::annotate("text", x = xmax + 1, y = y.means[1], label = "Fast", colour = "red3") +
    ggplot2::annotate("rect", xmin = 0.5, xmax = xmax + 1.5, #################
             ymin = y.components[3], ymax = y.components[4], alpha = 0.1, fill = "green3") +
    ggplot2::annotate("text", x = xmax + 1, y = y.means[2], label = "Medium", colour = "green3") +
    ggplot2::annotate("rect", xmin = 0.5, xmax = xmax + 1.5, #################
             ymin = y.components[5], ymax = y.components[6], alpha = 0.1, fill = "blue3") +
    ggplot2::annotate("text", x = xmax + 1, y = y.means[3], label = "Slow1", colour = "blue3") +
    ggplot2::annotate("rect", xmin = 0.5, xmax = xmax + 1.5, #################
             ymin = y.components[7], ymax = y.components[8], alpha = 0.1, fill = "darkorchid") +
    ggplot2::annotate("text", x = xmax + 1, y = y.means[4], label = "Slow2", colour = "darkorchid") +
    ggplot2::annotate("rect", xmin = 0.5, xmax = xmax + 1.5, #################
             ymin = y.components[9], ymax = y.components[10], alpha = 0.1, fill = "gold") +
    ggplot2::annotate("text", x = xmax + 1, y = y.means[5], label = "Slow3", colour = "gold") +
    ggplot2::geom_vline(mapping = ggplot2::aes(xintercept = xmax - 2.5), color = "black")
  }

  # Draw red rectangle which indicates the selected case
  if (!is.null(K.selected)) {
    p <- p + ggplot2::geom_rect(mapping = ggplot2::aes(xmin = K.selected - 0.5,
                                                       xmax = K.selected + 0.5,
                                     ymin = ymin, ymax = ymax),
                       colour = "red", size = 1, fill = NA)}


  # rotate diagram and delete unnecessary visual elements
  p <- p + ggplot2::coord_flip() +
    ggplot2::ylab(expression(italic(sigma) ~~ (cm^2))) +
    ggplot2::theme(panel.border = ggplot2::element_blank(),
          axis.title.y = ggplot2::element_blank(),
          axis.text = ggplot2::element_text(colour = "black", size = 10),
          axis.ticks.y = ggplot2::element_blank(),
          axis.ticks.x = ggplot2::element_blank(),
          panel.grid.major.y = ggplot2::element_blank())

  # save plot as file
  if (!is.null(filename)) {
    try(suppressMessages(ggplot2::ggsave(filename, plot = p, units = "cm")),
        silent = FALSE)}

  # show plot
  if (!hide.plot) gridExtra::grid.arrange(p, nrow = 1, top = title)

  # return plot object
  invisible(p)
}
