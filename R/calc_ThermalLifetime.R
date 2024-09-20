#' Calculates the Thermal Lifetime using the Arrhenius equation
#'
#' The function calculates the thermal lifetime of charges for given E (in eV), s (in 1/s) and
#' T (in deg. C.) parameters. The function can be used in two operational modes:
#'
#' **Mode 1 `(profiling = FALSE)`**
#'
#' An arbitrary set of input parameters (E, s, T) can be provided and the
#' function calculates the thermal lifetimes using the Arrhenius equation for
#' all possible combinations of these input parameters. An array with 3-dimensions
#' is returned that can be used for further analyses or graphical output (see example 1)
#'
#' **Mode 2 `(profiling = TRUE)`**
#'
#' This mode tries to profile the variation of the thermal lifetime for a chosen
#' temperature by accounting for the provided E and s parameters and their corresponding
#' standard errors, e.g., `E = c(1.600, 0.001)`
#' The calculation based on a Monte Carlo simulation, where values are sampled from a normal
#' distribution (for E and s).
#'
#' **Used equation (Arrhenius equation)**
#'
#' \deqn{\tau = 1/s exp(E/kT)}
#' where:
#' \eqn{\tau} in s as the mean time an electron spends in the trap for a given \eqn{T},
#' \eqn{E} trap depth in eV,
#' \eqn{s} the frequency factor in 1/s,
#' \eqn{T} the temperature in K and \eqn{k} the Boltzmann constant in eV/K (cf. Furetta, 2010).
#'
#'
#' @param E [numeric] (**required**):
#' vector of trap depths in eV,
#' if `profiling = TRUE` only the first two elements are considered
#'
#' @param s [numeric] (**required**):
#' vector of frequency factor in 1/s,
#' if `profiling = TRUE` only the first two elements are considered
#'
#' @param T [numeric] (*with default*):
#' temperature in deg. C for which the lifetime(s) will be calculated.
#' A vector can be provided.
#'
#' @param output_unit [character] (*with default*):
#' output unit of the calculated lifetimes, accepted
#' entries are: `"Ma"`, `"ka"`, `"a"`, `"d"`, `"h"`, `"min"`, `"s"`
#'
#' @param profiling [logical] (*with default*):
#' this option allows to estimate uncertainties based on
#' given E and s parameters and their corresponding standard error
#' (cf. details and examples section)
#'
#' @param profiling_config [list] (*optional*):
#' allows to set configuration parameters used for the profiling
#' (and only have an effect here). Supported parameters are:
#'
#' - `n` (number of MC runs),
#' - `E.distribution` (distribution used for the re-sampling for E) and
#' - `s.distribution` (distribution used for the re-sampling for s).
#'
#' Currently only the normal distribution is supported
#' (e.g., `profiling_config = list(E.distribution = "norm")`
#'
#' @param verbose [logical]:
#' enables/disables verbose mode
#'
#' @param plot [logical]:
#' enables/disables output plot, currently only in combination with `profiling = TRUE`.
#'
#' @param ... further arguments that can be passed in combination with the plot output.
#' Standard plot parameters are supported ([plot.default])
#'
#' @return
#' A [RLum.Results-class] object is returned a along with a plot (for
#' `profiling = TRUE`). The output object contain the following slots:
#'
#' **`@data`**
#'
#' \tabular{lll}{
#'  **Object** \tab **Type** \tab **Description** \cr
#'  `lifetimes` \tab [array] or [numeric] \tab calculated lifetimes \cr
#'  `profiling_matrix` \tab [matrix] \tab profiling matrix used for the MC runs
#' }
#'
#' **`@info`**
#'
#' \tabular{lll}{
#' **Object** \tab **Type** \tab **Description** \cr
#' `call` \tab `call` \tab the original function call
#' }
#'
#' @note
#' The profiling is currently based on re-sampling from a normal distribution, this
#' distribution assumption might be, however, not valid for given E and s parameters.
#'
#' @section Function version: 0.1.0
#'
#' @author
#' Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#'
#' @seealso [graphics::matplot], [stats::rnorm][stats::Normal], [get_RLum]
#'
#' @references
#'
#' Furetta, C., 2010. Handbook of Thermoluminescence, Second Edition. World Scientific.
#'
#' @keywords datagen
#'
#' @examples
#'
#' ##EXAMPLE 1
#' ##calculation for two trap-depths with similar frequency factor for different temperatures
#' E <- c(1.66, 1.70)
#' s <- 1e+13
#' T <- 10:20
#' temp <- calc_ThermalLifetime(
#'   E = E,
#'   s = s,
#'   T = T,
#'   output_unit = "Ma"
#' )
#' contour(x = E, y = T, z = temp$lifetimes[1,,],
#'         ylab = "Temperature [\u00B0C]",
#'         xlab = "Trap depth [eV]",
#'         main = "Thermal Lifetime Contour Plot"
#' )
#' mtext(side = 3, "(values quoted in Ma)")
#'
#' ##EXAMPLE 2
#' ##profiling of thermal life time for E and s and their standard error
#' E <- c(1.600, 0.003)
#' s <- c(1e+13,1e+011)
#' T <- 20
#' calc_ThermalLifetime(
#'   E = E,
#'   s = s,
#'   T = T,
#'   profiling = TRUE,
#'   output_unit = "Ma"
#')
#'
#' @md
#' @export
calc_ThermalLifetime <- function(
  E,
  s,
  T = 20,
  output_unit = "Ma",
  profiling = FALSE,
  profiling_config = NULL,
  verbose = TRUE,
  plot = TRUE,
  ...
) {
  .set_function_name("calc_ThermalLifetime")
  on.exit(.unset_function_name(), add = TRUE)

# Integrity -----------------------------------------------------------------------------------

  if (missing(E) || missing(s)) {
    .throw_error("'E' or 's' or both are missing, but required.")
  }


# Set variables -------------------------------------------------------------------------------

  ##Boltzmann constant
  k <- 8.6173324e-05 #eV/K

  ##recalculate temparature
  T.K <- T + 273.15 #K


  ##SETTINGS FOR PROFILING
  ##profiling settings
  profiling_settings <- list(
    n = 1000,
    E.distribution = "norm",
    s.distribution = "norm"

  )

  ##replace if set
  if(!is.null(profiling_config)){
    profiling_settings <- modifyList(profiling_settings, profiling_config)

  }

  ##check for odd input values
  if (profiling_settings$n < 1000){
    profiling_settings$n <- 1000
    .throw_warning("Minimum MC runs are 1000, parameter 'n' ",
                   "in profiling_config reset to 1000.")
  }

# Calculation ---------------------------------------------------------------------------------

 ##set function for the calculation
 f <- function(E, s, T.K) {
    1 / s * exp(E / (k * T.K))
 }

 ##PROFILING
  if(profiling) {
    ##set profiling matrix
    profiling_matrix <-
      matrix(NA, ncol = 4, nrow = profiling_settings$n)

    ##fill matrix

    ##E
    profiling_matrix[, 1] <-
      if( profiling_settings$E.distribution == "norm"){
        rnorm(profiling_settings$n, mean = E[1], sd = E[2])

      }else{
        .throw_error("Unknown distribution setting for E profiling")
      }


    ##s
    profiling_matrix[, 2] <-
      if (profiling_settings$s.distribution == "norm") {
        rnorm(profiling_settings$n, mean = s[1], sd = s[2])

      } else{
        .throw_error("Unknown distribution setting for s profiling")
      }

    ##T
    profiling_matrix[, 3] <-
      rep(T.K[1], each = profiling_settings$n)


    ##calulate lifetimes
    profiling_matrix[, 4] <-
      f(profiling_matrix[, 1], profiling_matrix[, 2], profiling_matrix[, 3])

    ##reduce E and s vector on the first entry
    T <- T[1]

    ##set lifetimes
    lifetimes <- profiling_matrix[, 4]

  } else{

    ##set empty profiling matrix
    profiling_matrix <- matrix()

    ##calculate lifetimes
    lifetimes <- vapply(
      X = T.K,
      FUN = function(i) {
        vapply(
          X = E,
          FUN = function(j) {
            f(E = j, s = s, T.K = i)

          },
          FUN.VALUE = vector(mode = "numeric", length = length(s))
        )

      },
      FUN.VALUE = matrix(numeric(), ncol = length(E), nrow = length(s))
    )



    ##transform to an arry in either case to have the same output
    if (!is(lifetimes, "array")) {
      lifetimes <-
        array(lifetimes, dim = c(length(s), length(E), length(T)))

    }

    ##set dimnames to make reading more clear
    dimnames(lifetimes) <- list(s, E, paste0("T = ", T, " \u00B0C"))

  }

 ##re-calculate lifetimes accourding to the chosen output unit
 temp.lifetimes <- switch (
    output_unit,
    "s" = lifetimes,
    "min" = lifetimes / 60,
    "h" = lifetimes / 60 / 60,
    "d" = lifetimes / 60 / 60 / 24,
    "a" = lifetimes / 60 / 60 / 24 / 365,
    "ka" = lifetimes / 60 / 60 / 24 / 365 / 1000,
    "Ma" = lifetimes / 60 / 60 / 24 / 365 / 1000 / 1000
  )

  ##check for invalid values
  if(is.null(temp.lifetimes)){
    output_unit <- "s"
    .throw_warning("'output_unit' unknown, reset to 's'")
  }else{
    lifetimes <- temp.lifetimes
    rm(temp.lifetimes)

  }


  # Terminal output -----------------------------------------------------------------------------

  if(verbose){
    cat("\n[calc_ThermalLifetime()]\n\n")

    if(profiling){


    cat("\tprofiling = TRUE")
    cat("\n\t--------------------------\n")
    }
    cat(paste("\tmean:\t", format(mean(lifetimes), scientific = TRUE), output_unit))
    cat(paste("\n\tsd:\t", format(sd(lifetimes), scientific = TRUE), output_unit))
    cat(paste("\n\tmin:\t", format(min(lifetimes), scientific = TRUE), output_unit))

    if(!profiling){
      cat(paste0(" (@",T[which(lifetimes == min(lifetimes), arr.ind = TRUE)[3]], " \u00B0C)"))
    }

    cat(paste("\n\tmax:\t", format(max(lifetimes), scientific = TRUE), output_unit))

    if(!profiling){
      cat(paste0(" (@",T[which(lifetimes == max(lifetimes), arr.ind = TRUE)[3]], " \u00B0C)"))
    }

    cat("\n\t--------------------------")
    cat(paste0("\n\t(", length(lifetimes), " lifetimes calculated in total)"))

  }


  # Plotting ------------------------------------------------------------------------------------
  if(plot & profiling){

    ##plot settings
    plot.settings <- list(
      main = "Thermal Lifetime Density Plot",
      xlab = paste0("Thermal lifetime [",output_unit,"]"),
      ylab = "Density",
      xlim = NULL,
      ylim = NULL,
      log = "",
      lwd = 1,
      lty = 1,
      col = rgb(0, 0, 0, 0.25)
    )

    ##modify on request
    plot.settings <-   modifyList(plot.settings, list(...))

    ##split data and calculate density
    ##set seq
    id_seq <- seq(
      from = 1,
      to = length(lifetimes),
      length.out = 200)

    ##calculate lifetime of the density
    lifetimes_density <-
      lapply(1:(length(id_seq) - 1),
             function(x) {
               density(lifetimes[id_seq[x]:id_seq[x+1]])

             })

    ##get x values
    lifetimes_density.x <- matrix(unlist(lapply(1:length(lifetimes_density), function(i){
      lifetimes_density[[i]]$x


    })), nrow = length(lifetimes_density[[1]]$x))

    ##get y values
    lifetimes_density.y <- matrix(unlist(lapply(1:length(lifetimes_density), function(i){
      lifetimes_density[[i]]$y


    })), nrow = length(lifetimes_density[[1]]$y))


    ##plot density curves
    graphics::matplot(
      lifetimes_density.x,
      lifetimes_density.y,
      type = "l",
      lwd = plot.settings$lwd,
      lty = plot.settings$lty,
      col = plot.settings$col,
      main = plot.settings$main,
      xlab = plot.settings$xlab,
      ylab = plot.settings$ylab,
      xlim = plot.settings$xlim,
      ylim = plot.settings$ylim,
      log = plot.settings$log
    )

  }

  # Return values -------------------------------------------------------------------------------
 return(set_RLum(
   class = "RLum.Results",
   data = list(lifetimes = lifetimes,
               profiling_matrix = profiling_matrix),
   info = list(call = sys.call())
 ))

}
