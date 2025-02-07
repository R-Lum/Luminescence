#' @title Calculate Moran's I
#'
#' @param object [RLum.Results-class] or [numeric] (**required**) containing
#' the values of the grains of one. Should have length 100; can contain `NA`
#' values.
#'
#' @param df_neighbours [data.frame] (*with default*): a data frame with 3
#' columns (`location`, `neighbour` and `weight`, respectively), with each row
#' indicating the index of one location and one of its neighbours (note that
#' the concept of "location" versus "neighbour" is symmetric, so each
#' neighbouring pair needs to be specified only once), alongside their relative
#' weight (generally set to 1). If `NULL` (default), this is constructed
#' automatically by the internal function `.get_Neighbours`.
#'
#' @param return_intermediate_values [logical] (*with default*): whether the
#' function should return a list with several intermediate calculation results
#' (defaults to `FALSE`).
#'
#' @return By default one numerical value, roughly between -1 and 1, where
#' close to zero means no spatial correlation, and value close to 1 a positive
#' spatial correlation given the pattern we interested in (by default all rook neighbours).
#' A value closer to -1 has no meaning within the context of luminescence crosstalk.
#' If `return_intermediate_values` is set to `TRUE`, a list with several
#' values used for calculation is returned instead of the single outcome.
#'
#' @author Anna-Maartje de Boer, Luc Steinbuch, Wageningen University & Research, 2025
#'
#' @references
#' de Boer, A-M., Steinbuch, L., Heuvelink, G.B.M., Wallinga, J., 2025.
#' A novel tool to assess crosstalk in single-grain luminescence detection.
#' Submitted.
#'
#' @examples
#'
#' ## Test a fictional sample with spatial correlation
#' calc_MoransI(object = c(1:100))
#'
#' ## Test some fictional samples without spatial correlation;
#' ## note the randomness with each repetition
#' calc_MoransI(object = rnorm(n = 100))
#' calc_MoransI(object = rnorm(n = 100))
#' calc_MoransI(object = rnorm(n = 100))
#'
#' @md
#' @export
calc_MoransI <- function(object,
                         df_neighbours = NULL,
                         return_intermediate_values = FALSE
) {
  .set_function_name("calc_MoransI")
  on.exit(.unset_function_name(), add = TRUE)

  ## Validate input arguments; set variables  -----------------------

  .validate_class(object, c("RLum.Results", "numeric", "integer"))
  ## To add
  #  - should contain a numerical vector of length 100
  if(is.numeric(object))
  {
    vn_values <- object
  } else
  {
    vn_values <- get_RLum(object)
  }

  if (is.null(df_neighbours)) {
    df_neighbours <- .get_Neighbours(object)
  } else {
    .validate_class(df_neighbours, "data.frame")
  ## To add
  #  - should be a valid df_neighbour dataframe
  }
  .validate_class(return_intermediate_values, "logical")
  ## To add
  #  - should be a single value


  if (nrow(df_neighbours) == 0) {
    .throw_warning("No bordering grain locations given in 'df_neighbours', ",
                   "returning NaN")
    return(NaN)
  }

  ## Core calculation  -----------------------
  n <- sum(!is.na(vn_values))
  n_mean <- mean(vn_values, na.rm = TRUE)
  n_spatial_auto_correlation <- sum((vn_values-n_mean)^2, na.rm = TRUE)/n # Population variance
  n_sum_similarities <-
    sum((vn_values[df_neighbours$location] - n_mean) *
        (vn_values[df_neighbours$neighbour] - n_mean) *
         df_neighbours$weight,
         na.rm = TRUE
    )
  n_sum_weights <- sum(df_neighbours$weight)

  n_average_auto_correlation <- n_sum_similarities/n_sum_weights

  n_moransI <- n_average_auto_correlation / n_spatial_auto_correlation

  if (return_intermediate_values) {
    li_return <- list(n = n,
                      n_mean = n_mean,
                      n_population_variance = n_spatial_auto_correlation,
                      n_sum_similarities = n_sum_similarities,
                      n_sum_weights = n_sum_weights,
                      n_average_auto_correlation = n_average_auto_correlation,
                      n_moransI = n_moransI
    )

    return(li_return)
  }

  return(n_moransI)
}


#' @title Calculate Moran's I expected value in case of no auto correlation (H_0)
#'
#' @description Perhaps a bit counter-intuitive, the expected value of Moran's I under
#' the null hypothesis of no spatial correlation is a value slightly smaller than zero. This function
#' calculates the expected value based on the number of observations or the length of the
#' observation vector (while taking out `NA` values). This can be useful for plotting.
#' Defaults, when no arguments are provided, to the expected value of one observation disc with
#' 100 valid observations.
#'
#' @details Note that the expected value only depends on the number of observed separate grain values.
#'
#' @param object [RLum.Results-class] or [numeric] (*with default*) containing
#' the values of the grains of one or more discs. Should have length 100;
#' can contain `NA` values.
#'
#' @param n [integer] (*with default*) The number of grain observations as
#' one integer. Defaults to `NULL`. If provided, overrules parameter `object`.
#' One full disc corresponds to `n = 100`.
#'
#' @return Numerical value, a bit smaller than 0: the expected Moran's I for
#' the null hypothesis (= no spatial correlation), according to `-1/(n-1)`
#' with `n` being the number of used observations.
#'
#' @author Anna-Maartje de Boer, Luc Steinbuch, Wageningen University & Research, 2025
#'
#' @references
#' de Boer, A-M., Steinbuch, L., Heuvelink, G.B.M., Wallinga, J., 2025.
#' A novel tool to assess crosstalk in single-grain luminescence detection.
#' Submitted.
#'
#' @examples
#' calc_MoransI_expt_no_cor()
#' calc_MoransI_expt_no_cor(n = 20)
#'
#' @md
#' @export
calc_MoransI_expt_no_cor <- function(object = rep(1, times = 100),
                                     n = NULL
) {
  .set_function_name("calc_MoransI")
  on.exit(.unset_function_name(), add = TRUE)

  ## Validate input arguments; set variables  -----------------------

  if (!is.null(n))
  {
    .validate_positive_scalar(n, int = TRUE)
    n <- as.integer(n)

  } else
  {
    .validate_class(object, c("RLum.Results", "numeric", "integer"))
    ## To add:
    #  - should contain a numerical vector of length 100


    if(is.numeric(object))
    {
      vn_values <- object
    } else
    {
      vn_values <- get_RLum(object)
    }

    n <- sum(!is.na(vn_values))
  }

  ## Check if valid input
  stopifnot(n > 1)


  n_MoransI_expt_no_cor <- -1/(n-1)

  return(n_MoransI_expt_no_cor)
}


#' @title Calculate Moran's I pseudo p
#'
#' @param object [RLum.Results-class] or [numeric] (**required**): containing a numerical vector of length 100,
#' representing one discs ("position") in a reader.
#'
#' @param n_moransI [numeric] (*with default*) The value of Moran's I to be tested against.
#' Defaults to `calc_MoransI(object)`.
#'
#' @param n_perm [integer] (*with default*) Number of random permutations tested to calculate the fraction
#' which is the pseudo p. Defaults to 999. Influences the calculation speed, which will have impact in case
#' of large scale simulation loops.
#'
#' @param df_neighbours [data.frame] (*with default*) Defaults to
#' `get_Neighbour(object)`.
#'
#' @param suppress_warnings [logical] (*with default*): whether warnings should
#' be suppressed (defaults to `FALSE`).
#'
#' @return Pseudo p-value
#'
#' @seealso https://geodacenter.github.io/workbook/5a_global_auto/lab5a.html#permutation-inference
#'
#' @author Anna-Maartje de Boer, Luc Steinbuch, Wageningen University & Research, 2025
#'
#' @references
#' de Boer, A-M., Steinbuch, L., Heuvelink, G.B.M., Wallinga, J., 2025.
#' A novel tool to assess crosstalk in single-grain luminescence detection.
#' Submitted.
#'
#' @md
#' @export
calc_MoransI_pseudo_p <- function(object,
                                  n_moransI = calc_MoransI(object),
                                  n_perm = 999,
                                  df_neighbours = NULL,
                                  suppress_warnings = FALSE
) {
  .set_function_name("calc_MoransI")
  on.exit(.unset_function_name(), add = TRUE)

  .validate_class(object, c("RLum.Results", "numeric", "integer"))
  ## To add: validation on `object`
  #  - should contain a numerical vector of length 100

  if(is.numeric(object))
  {
    vn_values <- object
  } else
  {
    vn_values <- get_RLum(object)
  }

  vb_contains_observation <-  !is.na(vn_values)

  .validate_positive_scalar(n_perm, int = TRUE)

  if (is.null(df_neighbours)) {
    df_neighbours <- .get_Neighbours(vn_values)
  } else {
    .validate_class(df_neighbours, "data.frame")
  }
  if (nrow(df_neighbours) == 0) {
    .throw_warning("No bordering grain locations given in 'df_neighbours', ",
                   "returning NaN")
    return(NaN)
  } else
  {

    ## Local helper function: reshuffle (=permutate) observations, an test if Moran's I
    ## from reshuffled observations if equal or above the calculated Moran's I
    reshuffle_and_test <- function(vn_values, n_moransI, df_neighbours) {
      vn_values_reshuffled <- rep(NA, length.out = length(vn_values))
      vn_values_reshuffled[vb_contains_observation] <- sample(vn_values[vb_contains_observation])
      (calc_MoransI(object = vn_values_reshuffled,
                    df_neighbours = df_neighbours) >= n_moransI)
    }

    ## How many different Moran's Is from reshuffled observations are equal or
    ## above the provided Moran's I?
    n_test_pos <- sum( replicate(n = n_perm,
                                 expr = reshuffle_and_test(vn_values,
                                                           n_moransI,
                                                           df_neighbours),
                                 simplify = TRUE)
    )

    n_pseudo_p <- (n_test_pos+1-1)/(n_perm+1)

    if (n_test_pos == 0 && !suppress_warnings && n_pseudo_p > 0)
      .throw_warning("Pseudo-p might be overestimation; real p-value closer to zero. Perhaps increase n_perm")

    return(n_pseudo_p)
  } ## END nrow(df_neighbour) > 0
}


#' @title Get neighbour positions
#'
#' @description  Determines which locations on one or more
#' 10x10 discs are neighbouring horizontally and/or vertically ("rook"), in
#' order to calculate Moran's I.
#' For the general case, we assume symmetry, and all adjacency get
#' an equal relative weight of one.
#' In case there are holes in the image, indicated by `NA` (no value observed),
#' the related border or borders are removed.
#'
#' @details Will mostly be used as an helper function. If called without any
#' arguments, it will return the neighbouring positions data frame for one
#' disc (POS) with on every grain location an observation.
#'
#' @param object [RLum.Results-class] or [numeric] (*with default*): numerical
#' vector containing the values of one or more discs, or an `RLum.Results`
#' object containing such values. Can contain `NA` values.
#' If `NULL` (default), a complete 10x10 disc is assumed.
#'
#' @param restrict_to_8x8 [logical] (*with default*) Defaults to `FALSE`.
#' If `TRUE`, only on-disc borders to grain locations who do not border the
#' disc are considered; thus only the inner 8x8 grain locations rather than
#' the full 10x10.
#'
#' @return Data frame with columns `location`, `neighbour` and `weight`,
#' respectively. The integers in the first two columns indicate the index
#' of the vector with observations; the relative weight is standard set to one.
#' Note that the concept of "location" versus "neighbour" is symmetric.
#'
#' @author Anna-Maartje de Boer, Luc Steinbuch, Wageningen University & Research, 2025
#'
#' @references
#' de Boer, A-M., Steinbuch, L., Heuvelink, G.B.M., Wallinga, J., 2025.
#' A novel tool to assess crosstalk in single-grain luminescence detection.
#' Submitted.
#'
#' @keywords internal
#'
#' @noRd
.get_Neighbours <- function(object = NULL,
                            restrict_to_8x8 = FALSE
) {
  .set_function_name(".get_Neighbours")
  on.exit(.unset_function_name(), add = TRUE)

  ## Validate input arguments -----------------------

  if (!is.null(object))
    .validate_class(object, c("RLum.Results", "numeric", "integer"))
  ## To add

  if(!is.null(object))
  {
    if(is.numeric(object))
    {
      vn_values <- object
    } else
    {
      vn_values <- get_RLum(object)
    }
    vb_contains_observation <-  !is.na(vn_values)

  } else
    vb_contains_observation <- rep(TRUE, times = 100)


  .validate_class(restrict_to_8x8, "logical")
  ## To add:
  ## - contains one element


  ## Construct the dataframe with borders and weights ---------------------------------

  df_neighbour <- data.frame(location = 1:100)

  ## for each neighbour, calculate two neighbouring locations,
  ## and remove when invalid

  df_neighbour$south <- df_neighbour$location - 10
  df_neighbour$west <- df_neighbour$location - 1

  ## Remove neighbouring locations at southside of disc
  df_neighbour$south <-
    ifelse(df_neighbour$south < 1,
           yes = NA,
           no = df_neighbour$south)

  ## Remove neighbouring locations at westside of disc
  df_neighbour$west <-
    ifelse(!(df_neighbour$west %% 10),
           yes = NA,
           no = df_neighbour$west)

  df_neighbour <- rbind(df_neighbour[,1:2],
                        setNames(df_neighbour[,c(1,3)],
                                 nm = names(df_neighbour)[1:2])
  )

  names(df_neighbour) <- c("location", "neighbour")

  df_neighbour <- na.omit(df_neighbour)

  if (restrict_to_8x8) {
    ## Remove the disc borders
    vn_disc_border_locations <- c(1:10,
                                  91:100,
                                  seq(from = 11, to = 81, by = 10),
                                  seq(from = 20, to = 90, by = 10)
    )

    contains_discborders <- df_neighbour$location %in% vn_disc_border_locations |
      df_neighbour$neighbour %in% vn_disc_border_locations

    df_neighbour <- df_neighbour[!contains_discborders, ]
  }

  ## Define relative weight factor ---------------------------------
  df_neighbour$weight <- 1

  ## If there are missing observations (NA's), we need to take those out ------------------
  if(any(!vb_contains_observation))
  {

    ## Create vector of indices of rows to throw away
    vi_observation_missing <- unique(
      c(which(df_neighbour$location %in%  which(!vb_contains_observation)),
        which(df_neighbour$neighbour %in% which(!vb_contains_observation))
      )
    )
    df_neighbour <- df_neighbour[-vi_observation_missing,]
  }

  return(df_neighbour)
}
