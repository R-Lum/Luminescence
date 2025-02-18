#' @title Calculate Moran's I
#'
#' @details
#'
#' **Case of no spatial autocorrelation**
#'
#' Perhaps a bit counter-intuitive, the expected value of Moran's I under the
#' null hypothesis of no spatial correlation is a value slightly smaller than
#' zero. When setting `spatial_autocorrelation = FALSE`, this function
#' calculates the expected value based on the number of observations or the
#' length of the observation vector (while taking out `NA` values). Note that
#' the expected value only depends on the number of observed separate grain
#' values. This can be useful for plotting.
#'
#' The expected Moran's I for the null hypothesis of no spatial correlation
#' corresponds to `-1 / (n - 1)`, with `n` being the number of non-missing
#' observations.
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
#' @param spatial_autocorrelation [logical] (*with default*): whether spatial
#' autocorrelation should be considered in the computation of Moran's I
#' (`TRUE` by default). If `FALSE`, the function computes Moran's I expected
#' value in case of no autocorrelation (H_0). See details for further
#' information.
#'
#' @param compute_pseudo_p [logical] (*with default*): whether a pseudo p-value
#' should be computed (`FALSE` by default).
#'
#' @param tested_moransI [numeric] (*with default*): The value of Moran's I to
#' be tested against when computing the pseudo p-value. If `NULL` (default), the
#' value calculated by the function will be used. Ignored if `compute_pseudo_p`
#' is `FALSE`.
#'
#' @param n_permutations [integer] (*with default*): number of random
#' permutations tested to calculate the fraction which is the pseudo p
#' (defaults to 999). Influences the calculation speed, which will have
#' impact in case of large scale simulation loops. Ignored if `compute_pseudo_p`
#' is `FALSE`.
#'
#' @param ignore_borders [logical] (*with default*): whether only grain
#' locations that do not lie on the border of the disc should be considered
#' (`FALSE` by default). Thus if `TRUE`, only the inner 8x8 grain locations
#' rather than the full 10x10 are considered. Ignored if `df_neighbours` is
#' not `NULL` or if `spatial_autocorrelation = FALSE`.
#'
#' @param return_intermediate_values [logical] (*with default*): whether the
#' function should return a list with several intermediate calculation results
#' (defaults to `FALSE`). Ignored if `spatial_autocorrelation` is `FALSE`.
#'
#' @return By default one numerical value, roughly between -1 and 1, where
#' close to zero means no spatial correlation, and value close to 1 a positive
#' spatial correlation given the pattern we interested in (by default all rook
#' neighbours). A value closer to -1 has no meaning within the context of
#' luminescence crosstalk. If `compute_pseudo_p = TRUE`, then the computed
#' pseudo p-value is returned. If `return_intermediate_values` is set to `TRUE`,
#' a list with several values used for calculation is returned instead of a
#' single outcome.
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
                         spatial_autocorrelation = TRUE,
                         compute_pseudo_p = FALSE,
                         tested_moransI = NULL,
                         n_permutations = 999,
                         ignore_borders = FALSE,
                         return_intermediate_values = FALSE
) {
  .set_function_name("calc_MoransI")
  on.exit(.unset_function_name(), add = TRUE)

  ## Validate input arguments; set variables  -----------------------

  .validate_class(object, c("RLum.Results", "numeric", "integer"))
  if (is.numeric(object)) {
    vn_values <- object
  } else {
    vn_values <- get_RLum(object)
  }
  .validate_length(vn_values, 100, name = "'object'")

  if (is.null(df_neighbours)) {
    .validate_logical_scalar(ignore_borders)
    df_neighbours <- .get_Neighbours(object, ignore_borders)
  } else {
    .validate_class(df_neighbours, "data.frame")
    if (ncol(df_neighbours) != 3)
      .throw_error("'df_neighbours' should be a data frame with 3 columns")
  }

  .validate_logical_scalar(spatial_autocorrelation)
  .validate_logical_scalar(compute_pseudo_p)
  if (!is.null(tested_moransI)) {
    .validate_class(tested_moransI, "numeric")
    .validate_length(tested_moransI, 1)
  }
  .validate_positive_scalar(n_permutations, int = TRUE)
  .validate_logical_scalar(return_intermediate_values)

  ## No spatial autocorrelation ---------------------------------------------

  if (!spatial_autocorrelation) {
    n.valid <- sum(!is.na(vn_values))
    if (n.valid < 2) {
      .throw_error("There should be at least 2 non-missing values")
    }
    n_MoransI_expt_no_cor <- -1 / (n.valid - 1)
    return(n_MoransI_expt_no_cor)
  }

  ## Core calculation -------------------------------------------------------

  if (nrow(df_neighbours) == 0) {
    .throw_warning("No bordering grain locations given in 'df_neighbours', ",
                   "returning NaN")
    return(NaN)
  }

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

  if (compute_pseudo_p) {
    if (is.null(tested_moransI))
      tested_moransI <- n_moransI
    pseudo_p <- .compute_pseudo_p(vn_values, tested_moransI,
                                  n_permutations, df_neighbours)
  }

  if (return_intermediate_values) {
    li_return <- list(n = n,
                      n_mean = n_mean,
                      n_population_variance = n_spatial_auto_correlation,
                      n_sum_similarities = n_sum_similarities,
                      n_sum_weights = n_sum_weights,
                      n_average_auto_correlation = n_average_auto_correlation,
                      n_moransI = n_moransI)
    if (compute_pseudo_p) {
      li_return$pseudo_p <- pseudo_p
    }

    return(li_return)
  }

  if (compute_pseudo_p) {
    return(pseudo_p$pseudo_p)
  }
  return(n_moransI)
}


#' @title Calculate Moran's I pseudo p
#'
#' @param values [numeric] (**required**): numerical vector of length 100
#' representing one discs ("position") in a reader.
#'
#' @param moransI [numeric] (**required**): The value of Moran's I to be
#' tested against.
#'
#' @param n_permutations [integer] (*with default*) Number of random
#' permutations tested to calculate the fraction which is the pseudo p.
#'
#' @param df_neighbours [data.frame] (**required**): Data frame indicating
#' which borders to consider, and their respective weights.
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
#' @keywords internal
#' @md
#' @noRd
.compute_pseudo_p <- function(values, moransI, n_permutations, df_neighbours) {

    ## Local helper function: reshuffle (=permutate) observations, an test if Moran's I
    ## from reshuffled observations if equal or above the calculated Moran's I
    reshuffle_and_test <- function(vn_values, n_moransI, df_neighbours) {
      vn_values_reshuffled <- rep(NA, length.out = length(vn_values))
      vn_values_reshuffled[vb_contains_observation] <- sample(vn_values[vb_contains_observation])
      (calc_MoransI(object = vn_values_reshuffled,
                    df_neighbours = df_neighbours) >= n_moransI)
    }
    vb_contains_observation <- !is.na(values)

    ## How many different Moran's Is from reshuffled observations are equal or
    ## above the provided Moran's I?
    n_test_pos <- sum(replicate(n = n_permutations,
                                expr = reshuffle_and_test(values,
                                                          moransI,
                                                          df_neighbours),
                                 simplify = TRUE)
    )

  n_pseudo_p <- (n_test_pos + 1) / (n_permutations + 1)

  if (n_test_pos == 0 && n_pseudo_p > 0)
    .throw_warning("Pseudo-p might be overestimated: the real p-value ",
                   "is closer to zero, try increasing 'n_permutations'")

  return(list(pseudo_p = n_pseudo_p,
              tested_moransI = moransI,
              n_permutations = n_permutations))
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
#' @param object [RLum.Results-class] or [numeric] (**required**): numerical
#' vector containing the values of one or more discs, or an `RLum.Results`
#' object containing such values. Can contain `NA` values.
#'
#' @param ignore_borders [logical] (*with default*) Defaults to `FALSE`.
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
.get_Neighbours <- function(object,
                            ignore_borders = FALSE
) {
  .set_function_name(".get_Neighbours")
  on.exit(.unset_function_name(), add = TRUE)

  ## Validate input arguments -----------------------

  .validate_class(object, c("RLum.Results", "numeric", "integer"))
  if (is.numeric(object)) {
      vn_values <- object
  } else{
      vn_values <- get_RLum(object)
  }
  vb_contains_observation <- !is.na(vn_values)

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

  df_neighbour <- stats::na.omit(df_neighbour)

  if (ignore_borders) {
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
  missing.idx <- which(!vb_contains_observation)
  if (length(missing.idx) > 0) {
    ## Create vector of indices of rows to throw away
    vi_observation_missing <- (df_neighbour$location  %in% missing.idx |
                               df_neighbour$neighbour %in% missing.idx)
    df_neighbour <- df_neighbour[!vi_observation_missing, ]
  }

  return(df_neighbour)
}
