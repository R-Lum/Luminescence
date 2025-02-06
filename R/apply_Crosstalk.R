## Based on research paper "A novel tool to assess crosstalk in single-grain luminescence detection",
##  de Boer et al., 2025

#' @title Apply crosstalk
#'
#' @description Add crosstalk, evenly spread in rook (top-right-bottom-left)
#' directions, to all grain whole locations on one measurement discs
#' (=on position on a measurement wheel in a reader). An
#' added crosstalk value of as example 0.2 means that 0.2 of the value of
#' the central grain is added to each grain in the rook directions. This is an additive
#' action: the central grain itself is not affected by this operation (but will on its
#' turn increase because of crosstalk from its neighbours).
#' This function is used for simulations: can this added crosstalk be detected?
#'
#' @details If an element in `object` is `NA`, it is internally set to 0, so it will not
#' be added.
#'
#' @param object [RLum.Results-class] or [numeric] (**required**): containing
#' a numerical vector of length 100, representing one or more measurement
#' discs ("positions") in a reader.
#' Each element in the vector represents one grain hole location on a disc.
#'
#' @param n_crosstalk [numeric] (*with default*): A single number quantifying the added
#' crosstalk. Defaults for testing purposes to 0.2. Can be any number, even negative,
#' but for realistic simulations we suggest something between 0 and 0.25.
#'
#' @keywords crosstalk, simulation
#'
#' @return A vector of size 100, with the value at each grain hole location including simulated crosstalk.
#'
#'
#' @author Anna-Maartje de Boer, Luc Steinbuch, Wageningen University & Research, 2025
#'
#' @examples
#' ## Create artificial disc observation
#' observations <- set_RLum(class = "RLum.Results",
#'                          data = list(vn_values = rep(x = c(1,2), each = 50))
#'                        )
#' hist(get_RLum(object = observations))
#'
#' ## Add crosstalk (with default set to 0.2), and visualize the difference
#' ## in the resulting histogram.
#' observations_with_simulated_crosstalk <- apply_Crosstalk(observations)
#' hist(observations_with_simulated_crosstalk)
#'
#'
#' @md
#' @export
apply_Crosstalk <- function(object,
                            n_crosstalk = 0.2
) {
  .set_function_name("apply_Crosstalk")
  on.exit(.unset_function_name(), add = TRUE)

  ## Validate input arguments -----------------------

  .validate_class(object, c("RLum.Results", "numeric", "integer"))
  ## To add: validation on `object`
  #  - should contain a numerical vector of length 100

  .validate_class(n_crosstalk, c("numeric"))
  ## To add: validate on `n_crosstalk`
  #  - should be a single numerical value

  ## Set variables  -----------------------
  if(is.numeric(object))
  {
    vn_values <- object
  } else
  {
    vn_values <- get_RLum(object)
  }


  vb_na_s <- is.na(vn_values)

  vn_values[vb_na_s] <- 0

  ## Prepare multiplication matrix -----------------------
  ## Note: the physical appearance of a measurement disc,
  ## 10x10 grains, is hard coded here
  df_disc_locations <- data.frame(location = 1:100,
                                  x = rep(1:10, times = 10),
                                  y = rep(1:10, each = 10))

  # Calculate matrix with euclidean distances
  mn_dist <- as.matrix(dist(df_disc_locations[,c("x", "y")]))

  ## All distances equal to one are subject to crosstalk
  mn_crosstalk <- ifelse(mn_dist == 1, yes = n_crosstalk, no = 0)

  ## The diagonal -- all distances equal to zero -- are one
  diag(mn_crosstalk) <- 1

  ## Matrix calculations  -----------------------

  vn_sig_with_crosstalk <- as.numeric(mn_crosstalk %*% vn_values)

  ## Assign original NA's to output
  vn_sig_with_crosstalk[vb_na_s] <- NA


  return(vn_sig_with_crosstalk)

}

#' @title Calculate Moran's I
#'
#' @param object [RLum.Results-class] or [numeric] (**required**) containing
#' the values of the grains of one. Should have length 100; can contain `NA`
#' values.
#'
#' @param df_neighbour [data.frame] (*with default*), indicating which
#' neighbours to consider, and their respective weights. Defaults to
#' `get_Neighbour(object)`.
#'
#' @param bo_return_inbetween_numbers [logical] (*with default*). Defaults
#' to `FALSE`. If `TRUE`, the function returns a list with several
#' intermediate calculation results.
#'
#' @return By default one numerical value, roughly between -1 and 1, where
#' close to zero means no spatial correlation, and value close to 1 a positive
#' spatial correlation given the pattern we interested in (by default all rook neighbours).
#' A value closer to -1 has no meaning within the context of luminescence crosstalk.
#' If `bo_return_inbetween_numbers` is set to `TRUE`, a list with several numbers
#' used for calculation is returned instead of the single outcome.
#'
#' @author Anna-Maartje de Boer, Luc Steinbuch, Wageningen University & Research, 2025
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
                         df_neighbour  =  get_Neighbour(object = object),
                         bo_return_inbetween_numbers = FALSE
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

  .validate_class(df_neighbour, "data.frame")
  ## To add
  #  - should be a valid df_neighbour dataframe

  .validate_class(bo_return_inbetween_numbers, "logical")
  ## To add
  #  - should be a single value


  if(nrow(df_neighbour)==0)
  {
    .throw_warning("There seems to be no bordering grain locations, returning NaN")
    return(NaN)
  }

  ## Core calculation  -----------------------
  n <- sum(!is.na(vn_values))
  n_mean <- mean(vn_values, na.rm = TRUE)
  n_spatial_auto_correlation <- sum((vn_values-n_mean)^2, na.rm = TRUE)/n # Population variance
  n_sum_similarities <-
    sum( (vn_values[df_neighbour$location] - n_mean) *
           (vn_values[df_neighbour$neighbour] - n_mean) *
           df_neighbour$weight,
         na.rm = TRUE
    )
  n_sum_weights <- sum(df_neighbour$weight)

  n_average_auto_correlation <- n_sum_similarities/n_sum_weights

  n_moransI <- n_average_auto_correlation / n_spatial_auto_correlation

  if(bo_return_inbetween_numbers)
  {
    li_return <- list(n = n,
                      n_mean = n_mean,
                      n_population_variance = n_spatial_auto_correlation,
                      n_sum_similarities = n_sum_similarities,
                      n_sum_weights = n_sum_weights,
                      n_average_auto_correlation = n_average_auto_correlation,
                      n_moransI = n_moransI
    )

    return(li_return)

  } else
  {
    return(n_moransI)
  }
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
#' @param df_neighbour [data.frame] (*with default*) Defaults to
#' `get_Neighbour(object)`.
#'
#' @param bo_suppress_warnings [logical] (*with default*) Should warnings be printed or suppressed?
#' Defaults to `FALSE`: warnings will be printed.
#'
#'
#' @return Pseudo p-value
#'
#' @seealso https://geodacenter.github.io/workbook/5a_global_auto/lab5a.html#permutation-inference
#'
#' @author Anna-Maartje de Boer, Luc Steinbuch, Wageningen University & Research, 2025
#'
#' @md
#' @export
calc_MoransI_pseudo_p <- function(object,
                                  n_moransI = calc_MoransI(object),
                                  n_perm = 999,
                                  df_neighbour = get_Neighbour(object = vn_values),
                                  bo_suppress_warnings = FALSE
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
  .validate_class(df_neighbour, "data.frame")

  if(nrow(df_neighbour) == 0)
  {
    .throw_warning("There seems to be no bordering grain locations, returning NaN")
    return(NaN)

  } else
  {

    ## Local helper function: reshuffle (=permutate) observations, an test if Moran's I
    ## from reshuffled observations if equal or above the calculated Moran's I
    reshuffle_and_test <- function(vn_values, n_moransI, df_neighbour){
      vn_values_reshuffled <- rep(NA, length.out = length(vn_values))
      vn_values_reshuffled[vb_contains_observation] <- sample(vn_values[vb_contains_observation])
      (calc_MoransI(object = vn_values_reshuffled,
                    df_neighbour = df_neighbour) >= n_moransI)
    }

    ## How many different Moran's Is from reshuffled observations are equal or
    ## above the provided Moran's I?
    n_test_pos <- sum( replicate(n = n_perm,
                                 expr = reshuffle_and_test(vn_values,
                                                           n_moransI,
                                                           df_neighbour),
                                 simplify = TRUE)
    )

    n_pseudo_p <- (n_test_pos+1-1)/(n_perm+1)

    if (n_test_pos == 0 && !bo_suppress_warnings && n_pseudo_p > 0)
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
#' @details Will mostly be used as an helper function. If called without any arguments,
#'  will return the neighbouring positions dataframe for one disc (POS) with on every
#'  grain location an observation.
#'
#' @param object [RLum.Results-class] or [numeric] (*with default*): numerical
#' vector containing the values of one or more discs, or an `RLum.Results`
#' object containing such values. Can contain `NA` values.
#' If `NULL` (default), a complete 10x10 disc is assumed.
#'
#' @param bo_restrict_to_8x8 [logical] (*with default*) Defaults to `FALSE`.
#' If `TRUE`, only on-disc borders to grain locations who do not border the
#' disc are considered; thus only the inner 8x8 grain locations rather than
#' the full 10x10.
#'
#' @return Dataframe with columns `location`, `neighbour` and `weight`, respectively.
#' The integers in the first two columns indicate the index of the vector with observations; the
#' relative weight is standard set to one. Note that the concept of "location" versus
#' "neighbour" is symmetric.
#'
#' @author Anna-Maartje de Boer, Luc Steinbuch, Wageningen University & Research, 2025
#'
#' @keywords crosstalk
#'
#' @examples
#' head(get_Neighbour())
#'
#'
#' @md
#' @export
get_Neighbour <- function(object = NULL,
                          bo_restrict_to_8x8 = FALSE
) {
  .set_function_name("calc_MoransI")
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


  .validate_class(bo_restrict_to_8x8, "logical")
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

  if(bo_restrict_to_8x8) ## Remove the disc borders
  {

    vn_disc_border_locations <- c(1:10,
                                  91:100,
                                  seq(from = 11, to = 81, by = 10),
                                  seq(from = 20, to = 90, by = 10)
    )

    bo_contains_discborders <- df_neighbour$location %in% vn_disc_border_locations |
      df_neighbour$neighbour %in% vn_disc_border_locations


    df_neighbour <- df_neighbour[!bo_contains_discborders, ]
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
