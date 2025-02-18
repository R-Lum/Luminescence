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
#' @keywords manip
#'
#' @return A vector of size 100, with the value at each grain hole location including simulated crosstalk.
#'
#' @author Anna-Maartje de Boer, Luc Steinbuch, Wageningen University & Research, 2025
#'
#' @references
#' de Boer, A-M., Steinbuch, L., Heuvelink, G.B.M., Wallinga, J., 2025.
#' A novel tool to assess crosstalk in single-grain luminescence detection.
#' Submitted.
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
#' @md
#' @export
apply_Crosstalk <- function(object,
                            n_crosstalk = 0.2
) {
  .set_function_name("apply_Crosstalk")
  on.exit(.unset_function_name(), add = TRUE)

  ## Integrity checks -------------------------------------------------------

  .validate_class(object, c("RLum.Results", "numeric", "integer"))
  if (is.numeric(object)){
    vn_values <- object
  } else {
    vn_values <- get_RLum(object)
  }
  .validate_length(vn_values, 100, name = "'object'")

  .validate_class(n_crosstalk, c("numeric"))
  .validate_length(n_crosstalk, 1)

  vb_na_s <- is.na(vn_values)
  vn_values[vb_na_s] <- 0

  ## Prepare multiplication matrix -----------------------
  ## Note: the physical appearance of a measurement disc,
  ## 10x10 grains, is hard coded here
  df_disc_locations <- data.frame(location = 1:100,
                                  x = rep(1:10, times = 10),
                                  y = rep(1:10, each = 10))

  # Calculate matrix with euclidean distances
  mn_dist <- as.matrix(stats::dist(df_disc_locations[, c("x", "y")]))

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
