# INTERNAL HELPER FUNCTIONS -----------------------------------------------
#+++++++++++++++++++++
#+ .set_pid()        +
#+++++++++++++++++++++
#' Set unique id of the RLum.Analysis object as parent id for each RLum.Data
#' object in the record list
#'
#' This function only applies on RLum.Analysis objects and was written for performance not
#' usability, means the functions runs without any checks and is for internal usage only.
#'
#' @param [Luminescence::RLum.Analysis-class] (**required**):
#' input object where the function should be applied on
#'
#' @return
#' Returns the same object as the input
#'
#' @section Function version: 0.1.0
#'
#' @author Sebastian Kreutzer, F2.1 Geophysical Parametrisation/Regionalisation, LIAG - Institute for Applied Geophysics (Germany)
#'
#' @examples
#'
#' ##example using self created data
#' object <- set_RLum(
#' "RLum.Analysis",
#' records = list(
#'  set_RLum("RLum.Data.Curve"),
#'  set_RLum("RLum.Data.Curve")))
#'
#' object <- .set_pid(object)
#'
#' @noRd
.set_pid <- function(object){
  object@records <-
    lapply(object@records, function(x) {
      x@.pid  <- object@.uid
      return(x)
    })

  object
}

#+++++++++++++++++++++
#+ .warningCatcher() +
#+++++++++++++++++++++

#' Catches warnings returned by a function and merges them into a single one.
#' The original return value of the function is returned. This function is
#' particularly helpful if a function returns a lot of identical warnings.
#'
#' @param expr [expression] (**required**):
#' the R expression, usually a function
#'
#' @return
#' Returns the same object as the input and throws a warning.
#'
#' @section Function version: 0.2.0
#'
#' @author Sebastian Kreutzer, F2.1 Geophysical Parametrisation/Regionalisation, LIAG - Institute for Applied Geophysics (Germany)
#'
#' @examples
#'
#' f <- function() {
#'  warning("warning 1")
#'  warning("warning 1")
#'  warning("warning 2")
#'  1:10
#' }
#' print(.warningCatcher(f()))
#'
#' @noRd
.warningCatcher <- function(expr) {
  ##set variables
  warning_collector <- list()

  ##run function and catch warnings
  results <- withCallingHandlers(
    expr = expr,
    warning = function(w) {
      warning_collector <<- c(warning_collector, w$message)
      tryInvokeRestart("muffleWarning")
    }
  )

  ##set new warning messages with merged results
  if (length(warning_collector) > 0) {
    w_table <- table(as.character(unlist(warning_collector)))
    w_single <- rep(length(w_table) == 1, length(w_table))
    msg <- sprintf("%s%s%s",
                   ifelse(w_single, "", sprintf("(%d) ", 1:length(w_table))),
                   names(w_table),
                   ifelse(w_table == 1, "", sprintf(" [%d times]", w_table)))
    warning(paste(msg, collapse = "\n"), call. = FALSE)
  }
  results
}

#+++++++++++++++++++++
#+ .smoothing()      +
#+++++++++++++++++++++

#' Smooth data based on rolling means or medians
#'
#' The function allows a direct and meaningful access to the functionality of
#' `data.table::frollmean()` and `data.table::frollapply()`. It also provides
#' an implementation of the Poisson smoother of Carter et al. (2018).
#'
#' @param x [numeric] (**required**):
#' the object for which the smoothing should be applied.
#'
#' @param k [integer] (*with default*):
#' window size for the rolling mean or median (ignored if
#' `method = "Carter_etal_2018"`).
#'
#' @param fill [numeric] (*with default*):
#' value used to pad the result so to have the same length as the input.
#'
#' @param align [character] (*with default*):
#' one of `"right"`, `"center"` or `"left"`, specifying whether the index
#' of the result should be right-aligned (default), centred, or left-aligned
#' compared to the rolling window of observations (ignored if
#' `method = "Carter_etal_2018"`).
#'
#' @param p_acceptance [numeric] (*with default*):
#' probability threshold of accepting a value to be a sample from a Poisson
#' distribution (only used for `method = "Carter_etal_2018"`). Values that
#' have a Poisson probability below the threshold are replaced by the average
#' over the four neighbouring values.
#'
#' @param method [character] (*with default*):
#' smoothing method to be applied: one of `"mean"`, `"median"` or
#' `"Carter_etal_2018"`.
#'
#' @return
#' Returns a numeric vector with smoothed values.
#'
#' @section Function version: 0.3
#'
#' @author
#' Sebastian Kreutzer, F2.1 Geophysical Parametrisation/Regionalisation, LIAG - Institute for Applied Geophysics (Germany)\cr
#' Marco Colombo, Institute of Geography, Heidelberg University (Germany)
#'
#' @examples
#'
#' v <- 1:100
#' .smoothing(v)
#'
#' @noRd
.smoothing <- function(
  x,
  k = NULL,
  fill = NA,
  align = "right",
  p_acceptance = 1e-7,
  method = "mean") {
  .set_function_name(".smoothing")
  on.exit(.unset_function_name(), add = TRUE)

  .validate_args(align, c("right", "center", "left"))
  method <- .validate_args(method, c("mean", "median", "Carter_etal_2018"))
  .validate_positive_scalar(p_acceptance)

  ##set k
  if (is.null(k))
   k <- ceiling(length(x) / 100)

  ##smooth data
  switch(method,
  mean = {
    data.table::frollmean(x, n = k, fill = fill, align = align)
  },
  median = {
    data.table::frollapply(x, N = k, FUN = "median",
                           fill = fill, align = align)
  },
  Carter_etal_2018 = {
    ## Code derived with corrections and improvements from the supplementary
    ## materials of Carter et al. (2018)
    ## https://doi.org/10.1016/j.radmeas.2018.05.010

    ## Poisson probability of sample counts
    ## We operate on logged values to avoid overflow that would happen with
    ## the naive formulation `exp(-mx) * mx^x / factorial(x)`
    mx <- mean(x, na.rm = TRUE)
    prob <- exp(-mx + x * log(mx) - lfactorial(x))

    ## remove counts with probability below the acceptance threshold
    ## so they are not considered when averaging over the neighbours
    na.idx <- prob < p_acceptance
    x[na.idx] <- NA
    if (all(is.na(x)))
      .throw_error("'p_acceptance' rejects all counts, set it to a smaller value")

    ## average over four neighbours: we set "n = 5" instead of "n = 4" because
    ## we want to include the 2 neighbours to the left and the 2 on the right,
    ## but the window size also counts the element itself; this is not a
    ## problem, as we use the rolling mean only for elements that we have set
    ## to NA just above, and they don't have a value to contribute to the mean
    x[na.idx] <- data.table::frollmean(x, n = 5, align = "center",
                                       fill = fill, na.rm = TRUE)[na.idx]
    round(x)
  })
}

#' Computation of a weighted median
#'
#' @param x [numeric] (**required**):
#' Values whose weighted median is to be computed.
#'
#' @param w [numeric] (**required**):
#' Weights to use for elements of `x`.
#'
#' @param na.rm [logical] (*with default*):
#' Whether `NA` values in `x` should be removed before the computation.
#'
#' @return
#' A numeric value corresponding to the weighted median of the input vector.
#'
#' @noRd
.weighted.median <- function(x, w, na.rm = FALSE) {
  ## based on limma::weighted.median
  if (na.rm) {
    keep.idx <- !is.na(x)
    x <- x[keep.idx]
    w <- w[keep.idx]
  }
  ox <- order(x)
  x <- x[ox]
  w <- w[ox]
  p <- cumsum(w) / sum(w)
  n <- sum(p < 0.5)
  if (p[n + 1] > 0.5)
    return(x[n + 1])
  (x[n + 1] + x[n + 2]) / 2
}

#++++++++++++++++++++++++++++++
#+ Curve normalisation        +
#++++++++++++++++++++++++++++++

#' Curve normalisation
#'
#' Details on the normalisation methods are specified in
#' [Luminescence::plot_RLum.Data.Curve].
#'
#' The function assumes that `NA` or other invalid values have already been
#' removed by the caller function, and that the `norm` option has already
#' been validated.
#'
#' @param data [numeric], [matrix] (**required**):
#' the curve data to be normalised.
#'
#' @param norm [logical] [character] (**required**):
#' if logical, whether curve normalisation should occur; alternatively, one
#' of `"max"` (used with `TRUE`), `"min"`, `"first"`, `"last"`, `"huot"`,
#' or a positive number (e.g., 2.2; here it can also be a vector of the same
#' length as `data` to support the `"intensity"` option).
#'
#' @noRd
.normalise_curve <- function(data, norm) {
  ## early return if no normalisation is requested
  if (isFALSE(norm)) {
    return(data)
  }

  if (inherits(norm, "numeric")) {
    data <- data / norm
  }
  else if (norm == "max" || isTRUE(norm)) {
    data <- data / max(data, na.rm = TRUE)
  }
  else if (norm == "last") {
    data <- data / data[length(data)]
  }
  else if (norm == "first") {
    data <- data / data[1]
  }
  else if (norm == "min") {
    data <- data / min(data, na.rm = TRUE)
  }
  else if (norm == "huot") {
    bg <- median(data[floor(length(data) * 0.8):length(data)], na.rm = TRUE)
    data <- (data - bg) / max(data - bg, na.rm = TRUE)
  }

  ## check for Inf and NA
  if (any(is.infinite(data)) || anyNA(data)) {
    data[is.infinite(data) | is.na(data)] <- 0
    .throw_warning("Curve normalisation produced Inf/NaN values, ",
                   "values replaced by 0")
  }

  data
}

#++++++++++++++++++++++++++++++
#+ LxTx error calculation     +
#++++++++++++++++++++++++++++++

#' Calculation of the `Lx/Tx` error
#'
#' Calculates the `Lx/Tx` error according Galbraith (2014).
#'
#' @param LnLxTnTx [data.frame] (**required**): containing columns
#'        `Net_LnLx`, `Net_TnTx`, `Net_LnLx.Error`, `Net_TnTx.Error`.
#' @param sig0 [numeric] (**required**): extra error component to be added
#'        to the final `Lx/Tx` error value (e.g., instrumental error).
#' @param digits [integer] (**required**): round numbers to the specified
#'        digits. If set to `NULL`, no rounding occurs.
#'
#' @noRd
.calculate_LxTx_error <- function(LnLxTnTx, sig0, digits) {

  ## extract fields from the data.frame
  LnLx <- LnLxTnTx$Net_LnLx
  TnTx <- LnLxTnTx$Net_TnTx
  LnLx.Error <- LnLxTnTx$Net_LnLx.Error
  TnTx.Error <- LnLxTnTx$Net_TnTx.Error

  ## calculate Ln/Tx
  LxTx <- LnLx / TnTx
  if (is.nan(LxTx)) LxTx <- 0

  ## calculate Ln/Tx error
  LxTx.relError <- sqrt((LnLx.Error / LnLx)^2 + (TnTx.Error / TnTx)^2)
  LxTx.Error <- abs(LxTx * LxTx.relError)
  if (is.nan(LxTx.Error)) LxTx.Error <- 0

  ## add an extra component of error
  LxTx.Error <- sqrt(LxTx.Error^2 + (sig0 * LxTx)^2)

  ## combined values
  res <- cbind(LnLxTnTx, LxTx, LxTx.Error)
  if (!is.null(digits)) {
    res[1, ] <- round(res[1, ], digits = digits)
  }

  res
}

#++++++++++++++++++++++++++++++
#+ Scientific axis annotation +
#++++++++++++++++++++++++++++++

#' Bored of the 1e10 notation of large numbers in R? Already tried to force
#' R to produce more fancy labels? Worry not, fancy_scientific() (written by
#' Jack Aidley) is at your help!
#'
#' Source:
#' [http://stackoverflow.com/questions/11610377/how-do-i-change-the-formatting-of-numbers-on-an-axis-with-ggplot]()
#'
#' @param l [numeric] (**required**):
#' a numeric vector, i.e. the labels that you want to add to your plot
#'
#' @return
#' Returns an expression
#'
#' @section Function version: 0.1.0
#'
#' @author Jack Aidley
#'
#' @examples
#' plot(seq(1e10, 1e20, length.out = 10),
#'      1:10,
#'      xaxt = "n")
#'
#' axis(1, at = axTicks(1),
#'      labels = fancy_scientific(axTicks(1)))
#'
#' @noRd
fancy_scientific <- function(l) {
  # turn in to character string in scientific notation
  l <- format(l, scientific = TRUE)
  # quote the part before the exponent to keep all the digits
  l <- gsub("^(.*)e", "'\\1'e", l)
  # turn the 'e+' into plotmath format
  l <- gsub("e", "%*%10^", l)
  # remove plus sign
  l <- gsub("\\+", "", l)
  # return this as an expression
  parse(text=l)
}

#'Add fancy log axis with minor ticks the fancy axis labelling
#'
#'@param side [numeric] (**required**): the side where to plot the axis
#'
#'@param ... extra arguments to be passed to [graphics::axis], `side`, `at`and `labels`
#'are pre-defined and cannot be modified
#'
#'@return
#'Returns fancy log axis
#'
#'@author Sebastian Kreutzer, F2.1 Geophysical Parametrisation/Regionalisation, LIAG - Institute for Applied Geophysics (Germany)
#'
#'@examples
#'
#'y <- c(0.1, 0.001, 0.0001)
#'plot(1:length(y), y, yaxt = "n", log = "y")
#'.add_fancy_log_axis(side = 2, las = 1)
#'
#'@noRd
.add_fancy_log_axis <- function(side, ...){
  ## do just nothing if it would cause an error
  if(!(par()$xlog && any(c(1,3) %in% side[1])) && !(par()$ylog && any(c(2,4) %in% side[1])))
    return(NULL)

  ## get current axis ticks and get exponent
  ticks <- graphics::axTicks(side, log = TRUE)
  ticks <- unique(floor(log10(ticks)))
  minor_ticks <- vapply(ticks, function(x) {
      seq(10^(x-1),10^x, length.out = 10)[-10]
  }, numeric(9))

  ## add minor ticks
  graphics::axis(
    side,
    at = as.numeric(minor_ticks),
    lwd.ticks = 0.5,
    tcl = -0.35,
    labels = FALSE)

  ## add main axis
    ## remove settings we set
    args <- list(...)
    args$side <- NULL
    args$at <- NULL
    args$labels <- NULL

  ## call the axis
  do.call(what = graphics::axis, args = c(
    list(side = side,
    at = 10^ticks,
    labels = fancy_scientific(10^ticks)),
    args))
}

#' Retrieve graphical parameters
#'
#' @return
#' A list of graphical parameters that can be restored, with the following
#' exceptions:
#' - The `pin` parameter is removed because at times (for example, when a pdf
#' file with too small size is used) it can become negative, and restoring it
#' would throw an error.
#' - The `new` parameter is set to `FALSE` because calling `par()` with no
#' graphical device open will set `par("new")` to `TRUE`, which is not what
#' we want to restore, as otherwise the next plot may accidentally overwrite
#' the previous one.
#' - The `usr` parameter is removed so that additional plot elements can be
#' added afterwards using the axis scale of the plot. If we restore that, the
#' axis would range to the default 0-1 interval.
#'
#' @noRd
.par_defaults <- function() {
  pars <- par(no.readonly = TRUE)
  pars$pin <- NULL
  pars$new <- FALSE
  pars$usr <- NULL
  pars
}

#' Convert position keywords into summary placement coordinates
#'
#' @param pos [numeric] or [character] (**required**) Position coordinates
#'        or keyword (one of "topleft", "top", "topright", "left", "center",
#'        "right", "bottomleft", "bottom", "bottomright").
#' @param xlim [numeric] (**required**) The x-axis limits.
#' @param ylim [numeric] (**required**) The y-axis limits.
#'
#' @return
#' A list of two elements: `pos` and `adj`.
#'
#' @noRd
.get_keyword_coordinates <- function(pos, xlim, ylim) {
  adj <- NA
  if (missing(pos)) {
    pos <- c(xlim[1], ylim[2])
    adj <- c(0, 1)
  } else if (length(pos) == 2) {
    pos <- pos
    adj <- c(0, 1)
  } else if (pos[1] == "topleft") {
    pos <- c(xlim[1], ylim[2])
    adj <- c(0, 1)
  } else if (pos[1] == "top") {
    pos <- c(mean(xlim), ylim[2])
    adj <- c(0.5, 1)
  } else if (pos[1] == "topright") {
    pos <- c(xlim[2], ylim[2])
    adj <- c(1, 1)
  } else if (pos[1] == "left") {
    pos <- c(xlim[1], mean(ylim))
    adj <- c(0, 0.5)
  } else if (pos[1] == "center") {
    pos <- c(mean(xlim), mean(ylim))
    adj <- c(0.5, 0.5)
  } else if (pos[1] == "right") {
    pos <- c(xlim[2], mean(ylim))
    adj <- c(1, 0.5)
  }else if (pos[1] == "bottomleft") {
    pos <- c(xlim[1], ylim[1])
    adj <- c(0, 0)
  } else if (pos[1] == "bottom") {
    pos <- c(mean(xlim), ylim[1])
    adj <- c(0.5, 0)
  } else if (pos[1] == "bottomright") {
    pos <- c(xlim[2], ylim[1])
    adj <- c(1, 0)
  }

  list(pos = pos, adj = adj)
}


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+ Statistical Summary for Plot functions
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' Create Statistical Summary Character Vector for Plot functions
#'
#' This function automatically generates the statistical summary for the plot functions within
#' the package. This should unify the approach how such things are created and support, theoretically
#' all keywords for all plot functions in a similar way.
#'
#' @param summary [data.frame] (**required**):
#' output from function `calc_Statistics()`.
#'
#' @param keywords[character] (*with default*): keywords supported by function
#' `calc_Statistics()`.
#'
#' @param digits [numeric] (*with default*): modifiy the digits independently
#' for the plot output.
#'
#' @param sep [character] (*with default*): separator used for the creation of
#' the output of the plot.
#'
#'@param prefix [character] (*with default*): prefix to add to the string
#'
#'@param suffix [character] (*with default*): suffix to add to the string
#'
#'@author Sebastian Kreutzer, F2.1 Geophysical Parametrisation/Regionalisation, LIAG - Institute for Applied Geophysics (Germany)
#'
#'@section Version: 0.1.0
#'
#'
#'@noRd
.create_StatisticalSummaryText <- function(
  summary,
  keywords = NULL,
  digits = 2, #allow for different digits
  sep = " \n ",
  prefix = "",
  suffix = ""
) {
  ## loop over all keywords provided
  l <- lapply(keywords, function(k) {
    if (!nzchar(k))
      return(NULL)

    ## strip the prefix if necessary
    keywords_prefix <- "unweighted"
    k_strip <- unlist(strsplit(k, split = "$", fixed = TRUE))
    if (length(k_strip) > 1) {
      keywords_prefix <- k_strip[1]
      k_strip <- k_strip[2]
    }

    ## return early if the keyword cannot be found
    value <- summary[[keywords_prefix]][[k_strip]]
    if (is.null(value))
      return(NULL)

    ## construct string
    paste(if (keywords_prefix == "unweighted") k_strip else k,
          "=", round(value, digits))
  })

  ##remove NULL entries
  l <- .rm_NULL_elements(l)

  ##construct final call
  paste0(prefix, paste(unlist(l), collapse = sep), suffix)
}

#++++++++++++++++++++++++++++++
#+ Unlist RLum               +
#++++++++++++++++++++++++++++++
#'
#' Recursive unlisting of lists until the first element in the list
#' is something other than a list. This function helps to get rid of nested
#' lists. The function stops running if a single level list is reached.
#'
#' @param x [list] (**required**): list with lists
#'
#' @author Sebastian Kreutzer, F2.1 Geophysical Parametrisation/Regionalisation, LIAG - Institute for Applied Geophysics (Germany)
#'
#' @examples
#' a <- list(b = list(c = list("test")))
#' .unlist_RLum(a)
#'
#' @return [list] with only one level left
#' @noRd
.unlist_RLum <- function(x){
  stopifnot(is.list(x))

  if (length(x) == 0 || !inherits(x[[1]], "list"))
    return(x)

  .unlist_RLum(unlist(x, recursive = FALSE))
}

#++++++++++++++++++++++++++++++
#+ .rm_nonRLum                +
#++++++++++++++++++++++++++++++
#' @title Removes all non-RLum objects from list
#'
#' @description
#' Removes all non-RLum objects from a list supposed to consist only of
#' [Luminescence::RLum-class] objects.
#' As an internal function, the function is rather unforgiving, no further
#' checks are applied.
#'
#' @param x [list] (**required**): list
#'
#' @param class [character] (*with default*):
#' class of elements to keep, by default [Luminescence::RLum-class].
#'
#' @author Sebastian Kreutzer, F2.1 Geophysical Parametrisation/Regionalisation, LIAG - Institute for Applied Geophysics (Germany)
#'
#' @examples
#' x <- c(list(set_RLum("RLum.Analysis"), set_RLum("RLum.Analysis")), 2)
#' .rm_nonRLum(x)
#'
#' @return
#' A list containing only objects of the specified class.
#'
#' @noRd
.rm_nonRLum <- function(x, class = "RLum") {
  x[vapply(x, inherits, logical(1), class)]
}

#++++++++++++++++++++++++++++++
#+ .rm_NULL                   +
#++++++++++++++++++++++++++++++
#' @title Removes all NULL elements from list
#'
#' @param x [list] (**required**): list
#'
#' @author Sebastian Kreutzer, F2.1 Geophysical Parametrisation/Regionalisation, LIAG - Institute for Applied Geophysics (Germany)
#'
#' @examples
#' l <- list("a", NULL)
#' .rm_NULL_elements(l)
#'
#' @return [list] without `NULL` elements, can be empty
#'
#' @noRd
.rm_NULL_elements <- function(x){
  x[vapply(x, is.null, logical(1))] <- NULL
  x
}

#++++++++++++++++++++++++++++++
#+ .matrix_binning            +
#++++++++++++++++++++++++++++++
#' @title Efficient binning of matrices
#'
#' @description This function allows efficient binning of matrices including
#' row and column name handling. Internally, the function uses [rowsum],
#' means the binning is always applied on the rows. For column binning the function
#' internally transposes the matrix first
#'
#' @param m [matrix] (**required**): the matrix uses the base function [rowsum]
#'
#' @param bin_size [integer] (*with default*): bin size
#'
#' @param bin_col [logical] (*with default*): applies the binning on the columns instead of the
#' rows. If you want to perform binning on rows and columns, you have to call this function twice.
#'
#' @param names [character] (*with default*): the handling of the row and column names. The default
#' `NULL` removes the column and row names. Other allowed input is: `'groups'` this uses the group
#' name, e.g., the last time value of a group, `'mean'` this calculates the mean value of a group,
#' `'sum'` to sum-up groups and you can provide any other value which will then be recycled throughout.
#' For example: `c('row1', 'row2')`.
#'
#' @author Sebastian Kreutzer, IRAMAT-CRP2A, UMR 5060, CNRS - Université Bordeaux Montaigne (France)
#'
#' @section Function version: 0.1.2
#'
#' @note Row and column names are transformed to numeric and also summed up; this is not a bug
#' but a feature!
#'
#' @return [matrix]
#'
#' @examples
#'  m <- matrix(data = c(rep(1:20,each = 20)), ncol = 10, nrow = 20)
#'  rownames(m) <- 1:nrow(m)
#'  colnames(m) <- 1:ncol(m)
#'
#' .matrix_binning(m, bin_size = 4)
#'
#' @noRd
.matrix_binning <- function(
  m,
  bin_size = 1,
  bin_col = FALSE,
  names = NULL) {
  .set_function_name(".matrix_binning")
  on.exit(.unset_function_name(), add = TRUE)

  #@ The only check
  .validate_class(m, "matrix")

  ## transpose in column mode
  if(bin_col) m <- t(m)

  ## binning calculation
  ##set groups
  ##with the correction in the 2nd line we
  ##get rid potential problems
  groups <- rep(1:nrow(m), each = bin_size)[1:nrow(m)]

  ##row binning (thats all)
  temp_m <- rowsum(m, group = groups)

  ## Correct names
  if(!is.null(names[1])){
    if(names[1] == "groups"){
      ##get rownames correct (it is the end of each bin)
      row_names <- rownames(m)[which(diff(groups) != 0)]

      ##correct last value
      if(length(row_names) < nrow(m))
        row_names <- c(row_names,rownames(m)[nrow(m)])

    }else if(names[1] == "mean"){
      groups <- rep(1:nrow(m), each = bin_size)[1:nrow(m)]
      row_names <- as.numeric(rownames(m))
      row_names <- tapply(X = row_names, INDEX = groups, FUN = mean)

    }else if(names[1] == "sum"){
      row_names <- rowsum(as.numeric(rownames(m)), group = groups)

    }else{
      row_names <- names
    }

    ##reset rownames and make sure it fits the length
    rownames(temp_m) <- rep_len(row_names, nrow(temp_m))

  }else{
    rownames(temp_m) <- NULL
  }

  ## re-transpose in column mode
  if(bin_col) temp_m <- t(temp_m)

  ## return
  temp_m
}

#++++++++++++++++++++++++++++++
#+ .expand_parameters         +
#++++++++++++++++++++++++++++++
#' @title Expand function parameters of self-call
#'
#' @description For the self-call, the function parameters need to
#' be expanded, this was done, so far in a non-consistent way and
#' repeated in every function using the self-call. This functions
#' does it once and for all similar in all functions.
#'
#' **NOTE**: the first argument is never extended due to performance reasons,
#' it might be a very large object
#'
#' @param len [numeric] (**required**): length of the parameter expansion
#'
#' @author Sebastian Kreutzer, F2.1 Geophysical Parametrisation/Regionalisation, LIAG - Institute for Applied Geophysics (Germany)
#'
#' @return [list] with expanded parameters
#'
#' @noRd
.expand_parameters <- function(len){
  ##get original definition and the call of f
  f_def <- sys.function(sys.parent())
  f_call <- sys.call(sys.parent())

  ## get parent environment because we have to evaluate
  ## objects in the parent environment.
  p_env <- parent.env(environment())

  ##extract arguments (do not consider the first argument, this might be a very
  ##large object)
  args_default <- as.list(f_def)[-length(as.list(f_def))][-1]
  args_new <- as.list(match.call(f_def, f_call, FALSE))[-(1:2)]

  ##now we have to make sure that we evaluate all language objects
  ##before passing them further down
  if(length(args_new) > 0){
    for(i in 1:length(args_new)){
      if(class(args_new[[i]])[1] == "name" |
         class(args_new[[i]])[1] == "call" |
         class(args_new[[i]])[1] == "(" ) {
        args_new[[i]] <- eval(args_new[[i]], envir = p_env)
      }
    }
  }

  ##combine the two argument lists
  args <- modifyList(
    x = args_default,
    val = args_new,
    keep.null = TRUE)

  ##evaluate arguments and take care of missing values
  for(i in 1:length(args)){
    if(is.na(names(args[i])) || names(args[i]) == "...") next
    if(class(args[[i]])[1] == "name" & names(args[i]) != "...") {
      .throw_error("Argument '", names(args[i]), "' missing, with no default")
    }

    ##evaluate and cover special cases
    if(!is.null(args[[i]])) args[[i]] <- eval(args[[i]])
    if(inherits(args[i],  "list") & length(args[[i]]) == 0) args[[i]] <- list()

  }
  ##expand all arguments
  ##we have two conditions and three cases
  ##1:  the argument is a list AND the list itself is not named
  ##    ... the case when the user what to use different values for the objects
  ##2:  the argument is no list ...
  ##    ... the standard automated expansion
  ##    ... OR it is a list with names (e.g., rejection.criteria = list(recycling.ration = 10))
  for(i in 1:length(args)){
    if(inherits(args[[i]], "list") & is.null(names(args[[i]]))){
      args[[i]] <- rep(args[[i]], length = len[1])

    } else {
      args[[i]] <- rep(list(args[[i]]), length = len[1])
    }
  }

  args
}

#++++++++++++++++++++++++++++++
#+ .calc_HPDI                +
#++++++++++++++++++++++++++++++
#' @title Calculates Highest Probability Density Interval
#'
#' @description The idea of this function is to provide a convenient
#' method to calculate the highest probability density intervals for
#' sets of data. This function might be exported later
#' Currently it follows roughly the idea of what is implemented
#' in `code` and `hdrcde`. If the results has more than one peak,
#' also this is shown, therefore the output is a matrix
#'
#' @param object [numeric] (**required**): numeric object with input data
#'
#' @param prob [numeric] (*with default*): sets aimed probability interval
#'
#' @param plot [logical] (*with default*): enable/disable the plot output.
#'
#' @param ... further arguments passed to [stats::density]
#'
#' @author Sebastian Kreutzer, F2.1 Geophysical Parametrisation/Regionalisation, LIAG - Institute for Applied Geophysics (Germany)
#'
#' @references
#' Hyndman, R.J., 1996. Computing and Graphing Highest Density Regions.
#' The American Statistician 50, 120–8. doi:10.2307/2684423
#'
#' @examples
#' x <- rnorm(100)
#' .calc_HPDI(x)
#'
#' @return [matrix] with HPDI
#'
#' @noRd
.calc_HPDI <- function(object, prob = 0.95, plot = FALSE, ...){
  ##estimate density
  dens <- density(object, ...)
  diff <- diff(dens$x[1:2])

  ##calculate probabilities
  m <- cbind(matrix(c(dens$x, dens$y), ncol = 2), dens$y * diff)
  o <- order(m[, 3], decreasing = TRUE)
  m_ind <- which(cumsum(m[o, 3]) <= prob)
  thres <- suppressWarnings(min(m[o, 2][m_ind]))

  ##get peaks
  peaks_id <- which(abs(diff((m[,2] - thres) > 0)) == 1)

  ##calculate HPDI
  HPDI <- matrix(NA, ncol = 2, nrow = 1)
  if (length(peaks_id) > 0)
    HPDI <- matrix(m[peaks_id,1], ncol = 2)

  colnames(HPDI) <- c("lower", "upper")
  attr(HPDI, "Probability") <- prob

  if(plot){
    plot(dens, main = "HPDI (control plot)")
    abline(h = thres, lty = 2)
    if (length(peaks_id) > 0) {
      for(i in seq(1,length(peaks_id),2)) {
        lines(x = m[peaks_id[i]:peaks_id[i + 1], 1],
              y = m[peaks_id[i]:peaks_id[i + 1], 2], col = "red")
      }
    }
  }

  HPDI
}

#++++++++++++++++++++++++++++++
#+ .download_file             +
#++++++++++++++++++++++++++++++
#'@title Internal File Download Handler
#'
#'@description For file imports using function commencing with `read_` the file download
#'was little consistent and surprisingly error-prone. This function should keep the requirements
#'more consistent
#'
#'@param url [character] (**required**)
#'
#'@param dest [character] (*with default*)
#'
#' @param verbose [logical] (*with default*)
#' enable/disable output to the terminal.
#'
#'@returns Returns either nothing (no URL) or the file path of the downloaded file
#'
#'@author Sebastian Kreutzer, F2.1 Geophysical Parametrisation/Regionalisation, LIAG - Institute for Applied Geophysics (Germany)
#'
#'@examples
#'
#'## returns just NULL (no URL detected)
#'.download_file(url = "teststs")
#'
#'## attempts download
#'.download_file(url = "https://raw.githubusercontent.com/R-Lum/rxylib/master/inst/extg")
#'
#'## attempts download silently
#' suppressMessages(
#' .download_file(url = "https://raw.githubusercontent.com/R-Lum/rxylib/master/inst/extg"))
#'
#'@noRd
.download_file <- function(
    url,
    destfile = tempfile(),
    verbose = TRUE
) {
  out_file_path <- NULL

  ## detect and extract URL
  if(grepl(pattern = "https?\\:\\/\\/", x = url, perl = TRUE)) {
    ## status reports
    if (verbose) {
      .throw_message("URL detected: ", url, error = FALSE)
      .throw_message("Attempting download ... ", appendLF = FALSE, error = FALSE)
    }

    ## extract URL from string only
    url <- regmatches(x = url, m = regexec(pattern = "https?\\:\\/\\/.+", text = url, perl = TRUE))[[1]]

    fail.msg <- function(w) {
      if (verbose)
        message("FAILED")
      NULL
    }
    ## use internal download
    t <- tryCatch(
      expr = download.file(
        url = url,
        destfile = destfile,
        quiet = TRUE,
        mode = "wb", ## this is needed for Windows otherwise the download does not work
        cacheOK = FALSE,
        method = "auto"),
      warning = fail.msg,
      error = fail.msg)

    if(!is.null(t) && t == 0) {
      if (verbose)
        message("OK ", appendLF = TRUE)
      out_file_path <- destfile
      unlink(url)
    }
  }

  ## return file path
  out_file_path
}

#' @title Set/unset the function name for error/warning reporting
#'
#' @description
#' These utilities allow more precise error reporting from `.throw_error()`,
#' `.throw_warning()` and `.throw_message()`. They must be called exactly once
#' if the function calls one of the `.throw_*()` or `.validate_*()` functions.
#'
#' @param name [character] (**required**): the name of the function
#'
#' @details
#' The `.LuminescenceEnv` package environment stores a list (`fn_stack`)
#' that is used to store the stack of function calls currently executing.
#' The stack is at the beginning an empty list, but whenever a function is
#' called, that function add its own name to it via `.set_function_name()`.
#' Conversely, when the function returns, it clears the top of the stack
#' via `.unset_function_name()`.
#'
#' In order to maintain the stack in a consistent state, it is important that
#' each call to `.set_function_name()` is accompanied by a corresponding call
#' to `.unset_function_name()`. As the stack must be updated also when a
#' function (or any of its callees) encounters an error, the calls to
#' `.unset_function_name()` must be delegated to `on.exit(..., add = TRUE)`.
#'
#' Therefore, it is suggested to put these two lines at the very beginning
#' of each function (if any of the throwing/validating functions is used):
#'
#'   .set_function_name("name_of_the_function")
#'   on.exit(.unset_function_name(), add = TRUE)
#'
#' @noRd
.set_function_name <- function(name) {
  .LuminescenceEnv$fn_stack[length(.LuminescenceEnv$fn_stack) + 1] <- name
}

#' @rdname .set_function_name
#' @noRd
.unset_function_name <- function() {
  .LuminescenceEnv$fn_stack[length(.LuminescenceEnv$fn_stack)] <- NULL
}

#'@title Throws a Custom Tailored Error Message
#'
#'@param ... the error message to throw
#'
#'@noRd
.throw_error <- function(...) {
  top.idx <- length(.LuminescenceEnv$fn_stack)
  stop("[", .LuminescenceEnv$fn_stack[[top.idx]], "()] ", ..., call. = FALSE)
}

#'@title Throws a Custom Tailored Warning Message
#'
#'@param ... the warning message to throw
#'
#'@noRd
.throw_warning <- function(...) {
  top.idx <- length(.LuminescenceEnv$fn_stack)
  warning("[", .LuminescenceEnv$fn_stack[[top.idx]], "()] ", ..., call. = FALSE)
}

#'@title Throws a Custom Tailored Message
#'
#' @param ... the message to throw
#' @param error Whether the message should be preceded by "Error:" (`TRUE` by
#' default).
#'
#'@noRd
.throw_message <- function(..., error = TRUE) {
  top.idx <- length(.LuminescenceEnv$fn_stack)
  message("[", .LuminescenceEnv$fn_stack[[top.idx]], "()] ",
          if (error) "Error: ", ...)
}

#' @title Throws a deprecation warning
#'
#' @param old Old argument names.
#' @param new New argument names.
#' @param since Release when the deprecation started (`NULL` by default).
#'
#' @noRd
.deprecated <- function(old, new, since = NULL) {
  plural <- length(old) > 1
  .throw_warning(.collapse(old, last_sep = " and "),
                 if (plural) " have " else " has ",
                 "been deprecated",
                 if (!is.null(since)) paste0(" since v", since),
                 ", use ",
                 .collapse(new, last_sep = " and "), " instead")
}

#' @title Silence Output and Warnings during Tests
#'
#' @description
#' This is helpful so that during tests the terminal is not filled up by
#' the output from the function tested, which is often left intentionally
#' verbose to facilitate the coverage analysis.
#'
#' This was originally defined in `tests/testthat/setup.R`, but unfortunately
#' that file is not sourced by `covr::file_coverage()` (as opposed to what
#' happens with `testthat::test_file()` and `covr::package_coverage()`),
#' which makes it harder to work iteratively with it.
#'
#' @param expr [expression] an R expression (often a function, but can be
#'        any amount of code) the output of which needs to be silenced
#'
#' @examples
#' SW({
#'   template_DRAC(preset = "DRAC-example_quartz")
#' })
#'
#' @noRd
SW <- function(expr) {
  capture.output(suppressMessages(suppressWarnings(expr)))
}

#' @title Retrieve the name of the variable used as first argument
#'
#' @noRd
.first_argument <- function() {
  sprintf("'%s'", all.vars(match.call(sys.function(sys.parent()),
                                      sys.call(sys.parent())))[1])
}

#' @title Throw an error or a warning based on a condition
#'
#' @noRd
.error_or_warning <- function(msg, throw.error) {
  if (throw.error)
    .throw_error(msg)
  .throw_warning(msg)
}

#' @title Validate a character argument from a list of choices
#'
#' @description
#' This is inspired by [base::match.arg], but is has a more user-friendly
#' error message as it reports the exact name of the argument that is being
#' validated. This function always requires the choices to be specified: this
#' better fits with the current state of the Luminescence package, which only
#' rarely lists all choices in the formal function arguments, so
#' [base::match.arg] would have very limited use.
#'
#' @param arg [character] (**required**): variable to validate.
#' @param choices [character] (**required**): vector of candidate values.
#' @param null.ok [logical] (*with default*): whether a `NULL` value should be
#'        considered valid (`FALSE` by default).
#' @param name [character] (*with default*): variable name to report in case
#'        of error; if specified, it's reported as is (so, it may need to be
#'        quoted by the caller); otherwise, it's inferred from the name of the
#'        variable tested and reported with quotes.
#' @param extra [character] (*with default*): additional choice to be reported
#'        after the valid choices and preceded by "or". If `null.ok = TRUE`,
#'        the text reported is automatically set to `"or NULL"`, otherwise it
#'        will take the form `"or {extra}"`.
#'
#' @return
#' The validated choice (may be `NULL` if `arg = NULL` and `null.ok = TRUE`).
#' If `arg` contains multiple elements, only the first matching one will be
#' returned.
#'
#' @noRd
.validate_args <- function(arg, choices, null.ok = FALSE,
                           name = NULL, extra = NULL) {

  if (is.null(arg) && null.ok)
    return(NULL)

  ## `arg` will have multiple values when the available choices are listed
  ## in the function's formal arguments: in that case all elements in `arg`
  ## are also in `choices` and we return the first one
  if (length(arg) > 1L) {
    if (all(arg %in% choices))
      return(arg[1L])

    ## we throw an error to catch cases when the formal arguments are
    ## changed but `choices` has not been updated
    .throw_error(name %||% .first_argument(),
                 " contains multiple values but not all of them match 'choices'")
  }

  ## additional text to append after the valid choices to account for
  ## extra options that cannot be validated or for NULL
  choices.extra <- c(sQuote(choices, q = FALSE), extra)
  if (null.ok)
    choices.extra <- c(choices.extra, "NULL")

  idx.match <- pmatch(arg, choices, nomatch = 0L, duplicates.ok = TRUE)
  if (all(idx.match == 0L))
    .throw_error(name %||% .first_argument(), " should be one of ",
                 .collapse(choices.extra, quote = FALSE, last_sep = " or "))
  idx <- idx.match[idx.match > 0L]
  choices[idx]
}

#' @title Validate an argument from a list of classes
#'
#' @param classes [character] (**required**): vector of candidate classes or
#'        types.
#' @param throw.error [logical] (*with default*): whether an error should be
#'        thrown in case of failed validation (`TRUE` by default). If `FALSE`,
#'        the function raises a warning and proceeds.
#' @param length [integer] (*with default*): if not `NULL`, validate that
#'        the length matches the one provided. This can be used only when
#'        `classes` contains only base vector types, not
#'        [Luminescence::RLum-class] objects
#'        or container types.
#' @inheritParams .validate_args
#'
#' @return
#' If `throw.error = TRUE`, the function throws an error and doesn't return
#' anything. Otherwise, it will return a boolean to indicate whether validation
#' was successful or not.
#'
#' @noRd
.validate_class <- function(arg, classes, null.ok = FALSE, throw.error = TRUE,
                            length = NULL, name = NULL, extra = NULL) {

  if (!missing(arg) && is.null(arg) && null.ok)
    return(TRUE)

  if (missing(arg) || sum(inherits(arg, classes)) == 0L ||
      !is.null(length) && length(arg) != length) {
    ## additional text to append after the valid classes to account for
    ## extra options that cannot be validated but we want to report
    classes.extra <- c(sQuote(classes, q = FALSE), extra)
    if (null.ok)
      classes.extra <- c(classes.extra, "NULL")

    msg <- paste0(name %||% .first_argument(), " should be of class ",
                  .collapse(classes.extra, quote = FALSE, last_sep = " or "))
    if (!is.null(length))
      msg <- paste0(msg, " and have length ", length)
    .error_or_warning(msg, throw.error)
    return(FALSE)
  }
  TRUE
}

#' @title Validate that a variable is not empty
#'
#' @param what [character] (**required**): the type of the variable, used
#'        only in the message reported; if not specified it's inferred from
#'        they type of the variable tested.
#' @param throw.error [logical] (*with default*): whether an error should be
#'        thrown in case of failed validation (`TRUE` by default). If `FALSE`,
#'        the function raises a warning and proceeds.
#' @inheritParams .validate_args
#'
#' @return
#' If `throw.error = TRUE`, the function throws an error and doesn't return
#' anything. Otherwise, it will return a boolean to indicate whether validation
#' was successful or not.
#'
#' @noRd
.validate_not_empty <- function(arg, what = NULL, throw.error = TRUE,
                                name = NULL) {

  ## type of the argument to report if not specified
  if (is.null(what))
    what <- class(arg)[1]

  if (NROW(arg) == 0 || NCOL(arg) == 0) {
    msg <- paste0(name %||% .first_argument(), " cannot be an empty ", what)
    .error_or_warning(msg, throw.error)
    return(FALSE)
  }
  TRUE
}

#' @title Validate the length of a variable
#'
#' @param exp.length [integer] (**required**): the expected length.
#' @param throw.error [logical] (*with default*): whether an error should be
#'        thrown in case of failed validation (`TRUE` by default). If `FALSE`,
#'        the function raises a warning and proceeds.
#' @inheritParams .validate_args
#'
#' @return
#' If `throw.error = TRUE`, the function throws an error and doesn't return
#' anything. Otherwise, it will return a boolean to indicate whether validation
#' was successful or not.
#'
#' @noRd
.validate_length <- function(arg, exp.length, throw.error = TRUE,
                            name = NULL) {
  if (length(arg) != exp.length) {
    msg <- paste0(name %||% .first_argument(), " should have length ", exp.length)
    .error_or_warning(msg, throw.error)
    return(FALSE)
  }
  TRUE
}

#' @title Validate scalar variables
#'
#' @description
#' This function is meant to validate unbounded (integer) scalar variables.
#' The `pos` and `log` arguments are not intended for direct use, they serve
#' to facilitate the implementation of `.validate_positive_scalar()` and
#' `.validate_logical_scalar()`.
#'
#' @param val [numeric] (**required**): value to validate.
#' @param int [logical] (*with default*): whether the value has to be an
#'        integer (`FALSE` by default).
#' @param pos [logical] (*with default*): whether the value has to be positive
#'        (`FALSE` by default).
#' @param log [logical] (*with default*): whether the value has to be logical
#'        (`FALSE` by default).
#' @inheritParams .validate_args
#'
#' @return
#' The validated value, unless the validation failed with an error thrown.
#'
#' @noRd
.validate_scalar <- function(val, int = FALSE, pos = FALSE, log = FALSE,
                             null.ok = FALSE, name = NULL, extra = NULL) {
  if (!missing(val) && is.null(val) && null.ok)
    return(NULL)
  if (missing(val) || NROW(val) != 1 || NCOL(val) != 1 || !is.null(dim(val)) || is.na(val) ||
      (!log && !is.numeric(val)) || (log && !is.logical(val)) ||
      (int && (is.infinite(val) || val != as.integer(val))) ||
      (pos && val <= 0)) {
    ## additional text to append for extra options that cannot be validated
    ## but we want to report
    if (null.ok)
      extra <- c(extra, "NULL")
    if (!is.null(extra))
      extra <- paste(" or", .collapse(extra, quote = FALSE, last_sep = " or "))
    .throw_error(name %||% .first_argument(), " should be a single ",
                 if (pos) "positive ", if (int) "integer ", if (log) "logical ",
                 "value", extra)
  }
  val
}

#' @title Validate scalar variables expected to be positive
#'
#' @inheritParams .validate_scalar
#'
#' @noRd
.validate_positive_scalar <- function(val, int = FALSE, null.ok = FALSE,
                                      name = NULL, extra = NULL) {
  .validate_scalar(val, int = int, pos = TRUE, null.ok = null.ok,
                   name = name %||% .first_argument(), extra = extra)
}

#' @title Validate logical scalar variables
#'
#' @inheritParams .validate_scalar
#'
#' @noRd
.validate_logical_scalar <- function(val, null.ok = FALSE,
                                     name = NULL, extra = NULL) {
  .validate_scalar(val, log = TRUE, null.ok = null.ok,
                   name = name %||% .first_argument(), extra = extra)
}

#' @title Validate the originator of an RLum object
#'
#' @param object [Luminescence::RLum-class] (**required**): object whose
#' originator should be
#'        checked.
#' @inheritParams .validate_args
#'
#' @return
#' The validated value, unless the validation failed with an error thrown.
#'
#' @noRd
.validate_originator <- function(object, choices, name = NULL) {
  if (.check_originator(object, choices))
    return(object@originator)
  .throw_error(paste0(name %||% .first_argument(), " has an unsupported originator ",
                      "(expected ", .collapse(choices), ", but found '",
                      object@originator, "')"))
}

#' @title Check that the originator matches a set of choices
#'
#' @inheritParams .validate_originator
#'
#' @return
#' A logical value.
#'
#' @noRd
.check_originator <- function(object, choices) {
  .hasSlot(object, "originator") && object@originator %in% choices
}

#' Check that a suggested package is installed
#'
#' Report a message with installation instructions if a suggested package
#' is not available.
#'
#' @param pkg [character] (**required**): name of the package to check.
#' @param reason [character] (*with default*): subject of the sentence,
#'        helpful to clarify why the package is being required.
#' @param throw.error [logical] (*with default*): whether an error should be
#'        thrown in case of failed validation (`TRUE` by default). If `FALSE`,
#'        the function raises a warning and proceeds.
#'
#' @return
#' If `throw.error = TRUE`, the function throws an error and doesn't return
#' anything. Otherwise, it will return a boolean to indicate whether validation
#' was successful or not.
#'
#' @noRd
.require_suggested_package <- function(pkg, reason = "This function",
                                       throw.error = TRUE) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    msg <- paste("%s requires the '%s' package: to install it, run",
                 "`install.packages('%s')` in your R console")
    msg <- sprintf(msg, reason, pkg, pkg)
    .error_or_warning(msg, throw.error)
    return(FALSE)
  }
  TRUE
}

#' Create a list of objects repeated a given number of times
#'
#' @param x [vector] (**required**) An object to be repeated into a list.
#' @param length [integer] (**required**) Length of the list to be produced.
#'
#' @return
#' A list with the object repeated the specified number of times.
#'
#' @noRd
.listify <- function(x, length) {
  if (!inherits(x, "list"))
    x <- list(x)
  rep(x, length = length)
}

#' Comma-separated string concatenation
#'
#' Collapse the elements of a vector into a comma-separated string, with
#' the option of quoting each element.
#'
#' @param x [vector] (**required**): A vector of elements to be collapsed into
#'        a comma-separated string.
#' @param quote [logical] (*with default*): Whether each element should be
#'        surrounded by single quotes (`TRUE` by default).
#' @param last_sep [character] (*with default*): Separator to use before the
#'        last element (`", "` by default). This should contain the desired
#'        spacing around it (e.g. " or ").
#'
#' @return
#' A comma-separated string where each element of the original vector is
#' optionally surrounded by single quotes.
#'
#' @noRd
.collapse <- function(x, quote = TRUE, last_sep = ", ") {
  x <- if (quote) sQuote(x, FALSE) else as.character(x)
  if (length(x) < 2)
    return(toString(x))
  head <- head(x, -2)
  tail <- paste0(tail(x, 2)[1], last_sep, tail(x, 1))
  toString(c(head, tail))
}

#' Compute and format the range of a vector
#'
#' @param vals [numeric] (**required**): A numeric vector.
#' @param sep [character] (*with default*): Separator character.
#' @param nsmall [integer] (*with default*): Minimum number of digits to the
#'        right of the decimal point.
#'
#' @return
#' A string with the range of the vector formatted as `min:max`.
#'
#' @noRd
.format_range <- function(vals, sep = ":", nsmall = 0L) {
  rng <- tryCatch(range(vals, na.rm = TRUE),
                  warning = function(w) c(NA, NA))
  paste(format(rng, nsmall = nsmall, trim = TRUE), collapse = sep)
}

#' Shorten a filename
#'
#' Shorten a filename to the given width by cutting out characters from the
#' middle, leaving the most significant parts.
#'
#' @param filename [character] (**required**) A file name; can be a vector
#' @param max.width [integer] (*with default*) The maximum width available
#'
#' @return
#' A filename not longer than the maximum available width.
#'
#' @noRd
.shorten_filename <- function(filename, max.width = 70) {
  ## optimised for speed with ChatGPT, 2025
  ## get length
  name.len <- nchar(filename, keepNA = FALSE)

  ## logical index of names to shorten
  to_shorten <- name.len > max.width

  ## get out if nothing needs shortening
  if (!any(to_shorten))
    return(filename)

  # calculate parts
  dots <- "..."  # \u2026
  max.width <- max.width - nchar(dots)
  part1_last <- floor(max.width / 2)
  part2_first <- floor(name.len[to_shorten] - max.width / 2) + 1

  # shorten what needs to be shortened
  filename[to_shorten] <- paste0(
    substring(filename[to_shorten], 1, part1_last),
    dots,
    substring(filename[to_shorten], part2_first)
  )

  ## return
  filename
}

#'@title Affine Transformation of Values
#'
#'@description Affine (linear) transformation of values; which we use
#'a couple of times within functions
#'
#'@param x [numeric] (*required*): values to be transformed
#'
#'@param range_old [numeric] (*required*): original scale limits
#'
#'@param range_new [numeric] (*required*): new scale limits
#'
#'@returns rescaled values values
#'
#'@examples
#'
#' x <- stats::rnorm(100)
#' y_dens <- stats::density(x)
#' .rescale(
#'  x = y_dens$y,
#'  range_old = c(min(y_dens$y), max(y_dens$y)),
#'  range_new = c(0,20))
#'
#'@noRd
.rescale <- function(x, range_old, range_new) {
  range_new[1] +
    (x - range_old[1]) * (range_new[2] - range_new[1]) /
    (range_old[2] - range_old[1])
}
