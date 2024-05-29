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
#' @param [RLum.Analysis-class] (**required**):
#' input object where the function should be applied on
#'
#' @return
#' Returns the same object as the input
#'
#' @section Function version: 0.1.0
#'
#' @author Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
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
#' @md
#' @noRd
.set_pid <- function(object){
  object@records <-
    lapply(object@records, function(x) {
      x@.pid  <- object@.uid
      return(x)
    })

  return(object)
}

#+++++++++++++++++++++
#+ .warningCatcher()        +
#+++++++++++++++++++++

#' Catches warning returned by a function and merges them.
#' The original return of the function is returned. This function is in particular
#' helpful if a function returns a lot of warnings with the same content.
#'
#' @param expr [expression] (**required**):
#' the R expression, usually a function
#'
#' @return
#' Returns the same object as the input and a warning table
#'
#' @section Function version: 0.1.0
#'
#' @author Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
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
#' @md
#' @noRd
.warningCatcher <- function(expr) {
  ##set variables
  warning_collector <- list()
  env <-  environment()

  ##run function and catch warnings
  results <- withCallingHandlers(
    expr = expr,
    warning = function(c) {
      temp <- c(get("warning_collector", envir = env), c[[1]])
      assign(x = "warning_collector",
             value = temp,
             envir = env)
      ##TODO should be replaced tryInvokeRestart once R 4.1 was released
      invokeRestart("muffleWarning")
    }
  )

  ##set new warning messages with merged results
  if (length(warning_collector) > 0) {
    w_table <- table(as.character(unlist(warning_collector)))
    w_table_names <- names(w_table)

    warning(paste0(
       "(",
        1:length(w_table),
        ") ",
        w_table_names,
        ": This warning occurred ",
        w_table,
        " times!"
      ,collapse = "\n"),
      call. = FALSE)

  }
  return(results)

}

#+++++++++++++++++++++
#+ .smoothing()      +
#+++++++++++++++++++++

#' Allows smmoothing of data based on the function zoo::rollmean
#'
#' The function just allows a direct and meaningfull access to the functionality of the zoo::rollmean()
#' function. Arguments of the function are only partly valid.
#'
#' @param x [numeric] (**required**):
#' the object for which the smoothing should be applied.
#'
#' @param k [integer] (*with default*):
#' window for the rolling mean; must be odd for rollmedian.
#' If nothing is set k is set automatically
#'
#' @param fill [numeric] (*with default*):
#' a vector defining the left and the right hand data
#'
#' @param align [character] (*with default*):
#' specifying whether the index of the result should be
#' left- or right-aligned or centered (default) compared to the rolling window of observations,
#' allowed `"right"`, `"center"` and `left`
#'
#' @param method [method] (*with default*):
#' defines which method should be applied for the smoothing: `"mean"` or `"median"`
#'
#' @return
#' Returns the same object as the input and a warning table
#'
#' @section Function version: 0.1.1
#'
#' @author Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#'
#' @examples
#'
#' v <- 1:100
#' .smoothing(v)
#'
#' @md
#' @noRd
.smoothing <- function(
  x,
  k = NULL,
  fill = NA,
  align = "right",
  method = "mean") {

  ##set k
  if (is.null(k)){
   k <- ceiling(length(x) / 100)
   if(method == "median" && k %%2 ==0)
     k <- k + 1
  }

  ##smooth data
  if(method == "mean"){
    zoo::rollmean(x, k = k, fill = fill, align = align)

  }else if(method == "median"){
    zoo::rollmedian(x, k = k, fill = fill, align = align)

  }else{
    stop("[Luminescence:::.smoothing()] Unvalid input for 'method'!")

  }

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
#' @md
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
#'@author Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#'
#'@examples
#'
#'y <- c(0.1, 0.001, 0.0001)
#'plot(1:length(y), y, yaxt = "n", log = "y")
#'.add_fancy_log_axis(side = 2, las = 1)
#'
#'@md
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
    tcl = -.35,
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


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+ Statistical Summary for Plot functions
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' Create Statistical Summary Character Vector for Plot functions
#'
#' This function automatically generates the statistical summary for the plot functions within
#' the package. This should unify the approach how such things are created and support, theoretically
#' all keywords for all plot functions in a similar way.
#'
#'@param x [data.frame] (optional): output from the function `calc_Statistics()`. If nothing is
#'provided a list of prefix keyword combinations supported by the function `calc_Statistics()` is returned.
#'
#'@param keywords[character] (with default): keywords supported by the function `calc_Statistics()`
#'
#'@param digits [numeric] (with default): modifiy the digits independently for the plot output
#'
#'@param sep [character] (with default): a separator used for the creation of the output of the plot
#'
#'@param prefix [character] (with default): allows to add a leading prefix to the string
#'
#'@param suffix [character] (with default): allows to add a suffix to the entire string
#'
#'@author Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#'
#'@section Version: 0.1.0
#'
#'
#'@md
#'@noRd
.create_StatisticalSummaryText <- function(
  x = NULL, #insert the output of calc_Statistics
  keywords = NULL,
  digits = 2, #allow for different digits
  sep = " \n ",
  prefix = "",
  suffix = ""
){

  ## Grep keyword information
  if (is.null(x)) {
    summary <- calc_Statistics(data.frame(x = 1:2, y = 1:2))

  } else {
    summary <- x

  }

  #all allowed combinations
  keywords_allowed <- unlist(lapply(names(summary), function(x){
    paste0(x, "$", names(summary[[x]]))

  }))

  ##return if for x == NULL
  if(is.null(x))
    return(keywords_allowed)

  ## Create call
  #create list
  l <- lapply(keywords, function(k) {
    ##strip keyword if necessary
    if (grepl(pattern = "$",
              x = k,
              fixed = TRUE)[1]) {
      strip <- strsplit(k, split = "$", fixed = TRUE)[[1]]
      keywords_prefix <- strip[1]
      k_strip <- strip[2]
    } else{
      keywords_prefix <- "unweighted"
      k_strip <- k

    }

    ##construct string
    if(!is.null(summary[[keywords_prefix]][[k_strip]])){
      if(keywords_prefix == "unweighted"){
        paste0(k_strip, " = ", round(summary[[keywords_prefix]][[k_strip]], digits))

      }else{
        paste0(k, " = ", round(summary[[keywords_prefix]][[k_strip]], digits))

      }

    }else{
      return(NULL)

    }

  })

  ##remove NULL entries
  l <- l[!sapply(l, is.null)]

  ##construct final call
  return(paste0(prefix, paste(unlist(l), collapse = sep), suffix))

}

#++++++++++++++++++++++++++++++
#+ Unlist RLum               +
#++++++++++++++++++++++++++++++
#'
#' Recursive unlisting of lists until the first element in the list
#' is something, but not a list. This funktion helps
#' to get rid of nested lists. The function stops running if a single
#' level list is reached.
#'
#' @param x [list] (**required**): list with lists
#'
#' @author Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#'
#' @examples
#' a <- list(b = list(c = list("test")))
#' .unlist_RLum(a)
#'
#' @return [list] with only one level left
#' @md
#' @noRd
.unlist_RLum <- function(x){
  stopifnot(class(x) == "list")

  if(length(x) > 0 && inherits(x[[1]], "list")){
    x <- unlist(x, recursive = FALSE)
    .unlist_RLum(x)
  }else{
    return(x)

  }

}

#++++++++++++++++++++++++++++++
#+ .rm_nonRLum                +
#++++++++++++++++++++++++++++++
#' @title Removes all non-RLum objects from list
#'
#' @description Removes all non RLum objects from a list
#' supposed to consist only of RLum-class objects
#' As an internal function, the function is rather unforgiving, no further
#' checks are applied.
#'
#' @param x [list] (**required**): list
#'
#' @param class [character]: class to look for, if nothing is set
#' it checks for RLum in general
#'
#' @author Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#'
#' @examples
#' x <- c(list(set_RLum("RLum.Analysis"), set_RLum("RLum.Analysis")), 2)
#' .rm_nonRLum(x)
#'
#' @return [list] with only RLum objects
#'
#' @md
#' @noRd
.rm_nonRLum <- function(x, class = NULL){
  if(is.null(class))
    return(x[vapply(x, inherits, logical(1), "RLum")])

  x[vapply(x, "class", character(1)) == class[1]]
}

#++++++++++++++++++++++++++++++
#+ .matrix_binning            +
#++++++++++++++++++++++++++++++
#' @title Efficient binning of matricies
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
#' @md
#' @noRd
.matrix_binning <- function(
  m,
  bin_size = 1,
  bin_col = FALSE,
  names = NULL) {

  #@ The only check
  if(!inherits(m, "matrix"))
    stop("[.matrix_binning()] Input is not of class 'matrix'!", call. = FALSE)

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
    rownames(temp_m) <- rep(row_names, length.out = nrow(temp_m))

  }else{
    rownames(temp_m) <- NULL

  }

  ## re-transpose in column mode
  if(bin_col) temp_m <- t(temp_m)

  ## return
  return(temp_m)
}

#++++++++++++++++++++++++++++++
#+ .expand_parameters         +
#++++++++++++++++++++++++++++++
#' @title Expand function parameters of self-call
#'
#' @description For the self-call, the function parameters need to
#' be expended, this was done, so far in a non-consistent way and
#' repeated in every function using the self-call. This functions
#' does it once and for all similar in all functions.
#'
#' **NOTE**: the first argument is never extended due to performance reasons,
#' it might be a very large object
#'
#' @param len [numeric] (**required**): length of the parameter expansion
#'
#' @author Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#'
#' @return [list] with expanded parameters
#'
#' @md
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
  args_new <- as.list(match.call(f_def, f_call, FALSE))[-c(1:2)]

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
      stop(paste0("[",f_call[[1]],"()]: Argument <",
                  names(args[i]), "> missing; with no default!"), call. = FALSE)
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

  return(args)
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
#' @param plot [logical] (*with default*): enables/disables additional control
#' plot
#'
#' @param ... further arguments passed to [stats::density]
#'
#' @author Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
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
#' @md
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
  if(length(peaks_id != 0))
    HPDI <- matrix(m[peaks_id,1], ncol = 2)

  colnames(HPDI) <- c("lower", "upper")
  attr(HPDI, "Probabilty") <- prob

  if(plot){
    xy <- m[m_ind,c(1,2)]
    plot(dens, main = "HPDI (control plot)")
    abline(h = thres, lty = 2)
    if(length(peaks_id != 0)) {
      for(i in seq(1,length(peaks_id),2)) {
        lines(x = m[peaks_id[i]:peaks_id[i + 1], 1],
              y = m[peaks_id[i]:peaks_id[i + 1], 2], col = "red")
      }
    }

  }

  return(HPDI)
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
#'@returns Returns either nothing (no URL) or the file path of the downloaded file
#'
#'@author Sebastian Kreutzer, Insitut of Geography, Heidelberg University, Germany
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
#'@md
#'@noRd
.download_file <- function(
    url,
    destfile = tempfile()
) {

  ## get name of calling function
  caller <- paste0("[", as.character(sys.call(which = -1)[[1]]), "()]")
  out_file_path <- NULL

  ## detect and extract URL
  if(grepl(pattern = "https?\\:\\/\\/", x = url, perl = TRUE)) {
    ## status reports
    message(paste0(caller, " URL detected: ", url), appendLF = TRUE)
    message(paste0(caller, " Attempting download ... "), appendLF = FALSE)

    ## extract URL from string only
    url <- regmatches(x = url, m = regexec(pattern = "https?\\:\\/\\/.+", text = url, perl = TRUE))[[1]]

    ## use internal download
    t <- tryCatch(
      expr = download.file(
        url = url,
        destfile = destfile,
        quiet = TRUE,
        mode = "wb", ## this is needed for Windows otherwise the download does not work
        cacheOK = FALSE,
        method = "auto"),
      warning = function(w) {
        message("FAILED ", appendLF = TRUE)
        return(NULL)
      },
      error = function(e) {
        message("FAILED ", appendLF = TRUE)
        return(NULL)
      })

    if(!is.null(t) && t == 0) {
      message("OK ", appendLF = TRUE)
      out_file_path <- destfile
      unlink(url)

    }

  }

  ## return file path
  return(out_file_path)

}

