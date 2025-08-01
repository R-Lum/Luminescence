##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++##
##                      METHODS FOR S3 GENERICS                                 ##
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++##

##CAUTION NOTE:
##(1) Please DO NOT access to the S4 objects by using the slots this causes inconsistent
## behaviour, please use the corresponding RLum-methods instead!
##
##(2) Especially, please DO NOT include S3-methods for which no S4-method is implemented! Especially
##for coercing.
##
##(3) Finally, what ever you want to implement, check whether a S4-method exists, it should
##be just passed to this methods, not the opposite, otherwise this will yield in undesired behaviour
##
##TODO: For this S3 generics so far no proper documentation exists ... we should consider
##to provide an overview within a separate document, as it becomes otherwise rather
##complicated for beginners to work with the documentation.
##


##  INTRODUCED WITH 0.5.0 ##

#' methods_RLum
#'
#' Methods for S3-generics implemented for the package 'Luminescence'.
#' This document summarises all implemented S3-generics. The name of the function
#' is given before the first dot, after the dot the name of the object that is
#' supported by this method is given, e.g. `plot.RLum.Data.Curve` can be called
#' by `plot(object, ...)`, where `object` is the `RLum.Data.Curve` object.
#'
#' The term S3-generics sounds complicated, however, it just means that something
#' has been implemented in the package to increase the usability for users new
#' in R and who are not familiar with the underlying `RLum`-object structure of
#' the package. The practical outcome is that operations and functions presented
#' in standard books on R can be used without knowing the specifics of the R
#' package `'Luminescence'`. For examples see the example section.
#'
#' @param x [RLum-class] or [Risoe.BINfileData-class] (**required**):
#' input object
#'
#' @param object [RLum-class] (**required**):
#' input object
#'
#' @param y [integer] (*optional*):
#' the row index of the matrix, data.frame
#'
#' @param z [integer] (*optional*):
#' the column index of the matrix, data.frame
#'
#' @param i [character] (*optional*):
#' name of the wanted record type or data object or row in the `RLum.Data.Curve` object
#'
#' @param j [integer] (*optional*):
#' column of the data matrix in the `RLum.Data.Curve` object
#'
#' @param value [numeric] (**required**):
#' numeric value which replace the value in the `RLum.Data.Curve` object
#'
#' @param drop [logical] (*with default*):
#' keep object structure or drop it
#'
#' @param subset `[subset]` [expression] (**required**):
#' logical expression indicating elements or rows to keep, this function works
#' in [Risoe.BINfileData-class] objects like [subset.data.frame], but takes care
#' of the object structure. Works also on [RLum.Analysis-class] objects.
#'
#' @param row.names [logical] (*with default*):
#' enable/disable row names (`as.data.frame`).
#'
#' @param recursive [logical] (*with default*):
#' enable/disable further sub-setting (`unlist`).
#'
#' @param optional [logical] (*with default*):
#' logical. If TRUE, setting row names and converting column names
#' (to syntactic names: see make.names) is optional (see [base::as.data.frame])
#'
#' @param ... further arguments that can be passed to the method
#'
#' @note
#' `methods_RLum` are not really new functions, everything given here are mostly just
#' surrogates for existing functions in the package.
#'
#' @examples
#'
#' ##load example data
#' data(ExampleData.RLum.Analysis, envir = environment())
#'
#' @keywords internal
#' @name methods_RLum
NULL

# plot()------------------------------------------------------------------------

#' @rdname methods_RLum
#' @method plot list
#' @export
plot.list <- function(x, y, ...) {
  if (all(sapply(x, function(x) inherits(x, "RLum")))) {
    plot_RLum(object = x, ...)
  }
  else {
    if (missing(y))
      y <- NULL
    graphics::plot.default(x, y, ...)
  }
}

#' @rdname methods_RLum
#' @method plot RLum.Results
#' @export
plot.RLum.Results <- function(x, y, ...) plot_RLum(object = x, ...)

#' @rdname methods_RLum
#' @method plot RLum.Analysis
#' @export
plot.RLum.Analysis <- function(x, y, ...) plot_RLum(object = x, ...)

#' @rdname methods_RLum
#' @method plot RLum.Data.Curve
#' @export
plot.RLum.Data.Curve <- function(x, y, ...) plot_RLum(object = x, ...)

#' @rdname methods_RLum
#' @method plot RLum.Data.Spectrum
#' @export
plot.RLum.Data.Spectrum <- function(x, y, ...) plot_RLum(object = x, ...)

#' @rdname methods_RLum
#' @method plot RLum.Data.Image
#' @export
plot.RLum.Data.Image <- function(x, y, ...) plot_RLum(object = x, ...)

#' @rdname methods_RLum
#' @method plot Risoe.BINfileData
#' @export
plot.Risoe.BINfileData <- function(x, y, ...) plot_Risoe.BINfileData(data = x, ...)

# hist() -----------------------------------------------------------------------

#' @rdname methods_RLum
#' @export
hist.RLum.Results <- function(x, ...) plot_Histogram(data = x, ...)

#' @rdname methods_RLum
#' @export
hist.RLum.Data.Image <- function(x, ...) hist(x = get_RLum(x), ...)

#' @rdname methods_RLum
#' @export
hist.RLum.Data.Curve <- function(x, ...) hist(as(get_RLum(x),"matrix")[,2])

#' @rdname methods_RLum
#' @export
hist.RLum.Analysis <- function(x, ...) lapply(1:length_RLum(x), function(z){
  hist(as(get_RLum(x, record.id = z, ...),"matrix")[,2])})

# summary() --------------------------------------------------------------------

#' @rdname methods_RLum
#' @method summary RLum.Results
#' @export
summary.RLum.Results <- function(object, ...) get_RLum(object = object, ...)

#' @rdname methods_RLum
#' @method summary RLum.Analysis
#' @export
summary.RLum.Analysis <- function(object, ...) lapply(object@records, function(x) summary(x@data))

#' @rdname methods_RLum
#' @method summary RLum.Data.Image
#' @export
summary.RLum.Data.Image <- function(object, ...) summary(object@data)

# summary.RLum.Data.Spectrum <- function(object, ...)

#' @rdname methods_RLum
#' @method summary RLum.Data.Curve
#' @export
summary.RLum.Data.Curve <- function(object, ...) summary(object@data, ...)

# subset() ---------------------------------------------------------------------

#' @rdname methods_RLum
#' @method subset Risoe.BINfileData
#'
#' @param records.rm [subset] [logical] (*with default*):
#' remove records from data set, can be disabled, to just set the column `SET` to `TRUE` or `FALSE`
#'
#' @export
subset.Risoe.BINfileData <- function(x, subset, records.rm = TRUE, ...) {
  .set_function_name("subset.Risoe.BINfileData")
  on.exit(.unset_function_name(), add = TRUE)

  if(length(list(...)))
    .throw_warning("Argument not supported and skipped:", names(list(...)))

  ##select relevant rows
  sel <- tryCatch(eval(
    expr = substitute(subset),
    envir = x@METADATA,
    enclos = parent.frame()
  ),
  error = function(e) {
    .throw_error("\nInvalid subset options, valid terms are: ",
                 .collapse(names(x@METADATA)))
  })

  ##probably everything is FALSE now?
  if (records.rm) {
    if (any(sel)) {
      x@METADATA <- x@METADATA[sel, ]
      x@DATA <- x@DATA[sel]
      x@METADATA[["ID"]] <- 1:length(x@METADATA[["ID"]])
      return(x)

    } else{
      return(NULL)
    }
  }else{
    x@METADATA[["SEL"]] <- sel
    return(x)
  }
}

#' @rdname methods_RLum
#' @method subset RLum.Analysis
#' @export
subset.RLum.Analysis <- function(x, subset = NULL, ...) {
  do.call(get_RLum, list(object = x, drop = FALSE, subset = substitute(subset), env = parent.frame())) }

# bin() ------------------------------------------------------------------------
#' @rdname methods_RLum
#' @export
bin <- function(x, ...) {
  UseMethod("bin")
}

#' @rdname methods_RLum
#' @export
bin.RLum.Data.Curve <- function(x, bin_size = 2, ...) bin_RLum.Data(x, bin_size = bin_size)

#' @rdname methods_RLum
#' @export
bin.RLum.Data.Spectrum <- function(x, bin_size.row = 1, bin_size.col = 1, ...){
  bin_RLum.Data(x, bin_size.row = bin_size.row, bin_size.col = bin_size.col)
}

# length() ---------------------------------------------------------------------

#' @rdname methods_RLum
#' @export
length.RLum.Results <- function(x, ...) length_RLum(x)

#' @rdname methods_RLum
#' @export
length.RLum.Analysis <- function(x, ...) length_RLum(x)

#' @rdname methods_RLum
#' @export
length.RLum.Data.Curve <- function(x, ...) length_RLum(x)

#' @rdname methods_RLum
#' @export
length.Risoe.BINfileData <- function(x, ...) length(x@METADATA$ID)

# dim() ------------------------------------------------------------------------

#' @rdname methods_RLum
#' @export
dim.RLum.Data.Curve <- function(x) dim(as(x, "matrix"))

#' @rdname methods_RLum
#' @export
dim.RLum.Data.Spectrum <- function(x) dim(as(x, "matrix"))

# rep() -------------------------------------------------------------------

#' @rdname methods_RLum
#' @export
rep.RLum <- function(x, ...) replicate_RLum(x, ...)


# names() -----------------------------------------------------------------

#' @rdname methods_RLum
#' @export
names.RLum.Data.Curve <- function(x, ...) names_RLum(x)

#' @rdname methods_RLum
#' @export
names.RLum.Data.Spectrum <- function(x, ...) names_RLum(x)

#' @rdname methods_RLum
#' @export
names.RLum.Data.Image <- function(x, ...) names_RLum(x)

#' @rdname methods_RLum
#' @export
names.RLum.Analysis <- function(x, ...) names_RLum(x)

#' @rdname methods_RLum
#' @export
names.RLum.Results <- function(x, ...) names_RLum(x)

#' @rdname methods_RLum
#' @export
names.Risoe.BINfileData <- function(x)  as.character(x@METADATA$LTYPE)


# row.name() --------------------------------------------------------------

#' @rdname methods_RLum
#' @export
row.names.RLum.Data.Spectrum <- function(x, ...) rownames(as(x, "matrix"))


# as.data.frame() ---------------------------------------------------------

#' @rdname methods_RLum
#' @export
as.data.frame.RLum.Data.Curve <- function(x, row.names = NULL, optional = FALSE, ...) as(x, "data.frame")

#' @rdname methods_RLum
#' @export
as.data.frame.RLum.Data.Spectrum <- function(x,  row.names = NULL, optional = FALSE, ...) as(x, "data.frame")
# for RLum.Results ... makes no sense and may yield in unpredictable behaviour

#' @rdname methods_RLum
#' @export
as.data.frame.Risoe.BINfileData <- function(x,  row.names = NULL, optional = FALSE, ...) {

  ## set matrix
  m <- matrix(NA, ncol = max(lengths(x@DATA)), nrow = length(x@DATA))

  ## rename columns
  colnames(m) <- paste0("C",1:ncol(m))

  ## fill matrix
  for(i in 1:length(x@DATA)) m[i,1:length(x@DATA[[i]])] <- x@DATA[[i]]

  ##convert to data.frame and bind
  cbind(x@METADATA, as.data.frame(m))
}


# as.list() ---------------------------------------------------------------

#' @rdname methods_RLum
#' @export
as.list.RLum.Results <- function(x, ...) as(x, "list")

#' @rdname methods_RLum
#' @export
as.list.RLum.Data.Curve <- function(x, ...) as(x, "list")

#' @rdname methods_RLum
#' @export
as.list.RLum.Data.Image <- function(x, ...) as(x, "list")

#' @rdname methods_RLum
#' @export
as.list.RLum.Data.Spectrum <- function(x, ...) as(x, "list")

#' @rdname methods_RLum
#' @export
as.list.RLum.Analysis <- function(x, ...) as(x, "list")


# as.matrix() -------------------------------------------------------------

#' @rdname methods_RLum
#' @export
as.matrix.RLum.Data.Curve <- function(x, ...) as(x, "matrix")

#' @rdname methods_RLum
#' @export
as.matrix.RLum.Data.Spectrum <- function(x, ...) as(x, "matrix")

#' @rdname methods_RLum
#' @export
as.matrix.RLum.Data.Image <- function(x, ...) as(x, "matrix")
# for RLum.Results ... makes no sense and may yield in unpredictable behaviour


# is() --------------------------------------------------------------------

#For this function no S4 method was written, as this would come at the cost of performance and
#is totally unnecessary

#' @rdname methods_RLum
#' @export
is.RLum <- function(x, ...) is(x, "RLum")

#' @rdname methods_RLum
#' @export
is.RLum.Data <- function(x, ...) is(x, "RLum.Data")

#' @rdname methods_RLum
#' @export
is.RLum.Data.Curve <- function(x, ...) is(x, "RLum.Data.Curve")

#' @rdname methods_RLum
#' @export
is.RLum.Data.Spectrum <- function(x, ...) is(x, "RLum.Data.Spectrum")

#' @rdname methods_RLum
#' @export
is.RLum.Data.Image <- function(x, ...) is(x, "RLum.Data.Image")

#' @rdname methods_RLum
#' @export
is.RLum.Analysis <- function(x, ...) is(x, "RLum.Analysis")

#' @rdname methods_RLum
#' @export
is.RLum.Results <- function(x, ...) is(x, "RLum.Results")

# merge() -----------------------------------------------------------------

#' @rdname methods_RLum
#' @export
merge.RLum <- function(x, y, ...) merge_RLum(append(list(...), values = c(x, y)))


# unlist() ----------------------------------------------------------------

#' @rdname methods_RLum
#' @method unlist RLum.Analysis
#' @export
unlist.RLum.Analysis <- function(x, recursive = TRUE, ...){

  temp <- get_RLum(object = x, recursive = recursive, ... )
  if(recursive){
    unlist(lapply(1:length(temp), function(x){
      get_RLum(temp)
    }), recursive = FALSE)

  }else{
    return(temp)
  }
}


# `+` ---------------------------------------------------------------------

#' @rdname methods_RLum
#'
#' @examples
#'
#' ##combine curve is various ways
#' curve1 <- IRSAR.RF.Data[[1]]
#' curve2 <-  IRSAR.RF.Data[[1]]
#' curve1 + curve2
#' curve1 - curve2
#' curve1 / curve2
#' curve1 * curve2
#'
#' @export
`+.RLum.Data.Curve` <- function(x, y) merge_RLum(list(x, y), merge.method = "sum")

#' @rdname methods_RLum
#' @export
`+.RLum.Data.Spectrum` <- function(x, y) merge_RLum(list(x, y), merge.method = "sum")


# `-` ---------------------------------------------------------------------

#' @rdname methods_RLum
#' @export
`-.RLum.Data.Curve` <- function(x, y) merge_RLum(list(x, y), merge.method = "-")

#' @rdname methods_RLum
#' @export
`-.RLum.Data.Spectrum` <- function(x, y) merge_RLum(list(x, y), merge.method = "-")


# `*` ---------------------------------------------------------------------

#' @rdname methods_RLum
#' @export
`*.RLum.Data.Curve` <- function(x, y) merge_RLum(list(x, y), merge.method = "*")

#' @rdname methods_RLum
#' @export
`*.RLum.Data.Spectrum` <- function(x, y) merge_RLum(list(x, y), merge.method = "*")


# `/` ---------------------------------------------------------------------

#' @rdname methods_RLum
#' @export
`/.RLum.Data.Curve` <- function(x, y) merge_RLum(list(x, y), merge.method = "/")

#' @rdname methods_RLum
#' @export
`/.RLum.Data.Spectrum` <- function(x, y) merge_RLum(list(x, y), merge.method = "/")


# `[` ---------------------------------------------------------------------

#' @rdname methods_RLum
#' @export
`[.RLum.Data.Curve` <- function(x,y,z, drop = TRUE) {as(x, "matrix")[y,z, drop = drop]}

#' @rdname methods_RLum
#' @export
`[.RLum.Data.Spectrum` <- function(x,y,z, drop = TRUE) {as(x, "matrix")[y,z, drop = drop]}

#' @rdname methods_RLum
#' @export
`[.RLum.Data.Image` <- function(x,y,z, drop = TRUE) {as(x, "matrix")[y,z, drop = drop]}

#' @rdname methods_RLum
#' @export
`[.RLum.Analysis` <- function(x, i, drop = FALSE) {
  if (is(i, "character")) {
    get_RLum(x, recordType = i, drop = drop)

  } else{
    get_RLum(x, record.id = i, drop = drop)
  }
}

#' @rdname methods_RLum
#' @export
`[.RLum.Results` <- function(x, i, drop = TRUE) {get_RLum(x, data.object = i, drop = drop)}


# `[<-` -------------------------------------------------------------------

#' @rdname methods_RLum
#' @export
`[<-.RLum.Data.Curve` <- function(x, i, j, value){
  x@data[i,j] <- value #this is without any S4-method, but otherwise the overhead it too high
  return(x)
}


# `[[` --------------------------------------------------------------------

#' @rdname methods_RLum
#' @export
`[[.RLum.Analysis` <- function(x, i) {
  if (is(i, "character")) {
    get_RLum(x, recordType = i)

  } else{
    get_RLum(x, record.id = i)

  }
}

#' @rdname methods_RLum
#' @export
`[[.RLum.Results` <- function(x, i) {get_RLum(x, data.object = i)}


# `$` ---------------------------------------------------------------------

#' @rdname methods_RLum
#' @export
`$.RLum.Data.Curve` <- function(x, i) {get_RLum(x, info.object = i)}

#' @rdname methods_RLum
#'
#' @examples
#'
#' ##`$` access curves
#' IRSAR.RF.Data$RF
#'
#' @export
`$.RLum.Analysis` <- function(x, i) {get_RLum(x, recordType = i)}

#' @rdname methods_RLum
#' @export
`$.RLum.Results` <- function(x, i) {get_RLum(x, data.object = i)}
