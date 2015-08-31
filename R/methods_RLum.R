##################################################################################
##                      METHODS FOR S3 GENERICS                                 ##
##################################################################################

## -------------------- INTRODUCE IN 0.5.0 ----------------------- ##

# methods for generic: plot()
#' @export
plot.RLum.Results <- function(x, y, ...) plot_RLum(object = x, ...)
#' @export
plot.RLum.Analysis <- function(x, y, ...) plot_RLum(object = x, ...)
#' @export
plot.RLum.Data.Curve <- function(x, y, ...) plot_RLum(object = x, ...)
#' @export
plot.RLum.Data.Spectrum <- function(x, y, ...) plot_RLum(object = x, ...)
#' @export
plot.RLum.Data.Image <- function(x, y, ...) plot_RLum(object = x, ...)
#' @export
plot.Risoe.BINfileData <- function(x, y, ...) plot_Risoe.BINfileData(BINfileData = x, ...)

# methods for generic: hist()
#' @export
hist.RLum.Results <- function(x, ...) plot_Histogram(data = x, ...)
#' @export
hist.RLum.Data.Image <- function(x, ...) hist(x = x@data@data@values, ...)
#' @export
hist.RLum.Data.Curve <- function(x, ...) hist(x= x@data[ ,2])
#' @export
hist.RLum.Analysis <- function(x, ...) lapply(x@records, function(x) hist(x@data[ ,2], ...))

# methods for generic: summary()
#' @export
summary.RLum.Results <- function(object, ...) get_RLum(object = object, ...)
#' @export
summary.RLum.Analysis <- function(object, ...) lapply(object@records, function(x) summary(x@data))
#' @export
summary.RLum.Data.Image <- function(object, ...) summary(object@data@data@values)
# summary.RLum.Data.Spectrum <- function(object, ...)
#' @export
summary.RLum.Data.Curve <- function(object, ...) summary(object@data, ...)

# methods for generic: length()
#' @export
length.RLum.Results <- function(x, ...) nrow(x@data$data)
#' @export
length.RLum.Analysis <- function(x, ...) length_RLum(x)
#' @export
length.RLum.Data.Curve <- function(x, ...) nrow(x@data)
#' @export
length.Risoe.BINfileData <- function(x, ...) length(x@METADATA$ID)

# methods for generic: as.data.frame()
#' @export
as.data.frame.RLum.Results <- function(x, row.names = NULL, optional = FALSE, slot = "summary", ...) get_RLum(x, slot)
#' @export
as.data.frame.RLum.Data.Curve <- function(x, row.names = NULL, optional = FALSE, ...) as.data.frame(get_RLum(x))

# methods for generic: as.list()
#' @export
as.list.RLum.Results <- function(x, slot = "summary", ...) as.list(get_RLum(x, slot))
#' @export
as.list.RLum.Data.Curve <- function(x, ...) as.list(as.data.frame(get_RLum(x)))

# methods for generic: as.matrix()
#' @export
as.matrix.RLum.Results <- function(x, slot = "summary", ...) as.matrix(get_RLum(x, slot))
#' @export
as.matrix.RLum.Data.Curve <- function(x, ...) as.matrix(get_RLum(x))

# methods for generic: merge()
#' @export
merge.RLum <- function(x, y, ...) merge_RLum(append(list(...), values = c(x, y)))