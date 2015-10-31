##################################################################################
##                      METHODS FOR S3 GENERICS                                 ##
##################################################################################

##CAUTION NOTE: Please DO NOT access to the S4 objects by using the slots
##this causes inconsistent behaviour, please use the correspong RLum-methods
##instead!
##TODO

## -------------------- INTRODUCED IN 0.5.0 ----------------------- ##

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
hist.RLum.Data.Image <- function(x, ...) hist(x =get_RLum(x)@data@values, ...)
#' @export
hist.RLum.Data.Curve <- function(x, ...) hist(as(get_RLum(x),"matrix")[,2])
#' @export
hist.RLum.Analysis <- function(x, ...) lapply(1:length_RLum(x), function(z){
  hist(as(get_RLum(x, record.id = z, ...),"matrix")[,2])})

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
length.RLum.Results <- function(x, ...) length_RLum(x)
#' @export
length.RLum.Analysis <- function(x, ...) length_RLum(x)
#' @export
length.RLum.Data.Curve <- function(x, ...) length_RLum(x)
#' @export
length.Risoe.BINfileData <- function(x, ...) length(x@METADATA$ID)

# methods for generic: dim()
#' @export
dim.RLum.Data.Curve <- function(x) dim(as(x, "matrix"))
#' @export
dim.RLum.Data.Spectrum <- function(x) dim(as(x, "matrix"))

# methods for generic: names()
#' @export
names.RLum.Data.Curve <- function(x, ...) names_RLum(x)
#' @export
names.RLum.Data.Spectrum <- function(x, ...) names_RLum(x)
#' @export
names.RLum.Data.Image <- function(x, ...) names_RLum(x)
#' @export
names.RLum.Analysis <- function(x, ...) names_RLum(x)
#' @export
names.RLum.Results <- function(x, ...) names_RLum(x)
#' @export
names.Risoe.BINfileData <- function(x)  as.character(x@METADATA$LTYPE)

# methods for generic: row.names()
#' @export
row.names.RLum.Data.Spectrum <- function(x, ...) rownames(as(x, "matrix"))

# methods for generic: as.data.frame()
#' @export
as.data.frame.RLum.Results <- function(x, row.names = NULL, optional = FALSE, slot = "summary", ...) get_RLum(x, slot)
#' @export
as.data.frame.RLum.Data.Curve <- function(x, row.names = NULL, optional = FALSE, ...) as(x, "data.frame")
#' @export
as.data.frame.RLum.Data.Spectrum <- function(x,  row.names = NULL, optional = FALSE, ...) as(x, "data.frame")

# methods for generic: as.list()
#' @export
as.list.RLum.Results <- function(x, slot = "summary", ...) as.list(get_RLum(x, slot))
#' @export
as.list.RLum.Data.Curve <- function(x, ...) as.list(as.data.frame(get_RLum(x)))

# methods for generic: as.matrix()
#' @export
as.matrix.RLum.Results <- function(x, slot = "summary", ...) as.matrix(get_RLum(x, slot))
#' @export
as.matrix.RLum.Data.Curve <- function(x, ...) as(x, "matrix")
#' @export
as.matrix.RLum.Data.Spectrum <- function(x, ...) as(x, "matrix")

# methods for generic: merge()
#' @export
merge.RLum <- function(x, y, ...) merge_RLum(append(list(...), values = c(x, y)))

# methods for generic: `+`
#' @export
`+.RLum.Data.Curve` <- function(x, y) merge_RLum(list(x, y), merge.method = "sum")

# methods for generic: `-`
#' @export
`-.RLum.Data.Curve` <- function(x, y) merge_RLum(list(x, y), merge.method = "-")

# methods for generic: `*`
#' @export
`*.RLum.Data.Curve` <- function(x, y) merge_RLum(list(x, y), merge.method = "*")

# methods for generic: `/`
#' @export
`/.RLum.Data.Curve` <- function(x, y) merge_RLum(list(x, y), merge.method = "/")

# methods for generic: `[`
#' @export
`[.RLum.Data.Curve` <- function(x,y,z, drop = TRUE) {as(x, "matrix")[y,z, drop = drop]}

#' @export
`[.RLum.Data.Spectrum` <- function(x,y,z, drop = TRUE) {as(x, "matrix")[y,z, drop = drop]}

#' @export
`[.RLum.Data.Image` <- function(x,y,z, drop = TRUE) {as(x, "matrix")[y,z, drop = drop]}

#' @export
`[.RLum.Analysis` <- function(x, i, drop = FALSE) {
  if (is(i, "character")) {
    get_RLum(x, recordType = i, drop = drop)

  } else{
    get_RLum(x, record.id = i, drop = drop)

  }
}

#' @export
`[.RLum.Results` <- function(x, i, drop = TRUE) {get_RLum(x, data.object = i, drop = drop)}

# methods for generic: `[[`
#' @export
`[[.RLum.Analysis` <- function(x, i) {
  if (is(i, "character")) {
    get_RLum(x, recordType = i)

  } else{
    get_RLum(x, record.id = i)

  }
}

#' @export
`[[.RLum.Results` <- function(x, i) {get_RLum(x, data.object = i)}

# methods for generic: `$`
#' @export
`$.RLum.Analysis` <- function(x, i) {get_RLum(x, recordType = i)}

#' @export
`$.RLum.Results` <- function(x, i) {get_RLum(x, data.object = i)}
