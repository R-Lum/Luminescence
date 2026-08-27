#' @rdname calc_MinDose
#' @order 2
#' @export
calc_MaxDose<- function(
  data,
  sigmab,
  plot=TRUE,
  ...
){
  res <- calc_MinDose(data, sigmab, plot = FALSE, invert = TRUE, ...)
  res@originator<- "calc_MaxDose"
  if (plot) {
    try(plot_RLum.Results(res, ...),
        outFile = stdout()) # redirect error messages so they can be silenced
  }

  invisible(res)
}
