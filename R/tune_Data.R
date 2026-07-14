#' Tune data for experimental purpose
#'
#' The error can be reduced and sample size increased for specific purpose.
#'
#' @param data [data.frame] (**required**):
#' input values, structure: values (`data[, 1]`) and error (`data[, 2]`)
#' are required.
#'
#' @param decrease.error [numeric]:
#' factor by which the error is decreased, ranges between 0 and 1.
#'
#' @param increase.data [numeric]:
#' factor by which the error is decreased, ranges between 0 and 1000.
#'
#' @return Returns a 2-column [data.frame] with tuned values.
#'
#' @note
#' You should not use this function to improve your poor data set!
#'
#' @section Function version: 0.5.1
#'
#' @author
#' Michael Dietze, GFZ Potsdam (Germany)
#'
#' @keywords manip
#'
#' @examples
#'
#' ## load example data set
#' data(ExampleData.DeValues, envir = environment())
#' x <- ExampleData.DeValues$CA1
#'
#' ## plot original data
#' plot_AbanicoPlot(data = x,
#'                  summary = c("n", "mean"))
#'
#' ## decrease error by 10 %
#' plot_AbanicoPlot(data = tune_Data(x, decrease.error = 0.1),
#'                  summary = c("n", "mean"))
#'
#' ## increase sample size by 200 %
#' #plot_AbanicoPlot(data = tune_Data(x, increase.data = 2),
#' #                summary = c("n", "mean"))
#'
#' @export
tune_Data <- function(
  data,
  decrease.error = 0,
  increase.data = 0
) {
  .set_function_name("tune_Data")
  on.exit(.unset_function_name(), add = TRUE)

  .validate_class(data, "data.frame")
  if (nrow(data) < 2)
    .throw_error("'data' should have at least two rows")
  if (ncol(data) < 2)
    .throw_error("'data' should have at least two columns")
  .validate_nonnegative_scalar(decrease.error)
  if (decrease.error > 1)
    .throw_error("'decrease.error' should be at most 1")
  .validate_nonnegative_scalar(increase.data)
  if (increase.data > 1000)
    .throw_error("'increase.data' should be at most 1000")

  if (decrease.error > 0) {
    error.rel <- data[,2] / data[,1]
    data[,2] <- error.rel * (1 - decrease.error) * data[,1]
  }

  if (increase.data > 0) {
    n <- round(x = increase.data * 100,
               digits = 0)

    i.new <- sample(x = 1:nrow(data),
                    size = n,
                    replace = TRUE)

    x.new <- rnorm(n = n,
                   mean = data[i.new, 1],
                   sd = data[i.new, 2])

    e.new <- rnorm(n = n,
                   mean = data[i.new, 2],
                   sd = data[i.new, 2] * 0.05)

    x.merge <- c(data[,1], x.new)
    e.merge <- c(data[,2], e.new)

    ord <- order(x.merge)
    e.merge <- e.merge[ord]
    x.merge <- x.merge[ord]

    data.out <- data.frame(x.merge, e.merge)

    names(data.out) <- names(data)[1:2]

    data <- data.out
  }

  info <- Sys.info()
  user <- info[length(info)]
  os <- info[1]

  .throw_warning("Dear ",
                user,
                ", these activities on your ",
                os,
                " machine have been tracked and will be submitted to ",
                "the R.Lum data base. Cheating does not pay off! [",
                Sys.time(),
                "]",
                sep = "")

  return(data)
}
