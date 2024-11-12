#' Tune data for experimental purpose
#'
#' The error can be reduced and sample size increased for specific purpose.
#'
#' @param data [data.frame] (**required**):
#' input values, structure: data (`values[,1]`) and data error (`values [,2]`)
#' are required
#'
#' @param decrease.error [numeric]:
#' factor by which the error is decreased, ranges between 0 and 1.
#'
#' @param increase.data [numeric]:
#' factor by which the error is decreased, ranges between 0 and `Inf`.
#'
#' @return Returns a [data.frame] with tuned values.
#'
#' @note
#' You should not use this function to improve your poor data set!
#'
#' @section Function version: 0.5.0
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
#' #plot_AbanicoPlot(data = tune_Data(x, increase.data = 2) ,
#' #                summary = c("n", "mean"))
#'
#' @md
#' @export
tune_Data <- function(
  data,
  decrease.error = 0,
  increase.data = 0
) {
  .set_function_name("tune_Data")
  on.exit(.unset_function_name(), add = TRUE)

  if(missing(decrease.error) == FALSE) {

    error.rel <- data[,2] / data[,1]

    data[,2] <- error.rel * (1 - decrease.error) * data[,1]
  }

  if(missing(increase.data) == FALSE) {

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

    e.merge <- e.merge[order(x.merge)]
    x.merge <- x.merge[order(x.merge)]

    data.out <- data.frame(x.merge, e.merge)

    names(data.out) <- names(data)

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
