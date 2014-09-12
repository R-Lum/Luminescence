\name{plot_Histogram}
\alias{plot_Histogram}
\title{Plot a histogram with a separate error plot}
\description{Function plots a predefined histogram with an accompanying error plot as suggested by 
Rex Galbraith at the UK LED in Oxford 2010.}
\usage{plot_Histogram(values, na.exclude = TRUE, mtext, cex.global, 
    breaks, se, rug, normal_curve, summary, summary.pos, colour, 
    ...)}
\arguments{
  \item{values}{\code{\link{data.frame}} or \code{\linkS4class{RLum.Results}} object (required): 
for \code{data.frame}: two columns: De 
(\code{values[,1]}) and De error (\code{values[,2]})}
  \item{na.exclude}{\code{\link{logical}} (with default): excludes \code{NA} values from the 
data set prior to any further operations.}
  \item{mtext}{\code{\link{character}} (optional): further sample information 
(\link{mtext}).}
  \item{cex.global}{\code{\link{numeric}} (with default): global scaling factor.}
  \item{breaks}{(with default): sets breakpoints for histogram. Works as in \link{hist}.}
  \item{se}{\code{\link{logical}} (optional): plots standard error points over the 
histogram, default is \code{FALSE}.}
  \item{rug}{\code{\link{logical}} (optional): adds rugs to the histogram, default is 
\code{TRUE}.}
  \item{normal_curve}{\code{\link{logical}} (with default): adds a normal curve to the histogram. 
Mean and sd are calculated from the input data. More see details section.}
  \item{summary}{\code{\link{character}} (optional): adds numerical output to the plot. Can 
be one or more out of: \code{"n"} (number of samples), \code{"mean"} (mean De value), 
\code{"median"} (median of the De values), \code{"kdemax"} (maximum value of probability 
density function), \code{"sdrel"} (relative standard deviation), \code{"sdabs"} 
(absolute standard deviation), \code{"serel"} (relative standard error) and 
\code{"seabs"} (absolute standard deviation).}
  \item{summary.pos}{\code{\link{numeric}} (with default): optional position coordinates for 
the statistical summary. Y-coordinate refers to the right hand y-axis.}
  \item{colour}{\code{\link{numeric}} or \link{character} (with default): optional vector 
of length 4 which specifies the colours of the following plot items in 
exactly this order: histogram bars, rug lines, normal distribution curve 
and standard error points\cr
(e.g., \code{c("grey", "black", "red", "grey")}).}
  \item{\dots}{further arguments and graphical parameters passed to \code{\link{plot}}. 
If y-axis labels are provided, these must be specified as a vector of 
length 2 since the plot features two axes (e.g. \code{ylab = c("axis 
label 1", "axis label 2")}). Y-axes limits (\code{ylim}) must be
provided as vector of length four, with the first two elements specifying
the left axes limits and the latter two elements giving the right axis
limits.}
}
\details{If the normal curve is added, the y-axis in the histogram will show 
the probability density. }


\author{Michael Dietze (GFZ Potsdam), \cr
Sebastian Kreutzer, JLU Giessen (Germany), \cr
R Luminescence Package Team}
\note{The input data is not restricted to a special type.}


\seealso{\code{\link{hist}}, \code{\link{plot}}}
\examples{
## load data
data(ExampleData.DeValues, envir = environment())
ExampleData.DeValues <- 
  Second2Gray(values = ExampleData.DeValues, dose_rate = c(0.0438,0.0019))

## plot histogram the easiest way
plot_Histogram(ExampleData.DeValues)

## plot histogram with some more modifications
plot_Histogram(ExampleData.DeValues, 
               rug = TRUE, 
               normal_curve = TRUE, 
               cex.global = 0.9, 
               pch = 2,
               colour = c("grey", "black", "blue", "green"),
               summary = c("n", "mean", "sdrel"),
               summary.pos = "topleft",
               main = "Histogram of De-values",
               mtext = "Example data set", 
               ylab = c(expression(paste(D[e], " distribution")),
                        "Error"),
               xlim = c(100, 250),
               ylim = c(0, 0.08, 50, 200))
}
\section{Function version}{0.4.1 (2014-07-28 16:32:26)}
