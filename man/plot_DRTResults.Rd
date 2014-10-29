\name{plot_DRTResults}
\alias{plot_DRTResults}
\title{Visualise dose recovery test results}
\description{The function provides a standardised plot output for dose recovery test
measurements.}
\usage{plot_DRTResults(values, given.dose, error.range = 10, preheat, 
    boxplot = FALSE, mtext, summary, summary.pos, legend, legend.pos, 
    na.rm = FALSE, ...)}
\arguments{
  \item{values}{\code{\linkS4class{RLum.Results}} or
\code{\link{data.frame}}, (\bold{required}): input values 
containing at least De and De error. To plot more than one data
set in one figure, a \code{list} of the individual data sets
must be provided (e.g. \code{list(dataset.1, dataset.2)}).}
  \item{given.dose}{\code{\link{numeric}}: given dose used for the dose recovery test. If only one given dose is 
provided this given dose is valid for all input data sets (i.e., \code{values} is a list). 
Otherwise a given dose for each input data set has to be provided 
(e.g., \code{given.dose = c(100,200)}).
Note: Unit has to be the same as from the input values (e.g., Seconds or Gray).}
  \item{error.range}{\code{\link{numeric}}: symmetric error range in percent will be shown 
as dashed lines in the plot. Set \code{error.range} to 0 to void  
plotting of error ranges.}
  \item{preheat}{\code{\link{numeric}}: optional vector of preheat temperatures to be
used for grouping the De values. If specified, the temperatures are
assigned to the x-axis.}
  \item{boxplot}{\code{\link{logical}}: optionally plot values, that are grouped by 
preheat temperature as boxplots. Only possible when \code{preheat}  
vector is specified.}
  \item{mtext}{\code{\link{character}}: additional text below the plot title.}
  \item{summary}{\code{\link{character}} (optional): adds numerical output to the plot. 
Can be one or more out of: \code{"n"} (number of samples), \code{"mean"} (mean De 
value), \code{"mean.weighted"} (error-weighted mean), \code{"median"} (median of 
the De values), \code{"sdrel"} (relative standard deviation in 
percent), \code{"sdabs"} (absolute standard deviation), \code{"serel"} (relative 
standard error) and \code{"seabs"} (absolute standard error).}
  \item{summary.pos}{\code{\link{numeric}} or \code{\link{character}} (with default): optional  
position coordinates or keyword (e.g. \code{"topright"}) for the 
statistical summary. Alternatively, the keyword \code{"sub"} may be
specified to place the summary below the plot header. However, this
latter option in only possible if \code{mtext} is not used.}
  \item{legend}{\code{\link{character}} vector (optional): legend content to be added
to the plot.}
  \item{legend.pos}{\code{\link{numeric}} or \code{\link{character}} (with default): optional  
position coordinates or keyword (e.g. \code{"topright"}) for the legend
to be plotted.}
  \item{na.rm}{\code{\link{logical}}: indicating wether \code{NA} values are removed 
before plotting from the input data set}
  \item{\dots}{further arguments and graphical parameters passed to \code{\link{plot}}.}
}
\details{Procedure to test the accuracy of a measurement protocol to reliably 
determine the dose of a specific sample. Here, the natural signal is 
erased and a known laboratory dose administered which is treated as 
unknown. Then the De measurement is carried out and the degree of 
congruence between administered and recovered dose is a measure of the 
protocol's accuracy for this sample.\cr
In the plot the normalised De is shown on the y-axis, i.e. obtained 
De/Given Dose.}
\value{A plot is returned.}
\references{Wintle, A.G., Murray, A.S., 2006. A review of quartz optically stimulated 
luminescence characteristics and their relevance in single-aliquot 
regeneration dating protocols. Radiation Measurements, 41, 369-391.}
\author{Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France),
Michael Dietze, GFZ Potsdam (Germany), \cr
R Luminescence Package Team}
\note{Further data and plot arguments can be added by using the appropiate
R commands.}


\seealso{\code{\link{plot}}}
\examples{

## read example data set and misapply them for this plot type
data(ExampleData.DeValues, envir = environment())

## plot values 
plot_DRTResults(values = ExampleData.DeValues[7:11,], 
given.dose = 2800, mtext = "Example data")

## plot values with legend
plot_DRTResults(values = ExampleData.DeValues[7:11,], 
                given.dose = 2800,
                legend = "Test data set")

## create and plot two subsets with randomised values
x.1 <- ExampleData.DeValues[7:11,]
x.2 <- ExampleData.DeValues[7:11,] * c(runif(5, 0.9, 1.1), 1)

plot_DRTResults(values = list(x.1, x.2),
                given.dose = 2800)

## some more user-defined plot parameters
plot_DRTResults(values = list(x.1, x.2),
                given.dose = 2800,
                pch = c(2, 5),
                col = c("orange", "blue"),
                xlim = c(0, 8),
                ylim = c(0.85, 1.15),
                xlab = "Sample aliquot")

## plot the data with user-defined statistical measures as legend
plot_DRTResults(values = list(x.1, x.2),
                given.dose = 2800,
                summary = c("n", "mean.weighted", "sd"))

## plot the data with user-defined statistical measures as sub-header
plot_DRTResults(values = list(x.1, x.2),
                given.dose = 2800,
                summary = c("n", "mean.weighted", "sd"),
                summary.pos = "sub")

## plot the data grouped by preheat temperatures
plot_DRTResults(values = ExampleData.DeValues[7:11,], 
                given.dose = 2800,
                preheat = c(200, 200, 200, 240, 240))

## plot two data sets grouped by preheat temperatures
plot_DRTResults(values = list(x.1, x.2), 
                given.dose = 2800,
                preheat = c(200, 200, 200, 240, 240))

## plot the data grouped by preheat temperatures as boxplots
plot_DRTResults(values = ExampleData.DeValues[7:11,], 
                given.dose = 2800,
                preheat = c(200, 200, 200, 240, 240),
                boxplot = TRUE)
}

\keyword{dplot}
\section{Function version}{0.1.3 (2014-10-29 17:31:51)}
