\name{plot_RLum.Results}
\alias{plot_RLum.Results}
\title{Plot function for an RLum.Results S4 class object}
\description{The function provides a standardised plot output for data of an 
RLum.Results S4 class object}
\usage{plot_RLum.Results(data, single = TRUE, ...)}
\arguments{
  \item{data}{\code{\linkS4class{RLum.Results}} (\bold{required}): S4 object of class 
\code{RLum.Results}}
  \item{single}{\code{\link{logical}} (with default): single plot output (\code{TRUE/FALSE}) to allow 
for plotting the results in as few plot windows as possible.}
  \item{\dots}{further arguments and graphical parameters will be passed to the \code{plot} function.}
}
\details{The function produces a multiple plot output. 
A file output is recommended (e.g., \code{\link{pdf}}).}
\value{Returns multiple plots.}
\references{#}
\author{Christoph Burow, University of Cologne (Germany) \cr
R Luminescence Package Team}
\note{Not all arguments available for \code{\link{plot}} will be passed!
Only plotting of \code{RLum.Results} objects are supported. }


\seealso{\code{\link{plot}}, \code{\link{plot_RLum}}, }
\examples{

###load data
data(ExampleData.DeValues, envir = environment())

# apply the un-logged minimum age model
mam<- calc_MinDose(data = ExampleData.DeValues$CA1, sigmab = 0.2, log = TRUE, plot = FALSE)

##plot
plot_RLum.Results(mam)

# estimate the number of grains on an aliquot
grains<- calc_AliquotSize(grain.size = c(100,150), sample.diameter = 1, plot = FALSE)

##plot
plot_RLum.Results(grains)

}

\keyword{aplot}
\section{Function version}{0.1 (2015-03-04 13:24:51)}
