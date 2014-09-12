\name{plot_RLum.Analysis}
\alias{plot_RLum.Analysis}
\title{Plot function for an RLum.Analysis S4 class object}
\description{The function provides a standardised plot output for curve data of an 
RLum.Analysis S4 class object}
\usage{plot_RLum.Analysis(object, nrows = 3, ncols = 2, ...)}
\arguments{
  \item{object}{\code{\linkS4class{RLum.Analysis}} (\bold{required}): S4 object of class 
\code{RLum.Analysis}}
  \item{nrows}{\code{\link{integer}} (with default): sets number of rows for plot output}
  \item{ncols}{\code{\link{integer}} (with default): sets number of columns for plot output}
  \item{\dots}{further arguments and graphical parameters will be passed to the \code{plot} function.}
}
\details{The function produces a multiple plot output. 
A file output is recommended (e.g., \code{\link{pdf}}).}
\value{Returns multiple plots.}
\references{#}
\author{Sebastian Kreutzer, JLU Giessen (Germany), \cr
R Luminescence Package Team}
\note{Not all arguments available for \code{\link{plot}} will be passed!
Only plotting of \code{RLum.Data.Curve} and \code{RLum.Data.Spectrum}
objects are currently supported. }


\seealso{\code{\link{plot}}, \code{\link{plot_RLum}}, 
\code{\link{plot_RLum.Data.Curve}}}
\examples{

###load data
data(ExampleData.BINfileData, envir = environment())

##convert values for position 1
temp <- Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data, pos=1)

##plot
plot_RLum.Analysis(temp)

}

\keyword{aplot}
