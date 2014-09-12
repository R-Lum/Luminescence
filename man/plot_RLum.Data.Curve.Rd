\name{plot_RLum.Data.Curve}
\alias{plot_RLum.Data.Curve}
\title{Plot function for an RLum.Data.Curve S4 class object}
\description{The function provides a standardised plot output for curve data of an 
RLum.Data.Curve S4 class object }
\usage{plot_RLum.Data.Curve(object, par.local = TRUE, norm = FALSE, 
    ...)}
\arguments{
  \item{object}{\code{\linkS4class{RLum.Data.Curve}} (\bold{required}): 
S4 object of class \code{RLum.Data.Curve}}
  \item{par.local}{\code{\link{logical}} (with default): use local graphical parameters for plotting, e.g.
the plot is shown in one column and one row. If \code{par.local = FALSE},  
global parameters are inherited.}
  \item{norm}{\code{\link{logical}} (with default): allows curve normalisation to the 
highest count value }
  \item{\dots}{further arguments and graphical parameters that will be passed to the 
\code{plot} function}
}
\details{Only single curve data can be plotted with this function. 
Arguments according to \code{\link{plot}}.}
\value{Returns a plot.}
\references{#}
\author{Sebastian Kreutzer, Universite Bordeaux Montaigne (France), \cr
R Luminescence Package Team}
\note{Not all arguments of \code{\link{plot}} will be passed!}


\seealso{\code{\link{plot}}, \code{\link{plot_RLum}}}
\examples{

##plot curve data

#load Example data
data(ExampleData.CW_OSL_Curve, envir = environment())

#transform data.frame to RLum.Data.Curve object
temp <- as(ExampleData.CW_OSL_Curve, "RLum.Data.Curve")

#plot RLum.Data.Curve object
plot_RLum.Data.Curve(temp)

}

\keyword{aplot}
\section{Function version}{0.1.4 (2014-08-14 09:22:49)}
