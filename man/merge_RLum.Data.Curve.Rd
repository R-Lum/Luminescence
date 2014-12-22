\name{merge_RLum.Data.Curve}
\alias{merge_RLum.Data.Curve}
\title{Merge function for RLum.Data.Curve S4 class objects}
\description{Function allows merging of RLum.Data.Curve objects in different ways}
\usage{merge_RLum.Data.Curve(object, merge.method = "mean", method.info)}
\arguments{
  \item{object}{\code{\link{list}} of \code{\linkS4class{RLum.Data.Curve}} (\bold{required}): 
list of S4 objects of class \code{RLum.Curve}.}
  \item{merge.method}{\code{\link{character}} (\bold{required}): method for combining of the objects, e.g. 
\code{'mean'}, \code{'sum'}, see details for further information and allowed methods. 
Note: Elements in slot info will be taken from the first curve in the list.}
  \item{method.info}{\code{\link{numeric}} (optional): allows to specify how info elements of the input 
objects are combined, e.g. \code{1} means that just the elements from the first 
object are kept, \code{2} keeps only the info elements from the 2 object etc. 
If nothing is provided all elements are combined.}
}
\details{This function simply allowing to merge \code{\linkS4class{RLum.Data.Curve}} objects without 
touching the objects itself. Merging is always applied on the 2nd colum of the data matrix
of the object.\cr

\bold{Supported merge operations are \code{\linkS4class{RLum.Data.Curve}}}\cr

\code{"sum"}\cr

All count values will be summed up using the function \code{\link{rowSums}}.

\code{"mean"}\cr

The mean over the count values is calculated using the function \code{\link{rowMeans}}.

\code{"-"}\cr

The row sums of the last objects are subtracted from the first object.

\code{"*"}\cr

The row sums of the last objects are mutliplied with the first object.

\code{"/"}\cr

Values of the first object are divided by row sums of the last objects.}
\value{Return an \code{\linkS4class{RLum.Data.Curve}} object.}
\references{-}
\author{Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France), \cr
R Luminescence Package Team}
\note{The information from the slot 'recordType' is taken 
from the first \code{\linkS4class{RLum.Data.Curve}} object in the input list.
The slot 'curveType' is filled with the name \code{merged}.}


\seealso{\code{\link{merge_RLum}}, \code{\linkS4class{RLum.Data.Curve}}}
\examples{

##load example data
data(ExampleData.XSYG, envir = environment())

##grep first and 3d TL curves
TL.curves  <- get_RLum.Analysis(OSL.SARMeasurement$Sequence.Object, recordType = "TL (UVVIS)")
TL.curve.1 <- TL.curves[[1]]
TL.curve.3 <- TL.curves[[3]]

##plot single curves
plot_RLum(TL.curve.1)
plot_RLum(TL.curve.3)

##subtract the 1st curve from the 2nd and plot
TL.curve.merged <- merge_RLum.Data.Curve(list(TL.curve.3, TL.curve.1), merge.method = "/")
plot_RLum(TL.curve.merged)

}

\keyword{utilities}
\section{Function version}{0.1 (2014-12-22 13:49:02)}
