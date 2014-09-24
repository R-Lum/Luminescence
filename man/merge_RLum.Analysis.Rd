\name{merge_RLum.Analysis}
\alias{merge_RLum.Analysis}
\title{Merge function for RLum.Analysis S4 class objects}
\description{Function allows merging of RLum.Analysis objects and adding of objects an RLum.Analysis 
object consists of. }
\usage{merge_RLum.Analysis(objects)}
\arguments{
  \item{objects}{\code{\link{list}} of \code{\linkS4class{RLum.Analysis}} (\bold{required}): 
list of S4 object of class \code{RLum.Analysis}. Furthermore objects 
of class \code{\linkS4class{RLum.Data.Curve}}, \code{\linkS4class{RLum.Data.Spectrum}},
and \code{\linkS4class{RLum.Data.Image}} can be added. Note: The input list 
requires at least on object of \code{\linkS4class{RLum.Analysis}}}
}
\details{This function simply allows to merge \code{\linkS4class{RLum.Analysis}} objects. 
Additionally other \code{\linkS4class{RLum}} objects can be added to an existing 
\code{\linkS4class{RLum.Analysis}} object. Supported objects to be added are: 
\code{\linkS4class{RLum.Data.Curve}}, \code{\linkS4class{RLum.Data.Image}} and
\code{\linkS4class{RLum.Data.Image}}.\cr

The order in the new \code{\linkS4class{RLum.Analysis}} object is the object order
provided with the input list. }
\value{Return an \code{\linkS4class{RLum.Analysis}} object.}
\references{-}
\author{Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France), \cr
R Luminescence Package Team}
\note{The information for the slot 'protocol' is taken from the first \code{\linkS4class{RLum.Analysis}} 
object in the input list. Therefore at least one object of type \code{\linkS4class{RLum.Analysis}} 
has to be provided.}


\seealso{\code{\link{merge_RLum}}, \code{\linkS4class{RLum.Analysis}}, 
\code{\linkS4class{RLum.Data.Curve}}, \code{\linkS4class{RLum.Data.Spectrum}},
\code{\linkS4class{RLum.Data.Image}}, \code{\linkS4class{RLum}}}
\examples{

##merge different RLum objects from the example data
data(ExampleData.RLum.Analysis, envir = environment())
data(ExampleData.BINfileData, envir = environment())

object <- Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data, pos=1)
curve <- get_RLum(object)[[2]]

temp.merged <- merge_RLum.Analysis(list(curve, IRSAR.RF.Data, IRSAR.RF.Data))

}

\keyword{utilities}
\section{Function version}{0.1 (2014-09-21 19:33:38)}
