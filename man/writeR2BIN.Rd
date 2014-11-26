\name{writeR2BIN}
\alias{writeR2BIN}
\title{Export Risoe.BINfileData into Risoe BIN-file}
\description{Exports a Risoe.BINfileData object in a *.bin or *.binx file that can be 
opened by the Analyst software or other Risoe software.}
\usage{writeR2BIN(object, file, version, txtProgressBar = TRUE)}
\arguments{
  \item{object}{
}
  \item{file}{
}
  \item{version}{
}
  \item{txtProgressBar}{
}
}
\details{The structure of the exported binary data follows the data structure 
published in the Appendices of the Analyst manual p. 42.\cr\cr
If \code{LTYPE}, \code{DTYPE} and \code{LIGHTSOURCE} are not of type 
\code{\link{character}}, no transformation into numeric values is done. }
\value{Write a binary file.}
\references{Duller, G., 2007. Analyst. }
\author{Sebastian Kreutzer, Freiberg Instruments/JLU Giessen (Germany), \cr
R Luminescence Package Team}
\note{The function just roughly checks the data structures. 
The validity of the output data depends on the user.\cr\cr
The validity of the file path is not further checked. \cr 
BIN-file conversions using the argument \code{version} may be a lossy conversion, 
depending on the chosen input and output data 
(e.g. conversion from version 06 to 04).}


\seealso{\code{\link{readBIN2R}}, \code{\linkS4class{Risoe.BINfileData}},
\code{\link{writeBin}}}
\examples{

##uncomment for usage 

##data(ExampleData.BINfileData, envir = environment())
##writeR2BIN(CWOSL.SAR.Data, file="[your path]/output.bin")

}

\keyword{IO}
\section{Function version}{0.1 (2014-09-15 02:28:00)}
