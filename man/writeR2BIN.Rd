\name{writeR2BIN}
\alias{writeR2BIN}
\title{Export Risoe.BINfileData into Risoe BIN-file}
\description{Exports a Risoe.BINfileData object in a *.bin or *.binx file that can be
opened by the Analyst software or other Risoe software.}
\usage{writeR2BIN(object, file, version, txtProgressBar = TRUE)}
\arguments{
  \item{object}{\code{\linkS4class{Risoe.BINfileData}} (\bold{required}): input object
to be stored in a bin file.}
  \item{file}{\code{\link{character}} (\bold{required}): file name and path of the
output file\cr
[WIN]: \code{writeR2BIN(object, "C:/Desktop/test.bin")}, \cr
[MAC/LINUX]: \code{writeR2BIN("/User/test/Desktop/test.bin")}}
  \item{version}{\code{\link{character}} (optional): version number for the output file.
If no value is provided the highest version number from the
\code{\linkS4class{Risoe.BINfileData}} is taken automatically.\cr\cr
Note: This argument can be used to convert BIN-file versions.}
  \item{txtProgressBar}{\link{logical} (with default): enables or disables \code{\link{txtProgressBar}}.}
}
\details{The structure of the exported binary data follows the data structure
published in the Appendices of the Analyst manual p. 42.\cr\cr
If \code{LTYPE}, \code{DTYPE} and \code{LIGHTSOURCE} are not of type
\code{\link{character}}, no transformation into numeric values is done.}
\value{Write a binary file.}
\references{Duller, G., 2007. Analyst.}
\author{Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France), \cr
R Luminescence Package Team}
\note{The function just roughly checks the data structures.
The validity of the output data depends on the user.\cr\cr
The validity of the file path is not further checked. \cr
BIN-file conversions using the argument \code{version} may be a lossy conversion,
depending on the chosen input and output data
(e.g., conversion from version 06 to 04 or 03).\cr

\bold{Warning}\cr

Although the coding was done carefully it seems that the BIN/BINX-files produced by
Risoe DA 15/20 TL/OSL readers slightly differ on the byte level. No obvious differences are observed
in the METADATA, however, the BIN/BINX-file may not fully compatible, at least not similar to
the once directly produced by the Risoe readers!}


\seealso{\code{\link{readBIN2R}}, \code{\linkS4class{Risoe.BINfileData}},
\code{\link{writeBin}}}
\examples{

##uncomment for usage

##data(ExampleData.BINfileData, envir = environment())
##writeR2BIN(CWOSL.SAR.Data, file="[your path]/output.bin")

}

\keyword{IO}
\section{Function version}{0.2.5 (2015-03-23 12:57:25)}
