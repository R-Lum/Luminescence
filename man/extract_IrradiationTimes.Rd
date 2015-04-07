\name{extract_IrradiationTimes}
\alias{extract_IrradiationTimes}
\title{Extract irradiation times from an XSYG file}
\description{Extracts irradiation times, dose and times since last irradiation, from a Freiberg Instruments
XSYG-file. These information can be further used to update an existing BINX-file}
\usage{extract_IrradiationTimes(object, file.BINX, recordType = c("irradiation (NA)", 
    "IRSL (UVVIS)", "OSL (UVVIS)", "TL (UVVIS)"), compatibility.mode = TRUE, 
    txtProgressBar = TRUE)}
\arguments{
  \item{object}{\code{\link{character}} (\bold{required}) or \code{\linkS4class{RLum.Analysis}} object:
path and file name of the XSYG file or an \code{\linkS4class{RLum.Analysis}} produced by the
function \code{\link{readXSYG2R}}. \cr

\bold{Note}: If an \code{\linkS4class{RLum.Analysis}} is used, any input for the arguments
\code{file.BINX} and \code{recordType} will be ignored!}
  \item{file.BINX}{\code{\link{character}} (optional): path and file name of an existing BINX-file. If a file name
is provided the file will be updated with the information from the XSYG file in the same
folder as the original BINX-file.\cr
Note: The XSYG and the BINX-file have to be originate from the same measurement!}
  \item{recordType}{\code{\link{character}} (with default): select relevant curves types from the XSYG file or
\code{\linkS4class{RLum.Analysis}} object.
As the XSYG-file format comprises much more information than usually needed for routine
data analysis and allowed in the BINX-file format, only the relevant curves are selected
by using the function \code{\link{get_RLum.Analysis}}. The argument
\code{recordType} works as described for this function. \cr

Note: A wrong selection will causes a function error. Please change this argument only
if you have reasons to do so.}
  \item{compatibility.mode}{\code{\link{logical}} (with default): this option is parsed only if a BIN/BINX file is produced
and it will reset all position values to a max. value of 48, cf.\code{\link{writeR2BIN}}}
  \item{txtProgressBar}{\code{\link{logical}} (with default): enables \code{TRUE} or disables \code{FALSE}
the progression bars during import and export}
}
\details{The function was written to compensate missing information in the BINX-file output of
Freiberg Instruments lexsyg readers. As all information are available within the XSYG-file anyway,
these information can be extracted and used for further analysis or/and to stored
in a new BINX-file, which can be further used by other software, e.g. Analyst (Geoff Duller). \cr

Typical application example: g-value estimation from fading measurements using the Analyst
or any other self written script.\cr

Beside the some simple data transformation steps the function applies the functions \code{\link{readXSYG2R}},
\code{\link{readBIN2R}}, \code{\link{writeR2BIN}} for data import and export.}
\value{An \code{\linkS4class{RLum.Results}} object is returned with the following structure:\cr
.. $irr.times (data.frame)\cr

If a BINX-file path and name is set, the output will be additionally transferred
to a new BINX-file with the function name as suffix. For the output
the path of the input BINX-file itself is used. Note that this will not work if the input
object is a file path to an XSYG-file. In this case the argument input is ignored.}
\references{Duller, G., 2007. Analyst.}
\author{Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France), \cr
R Luminescence Package Team}
\note{The produced output object contains still the irradiation steps to keep the output transparent.
However, for the BINX-file export this steps are removed as the BINX-file format description
does not allow irradiations as separat sequences steps.\cr

Know issue: The 'fading correction' menu in the Analyst will not work appear with the produced
BIN/BINX-file due to hidden bits, which are not reproduced by the function \code{writeR2BIN()}.}


\seealso{\code{\linkS4class{RLum.Analysis}}, \code{\linkS4class{RLum.Results}}, \code{\linkS4class{Risoe.BINfileData}},
\code{\link{readXSYG2R}}, \code{\link{readBIN2R}},
\code{\link{writeR2BIN}}}
\examples{

## (1) - example for your own data
##
## set files and run function
#
#   file.XSYG <- file.choose()
#   file.BINX <- file.choose()
#
#     output <- extract_IrradiationTimes(file.XSYG = file.XSYG, file.BINX = file.BINX)
#     get_RLum.Results(output)
#
## export results additionally to a CSV.file in the same directory as the XSYG-file
#       write.table(x = get_RLum.Results(output),
#                   file = paste0(file.BINX,"_extract_IrradiationTimes.csv"),
#                   sep = ";",
#                   row.names = FALSE)

}

\keyword{IO}
\keyword{manip}
\section{Function version}{0.2.1 (2015-04-07 11:38:42)}
