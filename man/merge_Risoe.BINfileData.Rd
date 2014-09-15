\name{merge_Risoe.BINfileData}
\alias{merge_Risoe.BINfileData}
\title{Merge Risoe.BINfileData objects or Risoe BIN-files}
\description{Function allows merging Risoe BIN files or Risoe.BINfileData objects.}
\usage{merge_Risoe.BINfileData(input.objects, output.file, keep.position.number = FALSE, 
    position.number.append.gap = 0)}
\arguments{
  \item{input.objects}{\code{\link{character}} or \code{\linkS4class{Risoe.BINfileData}} (\bold{required}):
Character vector with path and files names 
(e.g. \code{input.objects = c("path/file1.bin", "path/file2.bin")} 
or \code{\linkS4class{Risoe.BINfileData}} objects 
(e.g. \code{input.objects = c(object1, object2)})}
  \item{output.file}{\code{\link{character}} (optional): File output path and name. \cr
If no value is given, a \code{\linkS4class{Risoe.BINfileData}} 
is returned instead of a file.}
  \item{keep.position.number}{\code{\link{logical}} (with default): Allows keeping the original 
position numbers of the input objects. Otherwise the position numbers 
a recalculated.}
  \item{position.number.append.gap}{\code{\link{integer}} (with default): Set the position number gap between 
merged BIN-file sets, if the option \code{keep.position.number = FALSE} is
used. See details for further information.}
}
\details{The function allows mering different measurements to one file or one object.\cr
The record IDs are recalculated for the new object. Other values 
are kept for each object. The number of input objects is not limited. \cr

\code{position.number.append.gap} option \cr

If the option \code{keep.position.number = FALSE} is used, the position numbers
of the new data set are recalculated by adding the highest position
number of the previous data set to the each position number of the next data
set. For example: The highest position number is 48, then this number
will be added to all other position numbers of the next data set 
(e.g. 1 + 48 = 49)\cr

However, there might be cases where an additional addend (summand) is needed
before the next position starts. Example: \cr

Position number set (A): \code{1,3,5,7}\cr
Position number set (B): \code{1,3,5,7} \cr

With no additional summand the new position numbers would be: 
\code{1,3,5,7,8,9,10,11}.
That might be unwanted. Using the argument \code{position.number.append.gap = 1}
it will become: \code{1,3,5,7,9,11,13,15,17}.}
\value{Returns a \code{file} or a \code{\linkS4class{Risoe.BINfileData}} object.}
\references{Duller, G., 2007. Analyst. }
\author{Sebastian Kreutzer, JLU Giessen (Germany),\cr
R Luminescence Package Team}
\note{The validity of the output objects is not further checked.  }


\seealso{\code{\linkS4class{Risoe.BINfileData}},
\code{\link{readBIN2R}}, \code{\link{writeR2BIN}}  }
\examples{

##merge two objects
data(ExampleData.BINfileData, envir = environment())

object1 <- CWOSL.SAR.Data
object2 <- CWOSL.SAR.Data

object.new <- merge_Risoe.BINfileData(c(object1, object2))


}

\keyword{IO}
\keyword{manip}
\section{Function version}{0.2 (2014-09-15 02:28:00)}
