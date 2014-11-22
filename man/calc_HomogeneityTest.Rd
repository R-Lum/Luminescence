\name{calc_HomogeneityTest}
\alias{calc_HomogeneityTest}
\title{Apply a simple homogeneity test after Galbraith (2003)}
\description{A simple homogeneity test for De estimates}
\usage{calc_HomogeneityTest(input.data, log = TRUE, sample.id = "unknown sample", 
    ...)}
\arguments{
  \item{input.data}{\code{\linkS4class{RLum.Results}} or \link{data.frame} (\bold{required}):
for \code{data.frame}: two columns with De \code{(input.data[,1])} and
De error \code{(values[,2])}}
  \item{log}{\code{\link{logical}} (with default): peform the homogeniety test with
(un-)logged data}
  \item{sample.id}{\code{\link{character}} (with default): sample id}
  \item{\dots}{further arguments (for internal compatibility only).}
}
\details{For details see Galbraith (2003).}
\value{Returns a terminal output. In addition an 
\code{\linkS4class{RLum.Results}} object is 
returned containing the following element:

\item{results}{\link{data.frame} with statistical parameters.}

The output should be accessed using the function 
\code{\link{get_RLum.Results}}  }
\references{Galbraith, R.F., 2003. A simple homogeneity test for estimates of dose
obtained using OSL. Ancient TL, 21, pp. 75-77.}
\author{Christoph Burow, University of Cologne (Germany), \cr
R Luminescence Package Team}



\seealso{\code{\link{pchisq}}}
\examples{
## load example data
data(ExampleData.DeValues, envir = environment())

## apply the homogeneity test
calc_HomogeneityTest(ExampleData.DeValues)
}
\section{Function version}{0.2 (2014-09-15 02:28:00)}
\section{Function version}{0.2 (2014-09-15 02:28:00)}
