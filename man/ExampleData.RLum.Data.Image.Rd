\name{ExampleData.RLum.Data.Image}
\alias{ExampleData.RLum.Data.Image}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{Example data as \code{\linkS4class{RLum.Data.Image}} objects}
\description{Measurement of Princton Instruments camera imported with the function 
\code{\link{readSPE2R}} to R to produce an \code{\linkS4class{RLum.Data.Image}} object. }

\usage{
ExampleData.RLum.Data.Image
}
%- maybe also 'usage' for other objects documented here.
\format{
Object of class \code{\linkS4class{RLum.Data.Image}}

}
\source{
\bold{ExampleData.RLum.Data.Image}

These data were kindly provided by Regina DeWitt.

\tabular{ll}{

Lab.: \tab Department of Physics, East-Carolina University, NC, USA\cr
Lab-Code: \tab -\cr
Location: \tab - \cr
Material: \tab - \cr
Reference: \tab - \cr

}

Image data is a measurement of fluorescent ceiling lights with a cooled Princeton Instruments (TM)
camera fitted on Risoe DA-20 TL/OSL reader. 


}

\references{
%% ~put references to the literature/web site here ~
-

}
\section{Version}{0.1}
%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
##load data
data(ExampleData.RLum.Data.Image, envir = environment())

##plot data
plot_RLum(ExampleData.RLum.Data.Image)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{datasets}
