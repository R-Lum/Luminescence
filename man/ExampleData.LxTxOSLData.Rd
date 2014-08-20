\name{ExampleData.LxTxOSLData}
\alias{ExampleData.LxTxOSLData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Example Lx and Tx curve data from an artificial OSL measurement}
\description{Lx and Tx data of continous wave (CW-) OSL signal curves.}
\usage{
ExampleData.LxTxOSLData.RData
}
%- maybe also 'usage' for other objects documented here.
\format{
Two \code{data.frames} containing time and count values.
} 

\source{
%%  ~~ If necessary, more details than the description above ~~
Arbitrary OSL measurement.

}

\references{
%% ~put references to the literature/web site here ~
unpublished data
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
##load data
data(ExampleData.LxTxOSLData, envir = environment())

##plot data
plot(Lx.data)
plot(Tx.data)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{IO}
