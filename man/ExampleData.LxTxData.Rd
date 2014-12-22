\name{ExampleData.LxTxData}
\alias{ExampleData.LxTxData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Example Lx/Tx data from CW-OSL SAR measurement}
\description{LxTx data from a SAR measurement for the package Luminescence.}
\usage{
ExampleData.LxTxData
}
%- maybe also 'usage' for other objects documented here.
\format{
A \code{data.frame} with 4 columns (Dose, LxTx, LxTx.Error, TnTx).
} 

\source{
%%  ~~ If necessary, more details than the description above ~~
\tabular{ll}{
Lab: \tab Luminescence Laboratory Bayreuth\cr
Lab-Code: \tab BT607\cr
Location: \tab Ostrau (Saxony-Anhalt/Germany)\cr
Material: \tab Middle grain (38-63 \eqn{\mu}m) quartz measured on a Risoe TL/OSL DA-15 reader.\cr
}
}

\references{
%% ~put references to the literature/web site here ~
unpublished data
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
##plot Lx/Tx data vs dose [s]
data(ExampleData.LxTxData, envir = environment())
plot(LxTxData$Dose,LxTxData$LxTx)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{IO}
