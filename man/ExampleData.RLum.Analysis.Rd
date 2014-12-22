\name{ExampleData.RLum.Analysis}
\alias{ExampleData.RLum.Analysis}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Example data as \code{\linkS4class{RLum.Analysis}} objects}
\description{Collection of different \code{\linkS4class{RLum.Analysis}} objects for 
protocol analysis.}
\usage{
ExampleData.RLum.Analysis
}
%- maybe also 'usage' for other objects documented here.
\format{

\code{IRSAR.RF.Data}: IRSAR.RF.Data on coarse grain feldspar

Each object contains data needed for the given protocol analysis.
} 

\source{
\bold{IRSAR.RF.Data}

These data were kindly provided by Tobias Lauer and 
Matthias Krbetschek.

\tabular{ll}{

Lab: \tab Luminescence Laboratory TU Bergakademie Freiberg\cr
Lab-Code: \tab ZEU/SA1\cr
Location: \tab Zeuchfeld (Zeuchfeld Sandur; Saxony-Anhalt/Germany)\cr
Material: \tab K-feldspar (130-200 \eqn{\mu}m)\cr
Reference: \tab Kreutzer et al. (2014)\cr


}



}

\references{
%% ~put references to the literature/web site here ~
\bold{IRSAR.RF.Data}

Kreutzer, S., Lauer, T., Meszner, S., Krbetschek, M.R., Faust, D., Fuchs, M., 2014. Chronology of the Quaternary profile Zeuchfeld in Saxony-Anhalt / Germany - a preliminary luminescence dating study. Zeitschrift fuer Geomorphologie 58, 5-26. doi: 10.1127/0372-8854/2012/S-00112

}
\section{Version}{0.1}
%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
##load data
data(ExampleData.RLum.Analysis, envir = environment())

##plot data
plot_RLum(IRSAR.RF.Data)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{datasets}
