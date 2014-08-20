\name{ExampleData.BINfileData}
\alias{ExampleData.BINfileData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Example data from a SAR OSL and SAR TL measurement for the package Luminescence}
\description{Example data from a SAR OSL and TL measurement for package Luminescence directly extracted from
a Risoe BIN-file and provided in an object of type \link{Risoe.BINfileData-class}
}
\usage{
ExampleData.BINfileData
}
%- maybe also 'usage' for other objects documented here.
\format{

\code{CWOSL.SAR.Data}: SAR OSL measurement data

\code{TL.SAR.Data}: SAR TL measurement data

Each class object contains two slots: (a) \code{METADATA} is a \link{data.frame} with all metadata
stored in the BIN file of the measurements and (b) \code{DATA} contains a list of vectors of the measured data
(usually count values).
} 

\source{
\bold{CWOSL.SAR.Data}

\tabular{ll}{

Lab: \tab Luminescence Laboratory Bayreuth\cr
Lab-Code: \tab BT607\cr
Location: \tab Saxony/Germany\cr
Material: \tab Middle grain quartz measured \cr
          \tab on aluminum cups on a Risoe TL/OSL DA-15 reader\cr
Reference: \tab unpublished
}

\bold{TL.SAR.Data}

\tabular{ll}{

Lab: \tab Luminescence Laboratory of Cologne\cr
Lab-Code: \tab LP1_5\cr
Location: \tab Spain\cr
Material: \tab Flint \cr
Setup: \tab Risoe TL/OSL DA-20 reader \cr
       \tab (Filter: Semrock Brightline, \cr
       \tab HC475/50, N2, unpolished steel discs) \cr
Reference: \tab unpublished \cr
Remarks: \tab dataset limited to one position\cr
}



}

\references{
%% ~put references to the literature/web site here ~
\bold{CWOSL.SAR.Data}: unpublished data \cr

\bold{TL.SAR.Data}: unpublished data


}
\section{Version}{0.1}
%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
##show first 5 elements of the METADATA and DATA elements in the terminal
data(ExampleData.BINfileData, envir = environment())
CWOSL.SAR.Data@METADATA[1:5,]
CWOSL.SAR.Data@DATA[1:5]
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{datasets}
