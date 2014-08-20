\name{RLum.Data.Spectrum-class}
\Rdversion{1.1}
\docType{class}
\alias{RLum.Data.Spectrum-class}
\alias{coerce,RLum.Data.Spectrum-method}
\alias{show,RLum.Data.Spectrum-method}
\alias{set_RLum.Data.Spectrum}
\alias{set_RLum.Data.Spectrum-methods}
\alias{set_RLum.Data.Spectrum,RLum.Data.Spectrum-method}
\alias{set_RLum.Data.Spectrum,ANY-method}
\alias{set_RLum.Data.Spectrum,character,matrix-method}
\alias{get_RLum.Data.Spectrum}
\alias{get_RLum.Data.Spectrum-methods}
\alias{get_RLum.Data.Spectrum,ANY-method}

\title{Class \code{"RLum.Data.Spectrum"}}
\description{
Class for luminescence spectra data (TL/OSL/RF). 
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("RLum.Data.Spectrum", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{recordType}:}{Object of class \code{"character"} containing the type of the curve (e.g. "TL" or "OSL") }
    \item{\code{curveType}:}{Object of class \code{"character"} containing curve type, allowed values
    are \code{measured} or \code{predefined}}
    \item{\code{data}:}{Object of class \code{"matrix"} containing spectrum (count) values.\cr 
    
    \bold{row labels} indicating wavelength/pixel values \cr 
    \bold{column labels} temperature or time values.}
    \item{\code{info}:}{Object of class \code{"list"} containing further meta information objects }
    \item{\code{.S3Class}:}{Object of class \code{"character"}  }
  }
}
\section{Extends}{
Class \code{"\linkS4class{RLum.Data}"}, directly.
}
\section{Methods}{
  \describe{
    \item{coerce}{
    
      \code{signature(from = "data.frame", to = "RLum.Data.Spectrum")}
      
      \code{signature(from = "matrix", to = "RLum.Data.Spectrum")}
      
      Furthermore, lossy coercing is possible from \code{RLum.Data.Spectrum} to: \cr
      
      \code{\link{data.frame}}, \code{\link{matrix}}
    
    }
    \item{show}{\code{signature(object = "RLum.Data.Spectrum")}: ... }
    
    \item{set_RLum.Data.Spectrum}{\code{signature(recordType = "character", curveType = "character", data = "matrix", info = "list")}: 
    
    Construction method for \code{RLum.Data.Spectrum} object.
    The slot \code{info} is optional and predefined as empty \code{list} by default.    
    }
    
    \item{get_RLum.Data.Spectrum}{\code{signature(object = "RLum.Data.Spectrum", info.object = "character")}: 
    
    Accessor method for \code{RLum.Data.Spectrum} object. The argument \code{info.object}
    is optional to directly access the info elements. If no info element name is 
    provided, the raw curve data (\code{matrix}) will be returned.
    }
    
    
	 }
}
\references{
#
}
\author{Sebastian Kreutzer, JLU Giessen (Germany)
}

\section{Version}{0.1 (2013-11-23)}


\note{
The class should only contain data for a single spectra data set. For additional elements
the slot \code{info} can be used.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\linkS4class{RLum}}, \code{\linkS4class{RLum.Data}}, \code{\link{plot_RLum}}
}
\examples{
showClass("RLum.Data.Spectrum")

##show example data (uncomment for usage)
# data(ExampleData.XSYG, envir = environment())
# TL.Spectrum


}
\keyword{classes}
