\name{RLum.Data.Curve-class}
\Rdversion{1.1}
\docType{class}
\alias{RLum.Data.Curve-class}
\alias{coerce,RLum.Analysis-method}
\alias{show,RLum.Data.Curve-method}
\alias{set_RLum.Data.Curve}
\alias{set_RLum.Data.Curve-methods}
\alias{set_RLum.Data.Curve,RLum.Data.Curve-method}
\alias{set_RLum.Data.Curve,ANY-method}
\alias{set_RLum.Data.Curve,character,matrix-method}
\alias{get_RLum.Data.Curve}
\alias{get_RLum.Data.Curve-methods}
\alias{get_RLum.Data.Curve,ANY-method}

\title{Class \code{"RLum.Data.Curve"}}
\description{
Class for luminescence curve data. 
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("RLum.Data.Curve", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{recordType}:}{Object of class \code{"character"} containing the type of the curve (e.g. "TL" or "OSL") }
    \item{\code{curveType}:}{Object of class \code{"character"} containing curve type, allowed values
    are \code{measured} or \code{predefined}}
    \item{\code{data}:}{Object of class \code{"matrix"} containing curve x and y data }
    \item{\code{info}:}{Object of class \code{"list"} containing further meta information objects }
    \item{\code{.S3Class}:}{Object of class \code{"character"}  }
  }
}
\section{Extends}{
Class \code{"\linkS4class{RLum.Data}"}.
}
\section{Methods}{
  \describe{
    \item{coerce}{
    
      \code{signature(from = "data.frame", to = "RLum.Data.Curve")}
      
      \code{signature(from = "matrix", to = "RLum.Data.Curve")}
      
      Furthermore, lossy coercing is possible from \code{RLum.Data.Curve} to: \cr
      
      \code{\link{data.frame}}, \code{\link{matrix}}
    
    }
    \item{show}{\code{signature(object = "RLum.Data.Curve")}: ... }
    
    \item{set_RLum.Data.Curve}{\code{signature(recordType = "character", curveType = "character", data = "matrix", info = "list")}: 
    
    Construction method for \code{RLum.Data.Curve} object. The slot \code{info} is optional 
    and predefined as empty \code{list} by default.    
    }
    
    \item{get_RLum.Data.Curve}{\code{signature(object = "RLum.Data.Curve", 
    info.object = "character")}: 
    
    Accessor method for \code{RLum.Data.Curve} object. The argument \code{info.object}
    is optional to directly access the info elements. If no info element name is 
    provided, the raw curve data (\code{matrix}) will be returned.
    }
    
    
    
	 }
}
\references{
#
}
\author{Sebastian Kreutzer Freiberg Instruments/JLU Giessen (Germany)
}

\section{Version}{0.1.2 (2013-11-22)}


\note{
The class should only contain data for a single curve. For additional elements
the slot \code{info} can be used (e.g. providing additional heating ramp curve).
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\linkS4class{RLum}}, \code{\linkS4class{RLum.Data}}, \code{\link{plot_RLum}}
}
\examples{
showClass("RLum.Data.Curve")
}
\keyword{classes}
