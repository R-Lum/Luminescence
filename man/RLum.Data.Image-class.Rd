\name{RLum.Data.Image-class}
\Rdversion{1.1}
\docType{class}
\alias{RLum.Data.Image-class}
\alias{coerce,RLum.Data.Image-method}
\alias{show,RLum.Data.Image-method}
\alias{set_RLum.Data.Image}
\alias{set_RLum.Data.Image-methods}
\alias{set_RLum.Data.Image,RLum.Data.Image-method}
\alias{set_RLum.Data.Image,ANY-method}
\alias{set_RLum.Data.Image,character,matrix-method}
\alias{get_RLum.Data.Image}
\alias{get_RLum.Data.Image-methods}
\alias{get_RLum.Data.Image,ANY-method}

\title{Class \code{"RLum.Data.Image"}}
\description{
Class for luminescence image data (TL/OSL/RF). 
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("RLum.Data.Image", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{recordType}:}{Object of class \code{"character"} containing the type of the curve (e.g. "OSL image", "TL image") }
    \item{\code{curveType}:}{Object of class \code{"character"} containing curve type, allowed values
    are \code{measured} or \code{predefined}}
    \item{\code{data}:}{Object of class \code{"RasterBrick"} containing images (raster data).} 

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
    
      \code{signature(from = "data.frame", to = "RLum.Data.Image")}
      
      \code{signature(from = "matrix", to = "RLum.Data.Image")}
      
      Furthermore, lossy coercing is possible from \code{RLum.Data.Image} to: \cr
      
      \code{\link{data.frame}}, \code{\link{matrix}}
    
    }
    \item{show}{\code{signature(object = "RLum.Data.Image")}: ... }
    
    \item{set_RLum.Data.Image}{\code{signature(recordType = "character", curveType = "character", data = "RasterBrick", info = "list")}: 
    
    Construction method for \code{RLum.Data.Image} object.
    The slot \code{info} is optional and predefined as empty \code{list} by default.    
    }
    
    \item{get_RLum.Data.Image}{\code{signature(object = "RLum.Data.Image", info.object = "character")}: 
    
    Accessor method for \code{RLum.Data.Image} object. The argument \code{info.object}
    is optional to directly access the info elements. If no info element name is 
    provided, the raw image data (\code{RasterBrick}) will be returned.
    }
    
    
	 }
}
\references{
#
}
\author{Sebastian Kreutzer, Universite Bordeaux Montaigne (France)
}

\section{Version}{0.1 (2014-07-22)}


\note{
The class should only contain data for a set of images. For additional elements
the slot \code{info} can be used.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\linkS4class{RLum}}, \code{\linkS4class{RLum.Data}}, \code{\link{plot_RLum}}
}
\examples{
showClass("RLum.Data.Image")

##so far no further example available


}
\keyword{classes}
