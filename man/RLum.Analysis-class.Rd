\name{RLum.Analysis-class}
\Rdversion{1.1}
\docType{class}
\alias{RLum.Analysis-class}
\alias{show,RLum.Analysis-method}
\alias{set_RLum.Analysis}
\alias{set_RLum.Analysis,RLum.Analysis-method}
\alias{set_RLum.Analysis,list-method}
\alias{get_RLum.Analysis}
\alias{get_RLum.Analysis-methods}
\alias{get_RLum.Analysis,RLum.Analysis-method}
\alias{get_structure.RLum.Analysis}
\alias{get_structure.RLum.Analysis,RLum.Analysis-method}
\alias{length_RLum.Analysis}
\alias{length_RLum.Analysis-methods}
\alias{length_RLum.Analysis,RLum.Analysis-method}


\title{Class \code{"RLum.Analysis"}}
\description{
Object class containing analysis data for protocol analysis.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("RLum.Analysis", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{records}:}{Object of class \code{"list"} containing objects of class \code{\linkS4class{RLum.Data}}}
     \item{\code{protocol}:}{Object of class \code{"character"} describing the applied
     measurement protocol}
    
    
    \item{\code{.S3Class}:}{Object of class \code{"character"}}
  }
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "RLum.Analysis")}: ... }
    \item{set_RLum.Analysis}{\code{signature(records = "list", protocol = "character")}: 
    
    Construction method for \code{RLum.Analysis} object. The slot \code{protocol} is optional 
    and predefined as \code{UNKNOWN} by default.    
    }
    
    \item{get_RLum.Analysis}{\code{signature(object = "RLum.Analysis",                                                     record.id = "numeric", recordType = "character", curveType = "character", RLum.type = "character",
    get.index = "logical", keep.object = "logical")}: 
    
    Accessor method for \code{RLum.Analysis} object. 
    
    The slots \code{record.id}, \code{recordType}, \code{curveType} and \code{RLum.type}
    are optional to allow for records limited by their id (list index number), 
    their record type (e.g. \code{recordType = "OSL"}) or object type. 
    
    Example: curve type (e.g. \code{curveType = "predefined"} 
    or \code{curveType ="measured"})\cr
    
    \bold{The selection of a specific \code{RLum.type} object superimposes the default selection.}
    Currently supported objects are: \code{RLum.Data.Curve} and \code{RLum.Data.Spectrum}\cr
    
    The argument \code{get.index = TRUE} just returns a \code{\link{numeric}} 
    \code{\link{vector}} with the index of each element in the \code{RLum.Analysis} object. 
    
    The argument \code{keep.object} allowing returns an \code{RLum.Analysis} object 
    instead of the single elements. Default is \code{keep.object = FALSE}.
    
    }
    
    \item{get_structure.RLum.Analysis}{\code{signature(object = "RLum.Analysis")}: 
    get meta structure
    of object as \code{\link{data.frame}}
   }
   
   \item{length_RLum.Analysis}{\code{signature(object = "RLum.Analysis")}:
   returns length of the object, i.e., number of records in the object
   }
  
   
   
}}
   
\references{
#
}
\author{
Sebastian Kreutzer, Freiberg Instruments/JLU Giessen (Germany)
}

\section{Version}{0.1.4 (2014-02-26)}

\note{
The method \code{get_structure.RLum.Analysis} is currently just avaiblable for 
objects containing \code{\linkS4class{RLum.Data.Curve}}.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{Risoe.BINfileData2RLum.Analysis}}, 
\code{\linkS4class{Risoe.BINfileData}}, \code{\linkS4class{RLum}}
}

\examples{
showClass("RLum.Analysis")

## usage of get_RLum.Analysis() with returining an RLum.Analysis object
#  get_RLum.Analysis(object, keep.object = TRUE) 

}
\keyword{classes}
