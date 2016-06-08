#' Import PSL files to R
#' 
#' Imports PSL files produced by a SUERC portable OSL reader into R
#'
#' <placeholder>
#'
#' @param file \code{\link{character}} or \code{\link{list}} (\bold{required}): path and file name of the
#' PSL file. If input is a \code{list} it should comprise only \code{character}s representing
#' valid paths and PSL file names.
#' Alternatively the input character can be just a directory (path). In this case the
#' the function tries to detect and import all PSL files found in the directory.
#' 
#' @param ... currently not used.
#'
#' @return Returns an S4 \code{\linkS4class{RLum.Analysis}} object.
#' 
#' @seealso \code{\linkS4class{RLum.Analysis}},
#' \code{\linkS4class{RLum.Data.Curve}}
#' 
#'
#' @author Christoph Burow, University of Cologne (Germany)
#'
#' @section Function version: 0.0.1
#'
#' @keywords IO
#' 
#' @examples
#' # none available yet
#' 
#' @export
read_PSL2R <- function(file, ...) {
  
  # Nothing to see here at the moment
  cat("This is a placeholder function.")
  
  return(NULL)
}