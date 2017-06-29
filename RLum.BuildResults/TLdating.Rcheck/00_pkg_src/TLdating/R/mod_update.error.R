#' Update the error matrix
#'
#' This function update the error vector of each curve from a \code{\linkS4class{TLum.Analysis}} object.
#'
#' @param object
#'  \code{\linkS4class{TLum.Analysis}} (\bold{required}): object containing the initial TL curves.
#' @param method
#'  \link{character} (with default): Defines the methode use to update the error matrix ("poisson", "absolute", "relative", "combine").
#' @param absolute.error
#'  \link{numeric} (with default): absolute error of the TL signals (used by the "absolute" and "combine" methods).
#' @param relative.error
#'  \link{numeric} (with default): Relative error of the TL signals (used by the "relative" and "combine" methods).
#' @param k
#'  \link{numeric} (with default): corrective factor to use when using a poisson distribution for the uncertainties (used by the "poisson" method).
#'
#'
#' @return
#'  This function provides a new \code{\linkS4class{TLum.Analysis}} with the new error matrix. \cr
#'
#' @author David Strebler, University of Cologne (Germany).
#'
#' @export mod_update.error

mod_update.error <- function(

  object,

  method,

  absolute.error = NULL,

  relative.error = NULL,

  k = 1

){

  list.method <- c("poisson", "absolute", "relative", "combine")
  # ------------------------------------------------------------------------------
  # Integrity Check
  # ------------------------------------------------------------------------------
  if (missing(object)){
    stop("[mod_update.error] Error: Input 'object' is missing.")
  }else if (!is(object,"TLum.Analysis")){
    stop("[mod_update.error] Error: Input 'object' is not of type 'TLum.Analysis'.")
  }

  if(!is.character(method)){
    stop("[mod_update.error] Error: Input 'method' is not of type 'character'.")
  }else if(!(tolower(method) %in% list.method)){
    stop("[mod_update.error] Error: Input 'method' is unknown.")
  }

  if(!is.null(absolute.error)){
    if(!is.numeric(absolute.error)){
      stop("[mod_update.error] Error: Input 'absolute.error' is not of type 'numeric'.")

    }else if(absolute.error < 0 ){
      stop("[mod_update.error] Error: Input 'absolute.error' is < 0.")
    }
  }

  if(!is.null(relative.error)){
    if(!is.numeric(relative.error)){
      stop("[mod_update.error] Error: Input 'relative.error' is not of type 'numeric'.")

    }else if(relative.error < 0 ){
      stop("[mod_update.error] Error: Input 'relative.error' is < 0.")
    }
  }


  if(is.null(k) && tolower(method) =="poisson" ){
    stop("[mod_update.error] Error: Input 'k' is missing")

  }else if(!is.numeric(k)){
      stop("[mod_update.error] Error: Input 'k' is not of type 'numeric'.")

  }else if(k < 0 ){
    k <- abs(k)
    warning("[mod_update.error] warning: Input 'k' is < 0.")
  }
  # ------------------------------------------------------------------------------

  method <- tolower(method)
  new.protocol <- object@protocol
  records <- object@records

  nRecords <- length(records)

  new.records <- list()


  for(i in 1:nRecords){
    temp.curve <- records[[i]]

    temp.data <- temp.curve@data
    temp.error <- temp.curve@error

    if(method == "poisson"){
      new.error <- abs(k)*sqrt(temp.data)

    }else if(method == "absolute"){
      new.error <- rep(abs(absolute.error), length(temp.error))

    }else if(method == "relative"){
      new.error <- abs(temp.data*relative.error)

    }else if(method == "combine"){
      new.error <- abs(temp.data*relative.error) + abs(absolute.error)

    }else{
      stop("[mod_update.error] Error: Input 'method' is unknown.")
    }

    new.curve <- temp.curve
    new.curve@error <- new.error

    new.records <- c(new.records, new.curve)
  }


  #----------------------------------------------------------------------------------------------
  # Generate TLum.Analysis
  #----------------------------------------------------------------------------------------------

  new.protocol <- object@protocol

  new.history <- c(object@history,
                   as.character(match.call()[[1]]))

  new.plotData <- list()

  new.plotHistory <- object@plotHistory
  new.plotHistory[[length(new.plotHistory)+1]] <- new.plotData

  new.TLum.Analysis <- set_TLum.Analysis(records= new.records,
                                         protocol=new.protocol,
                                         history = new.history,
                                         plotHistory = new.plotHistory)

  #----------------------------------------------------------------------------------------------
  #Plot results
  #----------------------------------------------------------------------------------------------

  #----------------------------------------------------------------------------------------------
  #Return results
  #----------------------------------------------------------------------------------------------


  return(new.TLum.Analysis)
}
