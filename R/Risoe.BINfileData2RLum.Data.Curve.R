#' Convert an element from a Risoe.BINfileData object to an RLum.Data.Curve
#' object
#'
#' The function converts one specified single record from a Risoe.BINfileData
#' object to an RLum.Data.Curve object.
#'
#' The function extracts all \code{METADATA} from the \code{Risoe.BINfileData}
#' object and stores them in the \code{RLum.Data.Curve} object. This function
#' can be used stand-alone, but is the base function for \code{\link{Risoe.BINfileData2RLum.Analysis}}.
#'
#' @param object \code{\linkS4class{Risoe.BINfileData}} (\bold{required}):
#' \code{Risoe.BINfileData} object
#'
#' @param id \code{\link{integer}} (\bold{required}): record id in the
#' \code{Risoe.BINfileData} object of the curve that is to be stored in the
#' \code{RLum.Data.Curve} object. If no value for id is provided, the record
#' has to be specified by \code{pos}, \code{set} and \code{run}.
#'
#' @param pos \code{\link{integer}} (optional): record position number in the
#' \code{Risoe.BINfileData} object of the curve that is to be stored in the
#' \code{RLum.Data.Curve} object. If a value for \code{id} is provided, this
#' argument is ignored.
#'
#' @param run \code{\link{integer}} (optional): record run number in the
#' \code{Risoe.BINfileData} object of the curve that is to be stored in the
#' \code{RLum.Data.Curve} object. If a value for \code{id} is provided, this
#' argument is ignored.
#'
#' @param set \code{\link{integer}} (optional): record set number in the
#' \code{Risoe.BINfileData} object of the curve that is to be stored in the
#' \code{RLum.Data.Curve} object. If a value for \code{id} is provided, this
#' argument is ignored.
#'
#' @return Returns an \code{\linkS4class{RLum.Data.Curve}} object.
#'
#' @note Due to changes in the BIN-file (version 3 to version 4) format the recalculation of TL-curves might be not
#' overall correct for cases where the TL measurement is combined with a preheat.
#'
#' @section Function version: 0.3.0
#'
#' @author Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France),
#' Christoph Burow, Universtiy of Cologne (Germany)
#'
#' @seealso \code{\link{Risoe.BINfileData2RLum.Analysis}},
#' \code{\link{set_RLum}}, \code{\linkS4class{RLum.Data.Curve}},
#' \code{\linkS4class{RLum.Analysis}}, \code{\linkS4class{Risoe.BINfileData}},
#' \code{\link{plot_RLum}}
#'
#' @references #
#'
#' @keywords manip
#'
#' @examples
#'
#' ##get package example data
#' data(ExampleData.BINfileData, envir = environment())
#'
#' ##convert one record
#' Risoe.BINfileData2RLum.Data.Curve(CWOSL.SAR.Data, id = 1)
#'
#' @noRd
.Risoe.BINfileData2RLum.Data.Curve <- function(
  object,
  id,
  pos,
  run,
  set
){


  # grep id of record -------------------------------------------------------
  ##if id is set, no input for pos and rund is nescessary
  if (missing(id)) {
    id <- object@METADATA[object@METADATA[, "POSITION"] == pos &
                            object@METADATA[, "SET"] == set &
                            object@METADATA[, "RUN"] == run,
                          "ID"]

  }


  # Select values -----------------------------------------------------------

  ##build matrix
  if(object@METADATA[id,"NPOINTS"][1] != 0){

    if(object@METADATA[id, "LTYPE"] == "TL" && as.numeric(object@METADATA[id, "VERSION"]) >=4){

      temp.x <- c(
        seq(
          from = object@METADATA[id, "LOW"],
          to = object@METADATA[id, "AN_TEMP"],
          length.out = object@METADATA[id, "TOLDELAY"]
        ),
        seq(
          from = object@METADATA[id, "AN_TEMP"],
          to = object@METADATA[id, "AN_TEMP"],
          length.out = object@METADATA[id, "TOLON"]
        ),
        seq(
          from = object@METADATA[id, "AN_TEMP"],
          to = object@METADATA[id, "HIGH"],
          length.out = object@METADATA[id, "TOLOFF"]
        )
      )

    }else{

      temp.x <- seq(
        from = object@METADATA[id, "LOW"],
        to = object@METADATA[id, "HIGH"],
        length.out = object@METADATA[id, "NPOINTS"]
      )

    }

    temp.y <- unlist(object@DATA[id], use.names = FALSE)


  }else{
    temp.x <- NA
    temp.y <- NA

    warning("[.Risoe.BINfileData2RLum.Data.Curve()] NPOINTS was 0, RLum.Data.Curve-object with NA-values produced.")

  }


  # Build object ------------------------------------------------------------
  set_RLum(
    class = "RLum.Data.Curve",
    recordType = as.character(object@METADATA[id,"LTYPE"]),
    data = matrix(c(temp.x, temp.y), ncol = 2),
    info = as.list(object@METADATA[id,]))

}
