#' @title Calculate Lx/Tx ratio for decomposed CW-OSL signal components
#'
#' @description Calculate `Lx/Tx` ratios from a given set of decomposed
#' CW-OSL curves decomposed by `OSLdecomposition::RLum.OSL_decomposition`.
#'
#' @param OSL.component [integer] or [character] (*optional*):
#' a single index or a name describing which OSL signal component shall be evaluated.
#' This argument can either be the name of the OSL component assigned by
#' `OSLdecomposition::RLum.OSL_global_fitting` or the index of component.
#' Then `'1'` selects the fastest decaying component, `'2'` the
#' second fastest and so on. If not defined, the fastest decaying component is selected.
#'
#' @param Lx.data [data.frame] (**required**): Component table created by
#' `OSLdecomposition::RLum.OSL_decomposition` and per default located
#' at `object@records[[...]]@info$COMPONENTS`.The value of `$n[OSL.component]`
#' is set as `LnLx`. The value of `$n.error[OSL.component]` is set as `LnLx.error`
#'
#' @param Tx.data [data.frame] (*optional*): Component table created by
#' `OSLdecomposition::RLum.OSL_decomposition` and per default located at
#' `object@records[[...]]@info$COMPONENTS`. The value of `$n[OSL.component]`
#' is set as `TnTx`. The value of `$n.error[OSL.component]` is set as `TnTx.error`
#'
#' @param sig0 [numeric] (*with default*): allows adding an extra error component
#' to the final `Lx/Tx` error value (e.g., instrumental error).
#'
#' @param digits [integer] (*with default*): round numbers to the specified digits.
#' If digits is set to `NULL` nothing is rounded.
#'
#' @return Returns an S4 object of type [RLum.Results-class].
#'
#' Slot `data` contains a [list] with the following structure:
#'
#' **@data**
#' ```
#' $LxTx.table (data.frame)
#' .. $ LnLx
#' .. $ TnTx
#' .. $ Net_LnLx
#' .. $ Net_LnLx.Error
#' .. $ Net_TnTx
#' .. $ Net_TnTx.Error
#' .. $ LxTx
#' .. $ LxTx.relError
#' .. $ LxTx.Error
#' ```
#'
#' @section Function version: 0.1.0
#'
#' @author Dirk Mittelstrass
#'
#' @seealso [RLum.Data.Curve-class], [fit_DoseResponseCurve], [analyse_SAR.CWOSL]
#'
#' @references Mittelstrass D., Schmidt C., Beyer J., Straessner A., 2019.
#' Automated identification and separation of quartz CW-OSL signal components with R.
#' talk presented at DLED 2019, Bingen, Germany
#' [http://luminescence.de/OSLdecomp_talk.pdf]()\cr
#'
#' @keywords datagen
#' @md
#' @export
calc_OSLLxTxDecomposed <- function(
  Lx.data,
  Tx.data = NULL,
  OSL.component = 1L,
  sig0 = 0,
  digits = NULL
) {
  .set_function_name("calc_OSLLxTxDecomposed")
  on.exit(.unset_function_name(), add = TRUE)

  # ToDo:
  # - Integrity checks for the component table
  # - Handle background-signal-component if present
  # - add Tx.data integrity checks
  # - add previous-residual-subtraction functionality
  # - add list with decomposition algorithm parameters to return object
  # - add example in documentation

  ##--------------------------------------------------------------------------##
  ## (1) - integrity checks
  .validate_class(Lx.data, "data.frame")
  if (nrow(Lx.data) < 2)
    .throw_error("No valid component data.frame for Lx value")
  if (!all(c("n", "n.error") %in% colnames(Lx.data)))
    .throw_error("'Lx.data' should contain the following columns: 'n', 'n.error'")

  if (!is.null(Tx.data)) {
    .validate_class(Tx.data, "data.frame")
    if (nrow(Tx.data) < 2)
      .throw_error("No valid component data.frame for Tx value")
    if (!all(c("n", "n.error") %in% colnames(Tx.data)))
      .throw_error("'Tx.data' should contain the following columns: 'n', 'n.error'")
  }

  # define the component
  component_index <- NA

  #select only the first element; we do this silently because it is clearly
  #written in the documentation
  OSL.component <- OSL.component[1]
  .validate_class(OSL.component, c("integer", "numeric", "character"))

  # get component index from component name
  if (is.character(OSL.component)) {
    if (tolower(OSL.component) %in% tolower(Lx.data$name)) {
      ## FIXME(mcol): this seems unreachable, as Lx.data doesn't store the
      ## component names
      component_index <- which(tolower(OSL.component) == tolower(Lx.data$name)) # nocov

    } else {
      .throw_error("Invalid OSL component name, valid names are: ",
                   .collapse(Lx.data$name))
    }
  }

  # if a numeric is given, check if it matches with any component index
  if (is.numeric(OSL.component)) {
    OSL.component <- as.integer(OSL.component)
    if (OSL.component %in% 1:nrow(Lx.data)) {
      component_index <- OSL.component

      # insert background-signal-component check here

    } else {
      .throw_error("Invalid OSL component index, ",
                   "component table has ", nrow(Lx.data), " rows")
    }
  }

  .validate_positive_scalar(digits, int = TRUE, null.ok = TRUE)

  ##--------------------------------------------------------------------------##
  ## (2) - extract Lx and Tx values

  LnLx <- Lx.data$n[component_index]
  LnLx.Error <- Lx.data$n.error[component_index]

  TnTx <- 1
  TnTx.Error <- 0
  if (!is.null(Tx.data)) {
    TnTx <- Tx.data$n[component_index]
    TnTx.Error <- Tx.data$n.error[component_index]
  }

  ##combine results
  LnLxTnTx <- cbind(
    LnLx,
    LnLx.Error,
    TnTx,
    TnTx.Error
  )

  ##--------------------------------------------------------------------------##
  ##(4) Calculate LxTx error according Galbraith (2014)

  ## transform results to a data.frame
  LnLxTnTx <- as.data.frame(LnLxTnTx)
  colnames(LnLxTnTx)<-c("Net_LnLx", "Net_LnLx.Error",
                        "Net_TnTx", "Net_TnTx.Error")

  temp <- .calculate_LxTx_error(LnLxTnTx, sig0, digits)

  # ToDo: Add decomposition algorithm parameters here
  # calc.parameters <- list(...)

  ##set results object
  return(set_RLum(
      class = "RLum.Results",
      data = list(
        LxTx.table = temp),
      #  calc.parameters = calc.parameters),
      info = list(call = sys.call())
  ))
}
