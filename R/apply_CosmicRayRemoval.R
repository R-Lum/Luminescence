#' @title Cosmic-ray removal and spectrum smoothing for RLum.Data.Spectrum objects
#'
#' @description
#' The function provides several methods for cosmic-ray removal and spectrum
#' smoothing for [Luminescence::RLum.Data.Spectrum-class] objects, and those embedded in
#' [list] or [Luminescence::RLum.Analysis-class] objects.
#'
#' @details
#'
#' **`method = "Pych"`**
#'
#' This method applies the cosmic-ray removal algorithm described by Pych
#' (2003). There are some differences with respect to the publication:
#'
#' - For interpolation between neighbouring values, the median is used instead
#' of the mean.
#' - The number of breaks in the histogram is set to half the number of the
#' input values.
#'
#' For further details see references below.
#'
#' **Other methods**
#' Arguments supported through `...`
#' \tabular{llll}{
#' METHOD \tab ARGUMENT \tab TYPE \tab REMARKS\cr
#' `"smooth"` \tab `kind` \tab  [character] \tab see [stats::smooth] \cr
#'   \tab `twiceit` \tab  [logical] \tab see [stats::smooth] \cr
#' `"smooth.spline"` \tab `spar` \tab  [numeric] \tab see [stats::smooth.spline] \cr
#' `"smooth_RLum"` \tab `k` \tab [numeric] \tab see [Luminescence::smooth_RLum]\cr
#'  \tab `fill` \tab [numeric] \tab see [Luminescence::smooth_RLum]\cr
#'  \tab `align` \tab [character] \tab see [Luminescence::smooth_RLum]\cr
#'  \tab `method_smooth_RLum` \tab [character] \tab see [Luminescence::smooth_RLum]\cr
#'}
#'
#' **Best practice**
#'
#' There is no single silver-bullet strategy for cosmic-ray removal, as it depends
#' on the characteristic of the detector and the chosen settings. For instance,
#' high values for pixel binning will improve the light output, but also cause
#' multiple pixels to be affected by a single cosmic ray. The same is valid for
#' longer integration times. The best strategy is to combine methods and ensure
#' that the spectrum is not distorted on a case-to-case basis.
#'
#' **How to combine methods?**
#'
#' Different methods can be combined by applying the method repeatedly to the
#' dataset (see example).
#'
#' @param object [Luminescence::RLum.Data.Spectrum-class] or
#' [Luminescence::RLum.Analysis-class] (**required**):
#' input object to be treated, which can be also provided as [list]. If an
#' [Luminescence::RLum.Analysis-class] object is provided, only its
#' [Luminescence::RLum.Data.Spectrum-class]
#' objects are treated. Please note: this mixing of objects does not work for
#' a list of `RLum.Data` objects.
#'
#' @param method [character] (*with default*):
#' Defines method that is applied for cosmic ray removal. Allowed methods are
#' `smooth`, the default, ([stats::smooth]), `smooth.spline` ([stats::smooth.spline]),
#' `smooth_RLum` ([Luminescence::smooth_RLum]) and `Pych`. See details for further information.
#'
#' @param method.Pych.smoothing [integer] (*with default*):
#' Smoothing parameter for cosmic ray removal according to Pych (2003).
#' The value defines how many neighbouring values in each frame are used for smoothing
#' (e.g., `2` means that the two previous and two following values are used).
#'
#' @param method.Pych.threshold_factor [numeric] (*with default*):
#' Threshold for zero-bins in the histogram. Small values mean that more peaks
#' are removed, but signal might be also affected by this removal.
#'
#' @param MARGIN [integer] (*with default*):
#' on which part the function cosmic ray removal should be applied on:
#'
#' - 1 = along the time axis (line by line),
#' - 2 = along the wavelength axis (column by column).
#'
#' **Note:** This argument only affects methods `smooth`, `smooth.spline` and
#' `smooth_RLum`.
#'
#' @param verbose [logical] (*with default*):
#' enable/disable output to the terminal.
#'
#' @param plot [logical] (*with default*):
#' If `TRUE` the histograms used for the cosmic-ray removal are returned as plot
#' including the used threshold. Note: A separate plot is returned for each frame!
#' Currently only for `method = "Pych"` a graphical output is provided.
#'
#' @param ... further arguments and graphical parameters that will be passed
#' to the [stats::smooth], [stats::smooth.spline] or [Luminescence::smooth_RLum].
#' See details for more information.
#'
#' @return Returns same object as input.
#'
#' @section Function version: 0.4.1
#'
#' @author Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#'
#' @seealso [Luminescence::RLum.Data.Spectrum-class], [Luminescence::RLum.Analysis-class],
#' [stats::smooth], [stats::smooth.spline], [Luminescence::smooth_RLum]
#'
#' @references
#' Pych, W., 2004. A Fast Algorithm for Cosmic-Ray Removal from
#' Single Images. The Astronomical Society of the Pacific 116 (816), 148-153.
#' \doi{10.1086/381786}
#'
#' @keywords manip
#'
#' @examples
#'
#' ##(1) - use with your own data and combine (uncomment for usage)
#' ## run two times the default method and smooth with another method
#' ## your.spectrum <- apply_CosmicRayRemoval(your.spectrum, method = "Pych")
#' ## your.spectrum <- apply_CosmicRayRemoval(your.spectrum, method = "Pych")
#' ## your.spectrum <- apply_CosmicRayRemoval(your.spectrum, method = "smooth")
#'
#' @export
apply_CosmicRayRemoval <- function(
  object,
  method = "smooth",
  method.Pych.smoothing = 2,
  method.Pych.threshold_factor = 3,
  MARGIN = 2,
  verbose = FALSE,
  plot = FALSE,
  ...
) {
  .set_function_name("apply_CosmicRayRemoval")
  on.exit(.unset_function_name(), add = TRUE)

  # Self-call ----------------------------------------------------------------------------------
  ##Black magic: The function recalls itself until all RLum.Data.Spectrum objects have been treated
  ##If you want to test the basics of the function please only use a single RLum.Data.Spectrum-object
  ##if it comes in as an RLum.Analysis object ... make a list out of it
  class_original <- NULL
  if(inherits(object, "RLum.Analysis")){
    object <- list(object)
    class_original <- "RLum.Analysis"
  }

  ##handle the list and recall
  if(inherits(object, "list")){
    results_list <- lapply(object, function(o){
      ##RLum.Analysis
      if(inherits(o, "RLum.Analysis")){
         ##get id of RLum.Data.Spectrum objects in this object
        record_id.spectra <- vapply(o@records, inherits, "RLum.Data.Spectrum",
                                    FUN.VALUE = logical(1))

         ##rewrite o
         temp_o <- o@records[record_id.spectra]

      }else{
        record_id.spectra <- NULL
        temp_o <- o
      }

      ##call function
      results <- do.call(
        what = apply_CosmicRayRemoval,
        args = c(list(
          object = temp_o,
          method = method,
          method.Pych.smoothing = method.Pych.smoothing,
          method.Pych.threshold_factor = method.Pych.threshold_factor,
          MARGIN = MARGIN,
          verbose = verbose,
          plot = plot),
          list(...)))

      ##combine in RLum.Analysis object if needed
      if(!is.null(record_id.spectra)){
        o@records[record_id.spectra] <- results
        return(o)
      }else{
        return(results)
      }
    })

    ##final return, make sure that we return what we had as input
    if(!is.null(class_original)){
      return(results_list[[1]])
    }else{
      return(results_list)
    }
  }

  ## Integrity checks -------------------------------------------------------
  .validate_class(object, "RLum.Data.Spectrum")
  if (length(object@data) < 2)
    .throw_error("'object' contains no data")

  .validate_args(method, c("smooth", "smooth.spline", "smooth_RLum", "Pych"))
  .validate_positive_scalar(method.Pych.smoothing, int = TRUE)
  .validate_positive_scalar(method.Pych.threshold_factor)
  .validate_args(MARGIN, c(1, 2))
  .validate_logical_scalar(verbose)
  .validate_logical_scalar(plot)

  ##deal with additional arguments
  extraArgs <- modifyList(
    x = list(
      kind = "3RS3R",
      twiceit = TRUE,
      spar = NULL,
      method_smooth_RLum = "median",
      k = NULL,
      fill = NA,
      align = "right"),
    val = list(...),
    keep.null = TRUE)

  # Apply methods -0------------------------------------------------------------
  object.data.temp.smooth <- switch(method,

  ## +++++++++++++++++++++++++++++++++++ (smooth) ++++++++++++++++++++++++++++##
  smooth = {
    ##apply smooth
    apply(
      X = object@data,
      MARGIN = MARGIN,
      FUN = stats::smooth,
      kind = extraArgs$kind[1],
      twiceit = extraArgs$twiceit[1]
    )
  },

  ## +++++++++++++++++++++++++++++++++++ (smooth.spline) +++++++++++++++++++++##
  smooth.spline = {
    ## wrap the function in a new function to access the data more easily
    temp_smooth.spline <- function(x, spar){
      stats::smooth.spline(x, spar = spar)$y
    }

    ##apply smooth.spline
    apply(
        X = object@data,
        MARGIN = MARGIN,
        FUN = temp_smooth.spline,
        spar = extraArgs$spar[1])
  },

  ## +++++++++++++++++++++++++++++++++++ (smooth_RLum) +++++++++++++++++++++##
  smooth_RLum = {
    object.data.temp.smooth <- apply(
        X = object@data,
        MARGIN = MARGIN,
        FUN = .smoothing,
        method = extraArgs$method_smooth_RLum[1],
        k = max(c(1, min(c(
          if(MARGIN == 2)
           floor(ncol(object@data)/(ncol(object@data)/2))
          else
           floor(nrow(object@data)/(nrow(object@data)/2)),
          extraArgs$k[1])))),
        fill = extraArgs$fill[1],
        align = extraArgs$align[1])

    ## remove NA values
    if (is.na(extraArgs$fill[1])) {
      id_NA <- which(matrixStats::rowAnyNAs(object.data.temp.smooth))
      if (length(id_NA) > 0) {
        object.data.temp.smooth <- object.data.temp.smooth[-id_NA, , drop = FALSE]
        if (MARGIN == 1)
          object@data <- object@data[,-id_NA, drop = FALSE]
        else
          object@data <- object@data[-id_NA,, drop = FALSE]
      }
    }
    object.data.temp.smooth
  },

  ## +++++++++++++++++++++++++++++++++++ (Pych) ++++++++++++++++++++++++++++++##
  Pych = {
    ## apply smoothing
    frame.idx <- 0
    apply(object@data, 2, function(data.col) {
      frame.idx <<- frame.idx + 1
      ##(1) - calculate sd for each subframe
      temp.sd <- sd(data.col)

      ##(2) - correct estimation of sd by 1-sigma clipping
      temp.sd.corr <- sd(data.col[data.col >= (mean(data.col) - temp.sd) &
                                  data.col <= (mean(data.col) + temp.sd)])

      ##(3) - construct histogram of count distribution
      temp.hist <- hist(data.col,
                        breaks = length(data.col)/2, plot = FALSE)

      ##(4) - find mode of the histogram (e.g. peak)
      temp.hist.max <- which.max(temp.hist$counts)

      ##(5) - find gaps in the histogram (bins with zero value)
      temp.hist.zerobin <- which(temp.hist$counts == 0)

      ##(5.1)
      ##select just values right from the peak
      temp.hist.zerobin <- temp.hist.zerobin[
        (temp.hist.max[1] + 1):length(temp.hist.zerobin)]

      ##(5.2)
      ##select non-zerobins
      temp.hist.nonzerobin <- which(temp.hist$counts != 0)
      temp.hist.nonzerobin <- temp.hist.nonzerobin[
        temp.hist.nonzerobin >=  (temp.hist.zerobin[1]-1)]

      ##(6) - find the first gap which is wider than the threshold
      temp.hist.nonzerobin.diff <- diff(
        temp.hist$breaks[temp.hist.nonzerobin])

      ## select the first value where the threshold is reached
      ## factor 3 is defined by Pych (2003)
      temp.hist.thres <- which(
        temp.hist.nonzerobin.diff >= method.Pych.threshold_factor * temp.sd.corr)[1]

      ##(7) - use counts above the threshold and recalculate values
      ## on all further values
      if(!is.na(temp.hist.thres)){
        data.col <- sapply(1:length(data.col), function(n) {

          if((n + method.Pych.smoothing) <= length(data.col) &
             (n - method.Pych.smoothing) >= 0){
            ifelse(
              data.col[n] >= temp.hist$breaks[temp.hist.thres],
              median(data.col[(n-method.Pych.smoothing):(n+method.Pych.smoothing)]),
              data.col[n])
          }else{
            data.col[n]
          }
        })
      }

      ##(8) - return histogram used for the removal as plot
      if(plot){
        plot(temp.hist,
             xlab = "Signal intensity [a.u.]",
             main = "Cosmic-ray removal histogram")

        abline(v = temp.hist$breaks[temp.hist.thres],
               col = "red")

        mtext.text <- sprintf("Frame: %d (%s)", frame.idx, colnames(object@data)[frame.idx])
        if(!is.na(temp.hist$breaks[temp.hist.thres])){
          legend("topright", "threshold" ,lty = 1, lwd = 1, col = "red", bty = "n")
        }else{
          mtext.text <- paste(mtext.text, "- no threshold applied")
        }
        mtext(side = 3, mtext.text)
      }

      ##(9) - return information on the amount of removed cosmic-rays
      if(verbose){
        #sum up removed counts values above the threshold
        sum.corrected.channels <- try(
          sum(temp.hist$counts[temp.hist.thres:length(temp.hist$counts)]),
          silent = TRUE)

        if (inherits(sum.corrected.channels, "try-error"))
          sum.corrected.channels <- 0

        .throw_message(">> ", sum.corrected.channels,
                       " channels corrected in frame ", frame.idx, "\n", error = FALSE)
      }

      return(data.col)
    })#end loop
  })

  ## Rotate matrix if required
  if (MARGIN[1] == 1 && method != "Pych")
    object.data.temp.smooth <- t(object.data.temp.smooth)

  # Correct row and column names --------------------------------------------
  colnames(object.data.temp.smooth) <- colnames(object@data)
  rownames(object.data.temp.smooth) <- rownames(object@data)

  # Return Output------------------------------------------------------------
  set_RLum(
    class = "RLum.Data.Spectrum",
    recordType = object@recordType,
    curveType = object@curveType,
    data = object.data.temp.smooth,
    info = object@info)
}
