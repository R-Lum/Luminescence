#' @title Moran Scatter Plot: Visualizing Spatial Dependency
#'
#' @description Scatter plot, with on the x axis the original grain signal and on the y axis
#' the weighted mean of the neighbour grain signals. The plot area is divided into four quadrants,
#' and also a least square line (which slopes indicates, but not exactly represents, Moran's I) and
#' an 1:1 line (which indicates a Moran's I of around 1).
#'
#' @param object [RLum.Results-class] or [numeric] (**required**): containing a numerical vector of length 100,
#' representing one or more measurement discs ("positions") in a reader.
#' Each element in the vector represents one grain hole location on a disc.
#'
#' @param df_neighbours [data.frame] (*with default*) Data frame indicating
#' which borders to consider, and their respective weights (see the description
#' provided for [calc_MoransI]). If `NULL` (default), this is constructed
#' automatically by the internal function `.get_Neighbours`.
#'
#' @param str_y_def [character] (*with default*) Calculation of y position. Defaults to `"mean_neighbours"`
#' which is the plain mean of all neighbour values and the most illustrative. The other option is `"weighted_sum"`,
#' which means the sum of the border weights times the neighbour values, which is actually closer
#' to the way Moran's I is by default calculated in this package.
#'
#' @param ... Other parameters to be forwarded to the base R plot functions.
#' `legend` (`TRUE/FALSE`) to enable/disable the legend.
#' Note that `xlab` (x axis label), `ylab` (y axis label) and `cex` (scaling
#' value) are given default values. Because of sometimes large value
#' differences, `log = "x"`, `log = "y"` and `log = "xy"`are supported.
#' In case of negative values and logarithmic plotting, values are increased
#' so the smallest value to plot is 1. Summary elements such as means, least
#' square line etc. will still be based on the linear case.
#' `pch` accepts options "show_location_ids"` (plots grain location id's), `"show_n_neighbours"`
#' (indicates numbers of neighbours) and the normal base plot `pch` options.
#'
#' @details Note that this function plots on the y-axis the mean of the neighbours, while the function
#' [calc_MoransI] by default will for its global calculation weight every border the same. So, grain locations
#' with 1, 2 or 3 neighbours will appear higher on the y-axis than their influence on Moran's I justify -- apart
#' from scaling, this explains a part of the differences of Moran's scatter plots between different packages.
#' Also note that island' grain locations (=those not bordering other grains) are left out of these plots but
#' might still influence Moran's I calculations.
#'
#' @return Returns (invisibly) a data frame with plotting coordinates and
#' grain location id's, for creating user-defined plots.
#'
#' @examples
#' plot_MoranScatterplot(1:100)
#'
#' @author Anna-Maartje de Boer, Luc Steinbuch, Wageningen University & Research, 2025
#'
#' @references
#' de Boer, A-M., Steinbuch, L., Heuvelink, G.B.M., Wallinga, J., 2025.
#' A novel tool to assess crosstalk in single-grain luminescence detection.
#' Submitted.
#'
#' @md
#' @export
plot_MoranScatterplot <- function(
    object,
    df_neighbours = NULL,
    str_y_def = "mean_neighbours", # , mean_neighbours, weighted_sum
    ...
) {
  .set_function_name("plot_MoranScatterplot")
  on.exit(.unset_function_name(), add = TRUE)

  ## Integrity checks -------------------------------------------------------

  .validate_class(object, c("RLum.Results", "numeric", "integer"))
  if (is.numeric(object)) {
    vn_values <- object
  } else {
    vn_values <- get_RLum(object)
  }
  .validate_length(vn_values, 100, name = "'object'")

  if (is.null(df_neighbours)) {
    df_neighbours <- .get_Neighbours(object)
  } else {
    .validate_class(df_neighbours, "data.frame")
    if (ncol(df_neighbours) != 3)
      .throw_error("'df_neighbours' should be a data frame with 3 columns")
  }

  .validate_args(str_y_def, c("mean_neighbours", "weighted_sum"))

  ## get ... arguments
  plot_settings <- modifyList(
      x = list(
          legend = TRUE,
          pch = 20,
          cex = 0.8,
          xlab = "Value central grain",
          ylab = ifelse(str_y_def == "weighted_sum",
                        "Weighted sum neighbouring grains",
                        "Plain mean neighbour grains"),
          log = ""),
      val = list(...))

  vs_points_appearance <- plot_settings$pch

  show_location_ids <- FALSE
  show_n_neighbours <- FALSE
  if(plot_settings$pch == "show_location_ids") {
    show_location_ids <- TRUE

  } else if (plot_settings$pch == "show_n_neighbours") {
    show_n_neighbours <- TRUE
 }

  vs_log <- plot_settings$log  # because we need the function "log" later

  df_moran_plot <- data.frame(
    x = vn_values,
    y = NA,
    grain_id = NA,
    n_neighbours = NA)

  vn_locations <- unique(c(df_neighbours$location, df_neighbours$neighbour))
  for(i in 1:length(vn_locations) ) {
    n_location <- vn_locations[i]
    ## Find the neighbouring locations according to df_neighbour
    ## For a given n_location, in which row of df_neighbour are neighbouring locations provided?
    vi_neighbour_row <- which(df_neighbours$location == n_location)
    ## And thus, which locations are provided in these rows?
    vn_neighbour <- df_neighbours$neighbour[vi_neighbour_row]
    ## And add weights
    vn_weights <- df_neighbours$weight[vi_neighbour_row]
    df_neighbour_and_weights <- data.frame(adj_pos = vn_neighbour,
                                           weight = vn_weights)

    ## Find the mirror neighbouring locations ("locations") according to df_neighbour
    ## For a given n_location, in which row of df_neighbour are locations provided?
    vi_neighbour_mirror_row <- which(df_neighbours$neighbour == n_location)
    ## And thus, which locations are provided in these rows?
    vn_neighbour_mirror <- df_neighbours$location[vi_neighbour_mirror_row]
    ## And add weights
    vn_weights <- df_neighbours$weight[vi_neighbour_mirror_row]

    df_neighbour_and_weights <- rbind(df_neighbour_and_weights,
                                      data.frame(adj_pos = vn_neighbour_mirror,
                                                 weight = vn_weights)
    )


    if(str_y_def == "mean_neighbours") {
      n_mean_of_neighbours <- mean(
        df_moran_plot$x[df_neighbour_and_weights$adj_pos]*
          df_neighbour_and_weights$weight, na.rm = TRUE)

      df_moran_plot$y[n_location]  <- n_mean_of_neighbours

    } else if(str_y_def == "weighted_sum") {
      n_weighted_sum_neighbours <- sum(
        df_moran_plot$x[df_neighbour_and_weights$adj_pos]*
          df_neighbour_and_weights$weight, na.rm = TRUE) ## NOG AANPASSEN

      df_moran_plot$y[n_location]  <- n_weighted_sum_neighbours
    }

    ## For plotting: how many neighbours were involved?
    df_moran_plot$n_neighbours[n_location] <-
      length(vi_neighbour_mirror_row) + length(vi_neighbour_row)

    df_moran_plot$grain_id[n_location] <- n_location
  }

  n_grains_without_neighbour <- nrow(df_moran_plot) -
    sum(stats::complete.cases(df_moran_plot$y))

  if(n_grains_without_neighbour>0)
    .throw_warning("Grain observations removed from plot because no neighbours")

  df_moran_plot <- df_moran_plot[stats::complete.cases(df_moran_plot), ]

  ## If we want a log plot, make sure that all relevant values are 1 or more (per axis)
  if (grepl("x", vs_log, fixed = TRUE) && min(df_moran_plot$x) < 1) {
    if (min(df_moran_plot$x) < 0) {
      df_moran_plot$x <- df_moran_plot$x - min(df_moran_plot$x) + 1
    } else {
      df_moran_plot$x <- df_moran_plot$x + (1 - min(df_moran_plot$x))
    }
    .throw_warning("x-axis values rescaled because of log transform ",
                   "(also in return df)")
  }

  if (grepl("y", vs_log, fixed = TRUE) && min(df_moran_plot$y) < 1) {
    if (min(df_moran_plot$y) < 0) {
      df_moran_plot$y <- df_moran_plot$y - min(df_moran_plot$y) + 1

    } else {
      df_moran_plot$y <- df_moran_plot$y + (1 - min(df_moran_plot$y))
    }
    .throw_warning("y-axis values rescaled because of log transform ",
                   "(also in return df)")
  }

  ## Set point appearance
  vs_pch <- if (vs_points_appearance == "show_location_ids" ||
                vs_points_appearance == "show_n_neighbours") {
    NA # Numbers will be added later
  } else {
    plot_settings$pch
  }

  ## plot
  ## remove used arguments to avoid collusion
  args <- list(...)
  args <- args[!...names() %in% c("pch", "log", "xlab", "ylab", "cex", "legend")]

  do.call(
    what = plot,
    args = c(
     args,
     list(
      df_moran_plot[,1:2],
      pch = vs_pch,
      log = vs_log,
      xlab = plot_settings$xlab,
      ylab = plot_settings$ylab,
      cex = plot_settings$cex)))

  if(vs_points_appearance == "show_location_ids") {
    text(x = df_moran_plot[,1:2],
         labels = df_moran_plot$grain_id,
         cex = plot_settings$cex)
  }

  if(vs_points_appearance == "show_n_neighbours") {
    text(x = df_moran_plot[,1:2],
         labels = df_moran_plot$n_neighbours,
         cex = plot_settings$cex)
  }


  ## Mean lines
  abline(v = mean(df_moran_plot$x), h = mean(df_moran_plot$y), lty = "dotted", untf = TRUE)

  ## 1:1 one, only makes only sense for the plain mean of neighbours
  if(str_y_def == "mean_neighbours")
  {
    abline(a = mean(df_moran_plot$y) - mean(df_moran_plot$x), b = 1, lty = "dotdash", untf = TRUE)
  }

  ## Calculate least square line
  lm_moransI <- stats::lm(y ~ x, data = df_moran_plot)
  abline(a = coef(lm_moransI)[1], b = coef(lm_moransI)[2], col = "orange", untf=TRUE)

  if (plot_settings$legend[1]) {
    vs_legend <- c("means",  "least square")
    vs_cols   <- c("black",  "orange")
    vs_lty    <- c("dotted", "solid")
    vs_pch    <- c(NA,       NA)

    if(str_y_def == "mean_neighbours") {
      vs_legend <- c(vs_legend, "1:1 line")
      vs_cols   <- c(vs_cols,   "black")
      vs_lty    <- c(vs_lty,    "dotdash")
      vs_pch    <- c(vs_pch,    NA)
    }

    if(vs_points_appearance == "show_location_ids") {
      vs_legend <- c(vs_legend, "location ids")
      vs_cols   <- c(vs_cols,   "black")
      vs_lty    <- c(vs_lty,    NA)
      vs_pch    <- c(vs_pch,    "#")
    }

    if(vs_points_appearance == "show_n_neighbours") {
      vs_legend <- c(vs_legend, "numb. of neighb.")
      vs_cols   <- c(vs_cols,   "black")
      vs_lty    <- c(vs_lty,    NA)
      vs_pch    <- c(vs_pch,    "#")
    }

    legend(x = "topleft",
           legend = vs_legend,
           col    = vs_cols,
           lty    = vs_lty,
           pch = vs_pch,
           cex    = 0.8)
  }

  invisible(df_moran_plot)
}
