#' Moran scatter plot: visualizing spatial dependency
#'
#' Scatter plot, with on the x axis the original grain signal and on the y axis
#' the weighted mean of the neighbor grain signals. The plot area is divided into four quadrants,
#' and also a least square line (which slopes indicates, but not exactly represents, Moran's I) and
#' an 1:1 line (which indicates a Moran's I of around 1).
#'
#' @param object [RLum.Results-class] or [numeric] (**required**): containing a numerical vector of length 100,
#' representing one or more measurement discs ("positions") in a reader.
#' Each element in the vector represents one grain hole location on a disc.
#'
#' @param df_neighbour [data.frame] (*with default*) Dataframe, indicating
#' which borders to consider, and their respective weights.
#' Defaults to `get_Neighbour(object = vn_values)`.
#'
#' @param bo_show_legend [logical] (*with default*) Single Boolean. If `TRUE`
#' (default), a small legend explaining the different lines is plotted.
#'
#' @param str_y_def [character] (*with default*) Calculation of y position. Defaults to `"mean_neighbours"`
#' which is the plain mean of all neighbour values and the most illustrative. The other option is `"weighted_sum"`,
#' which means the sum of the border weights times the neighbour values, which is actually closer
#' to the way Moran's I is by default calculated in this package.
#'
#' @param pch [character], [integer] (*with default*): Accepts options
#' `"show_location_ids"` (plots grain location id's), `"show_n_neighbours"`
#' (indicates numbers of neighbours) and the normal base plot `pch` options.
#'
#' @param cex FIXME(mcol)
#' @param xlab FIXME(mcol)
#' @param ylab FIXME(mcol)
#' @param log FIXME(mcol)
#'
#' @param ... Other parameters to be forwarded to the base R plot functions.
#' Note that `xlab` (x axis label), `ylab` (y axis label) and `cex` (scaling
#' value) are given default values. Because of sometimes large value
#' differences, `log = "x"`, `log = "y"` and `log = "xy"`are supported.
#' In case of negative values and logarithmic plotting, values are increased
#' so the smallest value to plot is 1. Summary elements such as means, least
#' square line etc. will still be based on the linear case.
#'
#' @details Note that this function plots on the y-axis the mean of the neighbours, while the function
#' `calc_MoransI()` by default will for its global calculation weight every border the same. So, grain locations
#' with 1, 2 or 3 neighbours will appear higher on the y-axis than their influence on Moran's I justify -- apart
#' from scaling, this explains a part of the differences of Moran's scatter plots between different packages.
#' Also note that island' grain locations (=those not bordering other grains) are left out of these plots but
#' might still influence Moran's I calculations.
#'
#'
#' @return Returns (invisibly) a data frame with plotting coordinates and
#' grain location id's, for creating user-defined plots.
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
plot_MoranScatterplot <- function(object,
                                  df_neighbour= get_Neighbour(object = object),
                                  bo_show_legend = TRUE,
                                  str_y_def = "mean_neighbours", # , mean_neighbours, weighted_sum
                                  pch  = 20, #"show_location_ids", "show_n_neighbours", pch-value
                                  cex  = 0.8,
                                  xlab = "Value central grain",
                                  ylab = ifelse(str_y_def == "weighted_sum",
                                                yes = "Weighted sum neighbouring grains",
                                                no = "Plain mean neighbour grains"),
                                  log = "",
                                  ...
) {
  .set_function_name("plot_MoranScatterplot")
  on.exit(.unset_function_name(), add = TRUE)

  ## Validate input arguments; set variables  -----------------------

  .validate_class(object, c("RLum.Results", "numeric", "integer"))
  ## To add:
  #  - should contain a numerical vector of length 100

  .validate_class(df_neighbour, "data.frame")
  if (ncol(df_neighbour) != 3)
    .throw_error("'df_neighbour' should be a data frame with 3 columns")
  .validate_class(bo_show_legend, "logical")
  .validate_args(str_y_def, c("mean_neighbours", "weighted_sum"))

  if(is.numeric(object))
  {
    vn_values <- object
  } else
  {
    vn_values <- get_RLum(object)
  }


  vs_points_appearance <- pch

  bo_show_location_ids <- FALSE
  bo_show_n_neighbours <- FALSE
  if(pch == "show_location_ids")
  {
    bo_show_location_ids <- TRUE
  }
  else if (pch == "show_n_neighbours") {
    bo_show_n_neighbours <- TRUE
  }


  vs_log <- log  # because we need the function "log" later

  df_moran_plot <- data.frame(x = vn_values,
                              y = NA,
                              grain_id = NA,
                              n_neighbours = NA)

  vn_locations <- unique(c(df_neighbour$location, df_neighbour$neighbour))
  for(i in 1:length(vn_locations) )
  {
    n_location <- vn_locations[i]
    ## Find the neighbouring locations according to df_neighbour
    ## For a given n_location, in which row of df_neighbour are neighbouring locations provided?
    vi_neighbour_row <- which(df_neighbour$location == n_location)
    ## And thus, which locations are provided in these rows?
    vn_neighbour <- df_neighbour$neighbour[vi_neighbour_row]
    ## And add weights
    vn_weights <- df_neighbour$weight[vi_neighbour_row]
    df_neighbour_and_weights <- data.frame(adj_pos = vn_neighbour,
                                           weight = vn_weights)

    ## Find the mirror neighbouring locations ("locations") according to df_neighbour
    ## For a given n_location, in which row of df_neighbour are locations provided?
    vi_neighbour_mirror_row <- which(df_neighbour$neighbour == n_location)
    ## And thus, which locations are provided in these rows?
    vn_neighbour_mirror <- df_neighbour$location[vi_neighbour_mirror_row]
    ## And add weights
    vn_weights <- df_neighbour$weight[vi_neighbour_mirror_row]

    df_neighbour_and_weights <- rbind(df_neighbour_and_weights,
                                      data.frame(adj_pos = vn_neighbour_mirror,
                                                 weight = vn_weights)
    )


    if(str_y_def == "mean_neighbours")
    {
      n_mean_of_neighbours <- mean(
        df_moran_plot$x[df_neighbour_and_weights$adj_pos]*
          df_neighbour_and_weights$weight, na.rm = TRUE)

      df_moran_plot$y[n_location]  <- n_mean_of_neighbours
    }
    else if(str_y_def == "weighted_sum") {
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
    sum(complete.cases(df_moran_plot$y))

  if(n_grains_without_neighbour>0)
    .throw_warning("Grain observations removed from plot because no neighbours")

  df_moran_plot <- df_moran_plot[complete.cases(df_moran_plot), ]

  ## If we want a log plot, make sure that all relevant values are 1 or more (per axis)
  if (grepl("x", vs_log, fixed = TRUE) && min(df_moran_plot$x) < 1) {
    if (min(df_moran_plot$x) < 0)
    {
      df_moran_plot$x <- df_moran_plot$x - min(df_moran_plot$x) + 1
    } else
    {
      df_moran_plot$x <- df_moran_plot$x + (1 - min(df_moran_plot$x))
    }
    .throw_warning("x-axis values rescaled because of log transform ",
                   "(also in return df)")
  }

  if (grepl("y", vs_log, fixed = TRUE) && min(df_moran_plot$y) < 1) {
    if (min(df_moran_plot$y) < 0)
    {
      df_moran_plot$y <- df_moran_plot$y - min(df_moran_plot$y) + 1
    } else
    {
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
    pch
  }

  plot(df_moran_plot[,1:2],
       pch = vs_pch,
       log = vs_log,
       xlab = xlab,
       ylab = ylab,
       cex = cex,
       ...
  )

  if(vs_points_appearance == "show_location_ids")
  {
    text(x = df_moran_plot[,1:2],
         labels = df_moran_plot$grain_id,
         cex = cex)
  }
  if(vs_points_appearance == "show_n_neighbours")
  {
    text(x = df_moran_plot[,1:2],
         labels = df_moran_plot$n_neighbours,
         cex = cex)
  }


  ## Mean lines
  abline(v = mean(df_moran_plot$x), h = mean(df_moran_plot$y), lty = "dotted", untf = TRUE)

  ## 1:1 one, only makes only sense for the plain mean of neighbours
  if(str_y_def == "mean_neighbours")
  {
    abline(a = mean(df_moran_plot$y) - mean(df_moran_plot$x), b = 1, lty = "dotdash", untf = TRUE)
  }

  ## Calculate least square line
  lm_moransI <- lm(y ~  x, data = df_moran_plot)
  abline(a = coef(lm_moransI)[1], b = coef(lm_moransI)[2], col = "orange", untf=TRUE)

  if(bo_show_legend){
    vs_legend <- c("means",  "least square")
    vs_cols   <- c("black",  "orange")
    vs_lty    <- c("dotted", "solid")
    vs_pch    <- c(NA,       NA)

    if(str_y_def == "mean_neighbours")
    {
      vs_legend <- c(vs_legend, "1:1 line")
      vs_cols   <- c(vs_cols,   "black")
      vs_lty    <- c(vs_lty,    "dotdash")
      vs_pch    <- c(vs_pch,    NA)
    }

    if(vs_points_appearance == "show_location_ids")
    {
      vs_legend <- c(vs_legend, "location ids")
      vs_cols   <- c(vs_cols,   "black")
      vs_lty    <- c(vs_lty,    NA)
      vs_pch    <- c(vs_pch,    "#")
    }

    if(vs_points_appearance == "show_n_neighbours")
    {
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
