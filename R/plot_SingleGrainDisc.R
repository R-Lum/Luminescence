#' @title Plot a disc with its values
#'
#' @description Shows a schematic representation of the physical appearance
#' of one disc (one position in the reader)
#' and illustrates the measured or calculated values per grain location.
#'
#' @details Depending of the available plotting space, some optional elements might have not enough room
#' to be displayed. As this function is wrapped around the base plot function, one can also choose to add elements
#' manually.
#'
#' @param object [RLum.Results-class] or [numeric] (**required**): the values
#' to show, should have length 100.
#'
#' @param show_coordinates [logical] (*with default*): Show coordinates (1..10)
#' in x and in y direction. Defaults to `FALSE`.
#'
#' @param show_location_ids [logical] (*with default*): Show id with every
#' grain location (1..100). Defaults to `FALSE`.
#'
#' @param show_neighbours [logical]  (*with default*): Show which
#' neighbour connections are taken into account if calculating Moran's I.
#' This makes sense when there are `NA` observations, or when a non-standard
#' neighbour setting is defined.
#'
#' @param show_positioning_holes [logical] (*with default*): Show the 3
#' positioning holes for orientation. Defaults to `TRUE`.
#'
#' @param df_neighbours [data.frame] (*with default*): only relevant if
#' `show_neighbours` is `TRUE`. Data frame indicating which borders to
#' consider, and their respective weights (see the description provided for
#' [calc_MoransI]). If `NULL` (default), this is constructed automatically by
#' the internal function `.get_Neighbours`.
#'
#' @param ignore_borders [logical] (*with default*): whether only grain
#' locations that do not lie on the border of the disc should be considered
#' (`FALSE` by default). Thus if `TRUE`, only the inner 8x8 grain locations
#' rather than the full 10x10 are considered. Ignored if `df_neighbours` is
#' not `NULL` or if `show_neighbours = FALSE`.
#'
#' @param str_transform [character] (*with default*): The observed value of each individual grain is
#' reflected in the size of a triangle (or other dot-like element). To account for large value differences,
#' the transformation from value to triangle size can be `"lin"` (linear), `"log"` (logarithmic) and `"sqrt"`
#' (square root). Defaults to `"sqrt"`, so that the surface is linear to the value. Note that
#' the log and sqrt transformations can come with an addition to avoid negative values. When the legend
#' is shown, the actual lower, middle and upper values are printed.
#'
#' @param ... other arguments to be given to the base R plot function, such
#' as `main`, `col` and `pch`. `legend` can be used to enable/disable the
#' legend (`FALSE` by default).
#'
#' @author Anna-Maartje de Boer, Luc Steinbuch, Wageningen University & Research, 2025
#'
#' @references
#' de Boer, A-M., Steinbuch, L., Heuvelink, G.B.M., Wallinga, J., 2025.
#' A novel tool to assess crosstalk in single-grain luminescence detection.
#' Submitted.
#'
#' @examples
#'
#' plot_SingleGrainDisc(1:100)
#'
#' @md
#' @export
plot_SingleGrainDisc <- function(object,
                                 show_coordinates = FALSE,
                                 show_location_ids = FALSE,
                                 show_neighbours = FALSE,
                                 show_positioning_holes = TRUE,
                                 df_neighbours = NULL,
                                 ignore_borders = FALSE,
                                 str_transform = "sqrt", # Options: "lin", "log" and "sqrt"
                                 ...
) {
  .set_function_name("plot_SingleGrainDisc")
  on.exit(.unset_function_name(), add = TRUE)

  ## Validate input arguments -----------------------

  .validate_class(object, c("RLum.Results", "numeric", "integer"))
  if (is.numeric(object)) {
    vn_values_to_show <- object
  } else {
    vn_values_to_show <- get_RLum(object)
  }
  .validate_length(vn_values_to_show, 100, name = "'object'")
  if (all(is.na(vn_values_to_show))) {
    .throw_error("'object' should contain at least one non-NA value")
  }

  .validate_logical_scalar(show_coordinates)
  .validate_logical_scalar(show_location_ids)
  .validate_logical_scalar(show_neighbours)
  .validate_logical_scalar(show_positioning_holes)

  if (is.null(df_neighbours)) {
    .validate_logical_scalar(ignore_borders)
    df_neighbours <- .get_Neighbours(object, ignore_borders)
  } else {
    .validate_class(df_neighbours, "data.frame")
    if (ncol(df_neighbours) != 3)
      .throw_error("'df_neighbours' should be a data frame with 3 columns")
  }

  .validate_args(str_transform, c("sqrt", "lin", "log"))

  ## get ... arguments
  plot_settings <- modifyList(
      x = list(
          main = "",
          legend = FALSE,
          col =  "darkolivegreen",
          pch = 17),
      val = list(...))

  ## Settings  -----------------------

  n_lb_cex <- 0.8  # lower bound of plotted grain value size
  n_ub_cex <- 3.2  # upper bound of plotted grain value size

  str_title <- plot_settings$main

  ## Scaling from value to point size -----------------------

  if(sum(!is.na(vn_values_to_show)) == 1)
  {
    ## Scale if there is one observation
    vn_range_values <- range(c(vn_values_to_show, 0), na.rm = TRUE)
    n_mean_value <- mean(vn_range_values)
    vn_values_to_show[101] <- n_mean_value  ## Temporarily add mean value to vector for scaling
    vn_values_to_show <- (vn_values_to_show-vn_range_values[1]) / diff(vn_range_values)

  } else
  {
    vn_range_values <- range(vn_values_to_show, na.rm = TRUE)
    n_mean_value <- mean(vn_range_values)
    vn_values_to_show[101] <- n_mean_value ## Temporarily add mean value to vector for scaling
    if(str_transform == "lin") # No transformation, just rescaling
    {
      ## Scale to from 0 to inf
      vn_values_to_show <- vn_values_to_show - min(vn_values_to_show , na.rm = TRUE)

      ## Scale to from n_ub_cex to n_lb_cex)
      vn_values_to_show <- (vn_values_to_show)/
        (max(vn_values_to_show , na.rm = TRUE) / (n_ub_cex - n_lb_cex) ) +
        n_lb_cex
    }
    else if (str_transform == "log") {
      ## Scale to from 1 to inf, also to avoid problems with non-positive values
      vn_values_to_show <- vn_values_to_show - min(vn_values_to_show , na.rm = TRUE) + 1

      ## Transform
      vn_values_to_show <- log(vn_values_to_show)

      ## Scale to from n_ub_cex to n_lb_cex
      vn_values_to_show <- (vn_values_to_show)/
        (max(vn_values_to_show , na.rm = TRUE) / (n_ub_cex - n_lb_cex) ) +
        n_lb_cex
    }
    else if (str_transform == "sqrt") {
      ## Scale to from 0 to inf
      vn_values_to_show <- vn_values_to_show - min(vn_values_to_show , na.rm = TRUE)

      ## Transform
      vn_values_to_show <- sqrt(vn_values_to_show)

      ## Scale to from n_ub_cex to n_lb_cex)
      vn_values_to_show <- (vn_values_to_show)/
        (max(vn_values_to_show , na.rm = TRUE) / (n_ub_cex - n_lb_cex) ) +
        n_lb_cex
    }
  }


  n_mv_cex <- vn_values_to_show[101] ## Extract mean value point size
  vn_values_to_show <- vn_values_to_show[-101]

  ## Plotting  -----------------------
  df_disc <- data.frame(x = rep(1:10, length.out=100),
                        y = rep(1:10, each=10)
  )

  ## remove used arguments to avoid collisions
  args <- list(...)
  args <- args[!...names() %in% c("main", "legend", "col", "pch")]

  do.call(
      what = plot,
      args = c(list(
          x = df_disc$x,
          y = df_disc$y,
          cex = vn_values_to_show,
          asp = 1,
          col = plot_settings$col,
          pch = plot_settings$pch,
          xlab = "",
          ylab = "",
          xlim = c(1,10),
          ylim = c(10,1),
          axes = FALSE,
          mar = c(4, 4, 6, 0.5)),
          args))

  ## Show title (if any)
  if (show_positioning_holes || show_coordinates) {
    ## move title up
    mtext(str_title, side = 3, line = 2.5, adj = 0.5, cex=1.3)
  } else
  {
    mtext(str_title, side = 3, line = 1, adj = 0.5, cex=1.5)
  }

  ## Show NA values
  if(any(is.na(vn_values_to_show)))
  {
    points(x = df_disc$x,
           y = df_disc$y,
           cex = 0.5,
           pch = ifelse(is.na(vn_values_to_show), 4, NA),
           col = "orange"
    )
  }

  ## Show coordinates along axes
  if (show_coordinates) {
    axis(1, pos=-0.4, at=1:10, lwd=1, lwd.ticks=1)
    axis(4, pos=-0.3, at=1:10, las=2, lwd=1, lwd.ticks=1)
  }

  ## Show all grain location id's
  if (show_location_ids) {
    text(x = df_disc$x-0.2,
         y = df_disc$y-0.2,
         labels = 1:100,
         cex = 0.5,
         col = "black"
    )
  }

  ## Show the 3 big positioning holes
  if (show_positioning_holes) {
    ##            left middle  right
    points(x = c(-0.25,  5.5,   11.2),
           y = c(5.5,     0,    5.5),
           pch = 1,
           cex = 5,
           xpd=NA
    )
  }

  ## Indicate neighbouring positions used for Moran's I calculations
  if (show_neighbours) {
    n_lines <- nrow(df_neighbours)
    if (n_lines == 0)
      .throw_error("'show_neighbours' is TRUE but 'df_neighbours' is empty")


    ## From location ID to x y coordinates; put into lists for use with mapply
    list_x <- split((df_neighbours[,1:2] - 1) %% 10 + 1,
                    1:n_lines )
    list_y <- split((df_neighbours[,1:2] - 1) %/% 10 + 1,
                    1:n_lines )

    ## Scale weights for nice plotting
    li_weight <- split((2 * df_neighbours$weight / max(df_neighbours$weight)),
                       1:n_lines)

    mapply(lines, x = list_x, y = list_y, col = c("purple"), lwd = li_weight, lty = 'dotted')
  }

  if (plot_settings$legend) {
    pch <- plot_settings$pch
    col <- plot_settings$col
    n_low <-  prettyNum(x = vn_range_values[1], digits = 2, format = "fg")
    n_mean <- prettyNum(x = n_mean_value, digits = 2, format = "fg")
    n_high <- prettyNum(x = vn_range_values[2], digits = 2, format = "fg")

    vs_legend <- c( n_low,    n_mean,    n_high )
    vn_pch    <- c( pch,      pch,       pch)
    vs_lty    <- c( NA,       NA,        NA)
    vs_cols   <- c( col,      col,       col)
    vn_pt_cex <- c( n_lb_cex, n_mv_cex,  n_ub_cex)

    ## if also NA values
    if(any(is.na(vn_values_to_show)))
    {
      vs_legend <- c(vs_legend, "No obs.")
      vn_pch    <- c(vn_pch,     4 )
      vs_lty    <- c(vs_lty,    NA)
      vs_cols   <- c(vs_cols,   "orange")
      vn_pt_cex <- c(vn_pt_cex, 0.5)
    }

    if (show_neighbours) {
      vs_legend <- c(vs_legend, "Neighb.")
      vn_pch    <- c(vn_pch,     NA )
      vs_lty    <- c(vs_lty,    'dotted')
      vs_cols   <- c(vs_cols,   "purple")
      vn_pt_cex <- c(vn_pt_cex, NA)
    }

    legend(x = -0.0,
           y = 10.5,
           legend = vs_legend,
           pch = vn_pch,
           lty= vs_lty,
           lwd = 1,
           col = vs_cols,
           pt.cex = vn_pt_cex,
           cex = 0.8,
           bg = "gray98",
           horiz = TRUE,
           adj = c(0, 0.75),
           y.intersp=1.4,
           xpd=TRUE
    )
  }
}
