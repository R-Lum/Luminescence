#' @title Plot a disc with its values
#'
#' @description Shows a schematic representation of the physical appearance of one disc (on position in the reader) 
#' and illustrates the measured or calculated values per grain location.
#' 
#' @details Depending of the available plotting space, some optional elements might have not enough room 
#' to be displayed. As this function is wrapped around the base plot function, one can also choose to add elements
#' manually.
#'
#' @param object The values to show, a numerical vector of size 100. ## NOG AANPASSEN
#'
#' @param bo_show_coordinates [logical] (*with default*): Show coordinates (1..10)
#' in x and in y direction. Defaults to `FALSE`.
#'
#' @param bo_show_location_ids [logical] (*with default*): Show id with every
#' grain location (1..100). Defaults to `FALSE`.
#'
#' @param bo_show_legend [logical] (*with default*): Defaults to `FALSE`.
#' Show a rudimentary legend.
#' 
#' @param bo_show_neighbours [logical]  (*with default*): Show which
#' neighbour connections are taken into account if calculating Moran's I.
#' This makes sense when there are `NA` observations, or when a non-standard
#' neighbour setting is defined.
#'
#' @param df_neighbour [data.frame] (*with default*): Dataframe as returned
#' by `get_Neighbour()`. Is only relevant if `bo_show_neighbours` is `TRUE`.
#' Defaults to `get_Neighbour(object)`.
#'
#' @param bo_show_positioning_holes [logical] (*with default*): Show the 3
#' positioning holes for orientation. Defaults to `TRUE`.
#' 
#' @param str_transform [character] (*with default*): The observed value of each individual grain is
#' reflected in the size of a triangle (or other dot-like element). To account for large value differences,
#' the transformation from value to triangle size can be `"lin"` (linear), `"log"` (logarithmic) and `"sqrt"`
#' (square root). Defaults to `"sqrt"`, so that the surface is linear to the value. Note that
#' the log and sqrt transformations can come with an addition to avoid negative values. When the legend
#' is shown, the actual lower, middle and upper values are printed.
#' 
#' @param main FIXME(mcol)
#' @param col FIXME(mcol)
#' @param pch FIXME(mcol)
#'
#' @param ... other arguments to be given to the base R plot function, such as "main", "col" and "pch" (with
#' default values "", "darkolivegreen" and 17, respectively). Many other base plot arguments are ignored.
#'
#' @return none
#'
#' @author Anna-Maartje de Boer, Luc Steinbuch, Wageningen University & Research, 2025
#'
#' @examples
#' 
#'  plot_Disc(1:100)
#' 
#' @md
#' @export
plot_Disc <- function(object, 
                      bo_show_coordinates = FALSE,
                      bo_show_location_ids = FALSE,
                      bo_show_legend = FALSE,
                      bo_show_neighbours = FALSE,                      
                      df_neighbour = get_Neighbour(object = object), 
                      bo_show_positioning_holes = TRUE,
                      str_transform = "sqrt", # Options: "lin", "log" and "sqrt"
                      main = "",
                      col =  "darkolivegreen",
                      pch  = 17,
                      ...
                      
                      
) 
{
  
  ## Validate input arguments -----------------------    
  
  .validate_class(object, c("RLum.Results", "numeric", "integer"))    
  #  - should contain a numerical vector of length 100
  #  - should contain at least one non-NA value
  
  if(is.numeric(object))
  {
    vn_values_to_show <- object
  } else
  {
    vn_values_to_show <- get_RLum(object)
  }
  
  .validate_class(bo_show_coordinates, "logical")    
  # - should be a single element
  
  .validate_class(bo_show_location_ids, "logical")         
  # - should be a single element
  
  .validate_class(bo_show_legend, "logical")    
  # - should be a single element
  
  .validate_class(bo_show_neighbours, "logical")      
  # - should be a single element
  
  .validate_class(df_neighbour, "data.frame")      
  # - should be valid neighbour  defining dataframe
  
  .validate_class(bo_show_positioning_holes, "logical")      
  # - should be a single element
  
  .validate_class(str_transform, "character")      
  # - should be a single element, one of "lin", "log" or "sqrt"
  
  
  
  ## Settings  -----------------------   
  
  n_lb_cex <- 0.8  # lower bound of plotted grain value size
  n_ub_cex <- 3.2  # upper bound of plotted grain value size
  
  str_title <- main
  
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
    if(str_transform == "log")
    {
      
      ## Scale to from 1 to inf, also to avoid problems with non-positive values
      vn_values_to_show <- vn_values_to_show - min(vn_values_to_show , na.rm = TRUE) + 1
      
      ## Transform
      vn_values_to_show <- log(vn_values_to_show)
      
      ## Scale to from n_ub_cex to n_lb_cex
      vn_values_to_show <- (vn_values_to_show)/
        (max(vn_values_to_show , na.rm = TRUE) / (n_ub_cex - n_lb_cex) ) +
        n_lb_cex
    }
    if(str_transform == "sqrt")
    {
      
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
  
  plot(x = df_disc$x,
       y = df_disc$y,
       cex = vn_values_to_show,
       asp = 1,
       col = col, 
       main = "",
       xlab = "", 
       ylab = "", 
       xlim = c(1,10),
       ylim = c(10,1),  
       pch = pch,
       axes=F,
       mar=c(4, 4, 6, 0.5),
       ...
  )
  
  ## Show title (if any)
  if (bo_show_positioning_holes | bo_show_coordinates)  # move title up
  {
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
  if(bo_show_coordinates)
  {
    axis(1, pos=-0.4, at=1:10, lwd=1, lwd.ticks=1)
    axis(4, pos=-0.3, at=1:10, las=2, lwd=1, lwd.ticks=1)
    
  }  
  
  ## Show all grain location id's
  if(bo_show_location_ids)
  {
    text(x = df_disc$x-0.2,
         y = df_disc$y-0.2,
         labels = 1:100,
         cex = 0.5,
         col = "black"
    )
  }  
  
  ## Show the 3 big positioning holes
  if(bo_show_positioning_holes)
  {           ##  left  middle right
    points(x = c(-0.25,  5.5,   11.2),
           y = c(5.5,     0,    5.5),
           pch = 1,
           cex = 5,
           xpd=NA
    )
  }  
  
  
  ## Indicate neighbouring postions used for Moran's I calculations
  if(bo_show_neighbours)  
  {
    
    if(is.null(df_neighbour))
      warning("In plot_disc, bo_show_neighbours = TRUE but df_neighbour is NULL")
    
    if(nrow(df_neighbour) == 0)
      warning("In plot_disc, bo_show_neighbours = TRUE but df_neighbour is empty")
    
    
    n_lines <- nrow(df_neighbour)
    
    ## From location ID to x y coordinates; put into lists for use with mapply    
    
    list_x <- split((df_neighbour[,1:2] - 1) %% 10 + 1, 
                    1:n_lines )
    list_y <- split((df_neighbour[,1:2] - 1) %/% 10 + 1, 
                    1:n_lines )
    
    ## Scale weights for nice plotting
    li_weight <- split( (2*df_neighbour$weight/max(df_neighbour$weight)), 
                        1:n_lines)
    
    mapply(lines, x = list_x, y = list_y, col = c("purple"), lwd = li_weight, lty = 'dotted')   
    
    
  }
  
  
  
  if(bo_show_legend)
  {
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
    
    if(bo_show_neighbours)
    {
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
#' @param ... Other parameters to be forwarded to the base R plot functions. Note that `xlab` (x axis label), 
#' `ylab` (y axis label) and `cex` (scaling value) are given default values. Because of sometimes large value
#' differences, `log = "x"`, `log = "y"` and `log = "xy"`are supported. In case of negative values and logarithmic 
#' plotting, values are increased so the smallest value to plot is 1. Summary elements such as means, least 
#' square line etc. will still be based on the linear case.
#' 
#' 
#' @details Note that this function plots on the y-axis the mean of the neighbours, while the function 
#' `calc_MoransI()` by default will for its global calculation weight every border the same. So, grain locations
#' with 1, 2 or 3 neighbours will appear higher on the y-axis than their influence on Moran's I justify -- apart
#' from scaling, this explains a part of the differences of Moran's scatter plots between different packages.
#' Also note that island' grain locations (=those not bordering other grains) are left out of these plots but 
#' might still influence Moran's I calculations.
#' 
#' 
#' @return Dataframe with plotting coordinates and grain location id's, for creating user-defined
#' plots. Invisible.
#'
#' @author Anna-Maartje de Boer, Luc Steinbuch, Wageningen University & Research, 2025
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
)
{
  
  ## Validate input arguments; set variables  -----------------------
  
  
  .validate_class(object, c("RLum.Results", "numeric", "integer"))    
  ## To add: 
  #  - should contain a numerical vector of length 100
  
  
  if(is.numeric(object))
  {
    vn_values <- object
  } else
  {
    vn_values <- get_RLum(object)
  }
  
  
  vs_points_appearance <- pch
  
  bo_show_location_ids <- FALSE
  
  if(pch == "show_location_ids")
  {
    bo_show_location_ids <- TRUE
  }
  
  bo_show_n_neighbours <- FALSE
  
  if(pch == "show_n_neighbours")
  {
    bo_show_n_neighbours <- TRUE
  }
  
  
  
  vs_log <- log  # because we need the function "log" later
  
  df_moran_plot <- data.frame(x = vn_values)
  df_moran_plot$y <- NA
  df_moran_plot$grain_id <- NA
  df_moran_plot$n_neighbours <- NA
  
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
    if(str_y_def == "weighted_sum")
    {
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
    warning("Grain observations removed from plot because no neighbours")
  
  df_moran_plot <- df_moran_plot[complete.cases(df_moran_plot), ]
  
  
  
  
  
  ## If we want a log plot, make sure that all relevant values are 1 or more (per axis)
  if (grepl(pattern = "x", x = vs_log, fixed = TRUE) & min(df_moran_plot$x) < 1)
  {
    
    if (min(df_moran_plot$x) < 0)
    {
      df_moran_plot$x <- df_moran_plot$x - min(df_moran_plot$x) + 1
    } else
    {
      df_moran_plot$x <- df_moran_plot$x + (1 - min(df_moran_plot$x))
    }
    warning("x axis values rescaled because of log plot (also in return df)")
  }
  
  if (grepl(pattern = "y", x = vs_log, fixed = TRUE) & min(df_moran_plot$y) < 1)
  {
    
    if (min(df_moran_plot$y) < 0)
    {
      df_moran_plot$y <- df_moran_plot$y - min(df_moran_plot$y) + 1
    } else
    {
      df_moran_plot$y <- df_moran_plot$y + (1 - min(df_moran_plot$y))
    }
    warning("y axis values rescaled because of log plot (also in return df)")
  }
  
  ## Set point appearance
  if(vs_points_appearance == "show_location_ids" | vs_points_appearance == "show_n_neighbours")
  {
    vs_pch <- NA # Numbers will be added later
  } else
  {
    vs_pch <- pch 
    
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
  #browser()
  
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
