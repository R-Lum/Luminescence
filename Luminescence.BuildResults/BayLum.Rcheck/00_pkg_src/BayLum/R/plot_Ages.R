#' @title Create Age Plot
#'
#' @details This function creates an age plot showing the mean ages along with the credible intervals. The function
#' provides various arguments to modify the plot output, however, for an ultimate control the function returns
#' the [data.frame] extracted from the input object for own plots.
#'
#' @param object [list]  or [data.frame] (**required**): Output as created by functions like [AgeC14_Computation], which
#' is a list of class `BayLum.list`. Alternatively the function supports a [data.frame] as input, however,
#' in such a case the [data.frame] must resemble the ages [data.frame] created by the computation functions
#' otherwise the input will be silently ignored.
#'
#' @param sample_names [character] (optional): alternative sample names used for the plotting.
#'  If the length of the provided [character] vector is shorter than the real number of samples, the names are recycled.
#'
#' @param sample_order [numeric] (optional): argument to rearrange the sample order, e.g., `sample_order = c(4:1)` plots
#' the last sample first.
#'
#' @param plot_mode [character] (*with default*): allows to switch from displaying ages as points with lines (`"ages"`)
#' for the credible intervals to densities (`"density"`)
#'
#' @param ... further arguments to control the plot output,
#' standard arguments are: `cex`, `xlim`, `main`, `xlab`, `col` further (non-standard) arguments
#' are: `grid` (`TRUE`/`FALSE`), `legend` (`TRUE`/`FALSE`), `legend.text` ([character] input needed), `legend.pos` [graphics::legend], `legend.cex`. Additional arguments: `d_scale` (scales density plots), `show_ages` (add ages to density  plots)
#'
#' @return
#' The function returns a plot and the [data.frame] used to display the data
#'
#' @section Function version: 0.1.5
#'
#' @author Sebastian Kreutzer, Institute of Geography, Ruprecht-Karl-University of Heidelberg (Germany), based on code
#' written by Claire Christophe
#'
#' @seealso [AgeC14_Computation], [AgeS_Computation]
#'
#' @examples
#' ## load data
#' data(DATA_C14,envir = environment())
#' C14Cal <- DATA_C14$C14[,1]
#' SigmaC14Cal <- DATA_C14$C14[,2]
#' Names <- DATA_C14$Names
#' nb_sample <- length(Names)
#'
#'## Age computation
#' Age <- AgeC14_Computation(
#'    Data_C14Cal = C14Cal,
#'    Data_SigmaC14Cal = SigmaC14Cal,
#'    SampleNames = Names,
#'    Nb_sample = nb_sample,
#'    PriorAge = rep(c(20,60),nb_sample),
#'    Iter = 500,
#'    quiet = TRUE)
#'
#' ## plot output
#' plot_Ages(Age)
#'
#' ## plot output
#' plot_Ages(Age, plot_mode = "density")
#'
#' @md
#' @export
plot_Ages <- function(
  object,
  sample_names = NULL,
  sample_order = NULL,
  plot_mode = "ages",
  ...
){

  # Verify input --------------------------------------------------------------------------------
  # ## if the input is of type data.frame, we try to sanitize this object
  # ## however, we are very picky, otherwise we break the code below
  if(is(object, "data.frame") && ncol(object) == 6 && nrow(object) >= 1 && !any(is.na(object))){
    colnames(object) <- c("SAMPLE", "AGE", "HPD68.MIN", "HPD68.MAX", "HPD95.MIN", "HPD95.MAX")
    object[[1]] <- as.character(object[[1]])
    object <- .list_BayLum(Ages = object)

  }

  ## From here on everything must be a BayLum.list
  if (is.null(attributes(object)$class) || attributes(object)$class != "BayLum.list")
    stop("[plot_Ages()] Wrong input, only objects of type 'BayLum.list' are allowed. Please check the manual!",
      call. = FALSE
    )

  # Extract data --------------------------------------------------------------------------------
  df <- object[["Ages"]]

  ##set alternative sample names
  if(!is.null(sample_names)){
    df <- cbind(df, ALT_SAMPLE_NAME = rep_len(sample_names, length.out = nrow(df)))
    df <- cbind(df, AT = as.numeric(as.factor(df[["ALT_SAMPLE_NAME"]])))

  }else{
    df <- cbind(df, ALT_SAMPLE_NAME = NA)
    df <- cbind(df, AT = as.numeric(as.factor(df[["SAMPLE"]])))

  }

  ##set sample order
  if(!is.null(sample_order)){
    df[["AT"]] <- sample_order

  }else{
    ##this makes sure what we have the stratigraphic constraints
    df[["AT"]] <- rev(sort(df[["AT"]]))

  }

  # Plotting -----------------------------------------------------------------------------------
  ## extract chain values for density option (we need the values later)
  if(plot_mode == "density") {
    d <- lapply(df[["AT"]], function(x) {
      density(unlist(lapply(object$Sampling, function (y) y[,x])))

    })
  }

  ##PREPARATION
    ### plot settings ##########################################################
    ##define plot settings
    plot_settings <- list(
      pch = 21,
      cex = 1,
      xlim = if(plot_mode == "density") {
          range(vapply(d, function(x) range(x$x), numeric(2)))
         } else {
         c(min(df[["HPD95.MIN"]]),max(df[["HPD95.MAX"]]))
         },
      main = "Age Results",
      xlab = "Age (ka)",
      grid = TRUE,
      col = if(plot_mode == "density") {
          c(rgb(0,0,0,0.1), rgb(1,0.3,0,.3), rgb(1,.6,0,.3))
        } else {
          c("firebrick", "darkslategray3", "midnightblue")
        },
      legend = TRUE,
      legend.text = c("Bayes estimator", "68% credible interval", "95% credible interval"),
      legend.pos = c("topright"),
      legend.cex = 0.9,
      d_scale = 0.6,
      show_ages = FALSE
    )

      ##overwrite settings on demand
      plot_settings <- modifyList(x = plot_settings, val = list(...))

  ##PLOTTING
  ##adjust par and make sure that it resets
  old_par <- par(
    mfrow=c(1,1),
    las = 1,
    oma = c(2,5,0.5,0.5),
    cex = plot_settings$cex)
  on.exit(par(old_par))

  ##open plot area
  plot(
    x = NA,
    y = NA,
    xlim = plot_settings$xlim,
    ylim = if(plot_mode == "density") c(0.5, max(df[["AT"]]) + 1) else c(0.5, max(df[["AT"]])),
    main = plot_settings$main,
    xlab = plot_settings$xlab,
    ylab = "",
    yaxt = "n")

  ##add y-axis
  axis(
    side = 2,
    at = df[["AT"]],
    labels = if(!is.null(sample_names)){
     df[["ALT_SAMPLE_NAME"]]

    }else{
     df[["SAMPLE"]][df[["AT"]] == df[["AT"]]]
    }
  )

  ##add grid
  if(plot_settings$grid)
    grid(ny = NA, lwd = plot_settings$cex * 1.5)

  ## =========== DENSITY PLOTS ============
  ## plot densities vs ages
  if (plot_mode[1] == "density") {
    ## base polygon
    for(i in df[["AT"]]){
       polygon(
         x = c(d[[i]]$x, rev(d[[i]]$x)),
         y = c(d[[i]]$y / max(d[[i]]$y) * plot_settings$d_scale + i, rep(i, length(d[[i]]$x))),
         col = plot_settings$col[1],
         border = TRUE,
         lwd = 0.2)
    }

    ## HPD95
    for(i in df[["AT"]]){
      ind <- which(
        d[[i]]$x >= df[df[["AT"]] == i, "HPD95.MIN"] &
        d[[i]]$x <= df[df[["AT"]] == i, "HPD95.MAX"])

      polygon(
        x = c(d[[i]]$x[ind], rev(d[[i]]$x[ind])),
        y = c(d[[i]]$y[ind] / max(d[[i]]$y[ind]) * plot_settings$d_scale + i, rep(i, length(ind))),
        col = plot_settings$col[3],
        border = FALSE,
        lwd = 0.4)
    }

    ## HPD68
    for(i in df[["AT"]]){
      ind <- which(
        d[[i]]$x >= df[df[["AT"]] == i, "HPD68.MIN"] &
          d[[i]]$x <= df[df[["AT"]] == i, "HPD68.MAX"])

      polygon(
        x = c(d[[i]]$x[ind], rev(d[[i]]$x[ind])),
        y = c(d[[i]]$y[ind] / max(d[[i]]$y[ind]) * plot_settings$d_scale + i, rep(i, length(ind))),
        col = plot_settings$col[2],
        border = FALSE,
        lwd = 0.4)
    }

    ## add ages
    if(plot_settings$show_ages) {
      ## connection lines
      lines(
        x = df[["AGE"]],
        y = df[["AT"]],
        lty = 2,
        col = rgb(0,0,0,0.5)
      )

      ## add Bayes estimator
      points(
        x = df[["AGE"]],
        y = df[["AT"]],
        col = rgb(0, 0, 0, 0.5))

    }

    ##add legend
    if(plot_settings$legend){
      legend(
        plot_settings$legend.pos,
        legend = if(plot_settings$show_ages[1]) plot_settings$legend.text[1:3] else plot_settings$legend.text[2:3],
        pch = c(if(plot_settings$show_ages) plot_settings$pch else NA_integer_, NA_integer_, NA_integer_),
        bty = "n",
        lty = if(plot_settings$show_ages[1]) c(0, 1, 1) else c(1,1),
        lwd = 2,
        cex = plot_settings$legend.cex,
        col = if(plot_settings$show_ages[1]) plot_settings$col[1:3] else plot_settings$col[2:3],
        horiz = if(plot_settings$legend.pos == "top" || plot_settings$legend.pos == "bottom")
          TRUE else FALSE)
    }

  ## ======== STANDARD PLOT ============
  } else {
    ##ADD HDP95 and HDP58
    for(i in 1:nrow(df)){
      ##HDP95
      lines(
        x = c(df[["HPD95.MIN"]][[i]], df[["HPD95.MAX"]][[i]]),
        y = rep(df[["AT"]][[i]], each = 2),
        lwd = 5,
        col = plot_settings$col[3])

      ##HDP68
      lines(
        x = c(df[["HPD68.MIN"]][[i]], df[["HPD68.MAX"]][[i]]),
        y = rep(df[["AT"]][[i]], each = 2),
        lwd = 5,
        col = plot_settings$col[2])

    }

    ##ADD AGES
    points(
      x = df[["AGE"]],
      y = df[["AT"]],
      col = plot_settings$col[1],
      pch = plot_settings$pch,
      cex = plot_settings$cex * 1.8,
      bg = rgb(
        col2rgb(plot_settings$col)[1,1],
        col2rgb(plot_settings$col)[2,1],
        col2rgb(plot_settings$col)[3,1],
        alpha = 100, maxColorValue = 255),
      lwd = plot_settings$cex * 2
    )

    ##add legend
    if(plot_settings$legend){
      legend(
        plot_settings$legend.pos,
        legend = plot_settings$legend.text,
        pch = c(plot_settings$pch, NA_integer_, NA_integer_),
        bty = "n",
        lty = c(0, 1, 1),
        lwd = 2,
        cex = plot_settings$legend.cex,
        col = plot_settings$col,
        horiz = if(plot_settings$legend.pos == "top" || plot_settings$legend.pos == "bottom")
          TRUE else FALSE)
    }
  }

  # Return --------------------------------------------------------------------------------------
  return(df)

}
