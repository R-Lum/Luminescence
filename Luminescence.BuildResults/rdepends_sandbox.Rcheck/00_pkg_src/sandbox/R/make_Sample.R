#' @title Create a Virtual Sample.
#'
#' @description The function generates many virtual sediment grains based on the specified
#' sample geometry and depth, using the information from a rule book.
#'
#' @param book [list] object, initially produced by [get_RuleBook]
#' 
#' @param depth [numeric] scalar, depth of the sample centre (m).
#' 
#' @param geometry [character] scalar, keyword defining the geometry of
#'        the sample. One out of `"cuboid"` and `"cylinder"`,
#'        default is `"cuboid"`.
#' 
#' @param radius [numeric] scalar, radius of the cylinder (m).
#' 
#' @param height [numeric] scalar, height of the cuboid (m).
#' 
#' @param width [numeric] scalar, width of the cuboid (m).
#' 
#' @param length [numeric] scalar, length of the cuboid or cylinder (m).
#' 
#' @param slice [logical] scalar, option to sample in repeated slices of 
#' 10^6 grains until the required sample size is reached. Useful to avoid 
#' memory issues for large numbers of grains per sample volume.
#' 
#' @param force [logical] scalar, option to override the default 
#' maximum number of 10^7 grains per sample, set to avoid memory problems 
#' of the computer.
#' 
#' @param n_cores [integer] (*optional*) set the number of cores used for the parallel 
#' processing 
#' 
#' @return A [list] object.
#' 
#' @author Michael Dietze, GFZ Potsdam (Germany)
#' 
#' @examples
#'
#' set.seed(12234)
#' sample_01 <- make_Sample(
#'   book = get_RuleBook(), 
#'   depth = 1, 
#'   geometry = "cuboid",
#'   n_cores = 1,
#'   height = 0.001,
#'   width = 0.001, 
#'   length = 0.001)
#'
#' @md
#' @export make_Sample
make_Sample <- function(
  book,
  depth,
  geometry = "cuboid",
  radius,
  height,
  width,
  length,
  slice = TRUE,
  force = FALSE,
  n_cores = max(1,parallel::detectCores() - 2)
) {

# Check input -------------------------------------------------------------
if (!all(c("sandbox", "book") %in% attributes(book)[c("package", "medium")]))
  stop("[make_Sample()] 'book' is not an object created by sandbox!", call. = FALSE)
  
## calculations -------------------------------------------------------------
  ## determine depth range and sample volume based on sample geometry
  if (geometry == "cuboid") {
    ## calculate depth interval (m)
    depth_range <- c(
      depth[1] - height[1] / 2,
      depth[1] + height[1] / 2)
    
    ## calculate sample volume (cubic m)
    V_sample <- height[1] * width[1] * length[1]
    
  } else if (geometry == "cylinder") {
    ## calculate depth interval (m)
    depth_range <- c(
      depth[1] - radius[1],
      depth[1] + radius[1])
    
    ## calculate sample volume (cubic m)
    V_sample <- pi * radius[1]^2 * length[1]
    
 }
  
  ## define number of grains for average estimate
  n_estimate <- 1000
  
  ## draw random depths for sample size
  z_estimate <- runif(
    n = n_estimate,
    min = depth_range[1],
    max = depth_range[2])
  
  ## get test sample descriptions
  v_estimate <- lapply(
    X = z_estimate, 
    FUN = function(z, book) {
      ## get population probabilities
      p_z <- vapply(
        X = book$population[-1],
        FUN = function(x, z) {
            x$value(x = z)}, 
        FUN.VALUE = numeric(1), z = z)

      ## account for negative values
      p_z[p_z < 0] <- 0
      
      ## normalise probabilities
      p_z <- p_z / sum(p_z)
      
      ## generate population ID                         
      p_z <- base::sample(
        x = 1:length(p_z),
        size = 1,
        prob = p_z)
      
      ## get grain diameter
      if (book$grainsize[[p_z + 1]]$type == "exact") {
        d_z <- book$grainsize[[p_z + 1]][[2]](z)
        
      } else if (book$grainsize[[p_z + 1]]$type == "normal") {
        d_z <- stats::rnorm(
          n = 1, 
          mean = book$grainsize[[p_z + 1]]$mean(z), 
          sd = book$grainsize[[p_z + 1]]$sd(z))
        
      } else if (book$grainsize[[p_z + 1]]$type == "uniform") {
        d_z <- stats::runif(
          n = 1, 
          min = book$grainsize[[p_z + 1]]$min(z), 
          max = book$grainsize[[p_z + 1]]$max(z))
        
      } else if (book$grainsize[[p_z + 1]]$type == "gamma") {
         d_z <- stats::rgamma(
           n = 1, 
           shape = book$grainsize[[p_z + 1]]$shape(z), 
           scale = book$grainsize[[p_z + 1]]$scale(z)) + 
          book$grainsize[[p_z + 1]]$offset(z)
      }
      
      ## get packing density
      if (book$packing[[p_z + 1]]$type == "exact") {
        w_z <- book$packing[[p_z + 1]][[2]](z)
        
      } else if (book$packing[[p_z + 1]]$type == "normal") {
        w_z <- stats::rnorm(
          n = 1, 
          mean = book$packing[[p_z + 1]]$mean(z), 
          sd = book$packing[[p_z + 1]]$sd(z))
        
      } else if (book$packing[[p_z + 1]]$type == "uniform") {
        w_z <- stats::runif(
          n = 1, 
          min = book$packing[[p_z + 1]]$min(z),
          max = book$packing[[p_z + 1]]$max(z))
        
      }  else if (book$packing[[p_z + 1]]$type == "gamma") {
          w_z <- stats::rgamma(
            n = 1, 
            shape = book$packing[[p_z + 1]]$shape(z), 
            scale = book$packing[[p_z + 1]]$scale(z)) + 
          book$packing[[p_z + 1]]$offset(z)
      } 
      
      ## calculate metric grain radius
      r_z <- convert_units(phi = d_z) / 2000000
      
      ## calculate packing density-corrected grain volume
      v_z <- 4 / 3 * pi * r_z^3 * 1/w_z
      
      ## return output if volume is positive
      if (v_z > 0) return(v_z)
      
    }, book = book)
  
  
  ## estimate 110 % of total number of grains for sample volume
  n_grains <- round(V_sample * n_estimate / sum(unlist(v_estimate)) * 1.1)

  ## stop if sample volume is too small
  if (n_grains < 1) 
    stop("[make_Sample()] Sample volume is smaller than grain diameter!", call. = FALSE)
  
  ## create grains as data frame ----------------------------------------------
  
  ## get grain depths for sample
  if (geometry == "cuboid") {
   d_sample <- runif(
     n = n_grains,
     min = depth_range[1],
     max = depth_range[2])

  } else if (geometry == "cylinder") {
   depth_i <- runif(
     n = n_grains,
     min = depth_range[1],
     max = depth_range[2]) - depth
    
    depth_circle <- sqrt(radius^2 - depth_i^2)
    depth_weight <- depth_circle/max(depth_circle)
    
    d_sample <- sample(
      x = depth_i,
      size = n_grains,
      replace = TRUE,
      prob = depth_weight) + depth
  }
  
  ## flag warning for high number of grains
  if (n_grains > 10 ^ 7 & force == FALSE) {
    stop(paste0(
      "More than 10^7 grains (",
      n_grains,
      ") to model. Enable with force = TRUE."
    ), call. = FALSE)
  }
  
# Setup parallel computation ----------------------------------------------
  ## initiate cluster
  cl <- parallel::makeCluster(getOption("mc.cores", n_cores[1]))
  on.exit(parallel::stopCluster(cl))
  
  ## check for slice option
  if (slice[1]) {
    ## get slice limits in terms of grain size numbers
    i_slice <- c(
      seq(from = 1, 
       to = length(d_sample), 
       by = 1e+06), 
      length(d_sample) + 1)
    
    ## create and fill slice object with grain depths  
    d_sample_slice <- vector(
      mode = "list", length = length(i_slice) - 1)
    
    for (i in 1:length(d_sample_slice))
      d_sample_slice[[i]] <- d_sample[(i_slice[i]):(i_slice[i + 1] - 1)]
    
  } else {
    ## alternatively create one big slice
    d_sample_slice <- list(d_sample)
    
  }
  
  ## get populations
  population <- vector(mode = "list", length = length(d_sample_slice))
  
  for (i in 1:length(population)) {
    population[[i]] <- parallel::parLapply(
      cl = cl, 
      X = d_sample_slice[[i]], 
      fun = function(d_sample, book) {
        ## get population probability
        p_z <- lapply(
          X = book$population[-1], 
          FUN = function(x, d_sample) {x$value(x = d_sample)}, d_sample)
        
        ## convert list to vector
        p_z <- do.call(c, p_z)
        
        ## account for negative values        
        p_z[p_z < 0] <- 0
        
        ## normalise probabilities
        p_z <- p_z / sum(p_z)
        
        ## generate population ID 
        base::sample(
          x = 1:length(p_z),
          size = 1,
          prob = p_z)
        
      }, book = book)
    
    ## convert list to vector
    population[[i]] <- do.call(c, population[[i]])
  }
  
  ## merge populations
  population <- do.call(c, population)
  
  ## create matrix for other parameters
  parameters <- matrix(nrow = n_grains, ncol = length(book) - 1)
  
  colnames(parameters) <- names(book)[-1]
  
  ## append population and empty columns to grains data frame
  grains <- 1:n_grains
  grains <- as.data.frame(
    cbind(grains,d_sample, population, parameters))
  
  ## calculate all further parameters
  for (i in 2:length(book)) {
    ## calculate general parameters
    if (book[[i]]$group == "general") {
      
      if (book[[i]][[2]]$type == "exact") {
        x_out <- try({ 
          book[[i]][[2]][[2]](x = grains$d_sample)
          }, silent = TRUE)
        
        if (class(x_out[1])[1] == "try-error") 
          x_out <- rep(NA, nrow(grains))
        
        grains[,i + 2] <- x_out
        
      } else if (book[[i]][[2]]$type == "normal") {
        sd_pre <- try({
          book[[i]][[2]]$sd(x = grains$d_sample)
         }, silent = TRUE)
        
        try(sd_pre[sd_pre < 0] <- 0, silent = TRUE)
        
        x_out <- try(
          stats::rnorm(n = nrow(grains),
            mean = book[[i]][[2]]$mean(
            x = grains$d_sample),
            sd = sd_pre), 
          silent = TRUE)
        
        if (class(x_out[1])[1] == "try-error") 
          x_out <- rep(NA, nrow(grains))
        
        grains[,i + 2] <- x_out
        
      }  else if (book[[i]][[2]]$type == "uniform") {
        min_pre <- try({
          book[[i]][[2]]$min(x = grains$d_sample)
          }, silent = TRUE)
        
        max_pre <- try({
          book[[i]][[2]]$max(x = grains$d_sample)
          }, silent = TRUE)
        
        x_out <- try({
          stats::runif(
            n = nrow(grains),
            min = min_pre,
            max = max_pre)},
          silent = TRUE)
        
        if (class(x_out[1])[1] == "try-error") 
          x_out <- rep(NA, nrow(grains))
    
        grains[,i + 2] <- x_out
        
      } else if (book[[i]][[2]]$type == "gamma") {
        shape_pre <- try(book[[i]][[2]]$shape(x = grains$d_sample),
                         silent = TRUE)
        
        scale_pre <- try(book[[i]][[2]]$scale(x = grains$d_sample),
                         silent = TRUE)
        
        offset_pre <- try(book[[i]][[2]]$offset(x = grains$d_sample),
                          silent = TRUE)
        
        x_out <- try({
          stats::rgamma(
            n = nrow(grains),
            shape = shape_pre,
            scale = scale_pre) + offset_pre
          }, silent = TRUE)
        
        if (class(x_out[1])[1] == "try-error") 
          x_out <- rep(NA, nrow(grains))
        
        grains[,i + 2] <- x_out
      }
      
    } else {
      
      ## calculate specific parameters
      for (j in 1:(length(book$population) - 1)) {
        ## extract ID of grains of appropriate population
        ID_population_j <- (1:nrow(grains))[grains$population == j]
        
        if (book[[i]][[2]]$type == "exact") {
          x_out <- try(book[[i]][[j + 1]]$value(
            x = grains$d_sample[ID_population_j]),
            silent = TRUE)
          
          if (class(x_out[1])[1] == "try-error")
            x_out <- rep(NA, nrow(grains))

          grains[ID_population_j,i + 2] <- x_out
          
        } else if (book[[i]][[2]]$type == "normal") {
          x_out <- try(
            stats::rnorm(
              n = length(ID_population_j),
              mean = book[[i]][[j + 1]]$mean(x = grains$d_sample),
              sd = abs(book[[i]][[j + 1]]$sd(x = grains$d_sample))),
            silent = TRUE)
          
          if (class(x_out[1])[1] == "try-error") 
            x_out <- rep(NA, nrow(grains))
          
          grains[ID_population_j,i + 2] <- x_out
          
        } else if (book[[i]][[2]]$type == "uniform") {
          x_out <- try(
            stats::runif(
              n = length(ID_population_j),
              min = book[[i]][[j + 1]]$min(x = grains$d_sample),
              max = book[[i]][[j + 1]]$max(x = grains$d_sample)),
            silent = TRUE)
          
          if (class(x_out[1])[1] == "try-error")
            x_out <- rep(NA, nrow(grains))
          
          grains[ID_population_j,i + 2] <- x_out
          
        } else if (book[[i]][[2]]$type == "gamma") {
          x_out <- try(
            stats::rgamma(
              n = length(ID_population_j),
              shape = book[[i]][[j + 1]]$shape(x = grains$d_sample),
              scale = book[[i]][[j + 1]]$scale(x = grains$d_sample)) +
              book[[i]][[j + 1]]$offset(x = grains$d_sample),
            silent = TRUE)
          
          if (class(x_out[1])[1] == "try-error")
            x_out <- rep(NA, nrow(grains))
        
          grains[ID_population_j,i + 2] <- x_out
        }
      }
    }
  }
  
  ## calculate cumulative sample volume
  r_grains <- (convert_units(phi = grains$grainsize) / (2 * 1e+06))
  V_grains <- cumsum(4 / 3 * pi * r_grains^3 * 1/grains$packing)
  
  ## remove grains that over the top of sample volume
  grains <- grains[V_sample >= V_grains,]
  
  ## return output ------------------------------------------------------------
  ## set attributes
  attributes(grains) <- c(
    attributes(grains),
    list(package = "sandbox"))
  
  return(grains)
}


