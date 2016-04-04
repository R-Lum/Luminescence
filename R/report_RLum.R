#' Create HTML report of (RLum) objects
#'
#' This function creates a HTML report for a given object, listing its complete
#' structure and content. The object itself is saved as a serialised .Rds file.
#'
#' Provide some details here.
#'
#' @param object an object
#' @param file filename
#' @param timestamp prefix for the filename
#' @param clean remove intermediate files
#' @param ... currently not used
#' 
#' @section Function version: 0.1.0
#' 
#' @author 
#' Christoph Burow, University of Cologne (Germany) \cr
#' 
#' 
#' @seealso \code{\link[rmarkdown]{render}}
#' 
#' @return
#' Writes a HTML and .Rds file.
#' 
#' @export
#'
#' @examples
#' # coming soon
report_RLum <- function(object, 
                        file, 
                        timestamp = TRUE, 
                        clean = TRUE, 
                        ...) {
  
  ## INPUT VALIDATION ----
  
  # check if required namespace(s) are available
  if (!requireNamespace("rmarkdown", quietly = TRUE))
    stop("Creating object reports requires the 'rmarkdown' package.",
         " To install this package run 'install.packages('rmarkdown')' in your R console.", 
         call. = FALSE)
  if (!requireNamespace("pander", quietly = TRUE))
    stop("Creating object reports requires the 'pander' package.",
         " To install this package run 'install.packages('pander')' in your R console.", 
         call. = FALSE)
  
  # has a filename been provided?
  if (missing(file))
    stop("Please provide a directory/filename.", call. = FALSE)
  
  # CREATE FILE
  if (!grepl(".rmd$", file, ignore.case = TRUE))
    file <- paste0(file, ".Rmd")
  
  # timestamp currently added as a suffix to the filename
  # if we were to change it to a prefix, we need to first figure out the filename
  # (i.e., separate it from the possible path) using the following regular 
  # expression strsplit(string, "\\\\|\\\\\\\\|\\/|\\/\\/"). This looks for
  # \, \\, /, // and the last element is the filename.
  if (timestamp)
    file <- gsub(".rmd$", paste0(format(Sys.time(), "_%Y%b%d"), ".Rmd"), file,
                 ignore.case = TRUE)
  
  file.create(file)
  
  # OPEN CONNECTION
  tmp <- file(file, open = "w")
  
  # HEADER
  writeLines("---", tmp)
  writeLines("output:", tmp)
  writeLines("  html_document:", tmp)
  writeLines("    mathjax: null", tmp)
  writeLines("    title: RLum.Report", tmp)
  writeLines("    theme: united", tmp)
  writeLines("    toc: true", tmp)
  writeLines("    toc_float: true", tmp)
  writeLines("    toc_depth: 6", tmp)
  writeLines("---", tmp)
  
  # INFO
  pkg <- as.data.frame(installed.packages(), row.names = FALSE)
  if ("Luminescence" %in% pkg$Package)
    pkg <- pkg[which(pkg$Package == "Luminescence"), ]
  else
    pkg <- data.frame(LibPath = "-", Version = "not installed", Built = "-")
  
  writeLines(paste("# RLum.Report \n\n<hr>", #<div align='center'></div>
                   "<b>Date:</b>", Sys.time(), "\n\n",
                   "<b>R version:</b>", R.version.string, "\n\n",
                   "<b>Luminescence package</b> \n\n",
                   "<b>&nbsp;&nbsp;&raquo; Path:</b>", pkg$LibPath, "\n\n",
                   "<b>&nbsp;&nbsp;&raquo; Version:</b>", pkg$Version, "\n\n",
                   "<b>&nbsp;&nbsp;&raquo; Built:</b>", pkg$Built, "\n\n",
                   "<b>Object</b> \n\n",
                   "<b>&nbsp;&nbsp;&raquo; Created:</b>", 
                   try(paste(paste(strsplit(obj@.uid, '-|\\.')[[1]][1:3], collapse = "-"),
                             strsplit(obj@.uid, '-|\\.')[[1]][4])), "\n\n",
                   "<b>&nbsp;&nbsp;&raquo; Class:</b>", class(object), "\n\n",
                   "<b>&nbsp;&nbsp;&raquo; Originator:</b>", try(object@originator), "\n\n",
                   "<b>&nbsp;&nbsp;&raquo; Name:</b>", deparse(substitute(object)), "\n\n",
                   "<b>&nbsp;&nbsp;&raquo; Parent ID:</b>", try(object@.pid), "\n\n",
                   "<b>&nbsp;&nbsp;&raquo; Unique ID:</b>", try(object@.uid), "\n\n",
                   "<hr>"),
             tmp)
  
  # OBJECT
  elements <- .struct_RLum(object, root = deparse(substitute(object)))
  
  for (i in 1:nrow(elements)) {
    
    # HEADER
    short.name <- strsplit(as.character(elements$branch[i]), split = "\\$|@|\\[\\[")[[1]]
    links <- gsub("[^@$\\[]", "", as.character(elements$branch[i]))
    type <- ifelse(nchar(links) == 0, "", substr(links, nchar(links), nchar(links)))
    if (type == "[")
      type <- "[["
    
    if (i == 1)
      hlevel <- "#"
    else
      hlevel <- paste(rep("#", elements$depth[i]), collapse = "")
    
    writeLines(paste0(hlevel, " ",
                      paste(rep(".", elements$depth[i]), collapse = ""),
                      type,
                      short.name[length(short.name)],
                      "\n\n"),
               tmp)
    
    # SUBHEADER
    writeLines(paste0("<pre style='padding:0px;border:0px'>",
                      "<span style='color:#428bca'>",
                      " Class: </span>", elements$class[i],
                      "<span style='color:#428bca'>",
                      "   Length: </span>", elements$length[i],
                      "<span style='color:#428bca'>",
                      "   Path: </span>", gsub("@", "<span>@</span>", elements$branch[i]),
                      "</pre>",
                      "\n\n"),
               tmp)
    
    # TABLE CONTENT
    if (elements$endpoint[i]) {
      table <- tryCatch(eval(parse(text = elements$branch[i])),
                        error = function(e) {
                          return(NULL)
                        })
      if (is.null(table))
        table <- "NULL"
      if (elements$class[i] == "call") {
        table <- capture.output(table)
        writeLines("<pre>", tmp)
        for (i in 1:length(table))
          writeLines(table[i], tmp)
        writeLines("</pre>", tmp)
        table <- NULL
      }
      writeLines(pander::pander_return(table),
                 tmp)
      writeLines("\n\n<hr>", tmp)
    }
  }
  
  # OBJECT STRUCTURE
  writeLines(paste("\n\n# Object structure\n\n"), tmp)
  writeLines(pander::pander_return(elements),
             tmp)
  writeLines("\n\n", tmp)
  
  # SAVE SERIALISED OBJECT (.rds file)
  writeLines(paste("<hr># File \n\n"), tmp)
  
  file.rds <- gsub(".rmd$", ".Rsd", file, ignore.case = TRUE)
  saveRDS(object, file.rds)
  
  writeLines(paste0("<code>",
                    "<a href='", file.rds,"'>",
                    "Click here to access the data file", "</a>",
                    "</code>"), tmp)
  
  writeLines(paste("\nThe R object was saved to", file.rds,
                   "To import the object into your R session with the following command:",
                   paste0("<pre>",
                          "x <- readRDS('", file.rds, "')",
                          "</pre>"),
                   "<b>NOTE:</b> If you moved the file to another directory or",
                   "renamed the file you need to change the path/filename in the",
                   "code above accordingly!"),
             tmp)
  
  # SESSION INFO
  writeLines(paste("\n\n<hr># Session Info\n\n"), tmp)
  sessionInfo <- capture.output(sessionInfo())
  writeLines(paste(sessionInfo, collapse = "\n\n"),
             tmp)
  
  # PLOTTING
  if (length(grep("RLum", class(object)))) {
    
    writeLines(paste("\n\n<hr># Plots\n\n"), tmp)
    writeLines(paste0(
      "```{r}\n",
      "library(Luminescence) \n",
      "x <- readRDS('", file.rds,"') \n",
      "plot(x) \n", 
      "```"),
      tmp)
    
    if (inherits(object, "RLum.Results")) {
      
      # AGE MODELS ---- 
      if (grepl("Dose$", object@originator)) {
        writeLines(paste0(
          "```{r}\n",
          "plot_AbanicoPlot(get_RLum(x, 'data')) \n",
          "plot_Histogram(x) \n",
          "plot_KDE(x) \n", 
          "```"),
          tmp)
      }
    }
    
  }
  
  # CLOSE & RENDER
  close(tmp)
  on.exit(closeAllConnections())
  rmarkdown::render(file, clean = clean)
  
  # CLEANUP
  file.remove(file)
}


################################################################################
##                                                                            ##
##                        HELPER FUNCTIONS                                    ##
##                                                                            ##
################################################################################

# ---------------------------------------------------------------------------- #
# This is a recursive function that goes the objects structure and prints
# all slots/elements along with their class, length, depth. 
# ---------------------------------------------------------------------------- #
.tree_RLum <- function(x, root) {
  
  if (missing(root))
    root <- deparse(substitute(x))
  
  ## S4 object -----
  if (isS4(x)) {
    
    # print -----
    cat(c(root, class(x), base::length(x), .depth(root), FALSE, "\n"), sep = ",")
    
    for (slot in slotNames(x)) {
      s4.root <- paste0(root, "@", slot)
      .tree_RLum(slot(x, slot), root = s4.root)
    }
    invisible()
    
    ## List objects -----
  }  else if (inherits(x, "list") | inherits(x, "nls")) {
    
    if (!is.null(names(x)) && length(x) != 0) {
      
      # print -----
      cat(c(root, class(x), base::length(x), .depth(root), FALSE, "\n"), sep = ",") 
      
      for (i in 1:length(x)) {
        element <- names(x)
        if (element[i] == "")
          list.root <- paste0(root, "[[", i, "]]")
        else
          list.root <- paste0(root, "$", element[i])
        .tree_RLum(x[[i]], root = list.root)
      }
    } else if (length(x) != 0) {
      
      # print -----
      cat(c(root, class(x), base::length(x), .depth(root), FALSE, "\n"), sep = ",") 
      
      for (i in 1:length(x)) {
        element <- paste0("[[", seq(1, length(x),1), "]]")
        list.root <- paste0(root, element[i])
        .tree_RLum(x[[i]], root = list.root)
      }
    }
    
    invisible()
    
    ## Data frames -----
  } else if (inherits(x, "data.frame")) { 
    
    if (any(sapply(x, class) == "matrix")) {
      for (i in 1:length(x)) {
        element <- names(x)
        list.root <- paste0(root, "$", element[[i]])
        .tree_RLum(x[[i]], root = list.root)
      }
    } else {
      # print ----
      cat(c(root, class(x), base::length(x), .depth(root), TRUE, "\n"), sep = ",")
    }
    invisible()
    
    ## Last elements -----  
  }  else {
    
    # print ----
    cat(c(root, class(x), base::length(x), .depth(root), TRUE, "\n"), sep = ",") 
    
    invisible()
  }
}

# ---------------------------------------------------------------------------- #
# Derive depth in the structure tree by splitting the directory by 
# indicative accessors @, $, [[
# ---------------------------------------------------------------------------- #
.depth <- function(x) {
  length(strsplit(x, split = "\\$|@|\\[\\[")[[1]]) - 1
}

# ---------------------------------------------------------------------------- #
# This function captures the output of the real worker .tree_RLum and returns
# the structure of the object as a data.frame
# ---------------------------------------------------------------------------- #
.struct_RLum <- function(x, root) {
  if (missing(root))
    root <- deparse(substitute(x))
  s <- capture.output(.tree_RLum(x, root = root))
  df <- as.data.frame(do.call(rbind, strsplit(s, ",")), stringsAsFactors = FALSE)
  names(df) <- c("branch", "class", "length", "depth", "endpoint")
  df$depth <- as.integer(df$depth)
  df$length <- as.numeric(df$depth)
  df$endpoint <- as.logical(df$endpoint)
  invisible(df)
}