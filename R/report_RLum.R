#' Create a HTML report for (RLum) objects
#'
#' This function creates a HTML report for a given object, listing its complete
#' structure and content. The object itself is saved as a serialised .Rds file.
#' The report file serves both as a convenient way of browsing through objects with 
#' complex data structures as well as a mean of properly documenting and saving
#' objects.
#'
#' The HTML report is created with [rmarkdown::render] and has the
#' following structure:
#' 
#' \tabular{ll}{
#'  **Section** \tab **Description** \cr
#'  `Header` \tab A summary of general characteristics of the object \cr
#'  `Object content` \tab A comprehensive list of the complete structure and content of the provided object. \cr
#'  `Object structure` \tab Summary of the objects structure given as a table \cr
#'  `File` \tab Information on the saved RDS file \cr
#'  `Session Info` \tab Captured output from `sessionInfo()` \cr
#'  `Plots` \tab (*optional*) For `RLum-class` objects a variable number of plots \cr
#' }
#'
#' The structure of the report can be controlled individually by providing one or more of the
#' following arguments (all `logical`):
#' 
#' \tabular{ll}{
#' **Argument** \tab **Description** \cr
#' `header` \tab Hide or show general information on the object \cr
#' `main` \tab Hide or show the object's content \cr
#' `structure` \tab Hide or show object's structure \cr
#' `rds` \tab Hide or show information on the saved RDS file \cr
#' `session` \tab Hide or show the session info \cr
#' `plot` \tab Hide or show the plots (depending on object) \cr
#' }
#' 
#' Note that these arguments have higher precedence than `compact`.
#'
#' Further options that can be provided via the `...` argument:
#' 
#' \tabular{ll}{
#' **Argument** \tab **Description** \cr
#' `short_table` \tab If `TRUE` only show the first and last 5 rows of lang tables. \cr
#' `theme` \tab Specifies the Bootstrap
#' theme to use for the report. Valid themes include "default", "cerulean", "journal", "flatly", 
#' "readable", "spacelab", "united", "cosmo", "lumen", "paper", "sandstone", "simplex", and "yeti". \cr
#' `highlight` \tab Specifies the syntax highlighting
#'  style. Supported styles include "default", "tango", "pygments", "kate", "monochrome", 
#'  "espresso", "zenburn", "haddock", and "textmate". \cr
#' `css` \tab `TRUE` or `FALSE` to enable/disable custom CSS styling \cr
#' }
#' 
#' The following arguments can be used to customise the report via CSS (Cascading Style Sheets):
#' 
#' \tabular{ll}{
#' **Argument** \tab **Description** \cr
#' `font_family` \tab Define the font family of the HTML document (default: arial) \cr
#' `headings_size` \tab Size of the <h1> to <h6> tags used to define HTML headings (default: 166\%). \cr
#' `content_color` \tab Color of the object's content (default: #a72925). \cr
#' }
#' 
#' Note that these arguments must all be of class [character] and follow standard CSS syntax.
#' For exhaustive CSS styling you can provide a custom CSS file for argument `css.file`. 
#' CSS styling can be turned of using `css = FALSE`.
#'
#' @param object (**required**): 
#' The object to be reported on, preferably of any `RLum`-class.
#' 
#' @param file [character] (*with default*): 
#' A character string naming the output file. If no filename is provided a 
#' temporary file is created.
#' 
#' @param title [character] (*with default*):
#' A character string specifying the title of the document.
#' 
#' @param compact [logical] (*with default*):
#' When `TRUE` the following report components are hidden: 
#' `@@.pid`, `@@.uid`, `'Object structure'`, `'Session Info'`
#' and only the first and last 5 rows of long matrices and data frames are shown.
#' See details.
#' 
#' @param timestamp [logical] (*with default*):
#' `TRUE` to add a timestamp to the filename (suffix).
#' 
#' @param launch.browser [logical] (*with default*):
#' `TRUE` to open the HTML file in the system's default web browser after
#' it has been rendered.
#' 
#' @param css.file [character] (*optional*):
#' Path to a CSS file to change the default styling of the HTML document.
#' 
#' @param quiet [logical] (*with default*):
#' `TRUE` to supress printing of the pandoc command line.
#' 
#' @param clean [logical] (*with default*): 
#' `TRUE` to clean intermediate files created during rendering.
#' 
#' @param ... further arguments passed to or from other methods and to control
#' the document's structure (see details).
#' 
#' @section Function version: 0.1.0
#' 
#' @author 
#' Christoph Burow, University of Cologne (Germany) \cr
#' 
#' @note
#' This function requires the R packages 'rmarkdown', 'pander' and 'rstudioapi'.
#' 
#' @seealso [rmarkdown::render], [pander::pander_return],
#' [pander::openFileInOS], [rstudioapi::viewer],
#' [browseURL]
#' 
#' @return
#' Writes a HTML and .Rds file.
#' 
#' @examples
#' 
#' \dontrun{
#' ## Example: RLum.Results ----
#' 
#' # load example data
#' data("ExampleData.DeValues")
#' 
#' # apply the MAM-3 age model and save results
#' mam <- calc_MinDose(ExampleData.DeValues$CA1, sigmab = 0.2) 
#' 
#' # create the HTML report
#' report_RLum(object = mam, file = "~/CA1_MAM.Rmd",
#'             timestamp = FALSE,
#'             title = "MAM-3 for sample CA1")
#' 
#' # when creating a report the input file is automatically saved to a 
#' # .Rds file (see saveRDS()).
#' mam_report <- readRDS("~/CA1_MAM.Rds")
#' all.equal(mam, mam_report)
#' 
#' 
#' ## Example: Temporary file & Viewer/Browser ----
#' 
#' # (a)
#' # Specifying a filename is not necessarily required. If no filename is provided,
#' # the report is rendered in a temporary file. If you use the RStudio IDE, the
#' # temporary report is shown in the interactive Viewer pane.
#' report_RLum(object = mam)
#' 
#' # (b)
#' # Additionally, you can view the HTML report in your system's default web browser.
#' report_RLum(object = mam, launch.browser = TRUE)
#' 
#' 
#' ## Example: RLum.Analysis ----
#' 
#' data("ExampleData.RLum.Analysis")
#' 
#' # create the HTML report (note that specifying a file
#' # extension is not necessary)
#' report_RLum(object = IRSAR.RF.Data, file = "~/IRSAR_RF")
#' 
#' 
#' ## Example: RLum.Data.Curve ----
#' 
#' data.curve <- get_RLum(IRSAR.RF.Data)[[1]]
#' 
#' # create the HTML report
#' report_RLum(object = data.curve, file = "~/Data_Curve")
#' 
#' ## Example: Any other object ----
#' x <- list(x = 1:10, 
#'           y = runif(10, -5, 5), 
#'           z = data.frame(a = LETTERS[1:20], b = dnorm(0:9)),
#'           NA)
#' 
#' report_RLum(object = x, file = "~/arbitray_list")
#' }
#'
#' @md
#' @export
report_RLum <- function(object, 
                        file  = tempfile(),
                        title = "RLum.Report",
                        compact = TRUE,
                        timestamp = TRUE,
                        launch.browser = FALSE,
                        css.file = NULL,
                        quiet = TRUE,
                        clean = TRUE, 
                        ...) {
  
  ## ------------------------------------------------------------------------ ##
  ## PRE-CHECKS ----
  
  # check if required namespace(s) are available
  if (!requireNamespace("rmarkdown", quietly = TRUE))
    stop("Creating object reports requires the 'rmarkdown' package.",
         " To install this package run 'install.packages('rmarkdown')' in your R console.", 
         call. = FALSE)
  if (!requireNamespace("pander", quietly = TRUE))
    stop("Creating object reports requires the 'pander' package.",
         " To install this package run 'install.packages('pander')' in your R console.", 
         call. = FALSE)
  if (!requireNamespace("rstudioapi", quietly = TRUE)) {
    warning("Creating object reports requires the 'rstudioapi' package.",
            " To install this package run 'install.packages('rstudioapi')' in your R console.", 
            call. = FALSE)
    isRStudio <- FALSE
  } else {
    isRStudio <- TRUE
  }
  
  # check if files exist
  if (!is.null(css.file))
    if(!file.exists(css.file))
      stop("Couldn't find the specified CSS file at '", css.file, "'", call. = FALSE)
  
  ## ------------------------------------------------------------------------ ##
  ## STRUCTURE ----
  structure <- list(header = TRUE,
                    main = TRUE,
                    structure = ifelse(compact, FALSE, TRUE),
                    rds = TRUE,
                    session = ifelse(compact, FALSE, TRUE),
                    plot = TRUE)
  
  # specifying report components has higher precedence than the 'compact' arg
  structure <- modifyList(structure, list(...))
  
  
  ## OPTIONS ----
  options <- list(short_table = ifelse(compact, TRUE, FALSE),
                  theme = "cerulean",
                  highlight = "haddock",
                  css = TRUE)
  
  options <- modifyList(options, list(...))
  
  ## CSS DEFAULTS ----
  css <- list(font_family = "arial",
              headings_size = "166%",
              content_color = "#a72925")
  
  css <- modifyList(css, list(...))
  
  ## ------------------------------------------------------------------------ ##
  ## CREATE FILE ----
  
  isTemp <- missing(file)
  
  # make sure the filename ends with .Rmd extension
  if (!grepl(".rmd$", file, ignore.case = TRUE))
    file <- paste0(file, ".Rmd")
  
  # Timestamp: currently added as a suffix to the filename
  # if we were to change it to a prefix, we need to first figure out the filename
  # (i.e., separate it from the possible path) using the following regular 
  # expression strsplit(string, "\\\\|\\\\\\\\|\\/|\\/\\/"). This looks for
  # \, \\, /, // and the last element is the filename.
  if (timestamp)
    file <- gsub(".rmd$", paste0(format(Sys.time(), "_%Y%b%d"), ".Rmd"), file,
                 ignore.case = TRUE)
  
  # sanitize file name
  file <- gsub("\\\\", "\\/", file)
  file.html <- gsub(".rmd$", ".html", file, ignore.case = TRUE)
  file.rds <- gsub(".rmd$", ".Rds", file, ignore.case = TRUE)
  
  # Create and open the file
  file.create(file)
  tmp <- file(file, open = "w")
  
  # save RDS file
  saveRDS(object, file.rds)
  
  ## ------------------------------------------------------------------------ ##
  ## WRITE CONTENT ----
  
  # HEADER ----
  writeLines("---", tmp)
  writeLines("output:", tmp)
  writeLines("  html_document:", tmp)
  writeLines("    mathjax: null", tmp)
  writeLines("    title: RLum.Report", tmp)
  writeLines(paste("    theme:", options$theme), tmp)
  writeLines(paste("    highlight:", options$highlight), tmp)
  writeLines("    toc: true", tmp)
  writeLines("    toc_float: true", tmp)
  writeLines("    toc_depth: 6", tmp)
  if (!is.null(css.file))
    writeLines(paste("    css:", css.file), tmp)
  writeLines("    md_extensions: -autolink_bare_uris", tmp)
  writeLines("---", tmp)
  
  # CASCADING STYLE SHEETS ----
  if (options$css) {
    writeLines(paste0(
      "<style>",
      paste0("h1, h2, h3, h4, h5, h6 { font-size:", css$headings_size," } \n"),
      paste0("#root { color: ", css$content_color," } \n"),
      paste0("BODY { font-family:", css$font_family, " } \n"),
      "</style>"
    ),
    tmp)
  }
  
  # INFO ----
  # check if Luminescence package is installed and get details
  pkg <- as.data.frame(installed.packages(), row.names = FALSE)
  if ("Luminescence" %in% pkg$Package)
    pkg <- pkg[which(pkg$Package == "Luminescence"), ]
  else
    pkg <- data.frame(LibPath = "-", Version = "not installed", Built = "-")
  
  # Title
  writeLines(paste("<div align='center'><h1>", title, "</h1></div>\n\n<hr>"), tmp) 
  
  # write information on R, Luminescence package, Object
  if (structure$header) {
    writeLines(paste("**Date:**", Sys.time(), "\n\n",
                     "**R version:**", R.version.string, "\n\n",
                     "**Luminescence package** \n\n",
                     "**&nbsp;&nbsp;&raquo; Path:**", pkg$LibPath, "\n\n",
                     "**&nbsp;&nbsp;&raquo; Version:**", pkg$Version, "\n\n",
                     "**&nbsp;&nbsp;&raquo; Built:**", pkg$Built, "\n\n",
                     "**Object** \n\n",
                     "**&nbsp;&nbsp;&raquo; Created:**", 
                     tryCatch(paste(paste(strsplit(object@.uid, '-|\\.')[[1]][1:3], collapse = "-"),
                                    strsplit(object@.uid, '-|\\.')[[1]][4]),
                              error = function(e) "-"), "\n\n",
                     "**&nbsp;&nbsp;&raquo; Class:**", class(object), "\n\n",
                     "**&nbsp;&nbsp;&raquo; Originator:**", 
                     tryCatch(object@originator, error = function(e) "-"), "\n\n",
                     "**&nbsp;&nbsp;&raquo; Name:**", deparse(substitute(object)), "\n\n",
                     "**&nbsp;&nbsp;&raquo; Parent ID:**", 
                     tryCatch(object@.pid, error = function(e) "-"), "\n\n",
                     "**&nbsp;&nbsp;&raquo; Unique ID:**", 
                     tryCatch(object@.uid, error = function(e) "-"), "\n\n",
                     "<hr>"),
               tmp)
    
    if (isTemp) {
      writeLines(paste("<a href=", paste0("file:///", file.html),
                       "class='btn btn-primary' download>Save report</a>"), tmp)
      writeLines(paste("<a href=", paste0("file:///", file.rds),
                       "class='btn btn-primary' download>Save data</a> \n\n"), tmp)
    }
    
  }#EndOf::Header
  
  # OBJECT ----
  elements <- .struct_RLum(object, root = deparse(substitute(object)))
  
  if (structure$main) {
    for (i in 1:nrow(elements)) {
      
      # SKIP ELEMENT?
      # hide @.pid and @.uid if this is a shortened report (default)
      if (elements$bud[i] %in% c(".uid", ".pid") && compact == TRUE)
        next
      
      
      # HEADER
      short.name <- elements$bud[i]
      links <- gsub("[^@$\\[]", "", as.character(elements$branch[i]))
      type <- ifelse(nchar(links) == 0, "", substr(links, nchar(links), nchar(links)))
      if (type == "[")
        type = ""
      
      # HTML header level is determined by the elements depth in the object
      # exception: first row is always the object's name and has depth zero
      if (i == 1)
        hlevel <- "#"
      else
        hlevel <- paste(rep("#", elements$depth[i]), collapse = "")
      
      # write header; number of dots represents depth in the object. because there
      # may be duplicate header names, for each further occurence of a name
      # Zero-width non-joiner entities are added to the name (non visible)
      writeLines(paste0(hlevel, " ",
                        "<span style='color:#74a9d8'>",
                        paste(rep("..", elements$depth[i]), collapse = ""),
                        type,
                        "</span>",
                        paste(rep("&zwnj;", elements$bud.freq[i]), collapse = ""),
                        short.name[length(short.name)],
                        ifelse(elements$endpoint[i], "", "{#root}"),
                        "\n\n"),
                 tmp)
      
      # SUBHEADER
      # contains information on Class, Length, Dimensions, Path
      writeLines(paste0("<pre style='padding:0px;border:0px'>",
                        "<span style='color:#428bca'>",
                        " Class: </span>", elements$class[i],
                        "<span style='color:#428bca'>",
                        "   Length: </span>", elements$length[i],
                        "<span style='color:#428bca'>",
                        "   Dimensions: </span>", 
                        ifelse(elements$row[i] != 0, paste0(elements$row[i], ", ", elements$col[i]), "-"),
                        "<span style='color:#428bca'>",
                        "\n Path: </span>", gsub("@", "<span>@</span>", elements$branch[i]),
                        "</pre>",
                        "\n\n"),
                 tmp)
      
      # TABLE CONTENT
      # the content of a branch is only printed if it was determined an endpoint
      # in the objects structure
      if (elements$endpoint[i]) {
        table <- tryCatch(eval(parse(text = elements$branch[i])),
                          error = function(e) {
                            return(NULL)
                          })
        # exceptions: content may be NULL; convert raw to character to stay
        # compatible with pander::pander
        if (is.null(table) | length(table) == 0)
          table <- "NULL"
        if (any(class(table) == "raw"))
          table <- as.character(table)
        
        # exception: surround objects of class "call" with <pre> tags to prevent
        # HTML autoformatting
        if (elements$class[i] == "call") {
          table <- capture.output(table)
          writeLines("<pre>", tmp)
          for (i in 1:length(table))
            writeLines(table[i], tmp)
          writeLines("</pre>", tmp)
          table <- NULL
        }
        
        # shorten the table if it has more than 15 rows
        if (options$short_table) {
          if (is.matrix(table) || is.data.frame(table)) {
            if (nrow(table) > 15) {
              
              writeLines(pander::pander_return(rbind(head(table, 5),
                                                     tail(table, 5)),
                                               caption = "shortened (only first and last five rows shown)"), tmp)
              next
              
            }
          }
        }
        
        # write table using pander and end each table with a horizontal line
        writeLines(pander::pander_return(table),
                   tmp)
        writeLines("\n\n<hr>", tmp)
        
      }
    }
  }#EndOf::Main
  
  # OBJECT STRUCTURE ----
  if (structure$structure) {
    writeLines(paste("\n\n# Object structure\n\n"), tmp)
    
    elements.html <- elements
    elements.html$branch <- gsub("\\$", "&#36;", elements$branch)
    writeLines(pander::pander_return(elements.html, 
                                     justify = paste(rep("l", ncol(elements)), collapse = "")),
               tmp)
    writeLines("\n\n", tmp)
  }#EndOf::Structure
    
  if (structure$rds) {
    # SAVE SERIALISED OBJECT (.rds file) ----
    writeLines(paste("<hr># File \n\n"), tmp)
    
    writeLines(paste0("<code>",
                      "<a href='", paste0("file:///", gsub("\\~\\/", "", file.rds)),"' download>",
                      "Click here to access the data file", "</a>",
                      "</code>"), tmp)
    
    writeLines(paste("\nThe R object was saved to <span style='color:#428bca'>", file.rds, "</span>.",
                     "To import the object into your R session with the following command:",
                     paste0("<pre>",
                            "x <- readRDS('", file.rds, "')",
                            "</pre>"),
                     "**NOTE:** If you moved the file to another directory or",
                     "renamed the file you need to change the path/filename in the",
                     "code above accordingly!"),
               tmp)
  }#EndOf::File
  
  # SESSION INFO ----
  if (structure$session) {
    writeLines(paste("\n\n<hr># Session Info\n\n"), tmp)
    sessionInfo <- capture.output(sessionInfo())
    writeLines(paste(sessionInfo, collapse = "\n\n"),
               tmp)
  }
  
  # PLOTTING ----
  if (structure$plot) {
    isRLumObject <- length(grep("RLum", class(object)))
    
    if (is.list(object))
      isRLumList <- all(sapply(object, function(x) inherits(x, "RLum.Data.Curve")))
    else
      isRLumList <- FALSE
    
    if (isRLumObject | isRLumList) {
      
      # mutual exclusivity: it is either a list or an RLum-Object 
      if (isRLumList)
        plotCommand <- "invisible(sapply(x, plot)) \n"
      else
        plotCommand <- "plot(x) \n"
      
      writeLines(paste("\n\n<hr># Plots\n\n"), tmp)
      writeLines(paste0(
        "```{r}\n",
        "library(Luminescence) \n",
        "x <- readRDS('", file.rds,"') \n",
        plotCommand,
        "```"),
        tmp)
      
      if (inherits(object, "RLum.Results")) {
        
        # AGE MODELS ----
        models <- c("calc_CommonDose",
                    "calc_CentralDose",
                    "calc_FiniteMixture",
                    "calc_MinDose",
                    "calc_MaxDose",
                    "calc_IEU",
                    "calc_FuchsLang2001")
        
        if (object@originator %in% models) {
          writeLines(paste0(
            "```{r}\n",
            "plot_AbanicoPlot(x) \n",
            "plot_Histogram(x) \n",
            "plot_KDE(x) \n",
            "plot_ViolinPlot(x) \n",
            "```"),
            tmp)
        }
      }
      
    }
  }#EndOf::Plot
  
  ## ------------------------------------------------------------------------ ##
  ## CLOSE & RENDER ----
  close(tmp)
  on.exit(closeAllConnections())
  rmarkdown::render(file, clean = clean, quiet = quiet)
  
  ## ------------------------------------------------------------------------ ##
  ## SHOW FILE -----
  
  # SHOW REPORT IN RSTUDIOS VIEWER PANE ----
  if (isRStudio) {
    if (isTemp) {
      try(rstudioapi::viewer(file.html))
    } else {
      # The Viewer Pane only works for files in a sessions temp directory
      # see: https://support.rstudio.com/hc/en-us/articles/202133558-Extending-RStudio-with-the-Viewer-Pane
      file.copy(file.html, file.path(tempdir(), "report.html"), overwrite = TRUE)
      try(rstudioapi::viewer(file.path(tempdir(), "report.html")))
    }
  }
  
  # launch browser if desired
  # browseURL() listens on localhost to show the file with the problem that
  # the download links dont work anymore. hence, we try to open the file
  # with pander::openFileInOS and use browseURL() only as fallback
  if (launch.browser) {
    opened <- tryCatch(pander::openFileInOS(file.html), error = function(e) "error")
    if (!is.null(opened))
      try(browseURL(file.html))
  }
  
  
  ## ------------------------------------------------------------------------ ##
  ## CLEANUP ----
  
  # note that 'clean' as also passed to rmarkdown::render
  if (clean)
    file.remove(file)
  
  invisible()
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
    cat(c(root, .class(x), base::length(x), .depth(root), FALSE, .dimension(x), "\n"), sep = "|")
    
    for (slot in slotNames(x)) {
      s4.root <- paste0(root, "@", slot)
      .tree_RLum(slot(x, slot), root = s4.root)
    }
    invisible()
    
    ## List objects -----
  }  else if (inherits(x, "list") | typeof(x) == "list" & !inherits(x, "data.frame")) {
    
    if (!is.null(names(x)) && length(x) != 0) {
      
      # print -----
      cat(c(root, .class(x), base::length(x), .depth(root), FALSE, .dimension(x), "\n"), sep = "|") 
      
      element <- names(x)
      
      for (i in 1:length(x)) {
        
        if (grepl(" ", element[i]))
          element[i] <- paste0("`", element[i], "`")
        
        if (element[i] == "")
          list.root <- paste0(root, "[[", i, "]]")
        else
          list.root <- paste0(root, "$", element[i])
        
        .tree_RLum(x[[i]], root = list.root)
      }
    } else if (length(x) != 0) {
      
      # print -----
      cat(c(root, .class(x), base::length(x), .depth(root), FALSE, .dimension(x), "\n"), sep = "|") 
      
      element <- paste0("[[", seq(1, length(x),1), "]]")
      
      for (i in 1:length(x)) {
        if (grepl(" ", element[i]))
          element[i] <- paste0("`", element[i], "`")
        
        list.root <- paste0(root, element[i])
        .tree_RLum(x[[i]], root = list.root)
      }
    } else if (length(x) == 0) {
      
      cat(c(root, .class(x), base::length(x), .depth(root), FALSE, .dimension(x), "\n"), sep = "|") 
      
    }
    
    invisible()
    
    ## Data frames -----
  } else if (inherits(x, "data.frame")) { 
    
    if (any(sapply(x, function(col) { inherits(col, "matrix") } ))) {
      
      element <- names(x)
      
      for (i in 1:length(x)) {
        if (grepl(" ", element[i]))
          element[i] <- paste0("`", element[i], "`")
        
        list.root <- paste0(root, "$", element[[i]])
        .tree_RLum(x[[i]], root = list.root)
      }
    } else {
      # print ----
      cat(c(root, .class(x), base::length(x), .depth(root), TRUE, .dimension(x), "\n"), sep = "|")
    }
    invisible()
    
    ## Last elements -----  
  }  else {
    
    # print ----
    cat(c(root, .class(x), base::length(x), .depth(root), TRUE, .dimension(x), "\n"), sep = "|") 
    
    invisible()
  }
}

# ---------------------------------------------------------------------------- #
# a) Derive depth in the structure tree by splitting the directory by 
# indicative accessors @, $, [[
# b) Wrapper for dim() to cope with NULL values
# c) Wrapper for class() that collapses the classes of an object
# ---------------------------------------------------------------------------- #
.depth <- function(x) {
  length(strsplit(x, split = "\\$|@|\\[\\[")[[1]]) - 1
}
.dimension <- function(x) {
  if (!is.null(dim(x)))
    dim <- paste(dim(x), collapse = "|")
  else
    dim <- c(0, 0)
}
.class <- function(x) {
  paste(class(x), collapse = "/")
}

# ---------------------------------------------------------------------------- #
# This function captures the output of the real worker .tree_RLum and returns
# the structure of the object as a data.frame
# ---------------------------------------------------------------------------- #
.struct_RLum <- function(x, root) {
  if (missing(root))
    root <- deparse(substitute(x))
  s <- capture.output(.tree_RLum(x, root = root))
  df <- as.data.frame(do.call(rbind, strsplit(s, "|", fixed = TRUE)), stringsAsFactors = FALSE)
  names(df) <- c("branch", "class", "length", "depth", "endpoint", "row", "col")
  df$depth <- as.integer(df$depth)
  df$length <- as.numeric(df$length)
  df$endpoint <- as.logical(df$endpoint)
  df$row <- as.integer(df$row)
  df$col <- as.integer(df$col)
  df$bud <- do.call(c, lapply(strsplit(df$branch, "\\$|@|\\[\\["), 
                              function(x) x[length(x)]))
  if (length(grep("]", df$bud)) != 0)
    df$bud[grep("]", df$bud)] <- paste0("[[", df$bud[grep("]", df$bud)])
  df$bud.freq <- NA # 1:nrow(df)
  
  # reorder data.frame
  df <- df[ ,c("branch", "bud", "bud.freq", "class", 
               "length", "depth", "row", "col", "endpoint")]
  
  # for the report we must not have the same last element names of same
  # depth (HTML cannot discriminate between #links of <h> headers)
  ## TODO: this is highly inefficient for unnamed list due to recurrent indices
  dlevel <- max(table(df$bud))
  
  for (i in 1:dlevel) {
    unique.bud <- unique(df[is.na(df$bud.freq), ]$bud)
    df[is.na(df$bud.freq), ][match(unique.bud, df[is.na(df$bud.freq), ]$bud), ]$bud.freq <- i - 1
  }
  
  invisible(df)
}