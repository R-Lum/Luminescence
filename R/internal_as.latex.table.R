#' Create LaTex tables from data.frames and RLum objects
#' 
#' This function takes a data.frame and returns a table in LaTex code that
#' can be copied in any tex document.
#'
#' @param x a \code{\link{data.frame}} or \code{RLum} object
#' @param row.names currently unused
#' @param col.names currently unused
#' @param comments \code{\link{logical}} insert LaTex comments
#' @param pos \code{\link{character}} of length one specifying the alignment
#' of each column, e.g., pos'clr' for a three column data frame and center, left
#' and right alignment
#' @param digits \code{\link{numeric}} number of digits (numeric fields)
#' @param select a \code{\link{character}} vector passed to \code{\link{subset}}
#' @param ... options: \code{verbose}
#'
#' @section TODO:
#' - Improve by using RegEx to dynamically find error fields, eg. ( "([ ]err)|(^err)" )
#' - 
#' 
#' @return
#' Returns LaTex code
#'
#' @examples
#' df <- data.frame(x = 1:10, y = letters[1:10])
#' .as.latex.table(df)
#' .as.latex.table(df, pos = "lr")
#' .as.latex.table(df, select = "y", pos = "r")
#' 
#' @noRd
.as.latex.table <- function(x, 
                            row.names = NULL, 
                            col.names = NULL, 
                            comments = TRUE,
                            pos = "c",
                            digits = 3,
                            select,
                            ...) {
  
  args <- list(x = x,
               row.names = row.names,
               col.names = col.names,
               comments = comments,
               pos = pos,
               digits = digits,
               ... = ...)
  if (!missing(select))
    args$select <- select
  
  switch(class(x)[1],
         data.frame = do.call(".as.latex.table.data.frame", args),
         DRAC.highlights = do.call(".as.latex.table.data.frame", args),
         RLum.Results = do.call(".as.latex.table.RLum.Results", args))
}


.as.latex.table.RLum.Results <- function(x, 
                                         row.names = NULL, 
                                         col.names = NULL, 
                                         comments = TRUE,
                                         pos = "c",
                                         digits = 3,
                                         select,
                                         ...) {
  
  ## CUSTOM TEMPLATES ----
  
  ## Object: DRAC.highlights
  # -> Combine value and error fields
  if (x@originator == "use_DRAC") {
    x <- get_RLum(x)
    if (inherits(x, "DRAC.highlights")) {
      fields.w.error <- seq(4, 25, 2)
      for(i in fields.w.error) {
        x[ ,i] <- paste0(x[ ,i], "$\\pm{}$", x[ ,i+1])
      }
      x <- x[-c(fields.w.error + 1)]
    }
    .as.latex.table(x)
  }# EndOf::use_DRAC
  
}

.as.latex.table.data.frame <- function(x, 
                                       row.names = NULL, 
                                       col.names = NULL, 
                                       comments = TRUE,
                                       pos = "c",
                                       digits = 3,
                                       select,
                                       ...) {
  ## Integrity checks ----
  if (!is.data.frame(x))
    stop("x must be a data frame", call. = FALSE)
  if (!is.null(col.names) && length(col.names) != ncol(x))
    stop("length of col.names does not match the number of columns",
         call. = FALSE)
  if (!is.null(row.names) && length(row.names) != nrow(x))
    stop("length of row.names does not match the number of rows",
         call. = FALSE)
  if (length(pos) != 1) 
    stop("length of pos does not match the number of columns", 
         call. = FALSE)
  
  ## Default settings ----
  options <- list(verbose = TRUE)
  
  ## Override settings ----
  options <- modifyList(options, list(...))
  
  ## Subset data frame ----
  if (!missing(select)) {
    is.name <- select %in% names(x)
    if (any(!is.name))
      stop("Undefined columns selected. Please check provided column names in 'select'.", 
           call. = FALSE)
    x <- subset(x, select = select)
  }
  
  ## Format numeric fields ----
  for (i in 1:ncol(x)) {
    if (is.factor(x[ ,i]))
      x[ ,i] <- as.character(x[ ,i])
    test.numeric <- suppressWarnings(as.numeric(x[ ,i]))
    if (!is.na(test.numeric[1]))
      x[ ,i] <- format(test.numeric, nsmall = digits, digits = digits)
  }
  
  ## Comments ----
  tex.comment.usePackage <- ifelse(comments,
                                   "% add \\usepackage{adjustbox} to latex preamble \n",
                                   "")
  
  ## Header ----
  col.names <-   tex.table.header <- gsub(pattern = " ", 
                                          x = names(x), 
                                          replacement = " \\\\\\\\ ")
  tex.table.header <- paste0("\t", 
                             paste("\\multicolumn{1}{p{2cm}}{\\centering", 
                                   col.names, 
                                   "}", 
                                   collapse = " & \n\t"),
                             "\\\\ \n")
  
  ## Rows ----
  tex.table.rows <- ""
  for (i in 1:nrow(x)) {
    tex.table.rows <- paste0(tex.table.rows, 
                             paste(paste(x[i, ], collapse = " & "),
                                   "\\\\ \n"))
  }
  
  ## Tex table ----
  if (nchar(pos) != 1 && nchar(pos) != ncol(x))
    pos <- "c"
  if (!any(strsplit(pos, split = "")[[1]] %in% c("l", "c", "r")))
    pos <- "c"
  if (nchar(pos) == 1)
    pos <- strrep(pos, ncol(x))
  
  tex.table.begin <- paste0("\\begin{table}[ht] \n",
                            "  \\centering \n",
                            "  \\begin{adjustbox}{max width=\\textwidth} \n",
                            paste("  \\begin{tabular}{", pos, "}\n"),
                            "     \\hline \n")
  
  tex.table.end <-  paste0("     \\hline \n",
                           "   \\end{tabular} \n",
                           "   \\end{adjustbox} \n",
                           "\\end{table}")
  
  tex.table <- paste0(tex.comment.usePackage,
                      tex.table.begin,
                      tex.table.header,
                      "\\hline \n",
                      tex.table.rows,
                      tex.table.end)
  
  if (options$verbose)
    cat(tex.table)
  
  invisible(tex.table)
}