#' Create LaTex tables from data.frames and RLum objects
#'
#' This function takes a data.frame and returns a table in LaTex code that
#' can be copied into any tex document.
#'
#' @param x [data.frame] or `RLum` object (**required**)
#'
#' @param row.names currently unused
#'
#' @param col.names currently unused
#'
#' @param comments [logical] (*with default*):
#' insert LaTex comments
#'
#' @param pos [character] (*with default*):
#' `character` of length one specifying the alignment of each column, e.g.,
#' pos'clr' for a three column data frame and center, left
#'  and right alignment
#'
#' @param digits [numeric] (*with default*):
#' number of digits (numeric fields)
#'
#' @param rm.zero [logical] (*with default*): remove columns containing
#' only zeros, however this might not be wanted in all cases
#'
#' @param select [character] (*optional*):
#' a [character] vector passed to [subset]
#'
#' @param split [integer] (*optional*):
#' an [integer] specifying the number of individual tables
#' the data frame is split into. Useful for wide tables. Currently unused.
#'
#' @param tabular_only [logical] (*with default*): if `TRUE` only the tabular but not the
#' table environment is returned. This gives a lot of additional flexibility at hand
#'
#' @param ... options: `verbose`
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
#' @md
#' @noRd
.as.latex.table <- function(x,
                            row.names = NULL,
                            col.names = NULL,
                            comments = TRUE,
                            pos = "c",
                            digits = 3,
                            rm.zero = TRUE,
                            select,
                            split = NULL,
                            tabular_only = FALSE,
                            ...) {

  args <- list(x = x,
               row.names = row.names,
               col.names = col.names,
               comments = comments,
               pos = pos,
               digits = digits,
               rm.zero = rm.zero,
               split = split,
               tabular_only = tabular_only,
               ... = ...)
  if (!missing(select))
    args$select <- select

  switch(class(x)[1],
         data.frame = do.call(".as.latex.table.data.frame", args),
         DRAC.highlights = do.call(".as.latex.table.data.frame", args),
         RLum.Results = do.call(".as.latex.table.RLum.Results", args))
}

################################################################################
## "Method"                  RLum.Results                                     ##
##----------------------------------------------------------------------------##
.as.latex.table.RLum.Results <- function(x,
                                         row.names = NULL,
                                         col.names = NULL,
                                         comments = TRUE,
                                         pos = "c",
                                         digits = 3,
                                         rm.zero = TRUE,
                                         select,
                                         split = NULL,
                                         ...) {

  ## Object: DRAC.highlights
  if (x@originator == "use_DRAC") {
    x <- get_RLum(x)$highlights
    x <- .digits(x, digits)

    ##remove columns containing zero values ... they add no information
    if(rm.zero){
      x <- x[sapply(x, function(y){
        y <- suppressWarnings(as.numeric(y))
        if(anyNA(y) || sum(y, na.rm = TRUE) != 0){
          TRUE

        }else{
          FALSE

        }
      })]
    }

    ##add +/- symbol and remove the columns we don't need
    fields.w.error <- (grep(names(x), pattern = "err", fixed = TRUE) - 1)

    for(i in fields.w.error)
      x[ ,i] <- paste0(x[ ,i], "\\,$\\pm$\\,", trimws(x[ ,i + 1]))
    x <- x[-c(fields.w.error + 1)]

    ##create latex table
    text <- .as.latex.table(x, comments = comments, pos = pos, split = split, ...)

    ##split table
    text <- strsplit(text[[1]], split = "\n", fixed = TRUE)

    ##exchange columns ... or delete them at all (2nd step)

      ##Mineral ID
      for(i in 1:length(text)){
        text[[i]][grepl(pattern = "Mineral", x = text[[i]], fixed = TRUE)] <-
          "\t\\multicolumn{1}{p{0.5cm}}{\\centering \\textbf{M.}} & "
      }

    ##put things again together (single character)
    text <- paste(text[[1]], collapse = "\n")

    ##replace some latex stuff
    text <- gsub(pattern = "p{2cm}", replacement = "p{1.5cm}", x = text, fixed = TRUE)
    text <- gsub(pattern = "Gy.ka-1", replacement = "Gy~ka$^{-1}$", x = text, fixed = TRUE)
    text <- gsub(pattern = "De", replacement = "$D_{E}$", x = text, fixed = TRUE)
    text <- gsub(pattern = "alphadoserate", replacement = "$\\dot{D}_{\\alpha}$", x = text, fixed = TRUE)
    text <- gsub(pattern = "betadoserate", replacement = "$\\dot{D}_{\\beta}$", x = text, fixed = TRUE)
    text <- gsub(pattern = "gammadoserate", replacement = "$\\dot{D}_{\\gamma}$", x = text, fixed = TRUE)
    text <- gsub(pattern = "Cosmicdoserate", replacement = "$\\dot{D}_{cosm.}$", x = text, fixed = TRUE)
    text <- gsub(pattern = "External \\\\ doserate", replacement = "$\\dot{D}_{ext.}$", x = text, fixed = TRUE)
    text <- gsub(pattern = "Internal \\\\ doserate", replacement = "$\\dot{D}_{int.}$", x = text, fixed = TRUE)
    text <- gsub(pattern = "Environmental \\\\ Dose \\\\ Rate", replacement = "$\\dot{D}_{env.}$", x = text, fixed = TRUE)

    ## return result
    return(text)

  }# EndOf::use_DRAC
}

################################################################################
## "Method"                     data.frame                                    ##
##----------------------------------------------------------------------------##
.as.latex.table.data.frame <- function(x,
                                       row.names = NULL,
                                       col.names = NULL,
                                       comments = TRUE,
                                       pos = "c",
                                       digits = 3,
                                       select,
                                       split = NULL,
                                       tabular_only = FALSE,
                                       ...) {
  .set_function_name("as.latex.table.data.frame")
  on.exit(.unset_function_name(), add = TRUE)

  ## Integrity tests --------------------------------------------------------
  .validate_class(x, "data.frame")
  if (!is.null(col.names) && length(col.names) != ncol(x))
    .throw_error("Length of 'col.names' does not match the number of columns")
  if (!is.null(row.names) && length(row.names) != nrow(x))
    .throw_error("Length of 'row.names' does not match the number of rows")
  if (length(pos) != 1)
    .throw_error("Length of 'pos' does not match the number of columns")

  ## Default settings ----
  options <- list(verbose = TRUE)

  ## Override settings ----
  options <- modifyList(options, list(...))

  ## Subset data frame ----
  if (!missing(select)) {
    is.name <- select %in% names(x)
    if (any(!is.name))
      .throw_error("Undefined columns selected. Please check provided ",
                   "column names in 'select'.")
    x <- subset(x, select = select)
  }

  ## Format numeric fields ----
  x <- .digits(x, digits)

  ## Split the table
  if (is.null(split))
    split <- 1
  chunks <- ceiling(ncol(x) / split)
  chunks.start <- seq(1, ncol(x), chunks)
  chunks.end <- chunks.start + chunks - 1
  chunks.end[length(chunks.end)] <- ncol(x)

  tex.table.list <- vector("list", split)

  for (i in 1:length(tex.table.list)) {

    x.chunk <- x[ ,chunks.start[i]:chunks.end[i]]

    if (ncol(x) == 1) {
      x.chunk <- as.data.frame(x.chunk)
      colnames(x.chunk) <- names(x[i])
    }

    ## Comments ----
    tex.comment.usePackage <- ifelse(comments,
                                     "% add usepackage{adjustbox} to latex preamble \n",
                                     "")

    ## Header ----
    col.names <- tex.table.header <- gsub(pattern = " ",
                                          x = names(x.chunk),
                                          replacement = " \\\\\\\\ ")
    tex.table.header <- paste0("\t",
                               paste("\\multicolumn{1}{p{2cm}}{\\centering",
                                     col.names,
                                     "}",
                                     collapse = " & \n\t"),
                               "\\\\ \n")

    ## Rows ----
    tex.table.rows <- ""
    for (j in 1:nrow(x.chunk)) {
      tex.table.rows <- paste0(tex.table.rows,
                               paste(paste(x.chunk[j, ], collapse = " & "),
                                     "\\\\ \n"))
    }

    ## catch potential latex problems with underscores - after all are numbers, in can be only
    ## on the ID
    tex.table.rows <- gsub("_", "\\_", tex.table.rows, fixed = TRUE)

    ## Tex table ----
    pos.chars <- unlist(strsplit(pos, split = ""))
    if (!all(pos.chars %in% c("l", "c", "r")) ||
        (length(pos.chars) != 1 && length(pos.chars) != ncol(x))) {
      pos <- "c"
    }
    if (nchar(pos) == 1)
      pos <- paste0(rep(pos, ncol(x)), collapse = "")

    if(tabular_only){
      tex.table.begin <- paste0(paste("  \\begin{tabular}{", pos, "}\n"),
                                "     \\hline \n")

      tex.table.end <-  paste0("     \\hline \n",
                               "   \\end{tabular}")

    }else{
      tex.table.begin <- paste0("\\begin{table}[ht] \n",
                                "  \\centering \n",
                                "  \\begin{adjustbox}{max width=\\textwidth} \n",
                                paste("  \\begin{tabular}{", pos, "}\n"),
                                "     \\hline \n")

      tex.table.end <-  paste0("     \\hline \n",
                               "   \\end{tabular} \n",
                               "   \\end{adjustbox} \n",
                               "\\end{table}")
    }

    tex.table <- paste0(tex.comment.usePackage,
                        tex.table.begin,
                        tex.table.header,
                        "\\hline \n",
                        tex.table.rows,
                        tex.table.end)

    if (options$verbose)
      cat(tex.table)

    tex.table.list[[i]] <- tex.table
  }

  invisible(tex.table.list)
}

# This function takes a data.frame, checks each column and tries to
# force the specified amount of digits if numeric or coerceable to numeric
.digits <- function(x, digits) {
  for (i in 1:ncol(x)) {
    if (is.factor(x[ ,i]))
      x[ ,i] <- as.character(x[ ,i])
    test.numeric <- suppressWarnings(as.numeric(x[ ,i]))
    if (!is.na(test.numeric[1]))
      x[ ,i] <- format(round(test.numeric, digits), nsmall = digits, digits = digits)

  }
  return(x)
}
