printCode <- function(input, output, session, n_input, fun, args) {
  
  # prepare code as text output
  str1 <- "data <- data.table::fread(file, data.table = FALSE)"
  
  if (n_input == 2) {
    str2 <- "file2 <- file.choose()"
    str3 <- "data2 <- data.table::fread(file2, data.table = FALSE)"
    str4 <- "data <- list(data, data2)"
    str1 <- paste(str1, str2, str3, str4, sep = "\n")
  }
  
  header <- paste("# To reproduce the plot in your local R environment",
                  "# copy and run the following code to your R console.",
                  "library(Luminescence)",
                  "file <- file.choose()",
                  str1,
                  "\n",
                  sep = "\n")
  
  names <- names(args)
  
  if (is.null(names))
    names <- rep(NA, length(args))
  
  names[which(names == "")] <- NA
  
  verb.arg <- paste(mapply(function(name, arg) {
    if (all(inherits(arg, "character")))
      arg <- paste0("'", arg, "'")
    if (length(arg) > 1)
      arg <- paste0("c(", paste(arg, collapse = ", "), ")")
    if (is.null(arg))
      arg <- "NULL"
    if (!is.na(name))
      paste(name, "=", arg)
    else
      arg
  }, names[-1], args[-1]), collapse = ",\n")
  
  funCall <- paste0(fun, "\n", verb.arg, ")")
  
  code.output <- paste0(header, funCall, collapse = "\n")
  
  return(code.output)
}