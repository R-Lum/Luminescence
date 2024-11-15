#' Use DRAC to calculate dose rate data
#'
#' The function provides an interface from R to DRAC. An R-object or a
#' pre-formatted XLS/XLSX file is passed to the DRAC website and the
#' results are re-imported into R.
#'
#'
#' @param file [character] (**required**):
#' spreadsheet to be passed to the DRAC website for calculation. Can also be a
#' DRAC template object obtained from `template_DRAC()`.
#'
#' @param name [character] (*with default*):
#' Optional user name submitted to DRAC. If omitted, a random name will be generated
#'
#' @param print_references (*with default*):
#' Print all references used in the input data table to the console.
#'
#' @param citation_style (*with default*):
#' If `print_references = TRUE` this argument determines the output style of the
#' used references. Valid options are `"Bibtex"`, `"citation"`, `"html"`, `"latex"`
#' or `"R"`. Default is `"text"`.
#'
#' @param ... Further arguments.
#'
#' - `url` [character]: provide an alternative URL to DRAC
#' - `verbose` [logical]: show or hide console output
#'
#' @return Returns an [RLum.Results-class] object containing the following elements:
#'
#' \item{DRAC}{[list]: a named list containing the following elements in slot `@@data`:
#'
#' \tabular{lll}{
#'    `$highlights` \tab [data.frame] \tab summary of 25 most important input/output fields \cr
#'    `$header` \tab [character] \tab HTTP header from the DRAC server response \cr
#'    `$labels` \tab [data.frame] \tab descriptive headers of all input/output fields \cr
#'    `$content` \tab [data.frame] \tab complete DRAC input/output table \cr
#'    `$input` \tab [data.frame] \tab DRAC input table \cr
#'    `$output` \tab [data.frame] \tab DRAC output table \cr
#'    `references`\tab [list] \tab A list of bib entries of used references \cr
#' }
#'
#' }
#' \item{data}{[character] or [list] path to the input spreadsheet or a DRAC template}
#' \item{call}{[call] the function call}
#' \item{args}{[list] used arguments}
#'
#' The output should be accessed using the function [get_RLum].
#'
#' @section Function version: 0.14
#'
#' @author
#' Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)\cr
#' Michael Dietze, GFZ Potsdam (Germany)\cr
#' Christoph Burow, University of Cologne (Germany)
#'
#' @references
#' Durcan, J.A., King, G.E., Duller, G.A.T., 2015. DRAC: Dose Rate and Age Calculator for trapped charge dating.
#' Quaternary Geochronology 28, 54-61. doi:10.1016/j.quageo.2015.03.012
#'
#' @examples
#'
#' ## (1) Method using the DRAC spreadsheet
#'
#' file <-  "/PATH/TO/DRAC_Input_Template.csv"
#'
#' # send the actual IO template spreadsheet to DRAC
#' \dontrun{
#' use_DRAC(file = file)
#' }
#'
#'
#'
#' ## (2) Method using an R template object
#'
#' # Create a template
#' input <- template_DRAC(preset = "DRAC-example_quartz")
#'
#' # Fill the template with values
#' input$`Project ID` <- "DRAC-Example"
#' input$`Sample ID` <- "Quartz"
#' input$`Conversion factors` <- "AdamiecAitken1998"
#' input$`External U (ppm)` <- 3.4
#' input$`errExternal U (ppm)` <- 0.51
#' input$`External Th (ppm)` <- 14.47
#' input$`errExternal Th (ppm)` <- 1.69
#' input$`External K (%)` <- 1.2
#' input$`errExternal K (%)` <- 0.14
#' input$`Calculate external Rb from K conc?` <- "N"
#' input$`Calculate internal Rb from K conc?` <- "N"
#' input$`Scale gammadoserate at shallow depths?` <- "N"
#' input$`Grain size min (microns)` <- 90
#' input$`Grain size max (microns)` <- 125
#' input$`Water content ((wet weight - dry weight)/dry weight) %` <- 5
#' input$`errWater content %` <- 2
#' input$`Depth (m)` <- 2.2
#' input$`errDepth (m)` <- 0.22
#' input$`Overburden density (g cm-3)` <- 1.8
#' input$`errOverburden density (g cm-3)` <- 0.1
#' input$`Latitude (decimal degrees)` <- 30.0000
#' input$`Longitude (decimal degrees)` <- 70.0000
#' input$`Altitude (m)` <- 150
#' input$`De (Gy)` <- 20
#' input$`errDe (Gy)` <- 0.2
#'
#' # use DRAC
#' \dontrun{
#' output <- use_DRAC(input)
#' }
#'
#' @md
#' @export
use_DRAC <- function(
  file,
  name,
  print_references = TRUE,
  citation_style = "text",
  ...
) {
  .set_function_name("use_DRAC")
  on.exit(.unset_function_name(), add = TRUE)

  ## TODO:
  ## (1) Keep the data set as unmodified as possible. Check structure and order of parameters
  ## for meaningful combination.
  ##
  ## (2)
  ## Leave it to the user where the calculations made in our package should be used

  # Integrity tests -----------------------------------------------------------------------------
  if (inherits(file, "character")) {
    if(!file.exists(file)){
      stop("[use_DRAC()] It seems that the file doesn't exist!", call. = FALSE)

    }

    # Import data ---------------------------------------------------------------------------------

    ## Import and skip the first rows and remove NA lines and the 2 row, as this row contains
    ## only meta data

    ## DRAC v1.1 - XLS sheet
    ##check if is the original DRAC table
    if (tools::file_ext(file) == "xls" || tools::file_ext(file) == "xlsx") {
      if (readxl::excel_sheets(file)[1] != "DRAC_1.1_input")
        stop("[use_DRAC()] It looks like that you are not using the original DRAC v1.1 XLSX template. This is currently not supported!", call. = FALSE)

      warning("\n[use_DRAC()] The current DRAC version is 1.2, but you provided the v1.1 excel input template.",
              "\nPlease transfer your data to the new CSV template introduced with DRAC v1.2.", call. = FALSE)
      input.raw <- na.omit(as.data.frame(readxl::read_excel(path = file, sheet = 1, skip = 5)))[-1, ]
    }

    ## DRAC v1.2 - CSV sheet
    if (tools::file_ext(file) == "csv") {
      if (read.csv(file, nrows = 1, header = FALSE)[1] != "DRAC v.1.2 Inputs")
        stop("[use_DRAC()] It looks like that you are not using the original DRAC v1.2 CSV template. This is currently not supported!", call. = FALSE)

      input.raw <- read.csv(file, skip = 8, check.names = FALSE, header = TRUE, stringsAsFactors = FALSE)[-1, ]
    }

  } else if (inherits(file, "DRAC.list")) {
    input.raw <- as.data.frame(file)

  } else if (inherits(file, "DRAC.data.frame")) {
    input.raw <- file
  } else {
    .throw_error("The provided data object is not a valid DRAC template.")
  }

  if (nrow(input.raw) > 5000)
    .throw_error("The limit of allowed datasets is 5000!")

  # Settings ------------------------------------------------------------------------------------
  settings <- list(
    name = ifelse(missing(name),
                  paste0(sample(if(runif(1,-10,10)>0){LETTERS}else{letters}, runif(1, 2, 4))), name),
   verbose = TRUE,
   url = "https://www.aber.ac.uk/en/dges/research/quaternary/luminescence-research-laboratory/dose-rate-calculator/?show=calculator")

  # override defaults with args in ...
  settings <- modifyList(settings, list(...))

  # Set helper function -------------------------------------------------------------------------
  ## The real data are transferred without any encryption, so we have to mask the original

  ##(0) set masking function
  .masking <- function(mean, sd, n) {
    temp <- rnorm(n = 30 * n, mean = mean, sd = sd)
    t(vapply(seq(1, length(temp), 30), function(x) {
        c(format(mean(temp[x:(x + 29)]), digits = 2),
          format(sd(temp[x:(x + 29)]), digits = 2))
      }, character(2)))
  }


  # Process data --------------------------------------------------------------------------------
  if (settings$verbose) message("\n\t Preparing data...")
  ##(1) expand the rows in the data.frame a little bit
  mask.df <-  input.raw[rep(1:nrow(input.raw), each = 3), ]

  ##(2) generate some meaningful random variables
  mask.df <- lapply(seq(1, nrow(input.raw), 3), function(x) {
    if (mask.df[x,"TI:52"] != "X") {
      ##replace some values - the De value
      mask.df[x:(x + 2), c("TI:52","TI:53")] <- .masking(
        mean = as.numeric(mask.df[x,"TI:52"]),
        sd = as.numeric(mask.df[x,"TI:53"]),
        n = 3)
      return(mask.df)
    }

  })

  ##(3) bin values
  DRAC_submission.df <- rbind(input.raw, mask.df[[1]])

  ##(4) replace ID values
  DRAC_submission.df$`TI:1` <- paste0(
    paste0(
      paste0(
        sample(if(runif(1,-10,10)>0) LETTERS else letters, runif(1, 2, 4))),
      ifelse(runif(1,-10,10)>0, "-", "")),
    gsub(" ", "0", prettyNum(seq(sample(1:50, 1, prob = 50:1/50, replace = FALSE),
      by = 1, length.out = nrow(DRAC_submission.df)), width = 2)))


  ##(5) store the real IDs in a separate object
  DRAC_results.id <-  DRAC_submission.df[1:nrow(input.raw), "TI:1"]

  ##(6) create DRAC submission string
  DRAC_submission.df <- DRAC_submission.df[sample(x = 1:nrow(DRAC_submission.df), nrow(DRAC_submission.df),
                                                  replace = FALSE), ]

  ##convert all columns of the data.frame to class 'character'
  for (i in 1:ncol(DRAC_submission.df))
    DRAC_submission.df[ ,i] <- as.character(DRAC_submission.df[, i])

  if (settings$verbose) message("\t Creating submission string...")
  ##get line by line and remove unwanted characters
  DRAC_submission.string <- sapply(1:nrow(DRAC_submission.df), function(x) {
    paste0(gsub(",", "", toString(DRAC_submission.df[x, ])), "\n")
  })

  ##paste everything together to get the format we want
  DRAC_input <- paste(DRAC_submission.string, collapse = "")

  # Send data to DRAC ---------------------------------------------------------------------------
  if (settings$verbose) message(paste("\t Establishing connection to", settings$url))

  ## send data set to DRAC website and receive response
  DRAC.response <- httr::POST(settings$url,
                              body = list("drac_data[name]"  = settings$name,
                                          "drac_data[table]" = DRAC_input))
  ## check for correct response
  if (DRAC.response$status_code != 200) {
    stop(paste0("[use_DRAC()] transmission failed with HTTP status code: ",
                DRAC.response$status_code))
  } else {
    if (settings$verbose) message("\t The request was successful, processing the reply...")
  }

  ## assign DRAC response data to variables
  http.header <- DRAC.response$header
  DRAC.content <- httr::content(x = DRAC.response, as = "text")

  ## if the input was valid from a technical standpoint, but not with regard
  ## contents, we indeed get a valid response, but no DRAC output
  if (!grepl("DRAC Outputs", DRAC.content)) {
    error_start <- max(gregexpr("drac_field_error", DRAC.content)[[1]])
    error_end <- regexec('textarea name=', DRAC.content)[[1]]
    error_msg <- substr(DRAC.content, error_start, error_end)

    # nocov start
    on.exit({
      reply <- readline("Do you want to see the DRAC error message (Y/N)?")
      if (reply == "Y" || reply == "y" || reply == 1)
        cat(error_msg)
    }, add = TRUE)
    # nocov end

    .throw_error("\n\t We got a response from the server, but it\n",
                 "\t did not contain DRAC output. Please check\n",
                 "\t your data and verify its validity.\n")
  } else {
    if (settings$verbose) message("\t Finalising the results...")
  }

  ## split header and content
  DRAC.content.split <- strsplit(x = DRAC.content,
                                 split = "DRAC Outputs\n\n")

  ## assign DRAC header part
  DRAC.header <- as.character(DRAC.content.split[[1]][1])

  ## assign DRAC content part
  DRAC.raw <- read.table(text = as.character(DRAC.content.split[[1]][2]),
                         sep = ",",
                         stringsAsFactors = FALSE)

  ## remove first two lines
  DRAC.content <- data.table::fread(as.character(DRAC.content.split[[1]][2]),
                                    sep = ",", skip = 2,
                                    stringsAsFactors = FALSE, colClasses = c(V3 = "character"),
                                    data.table = FALSE)

  ##Get rid of all the value we do not need anymore
  DRAC.content <-  subset(DRAC.content, DRAC.content$V1 %in% DRAC_results.id)
  DRAC.content <- DRAC.content[with(DRAC.content, order(V1)), ]

  ##replace by original names
  DRAC.content[ ,1] <- input.raw[ ,1]

  ## assign column names
  colnames(DRAC.content) <- DRAC.raw[1, ]

  ## save column labels and use them as attributes for the I/O table columns
  DRAC.labels <- DRAC.raw[2, ]
  for (i in 1:length(DRAC.content)) {
    attr(DRAC.content[ ,i], "description") <- DRAC.labels[1,i]
  }

  ## DRAC also returns the input, so we need to split input and output
  DRAC.content.input <- DRAC.content[ ,grep("TI:", names(DRAC.content))]
  DRAC.content.output <- DRAC.content[ ,grep("TO:", names(DRAC.content))]

  ## The DRAC ouput also contains a highlight table, which results in
  ## duplicate columns. When creating the data.frame duplicate columns
  ## are automatically appended '.1' in their names, so we can identify
  ## and remove them easily
  DRAC.content.input <- DRAC.content.input[ ,-grep("\\.1", names(DRAC.content.input))]
  DRAC.content.output <- DRAC.content.output[ ,-grep("\\.1", names(DRAC.content.output))]

  ## for some reason the returned input table is unsorted, so we resort it in increasing order
  DRAC.content.input <- DRAC.content.input[ , paste0("TI:", 1:ncol(DRAC.content.input))]

  ## The output table (v1.2) has 198 columns, making it unreasonable complex
  ## for standard data evaluation. We reproduce the DRAC highlight table
  ## and use the descriptions (saved as attributes) as column names.
  highlight.keys <- c("TI:1","TI:2","TI:3","TO:FQ","TO:FR",
                      "TO:FS", "TO:FT", "TO:FU", "TO:FV", "TO:FW",
                      "TO:FX", "TO:FY", "TO:FZ", "TO:GG", "TO:GH",
                      "TO:GI", "TO:GJ", "TO:GK", "TO:GL", "TO:GM",
                      "TO:GN", "TI:52", "TI:53", "TO:GO", "TO:GP")
  DRAC.highlights <- subset(DRAC.content, select = highlight.keys)
  DRAC.highlights.labels <- as.character(DRAC.labels[1, which(unique(names(DRAC.content)) %in% highlight.keys)])
  colnames(DRAC.highlights) <- DRAC.highlights.labels
  for (i in 1:length(DRAC.highlights)) {
    attr(DRAC.highlights[ ,i], "key") <- highlight.keys[i]
  }

  ## finally, we add the 'DRAC.highlights' class so that we can use a custom print method
  class(DRAC.highlights) <- c("DRAC.highlights", "data.frame")

  ## Final Disclaimer
  messages <- list("\t Done! \n",
                   "\t We, the authors of the R package 'Luminescence', do not take any responsibility and we are not liable for any ",
                   "\t mistakes or unforeseen misbehaviour. All calculations are done by DRAC and it is outside our reference to",
                   "\t verify the input and output. \n",
                   "\t Note that this function is only compatible with DRAC version 1.2. Before using this function make sure that",
                   "\t this is the correct version, otherwise expect unspecified errors.\n",
                   "\t Please ensure you cite the use of DRAC in your work, published or otherwise. Please cite the website name and",
                   "\t version (e.g. DRAC v1.2) and the accompanying journal article:",
                   "\t Durcan, J.A., King, G.E., Duller, G.A.T., 2015. DRAC: Dose rate and age calculation for trapped charge",
                   "\t dating. Quaternary Geochronology 28, 54-61. \n",
                   "\t Use 'verbose = FALSE' to hide this message. \n")

  if (settings$verbose) lapply(messages, message)

  ## Get and print used references
  references <- get_DRAC_references(DRAC.content.input)

  if (print_references && settings$verbose) {
    for (i in 1:length(references$refs)) {
      message("\nReference for: ", references$desc[i])
      print(references$refs[[i]], style = citation_style)
    }
  }


  ## return output
  DRAC.return <- set_RLum(
    "RLum.Results",
    data = list(
    DRAC = list(highlights = DRAC.highlights,
          header = DRAC.header,
          labels = DRAC.labels,
          content = DRAC.content,
          input = DRAC.content.input,
          output = DRAC.content.output,
          references = references),
        data = file,
        call = sys.call(),
        args = as.list(sys.call()[-1])))

  invisible(DRAC.return)
}
