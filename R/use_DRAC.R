#' Use DRAC to calculate dose rate data.
#'
#' The function provides an interface from R to DRAC. An R-object or a
#' pre-formatted XLS/XLSX file is passed to the DRAC website and the
#' results are re-imported into R.
#'
#'
#' @param file \code{\link{character}}: spreadsheet to be passed
#' to the DRAC website for calculation. Can also be a DRAC template object
#' obtained from \code{template_DRAC()}.
#'
#' @param name \code{\link{character}}: Optional user name submitted to DRAC. If
#' omitted, a random name will be generated
#'
#' @return A results data set from the DRAC website.
#' 
#' @section Function version: 0.1.0
#'
#' @author Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France), Michael Dietze,
#' GFZ Potsdam (Germany)\cr
#'
#' @references
#'
#' Durcan, J.A., King, G.E., Duller, G.A.T., 2015. DRAC: Dose Rate and Age Calculator for trapped charge dating.
#' Quaternary Geochronology 28, 54-61. doi:10.1016/j.quageo.2015.03.012
#'
#' @examples
#'
#' \dontrun{
#' ## re-create DRAC example data set
#' text <- paste0("DRAC-example Quartz Q AdamiecAitken1998 3.4000 0.5100 14.4700 ",
#'                "1.6900 1.2000 0.1400 0.0000 0.0000 N X X X X X X X X X X X X ",
#'                "X X X X X N 90.0000 125.0000 Brennanetal1991 Guerinetal2012-Q ",
#'                "8.0000 10.0000 Bell1979 0.0000 0.0000 5.0000 2.0000 2.2000 ",
#'                "0.2200 1.8000 0.1000 30.0000 70.0000 150.0000 X X 20.0000 ",
#'                "0.2000\n",
#'                "DRAC-example Feldspar F AdamiecAitken1998 2.0000 0.2000 8.0000 ",
#'                "0.4000 1.7500 0.0500 0.0000 0.0000 Y X X X X 12.5000 0.5000 X ",
#'                "X N X X X X X X X X Y 180.0000 212.0000 Bell1980 Mejdahl1979 ",
#'                "0.0000 0.0000 Bell1979 0.1500 0.0500 10.0000 3.0000 0.1500 ",
#'                "0.0150 1.8000 0.1000 60.0000 100.0000 200.0000 X X 15.0000 ",
#'                "1.5000\n",
#'                "DRAC-example Polymineral PM AdamiecAitken1998 4.0000 0.4000 ",
#'                "12.0000 0.1200 0.8300 0.0800 0.0000 0.0000 Y X X X X 12.5000 ",
#'                "0.5000 X X N X X 2.5000 0.1500 X X X X Y 4.0000 11.0000 ",
#'                "Bell1980 Mejdahl1979 0.0000 0.0000 X 0.0860 0.0038 10.0000 ",
#'                "5.0000 X X X X X X X 0.2000 0.1000 204.4700 2.6900")
#'                
#' ## send example data set let DRAC do the calculations
#' Reply <- use_DRAC(file = text)
#'
#' # send the actual IO template spreadsheet to DRAC 
#' file <-  "~/DRAC_Input_and_Output_Template-2.xlsx"
#' use_DRAC(file = file)
#' }
#'
use_DRAC <- function(
  file,
  name
){
  
  ##When providing a *.xls or *.xlsx input file please make sure to maintain
  ## TODO
  ##
  ## (1)
  ## THE DATA SET AS UNMODIFIED AS POSSIBLE. THIS IS A CLEAR CASE FOR ERROR
  ## HANDLING OF THE MOST STUPID USER. MAYBE CHECK STRUCTURE OF THE ORDER OF
  ## PARAMETERS FOR MEANINGFUL COMBINATION. WRITE DISCLAIMER THAT THE FUNCTION
  ## HAS BEEN CHECKED WITH DRAC EXAMPLE DATA SET.
  ##
  ## (2)
  ## Leave it to the user where the calculations made in our package should be used
  ##
  ## (3)
  ## DO NOT forget to mention that everything was calculated in DRAC
  ##
  ##  (a) reference
  ##  (b) legal statement ... why? The DRAC website may prohibit a usage like suggested
  ##
  ##  (4)
  ##  @CB: Add your name in the authors list wherever you feel it should belong to
  ##
  ##  (5)
  ##  Example is necessary, however, the example must not pass data to DRAC!
  ##
  ##  (6)
  ##  If everything is ready we should anyway keed the dependency for XLS import to
  ##  keep the function as easy as possible.
  
  # Integrity tests -----------------------------------------------------------------------------
  if (inherits(file, "character")) {
    assertive::is_existing_file(file)
    
    # Import data ---------------------------------------------------------------------------------
    
    ## Import and skipt the first rows and remove NA lines and the 2 row, as this row contains
    ## only meta data
    
    ##check if is the original DRAC table
    if (readxl::excel_sheets(file)[1] != "DRAC_1.1_input") {
      stop("[use_DRAC()] It looks like that you are not using the original DRAC XLSX template. This is currently
         not supported!")
    }
    input.raw <- na.omit(as.data.frame(readxl::read_excel(path = file, sheet = 1, skip = 5)))[-1, ]
    
  } else if (inherits(file, "DRAC.list")) {
    input.raw <- as.data.frame(file)
    
  } else if (inherits(file, "DRAC.data.frame")) {
    input.raw <- file
    
  } else {
    stop("The provided data object is not a valid DRAC template.", call. = FALSE)
  }
  
  
  # Set helper function -------------------------------------------------------------------------
  ## The real data are transferred without any encryption, so we have to mask the original
  
  ##(0) set masking function
  .masking <- function(mean, sd, n) {
    temp <- rnorm(n = 30 * n, mean = mean,sd = sd)
    temp.result <-
      sapply(seq(1, length(temp), by = 30), function(x) {
        c(round(mean(temp[x:(x + 29)], digits = 2)),
          round(sd(temp[x:(x + 29)]), digits = 2))
      })
    return(t(temp.result))
  }
  
  
  # Process data --------------------------------------------------------------------------------
  
  ##(1) expand the rows in the data.frame a little bit
  mask.df <-  input.raw[rep(1:nrow(input.raw), each = 3), ]
  
  ##(2) generate some meaningful randome variables
  mask.df <- lapply(seq(1, nrow(input.raw), by = 3), function(x) {
    
    ##replace some values - the De value
    mask.df[x:(x + 2), c("TI:52","TI:53")] <- .masking(
      mean = as.numeric(mask.df[x,"TI:52"]),
      sd = as.numeric(mask.df[x,"TI:53"]),
      n = 3)
    return(mask.df)
  })
  
  ##(3) bin values
  DRAC_submission.df <- rbind(input.raw,mask.df[[1]])
  
  ##(4) replace ID values
  DRAC_submission.df$`TI:1` <- paste0(paste0(paste0(sample(if(runif(1,-10,10)>0){LETTERS}else{letters}, 
                                                           runif(1, 2, 4)), collapse = ""), 
                                             ifelse(runif(1,-10,10)>0, "-", "")), 
                                      seq(sample(1:50, 1, prob = 50:1/50, replace = FALSE), 
                                          by = 1, length.out = nrow(DRAC_submission.df)))
  
  ##(5) store the real IDs in a sperate object
  DRAC_results.id <-  DRAC_submission.df[1:nrow(input.raw), "TI:1"]
  
  ##(6) create DRAC submission string
  DRAC_submission.df <- DRAC_submission.df[sample(x = 1:nrow(DRAC_submission.df), nrow(DRAC_submission.df),
                                                  replace = FALSE), ]
  
  ##convert all columns of the data.frame to class 'character'
  for (i in 1:ncol(DRAC_submission.df)) 
    DRAC_submission.df[ ,i] <- as.character(DRAC_submission.df[, i])
  
  ##get line by line and remove unwanted characters
  DRAC_submission.string <- sapply(1:nrow(DRAC_submission.df), function(x) {
    paste0(gsub(",", "", toString(DRAC_submission.df[x, ])), "\n")
  })
  
  ##paste everything together to get the format we want
  DRAC_input <- paste(DRAC_submission.string, collapse = "")
  
  
  # Send data to DRAC ---------------------------------------------------------------------------
  
  ## send data set to DRAC website and receive repsonse
  # url <- paste0("https://www.aber.ac.uk/en/iges/research-groups/quaternary/luminescence-",
  #               "research-laboratory/dose-rate-calculator/?show=calculator")
  url <- "http://zerk.canopus.uberspace.de/drac/?show=calculator"
    
  DRAC.response <- httr::POST(url,
                              body = list("drac_data[name]"  = paste(sample(if(runif(1,-10,10)>0){LETTERS}else{letters}, 
                                                                            runif(1, 2, 4)), collapse = ""),
                                          "drac_data[table]" = DRAC_input))
  
  ## check for correct response
  if (DRAC.response$status_code != 200) {
    stop(paste0("[use_DRAC()] transmission failed with HTTP status code: ",
                DRAC.response$status_code))
  }
  
  ## assign DRAC response data to variables
  http.header <- DRAC.response$header
  DRAC.content <- httr::content(x = DRAC.response, as = "text")
  
  ## if the input was valid from a technical standpoint, but not with regard
  ## contents, we indeed get a valid response, but no DRAC output
  if (!grepl("DRAC Outputs", DRAC.content)) {
    stop(message(paste("\n\t We got a response from the server, but it\n",
                       "\t did not contain DRAC output. Please check\n",
                       "\t your data and verify its validity.\n")),
         call. = FALSE)
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
  DRAC.content <- DRAC.raw[-c(1, 2), ]
  
  ##Get rid of all the value we do not need anymore
  DRAC.content <-  subset(DRAC.content, DRAC.content$V1 %in% DRAC_results.id)
  
  ##replace by original names
  DRAC.content[ ,1] <- input.raw[ ,1]
  
  ## assign column names
  colnames(DRAC.content) <- DRAC.raw[1, ]
  
  ## assign labels
  DRAC.labels <- DRAC.raw[2, ]
  
  ## DRAC also returns the input, so we need to split input and output
  DRAC.content.input <- DRAC.content[ ,grep("TI:", names(DRAC.content))]
  DRAC.content.output <- DRAC.content[ ,grep("TO:", names(DRAC.content))]
  
  ## The DRAC ouput also contains a hightlight table, which results in 
  ## duplicate columns. When creating the data.frame duplicate columns 
  ## are automatically appended '.1' in their names, so we can identify 
  ## and remove them easily
  DRAC.content.input <- DRAC.content.input[ ,-grep("\\.1", names(DRAC.content.input))]
  DRAC.content.output <- DRAC.content.output[ ,-grep("\\.1", names(DRAC.content.output))]
  
  ## return output
  invisible(list(DRAC.header = DRAC.header,
                 DRAC.labels = DRAC.labels,
                 DRAC.content = DRAC.content,
                 DRAC.input = DRAC.content.input,
                 DRAC.output = DRAC.content.output))
}
