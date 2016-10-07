### ===============================================================================================
### R package Luminescence BUILDSCRIPTS
### Citation
### christoph.burow@uni-koeln.de and sebastian.kreutzer@u-bordeaux-montaigne.fr
### 2016-09-26
### ===============================================================================================

require(stringi)

# clean workspace
rm(list = ls())

## -------------------------------------------------------------------------- ##
## FIND AUTHORS ----
## -------------------------------------------------------------------------- ##
DESC <- readLines("DESCRIPTION")

authors <- DESC[grep("author", DESC, ignore.case = TRUE)[1]:
                c(grep("author", DESC, ignore.case = TRUE)[2] - 1)]

author.list <- do.call(rbind, lapply(authors, function(str) {

  # check if person is author
  is.auth <- grepl("aut", str)

  # remove "Author: "
  str <- stringi::stri_replace_all_coll(str, pattern = "Author: ", replacement = "")
  # remove all role contributions given in square brackets
  str <- strtrim(str, min(unlist(gregexpr("\\[|<", str))) - 2)
  # remove all leading whitespaces
  str <- stringi::stri_trim(str, "left")

  # get surname
  strsplit <- strsplit(str, " ")[[1]]
  surname <- strsplit[length(strsplit)]

  # get name
  name <- character()
  for (i in 1:c(length(strsplit)-1))
    name <- paste0(name, strtrim(strsplit[i], 1), ".")

  # bind as data.frame and return
  df <- data.frame(name = name, surname = surname, author = is.auth)
  return(df)
}))


## -------------------------------------------------------------------------- ##
## ADD CITATION ----
## -------------------------------------------------------------------------- ##

##add citation section
file.list.man <- list.files("man/")

# build package citation
pkg.authors <- character()
author.list.authorsOnly <- author.list[which(author.list$author),]
for (i in 1:nrow(author.list.authorsOnly)) {
  if (author.list.authorsOnly$author[i])
    pkg.authors <- paste0(pkg.authors,
                          author.list.authorsOnly$surname[i],", ",
                          author.list.authorsOnly$name[i],
                          ifelse(i == nrow(author.list.authorsOnly),"", ", "))
}
pkg.citation <- paste0(pkg.authors, " (", format(Sys.time(), "%Y"), "). ",
                       "Luminescence: Comprehensive Luminescence Dating Data Analysis. ",
                       "R package version ", packageVersion("Luminescence"), ". ",
                       "https://CRAN.R-project.org/package=Luminescence")


for (i in 1:length(file.list.man)) {
  temp.file.man <-  readLines(paste0("man/", file.list.man[i]))

  # determine function and title
  fun <-
    temp.file.man[grep("\\\\name", temp.file.man, ignore.case = TRUE)]
  fun <- stri_replace_all_regex(fun, "\\\\name|\\{|\\}", "")

  title.start <-
    grep("\\\\title", temp.file.man, ignore.case = TRUE)
  title.end <- grep("\\\\usage", temp.file.man, ignore.case = TRUE)

  if (length(title.end) != 0) {
    title <-
      paste(temp.file.man[title.start:c(title.end - 1)], collapse = " ")
    title <- stri_replace_all_regex(title, "\\\\title|\\{|\\}", "")
    title <-
      stri_replace_all_regex(title, "\\\\code", "", ignore.case  = TRUE)
    title <-
      stri_replace_all_regex(title, '"', "'", ignore.case  = TRUE)

    ##search for start and end author field
    author.start <- which(grepl("\\\\author", temp.file.man))

    ##search for Reference start field
    reference.start <- which(grepl("\\\\references", temp.file.man))

    if (length(author.start) > 0) {
      author.end <- which(grepl("\\}", temp.file.man)) - author.start
      author.end <- min(author.end[author.end > 0]) + author.start

      ##account for missing reference section
      if(length(reference.start) == 0){
        reference.start <- author.end + 1
      }

      
      relevant.authors <- do.call(rbind, sapply(author.list$surname, function(x) {
        str <- paste(temp.file.man[author.start:author.end], collapse = " ")
        str <- stri_replace_all_regex(str, ",|\\.", " ")
        included <- grepl(paste0(x, " "), str, ignore.case = TRUE)
        if (included)
          pos <- regexpr(x, str)[[1]]
        else 
          pos <- NA
        return(data.frame(included = included, position = pos))
      }, simplify = FALSE))
      
      # retain order of occurence, assuming that the name first mentioned is
      # also the main author of the function
      included.authors <- author.list[relevant.authors$included, ]
      included.authors <- included.authors[order(na.omit(relevant.authors$position)), ]

      fun.authors <- character()
      for (j in 1:nrow(included.authors)) {
        fun.authors <- paste0(
          fun.authors,
          included.authors$surname[j],
          ", ",
          included.authors$name[j],
          ifelse(j == nrow(included.authors), "", ", ")
        )
      }
      
      ##search for function version
      fun.version <-
        which(grepl("\\\\section\\{Function version\\}", temp.file.man))

      if (length(fun.version) != 0) {
        fun.version <- stringr::str_trim(strsplit(
          x = temp.file.man[fun.version + 1],
          split = "(",
          fixed = TRUE
        )[[1]][1])

      } else{
        fun.version <- ""
      }


      citation.text <- paste0(
        "\n\n\\section{How to cite}{\n",
        fun.authors,
        " (",
        format(Sys.time(), "%Y"),
        "). ",
        fun,
        "(): ",
        title,
        ifelse(fun.version != "", ". Function version ", ""),
        fun.version,
        ". In: ",
        pkg.citation,
        "\n}\n"
      )

      temp.file.man[reference.start - 1] <-
        paste(temp.file.man[reference.start - 1],
              citation.text)

      ##write file back to the disc
      if (length(author.start) > 0) {
        write(temp.file.man, paste0("man/", file.list.man[i]))
      }
    }
  }
}
