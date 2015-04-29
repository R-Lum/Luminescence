### ===============================================================================================
### R package Luminescence BUILDSCRIPTS
### AddRLumTeam
### sebastian.kreutzer@u-bordeaux-montaigne.fr
### 2015-04-29
### ===============================================================================================

# Reading file ------------------------------------------------------------

file.list.man <- list.files("man/")
file.list.R <- list.files("R/")

# Adding additional information ---------------------------------------------------------------


##Adding change time time stamp and the R Luminescence team to as package author
for (i in 1:length(file.list.man)) {
  temp.file.man <-  readLines(paste0("man/",file.list.man[i]))

  ##seach for start and end author field
  author.start <- which(grepl("\\\\author",temp.file.man))
  version.start <-
    which(grepl("\\\\section\\{Function version\\}",temp.file.man))

  ##add luminescence team as author
  ##to avoid that the field was not found
  if (length(author.start) > 0) {
    author.end <- which(grepl("\\}",temp.file.man)) - author.start
    author.end <- min(author.end[author.end > 0]) + author.start

    ##replace this entry
    temp.file.man[author.end] <-
      "\\cr R Luminescence Package Team}"


  }

  ##add version time stamp
  if (length(version.start) > 0) {
    ##check if a correspoding R files exists
    if (any(grepl(paste0(
      strsplit(file.list.man[i], split = ".Rd")[[1]], ".R"
    ), file.list.R))) {
      ##select the corresponding R file (the limitation to the 1st element to avoid problems with
      ## the RLum.Results and the RLum.R file)
      temp.id <-
        which(grepl(paste0(
          strsplit(file.list.man[i], split = ".Rd")[[1]], ".R"
        ), file.list.R))[1]

      ##select mtime
      temp.mtime <-
        file.info(paste0("R/",file.list.R[temp.id]))$mtime

      ##update information
      temp.file.man[version.start + 1] <-
        paste0(temp.file.man[version.start + 1], " (",  temp.mtime, ")")

    }

  }

  ##write file back to the disc
  if (length(version.start) > 0 & length(author.start) > 0) {
    write(temp.file.man,paste0("man/",file.list.man[i]))
  }

}
