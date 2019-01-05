### ===============================================================================================
### R package Luminescence BUILDSCRIPTS
### Date and version number
### sebastian.kreutzer@u-bordeaux-montaigne.fr
### 2019-01-02
### ===============================================================================================

# Set new date and version number -----------------------------------------------------------------
##read DESCRIPTION
temp <- readLines("DESCRIPTION")

##grep date and version number
version_date <- grep(pattern = "Date:", x = temp, fixed = TRUE)
version_id <- grep(pattern = "Version:", x = temp, fixed = TRUE)

##update date and version number
temp[version_date] <- paste0("Date: ", Sys.Date())

version_number <-
  unlist(strsplit(x = temp[version_id], split = ".", fixed = TRUE))
version_number <- paste(
  c(version_number[-length(version_number)],
    as.numeric(version_number[length(version_number)]) + 1),
  collapse = ".")

temp[version_id] <- version_number

##write back to file
writeLines(text = temp, con = "DESCRIPTION")
