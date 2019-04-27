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

##update date
temp[version_date] <- paste0("Date: ", Sys.Date())

##update version number only if it does not contain .9000
##(the unwritten R package convention proposed by H.W.)
if(grepl(x = temp[version_id], pattern = ".9000", fixed = TRUE)){
version_number <-
  unlist(strsplit(x = temp[version_id], split = ".9000-", fixed = TRUE))

if(length(version_number) == 1){
  version_number <- paste0(version_number, "-1")

}else{
  version_number <- paste0(version_number[1],".9000-",as.numeric(version_number[2]) + 1)

}

temp[version_id] <- version_number

}

##write back to file
writeLines(text = temp, con = "DESCRIPTION")
