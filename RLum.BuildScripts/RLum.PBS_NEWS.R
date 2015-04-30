### ===============================================================================================
### R package Luminescence BUILDSCRIPTS
### NEWS
### sebastian.kreutzer@u-bordeaux-montaigne.fr
### 2015-04-29
### ===============================================================================================

require(tools)

NEWS.txt <- capture.output(tools:::Rd2txt("inst/NEWS.Rd", outputEncoding = "UTF-8"))
NEWS.txt <- gsub( "_\b","", NEWS.txt) #remove unwanted characters
write(NEWS.txt,"NEWS")
