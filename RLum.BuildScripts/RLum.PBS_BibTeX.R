### ===============================================================================================
### R package Luminescence BUILDSCRIPTS
### BibTeX
### sebastian.kreutzer@u-bordeaux-montaigne.fr
### 2015-04-29
### ===============================================================================================

require(tools)
require(Luminescence)

##get version number
temp <- readLines("DESCRIPTION")
temp <- temp[grep("Version", temp)]
temp.version <- sub(" ","",unlist(strsplit(temp,":"))[2])

package.citation <- toBibtex(citation("Luminescence"))
write(package.citation, file=paste0("RLum.BuildResults/Luminescence_", temp.version,"-bibliography.bib"))
