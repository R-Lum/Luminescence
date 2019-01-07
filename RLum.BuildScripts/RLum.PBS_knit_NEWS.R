### ===============================================================================================
### R package Luminescence BUILDSCRIPTS
### knit NEWS and README
### sebastian.kreutzer@u-bordeaux-montaigne.fr
### 2019-01-02
### ===============================================================================================

# IMPORTANT NOTE ------------------------------------------------------------------------------
# On WINDOWS machines make sure to include pandoc in the PATH environment. 
# Pandoc is usually not (pre-)installed, which is why RStudio uses its bundled
# version found in 'C:\Program Files\RStudio\bin\pandoc'. Running this build 
# script from CMD will not find the pandoc.exe unless it's been added to the
# search path.

# Knit NEWS -----------------------------------------------------------------------------------
rmarkdown::render("NEWS.Rmd", output_format = "github_document")

if(file.exists("NEWS.html"))
  file.remove("NEWS.html")
