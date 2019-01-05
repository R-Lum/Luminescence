### ===============================================================================================
### R package Luminescence BUILDSCRIPTS
### knit NEWS and README
### sebastian.kreutzer@u-bordeaux-montaigne.fr
### 2019-01-02
### ===============================================================================================

# Knit NEWS -----------------------------------------------------------------------------------
rmarkdown::render("NEWS.Rmd", output_format = "github_document")

if(file.exists("NEWS.html"))
  file.remove("NEWS.html")
