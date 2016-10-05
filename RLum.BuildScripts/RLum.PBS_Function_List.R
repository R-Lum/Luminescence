### ===============================================================================================
### R package Luminescence BUILDSCRIPTS
### Function_List
### sebastian.kreutzer@u-bordeaux-montaigne.fr
### 2015-04-29
### ===============================================================================================
if(!require("R2HTML"))
  install.packages("R2HTML")

if(!require("xtable"))
  install.packages("xtable")

library(tools)
library(R2HTML)
library(xtable)

# Clean Workspace ---------------------------------------------------------
rm(list = ls())

# Reading file ------------------------------------------------------------

file.list.man <- list.files("man/")

##get version number
temp <- readLines("DESCRIPTION")
temp <- temp[grep("Version", temp)]
temp.version <- sub(" ","",unlist(strsplit(temp,":"))[2])

output.file <- paste0("RLum.BuildResults/Luminescence_",temp.version,"-Functions.html")

##exclude package itself
file.list.man <- file.list.man[which(file.list.man!="Luminescence-package.Rd")]

for(i in 1:length(file.list.man)) {

  file <- paste0("man/",file.list.man[i])
  Rd <- parse_Rd(file)
  tags <- tools:::RdTags(Rd)
  tag.name <- unlist(Rd[[which(tags == "\\name")]])

  ##AUTHOR
  if("\\author" %in% tags){

    tag.author <- gsub("\n","<br />",paste(unlist(Rd[[which(tags == "\\author")]]), collapse= " "))

  }else{

    tag.author <- NA

  }

  ##VERSION
  if(length(grep("Function version", unlist(Rd)))>0){

    tag.version <- unlist(Rd)[grep("Function version", unlist(Rd))+2]
    tag.mdate <- strsplit(tag.version, " ")[[1]][3]
    tag.mdate <-  gsub("\\(", "", tag.mdate)
    tag.mtime <- strsplit(tag.version, " ")[[1]][4]
    tag.mtime <-  gsub("\\)", "", tag.mtime)
    tag.version <- strsplit(tag.version, " ")[[1]][2]

  }else{

    tag.version <- NA
    tag.mtime <- NA
    tag.mdate <- NA

  }


  ##TITLE
  if("\\title" %in% tags){

    tag.title <- gsub("\n","<br />",paste(unlist(Rd[[which(tags == "\\title")]]),
                                          collapse= " "))

  }

  ##DESCRIPTION
  if("\\description" %in% tags){

    tag.description <- gsub("\n","<br />",paste(unlist(Rd[[which(tags == "\\description")]]),
                                                collapse= " "))

  }


  if(exists("output")==FALSE){

    output <- data.frame(Name=tag.name,
                         Title = tag.title,
                         Description = tag.description,
                         Version=tag.version,
                         m.Date = tag.mdate,
                         m.Time = tag.mtime,
                         Author = tag.author)

  }else{

    temp.output <- data.frame(Name=tag.name,
                              Title = tag.title,
                              Description = tag.description,
                              Version=tag.version,
                              m.Date = tag.mdate,
                              m.Time = tag.mtime,
                              Author = tag.author)

    output <- rbind(output,temp.output)

  }

}

# HTML Output -------------------------------------------------------------

if(file.exists(output.file)==TRUE){

  file.remove(output.file)

}

HTML(paste("<h2 align=\"center\">Major functions in the R package 'Luminescence'</h2>
           <h4 align=\"center\"> [version:", temp.version,"]</h4>
           <style type=\"text/css\">
           <!--

           h2 {font-family: Arial, Helvetica, sans-serif
           }

           h4 {font-family: Arial, Helvetica, sans-serif
           }

           table {text-align:left;
           vertical-align:top;
           border: 1px solid gray;
           }

           th, td {
           border: 1px solid gray;
           padding: 3px;
           font-family: 'Liberation Sans', Arial, Helvetica, sans-serif;
           font-size: 90%;
           text-align: left;
           vertical-align: top;
           }

           th {
           background-color: #DDD;
           font-weight: bold;
           }

           -->
           </style>


           "),
     file=output.file)

HTML(output,
     align="center",
     Border = 0,
     innerBorder = 1,
     classtable = "style=caption-side:bottom",
     file=output.file)

# CSV Output --------------------------------------------------------------

write.table(output,
            file =  paste0("RLum.BuildResults/Luminescence_",temp.version,"-Functions.csv"),
            sep = ",", row.names = FALSE)


# LaTeX Output ------------------------------------------------------------

latex.table <- xtable(output)
write(print(latex.table),
      file =  paste0("RLum.BuildResults/Luminescence_",temp.version,"-Functions.tex"))
