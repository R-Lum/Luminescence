### ===============================================================================================
### R package Luminescence BUILDSCRIPTS
### Function_Arguments
### sebastian.kreutzer@u-bordeaux-montaigne.fr
### 2015-04-29
### Note: Previous version based on code of Michal Dietze
### ===============================================================================================
library(tools)


##get version number
temp <- readLines("DESCRIPTION")
temp <- temp[grep("Version", temp)]
temp.version <- sub(" ","",unlist(strsplit(temp,":"))[2])

## list files in rd-directory
rd.files <- list.files("man/")

## remove file extensions
functions <- unlist(strsplit(x = rd.files, split = ".Rd"))

## remove non-functions and unwanted stuff
functions <- functions[!grepl(x = functions, pattern = "-class")]
functions <-
  functions[!grepl(x = functions, pattern = "ExampleData")]
functions <- functions[!functions == "BaseDataSet.CosmicDoseRate"]
functions <- functions[!functions == "Luminescence-package"]

## create output matrix
M <- matrix(nrow = 100, ncol = length(functions))
colnames(M) <- functions

## parse rd-files for argument names
for (i in 1:ncol(M)) {
  rd.file.i <- readLines(con = paste0("man/",functions[i], ".Rd"))

  line.ID.i <- seq(from = 1, to = length(rd.file.i))

  arguments.start <- line.ID.i[rd.file.i == "\\arguments{"]

  arguments.end <- line.ID.i[which(rd.file.i == "}")]
  arguments.end <- arguments.end[which(arguments.end > arguments.start)][1]

  rd.arguments.i <- rd.file.i[arguments.start:arguments.end]

  arguments.i <- rd.arguments.i[grepl(x = rd.arguments.i,
                                      pattern = "\\item",
                                      fixed = TRUE)]

  arguments.i <- sub(
    x = arguments.i,
    pattern = "\\item{",
    replacement = "",
    fixed = TRUE
  )

  arguments.i <- strsplit(x = arguments.i,
                          split = "}{",
                          fixed = TRUE)

  arguments.j <- character(length = length(arguments.i))

  for (j in 1:length(arguments.i)) {
    arguments.j[j] <- arguments.i[[j]][1]
  }

  arguments.i <- sub(
    x = arguments.j,
    pattern = "\\",
    replacement = "",
    fixed = TRUE
  )


  M[1:length(arguments.i),i] <- arguments.i

}

n.max <- max(100 - colSums(apply(X = M, MARGIN = 2, FUN = is.na)))

M <- M[1:n.max,]

write.table(x = M,
            row.names = FALSE,
            file = paste0("Luminescence_",temp.version,"-Function_Arguments.csv"),
            sep = ",")
