pkgname <- "sandbox"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "sandbox-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('sandbox')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("add_Population")
### * add_Population

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: add_Population
### Title: Add a Population to a Rule Book
### Aliases: add_Population

### ** Examples


## create simple true age-depth-relationship
book_1 <- get_RuleBook()

book_2 <- add_Population(
 book = book_1,
 populations = 1)
 



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("add_Population", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("add_Rule")
### * add_Rule

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: add_Rule
### Title: Add a Rule to a Rule Book
### Aliases: add_Rule

### ** Examples


## create simple true age-depth-relationship
book_1 <- get_RuleBook()

book_2 <- add_Rule(
 book = book_1, 
 name = "extrarule", 
 group = "general", 
 type = "normal", 
 populations = 1)
                



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("add_Rule", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("convert_units")
### * convert_units

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: convert_units
### Title: Convert between phi units and micrometers
### Aliases: convert_units

### ** Examples

## load example data set
## generate phi-values
phi <- -2:5

## convert and show phi to mu
mu  <- convert_units(phi = phi)
mu

## convert and show mu to phi
convert_units(mu = mu)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("convert_units", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_RuleBook")
### * get_RuleBook

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_RuleBook
### Title: Get One of a Series of Predefined Rule Books for a Model Run.
### Aliases: get_RuleBook

### ** Examples


## create simple true age-depth-relationship
book_flat <- get_RuleBook(book = "empty")




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_RuleBook", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("make_Sample")
### * make_Sample

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: make_Sample
### Title: Create a Virtual Sample.
### Aliases: make_Sample

### ** Examples


set.seed(12234)
sample_01 <- make_Sample(
  book = get_RuleBook(), 
  depth = 1, 
  geometry = "cuboid",
  n_cores = 1,
  height = 0.001,
  width = 0.001, 
  length = 0.001)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("make_Sample", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("measure_SAR_OSL")
### * measure_SAR_OSL

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: measure_SAR_OSL
### Title: Measure an aliquot with the CW SAR OSL protocol
### Aliases: measure_SAR_OSL

### ** Examples


## Not run: 
##D 
##D ## load example data set
##D data(sample_osl_aliquots, envir = environment())
##D 
##D sequence <- list(
##D   RegDose = c(0, 1, 2, 5, 10, 0, 1),
##D   TestDose = 2,
##D   PH = 220,
##D   CH = 200,
##D   OSL_temp = 125,
##D   OSL_duration = 70)
##D 
##D ## reduce number of 
##D ## grains to two
##D sample_osl_aliquots$aliquot_1 <- 
##D sample_osl_aliquots$aliquot_1[1:2,]
##D 
##D ## or measure all aliquots in a row
##D sar_all <- measure_SAR_OSL(
##D  aliquot = sample_osl_aliquots,
##D  sequence = sequence,
##D  dose_rate = 0.1)
##D  
##D  
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("measure_SAR_OSL", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("prepare_Aliquot")
### * prepare_Aliquot

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: prepare_Aliquot
### Title: Prepare Aliquots from Sample Dataset
### Aliases: prepare_Aliquot

### ** Examples

## load example data set
data(sample, envir = environment())

A <- prepare_Aliquot(
 sample = sample, 
 diameter = 0.1)

B <- prepare_Aliquot(
 sample = sample, 
 diameter = 1, 
 density = 0.6)
 



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("prepare_Aliquot", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("prepare_Sieving")
### * prepare_Sieving

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: prepare_Sieving
### Title: Sieve a Sample
### Aliases: prepare_Sieving

### ** Examples

## load example data set
data(sample, envir = environment())

## sieve sample (in phi units)
sample_sieved <- prepare_Sieving(
  sample = sample,
  interval = c(5, 6))
                                 
## plot results
plot(density(
  x = sample$grainsize, 
  from = -1, 
  to = 11))
lines(density(
  x = sample_sieved$grainsize, 
  from = -1, 
  to = 11), 
  col = 2)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("prepare_Sieving", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("prepare_Subsample")
### * prepare_Subsample

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: prepare_Subsample
### Title: Prepare Subsamples from a Sample Dataset
### Aliases: prepare_Subsample

### ** Examples

## load example data set
data(sample, envir = environment())

## create 10 subsamples
prepare_Subsample(sample, 10) 




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("prepare_Subsample", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("sample")
### * sample

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: sample
### Title: Example Grain Size Data
### Aliases: sample
### Keywords: datasets

### ** Examples


## load example data set
data(sample, envir = environment())

## plot grain-size distribution
plot(density(sample$grainsize))




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("sample", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("sample_osl_aliquots")
### * sample_osl_aliquots

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: sample_osl_aliquots
### Title: Aliquots Prepared to Measured Virtually
### Aliases: sample_osl_aliquots
### Keywords: datasets

### ** Examples


## load example data set
data(sample_osl_aliquots, envir = environment())

## plot grain-size distribution
plot(density(sample_osl_aliquots[[1]]$age))




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("sample_osl_aliquots", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("set_Parameter")
### * set_Parameter

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: set_Parameter
### Title: Set Profile- and Grain-Specific Model Parameters.
### Aliases: set_Parameter

### ** Examples

## get empty rule book
book_1 <- get_RuleBook(book = "empty")

## set density from default "normal" to "exact"
book_2 <- set_Parameter(book = book_1,
                        parameter = "density",
                        type = "exact")

book_1$density$density_1$type
book_2$density$density_1$type




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("set_Parameter", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("set_Rule")
### * set_Rule

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: set_Rule
### Title: Set depth-dependent rule for model parameter.
### Aliases: set_Rule

### ** Examples


## create empty rule book
book_01 <- get_RuleBook()

## assign rule definitions to lists
depth <- list(c(0, 10))
age <- list(c(0, 1000))

## add age definition
book_01 <- set_Rule(
 book = book_01, 
 parameter = "age", 
 value = age, 
 depth = depth)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("set_Rule", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
