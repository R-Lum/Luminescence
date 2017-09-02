### ===============================================================================================
### R package Luminescence BUILDSCRIPTS
### EntryPointRegistering
### sebastian.kreutzer@u-bordeaux-montaigne.fr
### 2017-03-29
### ===============================================================================================

##this script bases on
##http://stackoverflow.com/questions/42313373/r-cmd-check-note-found-no-calls-to-r-registerroutines-r-usedynamicsymbols

##run registration
Luminescence_init <- utils::capture.output(tools::package_native_routine_registration_skeleton(".", character_only = FALSE))

##add header text
header <-  c(
"/* DO NOT CHANGE MANUALLY! */",
"/* This file was produced by the function RLum.BuildScripts/RLum.PBS_EntryPointRegistration.R */")

##write file
write(x = c(header, Luminescence_init), file = "src/Luminescence_init.c")
