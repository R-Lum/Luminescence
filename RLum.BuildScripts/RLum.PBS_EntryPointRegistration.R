### ===============================================================================================
### R package Luminescence BUILDSCRIPTS
### EntryPointRegistering
### sebastian.kreutzer@u-bordeaux-montaigne.fr
### 2017-03-29
### ===============================================================================================

##
##based on
##http://stackoverflow.com/questions/42313373/r-cmd-check-note-found-no-calls-to-r-registerroutines-r-usedynamicsymbols
Luminescence_init <- utils::capture.output(tools::package_native_routine_registration_skeleton("."))
write(x = Luminescence_init, file = "src/Luminescence_init.c")
