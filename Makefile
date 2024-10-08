## PACKAGE MAKEFILE
RSCRIPT = "$(R_HOME)/bin/Rscript"

all:
	${RSCRIPT} -e "RLumBuild::build_package(\
		exclude = c(\
		'module_check_ReverseDependencies',\
		'module_write_codemetar',\
		'module_add_RLumTeam',\
		'module_add_HowToCite',\
		'module_write_BibTeX',\
		'module_write_FunctionList',\
		'module_write_PDF_manual'),\
		as_cran = TRUE,\
		write_Rbuildignore = TRUE)";

#all:
#	${RSCRIPT} -e "RLumBuild::build_package(\
#		exclude = c(\
#		'module_check_ReverseDependencies',\
#		'module_write_codemetar'),\
#		as_cran = TRUE,\
#		write_Rbuildignore = TRUE)";
