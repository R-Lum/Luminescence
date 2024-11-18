## PACKAGE MAKEFILE
RSCRIPT = "$(R_HOME)/bin/Rscript"

## set to 1 when on a development version, or to 0 when preparing a release
DEVEL := 0

## modules to exclude: it should not be necessary to touch these, they are
## controlled by the DEVEL variable set before
EXCLUDE_MODULES := \
	'module_check_ReverseDependencies', \
	'module_write_codemetar' \
	$(NULL)
ifeq ($(DEVEL), 1)
  EXCLUDE_MODULES += ,\
	'module_add_RLumTeam', \
	'module_add_HowToCite', \
	'module_write_BibTeX', \
	'module_write_FunctionList', \
	'module_write_PDF_manual' \
	$(NULL)
endif

all:
	${RSCRIPT} -e "RLumBuild::build_package(\
		exclude = c(\
		$(EXCLUDE_MODULES) ),\
		as_cran = TRUE,\
		write_Rbuildignore = TRUE)";
