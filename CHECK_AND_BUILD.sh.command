#!/bin/bash
#
# CONFIG AND DEFINITIONS
# =================================================================================================
#
export TERM=xterm
PATHPACKAGE=$(dirname $0)
#
check_status(){
  if [ $? == 0 ]; then
    echo "[OK]"
  else
    echo "[FAILED]"
  fi
}
#
#
# REMOVING UNWANTED FILES
# =================================================================================================
echo ""
echo "[PREPARE FOR PACKAGE CHECK]"
echo ""
#

  echo -ne "-> Remove old *.tar.gz ... \t\t\t"
  find ${PATHPACKAGE} -name "*.tar.gz" -depth -exec rm {} \;
  check_status

  echo -ne "-> Remove old *.tgz ... \t\t\t"
  find ${PATHPACKAGE} -name "*.tgz" -depth -exec rm {} \;
  check_status

  echo -ne "-> Remove old *.bib ... \t\t\t"
  find ${PATHPACKAGE} -name "*.bib" -depth -exec rm {} \;
  check_status

  echo -ne "-> Remove old *.csv ... \t\t\t"
  find ${PATHPACKAGE} -name "*.csv" -depth -exec rm {} \;
  check_status

  echo -ne "-> Remove old *.tex ... \t\t\t"
  find ${PATHPACKAGE} -name "*.tex" -depth -exec rm {} \;
  check_status

  echo -ne "-> Remove .DS_Store ... \t\t\t"
  find ${PATHPACKAGE} -name ".DS_Store" -depth -exec rm {} \;
  check_status

  echo -ne "-> Remove .Rhistory ... \t\t\t"
  find ${PATHPACKAGE} -name ".Rhistory" -depth -exec rm {} \;
  check_status

  echo -ne "-> Remove .RData ... \t\t\t\t"
  find ${PATHPACKAGE} -name ".RData" -depth -exec rm {} \;
  check_status

  echo -ne "-> Remove .RcppExports.cpp ... \t\t\t"
  find ${PATHPACKAGE}/src -name "RcppExports.cpp" -depth -exec rm {} \;
  check_status

  echo -ne "-> Remove .RcppExports.R ... \t\t\t"
  find ${PATHPACKAGE}/R -name "RcppExports.R" -depth -exec rm {} \;
  check_status


# Rcpp
# =================================================================================================

  echo -ne "-> Build Rcpp ... \t\t\t\t"
  eval R CMD BATCH --no-timing ${PATHPACKAGE}/Package_Buildscripts/RLum.PBS_Rcpp.R /dev/null
  check_status


# roxygen2
# =================================================================================================

  echo -ne "-> Build documentation ... \t\t\t"
  eval R CMD BATCH --no-timing ${PATHPACKAGE}/Package_Buildscripts/RLum.PBS_roxygen2.R /dev/null
  check_status


#
# COMPILE FUNCTION PARAMTER LIST
# =================================================================================================

  echo -ne "-> Compile function argument list ...\t\t"
  eval R CMD BATCH ${PATHPACKAGE}/Package_Buildscripts/RLum.PBS_Function_Arguments.R /dev/null
  check_status


#
# NEWS
# =================================================================================================

  echo -ne "-> Build ASCII NEWS ... \t\t\t"
  eval R CMD BATCH --no-timing ${PATHPACKAGE}/Package_Buildscripts/RLum.PBS_NEWS.R /dev/null
  check_status

#
# PARSE RD files
# =================================================================================================

  echo -ne "-> Add RLum.Team ... \t\t\t\t"
  eval R CMD BATCH --no-timing ${PATHPACKAGE}/Package_Buildscripts/RLum.PBS_AddRLumTeam.R /dev/null
  check_status

#
# BUILD PACKAGE
# =================================================================================================
echo ""
echo "[BUILD PACKAGE]"
echo ""

  eval R CMD BUILD ${PATHPACKAGE}

#
# CHECK PACKAGE
# =================================================================================================
echo ""
echo "[CHECK PACKAGE]"
echo ""

  eval R CMD check --timings ${PATHPACKAGE}/Luminescence*.tar.gz

  echo -ne 'Example timing warnings...:\n\n'
  eval R CMD BATCH ${PATHPACKAGE}/Package_Buildscripts/RLum.PBS_Timings.R /dev/null
  cat ${PATHPACKAGE}/Luminescence-Ex.timings.*.WARNING


#
# INSTALL PACKAGE
# =================================================================================================
echo ""
echo "[INSTALL PACKAGE]"
echo ""

  eval R CMD INSTALL --build ${PATHPACKAGE}/Luminescence*.tar.gz

#
# COPY FILES AND CLEANING UP
# =================================================================================================
echo ""
echo "[OUTRO]"
echo ""

  echo -ne "-> Write BibTeX ... \t\t\t\t"
  eval R CMD BATCH --no-timing ${PATHPACKAGE}/Package_Buildscripts/RLum.PBS_BibTeX.R /dev/null
  check_status

  echo -ne "-> Build function list ... \t\t\t"
  eval R CMD BATCH --no-timing ${PATHPACKAGE}/Package_Buildscripts/RLum.PBS_Function_List.R /dev/null
  check_status

  echo -ne "-> Copy manual ... \t\t\t\t"
  cp Luminescence.Rcheck/Luminescence-manual.pdf Luminescence-manual.pdf
  check_status

  echo -ne "-> Copy check results ... \t\t\t"
  cp Luminescence.Rcheck/Luminescence-Ex.pdf Luminescence-Ex.pdf
  check_status

  echo -ne "-> Remove Luminescence.Rcheck ... \t\t"
  rm -r ${PATHPACKAGE}/Luminescence.Rcheck
  check_status

  echo -ne "-> Remove src/*.so ... \t\t\t\t"
  find ${PATHPACKAGE}/src -name "*.so" -depth -exec rm {} \;
  check_status

  echo -ne "-> Remove src/*.o ... \t\t\t\t"
  find ${PATHPACKAGE}/src -name "*.o" -depth -exec rm {} \;
  check_status

  echo -ne "-> Remove src/*.rds ... \t\t\t"
  find ${PATHPACKAGE}/src -name "*.rds" -depth -exec rm {} \;
  check_status

  echo ""
  echo "FINE"
