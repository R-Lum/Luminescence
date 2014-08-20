##//////////////////////////////////////////////////////////////
##//RLum.Data-class.R
##/////////////////////////////////////////////////////////////
##======================================
#author: Sebastian Kreutzer
#organisation: JLU Giessen
#vers.: 0.1
#date: 18/01/2013
##======================================

setClass("RLum.Data",
         representation(),
         contains = "RLum",
         S3methods=TRUE
         )
