analyse_SAR.CWOSL<- structure(function(#Analyse SAR CW-OSL measurements
  ### The function performs a SAR CW-OSL analysis on a \code{\linkS4class{RLum.Analysis}}
  ### object including growth curve fitting.
  
  # ===========================================================================
  ##author<<
  ## Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France)\cr
  
  ##section<<
  ## version 0.4.2
  # ===========================================================================

  object,
  ### \code{\linkS4class{RLum.Analysis}}(\bold{required}): 
  ### input object containing data for analysis
  
  signal.integral.min,
  ### \code{\link{integer}} (\bold{required}): lower bound of the signal integral
  
  signal.integral.max,
  ### code{\link{integer}} (\bold{required}): upper bound of the signal integral
  
  background.integral.min,
  ### \code{\link{integer}} (\bold{required}): lower bound of the background integral
  
  background.integral.max,
  ### \code{\link{integer}} (\bold{required}): upper bound of the background integral
  
  rejection.criteria = list(recycling.ratio = 10, 
                            recuperation.rate = 10, 
                            palaeodose.error = 10),
  ### \link{list} (with default): list containing rejection criteria in 
  ### percentage for the calculation. 
  
  dose.points,
  ### \link{numeric} (optional): a numeric vector containg the dose points values
  ### Using this argument overwrites dose point values in the signal curves. 
  
  output.plot = TRUE,
  ### \link{logical} (with default): enables or disables plot output.
  
  output.plot.single = FALSE,
  ### \link{logical} (with default): single plot output (\code{TRUE/FALSE}) to 
  ### allow for plotting the results in single plot windows. 
  ### Requires \code{output.plot = TRUE}.
  
  ... 
  ### further arguments that will be passed to the function 
  ### \code{\link{plot_GrowthCurve}}

){

# CONFIG  -----------------------------------------------------------------
  
  ##build signal and background integrals
  signal.integral <- c(signal.integral.min:signal.integral.max)
  background.integral <- c(background.integral.min:background.integral.max)
  
# General Integrity Checks ---------------------------------------------------

  ##GENERAL 

    ##MISSING INPUT
    if(missing("object")==TRUE){
      stop("[analyse_SAR.CWOSL] No value set for 'object'!")
    }

    if(missing("signal.integral")==TRUE){
      stop("[analyse_SAR.CWOSL()] No value set for 'signal.integral'!")
    }
    
    if(missing("background.integral")==TRUE){
     stop("[analyse_SAR.CWOSL()] No value set for 'background.integral'!")
    }

    ##INPUT OBJECTS
    if(is(object, "RLum.Analysis")==FALSE){
      stop("[analyse_SAR.CWOSL()] Input object is not of type 'RLum.Analyis'!")
    }

    ##INTEGRAL LIMITS
    if(is(signal.integral, "integer")==FALSE | is(background.integral, 
                                                  "integer")==FALSE){
      stop("[analyse_SAR.CWOSL()] 'signal.integral' or background.integral is not 
           of type integer!")
    }


    ##CHECK IF DATA SET CONTAINS ANY OSL curve
    if(!TRUE%in%grepl("OSL", get_structure.RLum.Analysis(object)$recordType) && 
       !TRUE%in%grepl("IRSL", get_structure.RLum.Analysis(object)$recordType)){
      
      stop("[analyse_SAR.CWOSL()] No record of type 'OSL' or 'IRSL' are detected in the sequence
object!")
      
    }

    ##Check if any OSL curve is measured, if not set curve type on IRSL 
    ##to allow further proceedings
    CWcurve.type  <- ifelse(!TRUE%in%grepl("OSL", get_structure.RLum.Analysis(object)$recordType),
                            "IRSL","OSL")


# Deal with extra arguments -------------------------------------------------------------------

  ##deal with addition arguments 
  extraArgs <- list(...) 

  mtext <- if("mtext" %in% names(extraArgs)) {extraArgs$mtext} else 
  {""}

  log <- if("log" %in% names(extraArgs)) {extraArgs$log} else 
  {""}

  cex <- if("cex" %in% names(extraArgs)) {extraArgs$cex} else 
  {0.6}


# Protocol Integrity Checks -------------------------------------------------- 
  
  ##check overall structur of the object
  ##every SAR protocol has to have equal number of curves
  

  ##grep curve types from analysis value and remove unwanted information
  temp.ltype <- sapply(1:length(object@records), function(x) {
          
                ##export as global variable
                object@records[[x]]@recordType <<- gsub(" .*", "", 
                                                        object@records[[x]]@recordType)
                
                object@records[[x]]@recordType        
              
  })


  ##problem: FI lexsyg devices provide irradtion information in a separat curve 
  if("irradiation"%in%temp.ltype){
    
    ##grep irraditation times
    temp.irradiation <- get_structure.RLum.Analysis(object)
    temp.irradiation <- temp.irradiation[temp.irradiation$recordType == "irradiation",
                                         "x.max"]
    
    ##remove every 2nd entry (test dose) and add "0" dose for natural signal
    temp.Dose <- c(0,temp.irradiation)
    
    ##remove irradiation entries from file
    object <- set_RLum.Analysis(
               records = get_RLum.Analysis(object, recordType = c(CWcurve.type, "TL")),
               protocol = "SAR")

  }

  ##check if the wanted curves are a multiple of two
  ##gsub removes unwanted information from the curves
  if(table(temp.ltype)[CWcurve.type]%%2!=0){
    stop("[analyse_SAR.CWOSL] Input OSL/IRSL curves are not a multiple of two.")
  }

  ##check if the curve lengths differ
  temp.matrix.length <- unlist(sapply(1:length(object@records), function(x) {
                          if(object@records[[x]]@recordType==CWcurve.type){                            
                              length(object@records[[x]]@data[,1])
                          }
  }))
  
  if(length(unique(temp.matrix.length))!=1){
    stop("[analyse_SAR.CWOSL] Input curves lengths differ.")
  }


  ##check background integral
  ##background integral should not longer than curve channel length
  if(max(background.integral)>temp.matrix.length[1]){
    
    background.integral <- c((temp.matrix.length[1]-length(background.integral)):
                               temp.matrix.length[1])
    
    warning("Background integral out of bounds. Set to: c(", 
            min(background.integral),":", max(background.integral),")")
    
  }
  

# Grep Curves -------------------------------------------------------------

  ##grep relevant curves from RLum.Analyis object
  OSL.Curves.ID <- unlist(sapply(1:length(object@records), function(x) {
    if(object@records[[x]]@recordType == CWcurve.type){x} 
  }))

  ##separate curves by Lx and Tx (it makes it much easier)
  OSL.Curves.ID.Lx <- OSL.Curves.ID[seq(1,length(OSL.Curves.ID),by=2)]
  OSL.Curves.ID.Tx <- OSL.Curves.ID[seq(2,length(OSL.Curves.ID),by=2)]

  TL.Curves.ID <- unlist(sapply(1:length(object@records), function(x) {
    if(object@records[[x]]@recordType == "TL"){x} 
  }))

  ##separate TL curves
  TL.Curves.ID.Lx <- sapply(1:length(OSL.Curves.ID.Lx), function(x) {
    TL.Curves.ID[which(TL.Curves.ID == (OSL.Curves.ID.Lx[x]-1))]
  })

  TL.Curves.ID.Tx <- sapply(1:length(OSL.Curves.ID.Tx), function(x) {
    TL.Curves.ID[which(TL.Curves.ID == (OSL.Curves.ID.Tx[x]-1))]
  })


# COMPONENT FITTING -------------------------------------------------------


# for(x in seq(1,length(OSL.Curves.ID),by=2)){
#   
#   
#   temp.fit.output <- fit_CWCurve(object@records[[OSL.Curves.ID[x]]], 
#                 n.components.max=3,
#                 output.terminal = FALSE, 
#                 output.terminalAdvanced = FALSE, 
#                 output.plot = FALSE
# 
#               )
#   if(exists("fit.output") == FALSE){
#     
#     fit.output <- get_RLum.Results(temp.fit.output)
#     
#   }else{
#     
#     fit.output <- rbind(fit.output, get_RLum.Results(temp.fit.output))
#     
#   }
# 
# }

##TODO

# Calculate LnLxTnTx values  --------------------------------------------------

  ##calculate LxTx values using external function   
  for(x in seq(1,length(OSL.Curves.ID),by=2)){
                
               temp.LnLxTnTx <- get_RLum.Results(
                 calc_OSLLxTxRatio(Lx.data=object@records[[OSL.Curves.ID[x]]]@data,
                                 Tx.data=object@records[[OSL.Curves.ID[x+1]]]@data,
                                 signal.integral,
                                 background.integral))
              
               ##grep dose
               if(exists("temp.irradiation") == FALSE){
               
                temp.Dose <- object@records[[OSL.Curves.ID[x]]]@info$IRR_TIME
                
                  ##for the case that no information on the dose can be found
                  if(is.null(temp.Dose)){temp.Dose <- NA}
                
                temp.LnLxTnTx <- cbind(Dose=temp.Dose, temp.LnLxTnTx)
           
               }else{
            
                 temp.LnLxTnTx <- cbind(Dose=temp.Dose[x], temp.LnLxTnTx)
                 
               }
         
               if(exists("LnLxTnTx")==FALSE){
                 
                 LnLxTnTx <- data.frame(temp.LnLxTnTx)
                 
               }else{
                 
                 LnLxTnTx <- rbind(LnLxTnTx,temp.LnLxTnTx)
                 
               }
    }

# Set regeneration points -------------------------------------------------

        ##overwrite dose point manually
        if(missing(dose.points) == FALSE){
          
          if(length(dose.points)!=length(LnLxTnTx$Dose)){
            
            stop("[analyse_SAR.CWOSL] length 'dose.points' differs from number of curves.")
            
          }
          
          LnLxTnTx$Dose <- dose.points
          
        }

        #generate unique dose id - this are also the # for the generated points
        temp.DoseID <- c(0:(length(LnLxTnTx$Dose)-1))
        temp.DoseName <- paste("R",temp.DoseID,sep="")
        temp.DoseName <- cbind(Name=temp.DoseName,Dose=LnLxTnTx$Dose)
     
        
        ##set natural
        temp.DoseName[temp.DoseName[,"Name"]=="R0","Name"]<-"Natural"
      
        
        ##set R0
        temp.DoseName[temp.DoseName[,"Name"]!="Natural" & 
                        temp.DoseName[,"Dose"]==0,"Name"]<-"R0"
          
        ##find duplicated doses (including 0 dose - which means the Natural) 
        temp.DoseDuplicated<-duplicated(temp.DoseName[,"Dose"])
         
        ##combine temp.DoseName
        temp.DoseName<-cbind(temp.DoseName,Repeated=temp.DoseDuplicated)
       
        ##correct value for R0 (it is not really repeated)
        temp.DoseName[temp.DoseName[,"Dose"]==0,"Repeated"]<-FALSE

        ##combine in the data frame
        temp.LnLxTnTx<-data.frame(Name=temp.DoseName[,"Name"],
                                  Repeated=as.logical(temp.DoseName[,"Repeated"]))

        LnLxTnTx<-cbind(temp.LnLxTnTx,LnLxTnTx)
        LnLxTnTx[,"Name"]<-as.character(LnLxTnTx[,"Name"])

# Calculate Recycling Ratio -----------------------------------------------

      ##Calculate Recycling Ratio 
   
      if(length(LnLxTnTx[LnLxTnTx[,"Repeated"]==TRUE,"Repeated"])>0){
                     
              ##identify repeated doses
              temp.Repeated<-LnLxTnTx[LnLxTnTx[,"Repeated"]==TRUE,c("Name","Dose","LxTx")]
            
              ##find concering previous dose for the repeated dose
              temp.Previous<-t(sapply(1:length(temp.Repeated[,1]),function(x){
                  LnLxTnTx[LnLxTnTx[,"Dose"]==temp.Repeated[x,"Dose"] & 
                  LnLxTnTx[,"Repeated"]==FALSE,c("Name","Dose","LxTx")]
                 }))
             
              ##convert to data.frame
              temp.Previous<-as.data.frame(temp.Previous)
              
              ##set column names
              temp.ColNames<-sapply(1:length(temp.Repeated[,1]),function(x){
                     paste(temp.Repeated[x,"Name"],"/",
                           temp.Previous[temp.Previous[,"Dose"]==temp.Repeated[x,"Dose"],"Name"],
                           sep="")
                      })
                                            
              ##Calculate Recycling Ratio
              RecyclingRatio<-as.numeric(temp.Repeated[,"LxTx"])/as.numeric(temp.Previous[,"LxTx"])
              
              ##Just transform the matrix and add column names
              RecyclingRatio<-t(RecyclingRatio)
              colnames(RecyclingRatio)<-temp.ColNames
        
      }else{RecyclingRatio<-NA}  



# Calculate Recuperation Rate ---------------------------------------------

        
         ##Recuperation Rate (capable to handle multiple type of recuperation values)

         if(length(LnLxTnTx[LnLxTnTx[,"Name"] == "R0","Name"])>0){
          
           Recuperation <- sapply(1:length(LnLxTnTx[LnLxTnTx[,"Name"] == "R0","Name"]), 
                                  function(x){
                                    round(
                                      LnLxTnTx[LnLxTnTx[,"Name"]=="R0","LxTx"][x]/
                                      LnLxTnTx[LnLxTnTx[,"Name"]=="Natural","LxTx"],
                                      digits=4)
                                  })
           ##Just transform the matrix and add column names
           Recuperation  <-  t(Recuperation)
           colnames(Recuperation)  <-  unlist(strsplit(paste("recuperation rate", 
             1:length(LnLxTnTx[LnLxTnTx[,"Name"] == "R0","Name"]), collapse = ";"), ";"))
           
         }else{Recuperation<-NA}
      

# Evaluate and Combine Rejection Criteria ---------------------------------

    temp.criteria <- c(colnames(RecyclingRatio), colnames(Recuperation))
    temp.value <- c(RecyclingRatio,Recuperation)
    temp.threshold <- c(rep(paste("+/-", rejection.criteria$recycling.ratio/100),
                                  length(RecyclingRatio)),
                        rep(paste("", rejection.criteria$recuperation.rate/100),
                                  length(Recuperation)))
  
    ##RecyclingRatio
    if(is.na(RecyclingRatio)[1] == FALSE){
     
      temp.status.RecyclingRatio <- sapply(1:length(RecyclingRatio), function(x){
        if(abs(1-RecyclingRatio[x])>(rejection.criteria$recycling.ratio/100)){
          "FAILED"
        }else{"OK"}})
    }else{
      
      temp.status.RecyclingRatio <- "OK"
   
    }

    ##Recuperation
    if(is.na(Recuperation)[1] == FALSE){
      
      temp.status.Recuperation  <- sapply(1:length(Recuperation), function(x){
        ifelse(Recuperation[x]>rejection.criteria$recuperation.rate, 
               "FAILED", "OK")
        
        
      }) 

    }else{
      
      temp.status.Recuperation <- "OK"
      
    }
 
    RejectionCriteria <- data.frame(
      criteria = temp.criteria,
      value = temp.value,
      threshold = temp.threshold,
      status = c(temp.status.RecyclingRatio,temp.status.Recuperation)) 
               
##============================================================================##
##PLOTTING
##============================================================================##

if(output.plot == TRUE){

# Plotting - Config -------------------------------------------------------
  
  par.default <- par(no.readonly = TRUE)

  ##colours and double for plotting
  col <- get("col", pos = .LuminescenceEnv)

  if(output.plot.single == FALSE){
    
    layout(matrix(c(1,1,3,3,
                    1,1,3,3,
                    2,2,4,4,
                    2,2,4,4,
                    5,5,5,5),5,4,byrow=TRUE))
    
    par(oma=c(0,0,0,0), mar=c(4,4,3,3), cex = cex)
    
    ## 1 -> TL previous LnLx
    ## 2 -> LnLx
    ## 3 -> TL previous TnTx
    ## 4 -> TnTx 
    ## 5 -> Legend
  }else{par(mfrow=c(1,1))}   
  

  ##warning if number of curves exceed colour values
  if(length(col)<length(OSL.Curves.ID)/2){
    
       temp.message  <- paste("\n[analyse_SAR.CWOSL()] To many curves! Only the first",
           length(col),"curves are plotted!")
       warning(temp.message)
   }
 
  ##legend text
  legend.text <- paste(LnLxTnTx$Name,"\n(",LnLxTnTx$Dose,")", sep="")


    ##get channel resolution (should be equal for all curves)
    resolution.OSLCurves <- round(
      object@records[[OSL.Curves.ID[1]]]@data[2,1]-
      object@records[[OSL.Curves.ID[1]]]@data[1,1],
      digits=2)


# Plotting TL Curves previous LnLx ----------------------------------------

  
  ##check if TL curves are available
  if(length(TL.Curves.ID.Lx[[1]]>0)) {
    
     ##It is just an approximation taken from the data
     resolution.TLCurves <-  round(
       mean(
         diff(
           round(object@records[[TL.Curves.ID.Lx[1]]]@data[,1], digits = 1)
           )
         ), digits = 1)
                            
     ylim.range <- sapply(seq(1,length(TL.Curves.ID.Lx),by=1) ,function(x){      
                          
                           range(object@records[[TL.Curves.ID.Lx[x]]]@data[,2])
                          
                          })

     plot(NA,NA,
          xlab="T [\u00B0C]",
          ylab=paste("TL [cts/",resolution.TLCurves," \u00B0C]",sep=""),
          xlim=c(object@records[[TL.Curves.ID.Lx[1]]]@data[1,1],
                 max(object@records[[TL.Curves.ID.Lx[1]]]@data[,1])),
          ylim=c(1,max(ylim.range)),
          main=expression(paste("TL previous ", L[n],",",L[x]," curves",sep="")),
          log=if(log=="y" | log=="xy"){"y"}else{""}
     )
    
     ##plot TL curves
     sapply(1:length(TL.Curves.ID.Lx) ,function(x){
                  
            lines(object@records[[TL.Curves.ID.Lx[x]]]@data,col=col[x])
      
     })
     
     

  }else{

  
    plot(NA,NA,xlim=c(0,1), ylim=c(0,1), main="",
        axes=FALSE,
        ylab="",
        xlab=""
         )
    text(0.5,0.5, "No TL curve detected")
    
  }


# Plotting LnLx Curves ----------------------------------------------------


      ylim.range <- sapply(1:length(OSL.Curves.ID.Lx) ,function(x){
                      
                        range(object@records[[OSL.Curves.ID.Lx[x]]]@data[,2])
      })
          
      xlim  <- c(object@records[[OSL.Curves.ID.Lx[1]]]@data[1,1],
                 max(object@records[[OSL.Curves.ID.Lx[1]]]@data[,1]))

      #open plot area LnLx
      plot(NA,NA,
          xlab="Time [s]",
          ylab=paste(CWcurve.type," [cts/",resolution.OSLCurves," s]",sep=""),
          xlim=xlim,
          ylim=range(ylim.range),
          main=expression(paste(L[n],",",L[x]," curves",sep="")),
          log=log
       )
      
        ##plot curves
           sapply(1:length(OSL.Curves.ID.Lx), function(x){
               
                  lines(object@records[[OSL.Curves.ID.Lx[[x]]]]@data,col=col[x])
                  
                  })
           
        
          ##mark integration limits
          abline(v=(min(xlim) + min(signal.integral)*resolution.OSLCurves), lty=2, col="gray")
          abline(v=(min(xlim) + max(signal.integral)*resolution.OSLCurves), lty=2, col="gray")
          abline(v=(min(xlim) + min(background.integral)*resolution.OSLCurves), lty=2, col="gray")
          abline(v=(min(xlim) + max(background.integral)*resolution.OSLCurves), lty=2, col="gray")


          ##mtext, implemented here, as a plot window has to be called first
          mtext(mtext, side = 4, outer = TRUE, line = -1.7, cex = cex, col = "blue")

# Plotting TL Curves previous TnTx ----------------------------------------

##check if TL curves are available

if(length(TL.Curves.ID.Tx[[1]]>0)) {
  
  ##It is just an approximation taken from the data
  resolution.TLCurves <-  round(
    mean(
      diff(
        round(object@records[[TL.Curves.ID.Tx[1]]]@data[,1], digits = 1)
      )
    ), digits = 1)
  
  
  ylim.range <- sapply(1:length(TL.Curves.ID.Tx) ,function(x){      
    
    range(object@records[[TL.Curves.ID.Tx[x]]]@data[,2])
 
  })
  
 
  
  plot(NA,NA,
       xlab="T [\u00B0C]",
       ylab=paste("TL [cts/",resolution.TLCurves," \u00B0C]",sep=""),
       xlim=c(object@records[[TL.Curves.ID.Tx[1]]]@data[1,1],
              max(object@records[[TL.Curves.ID.Tx[1]]]@data[,1])),
       
       ylim=c(1,max(ylim.range)),
       
       main=expression(paste("TL previous ", T[n],",",T[x]," curves",sep="")),
       log=if(log=="y" | log=="xy"){"y"}else{""}
  )
  
  ##plot TL curves
  sapply(1:length(TL.Curves.ID.Tx) ,function(x){
    
    lines(object@records[[TL.Curves.ID.Tx[x]]]@data,col=col[x])
    
  })
  
  
  
}else{
  
  
  plot(NA,NA,xlim=c(0,1), ylim=c(0,1), main="",
       axes=FALSE,
       ylab="",
       xlab=""
  )
  text(0.5,0.5, "No TL curve detected")
  
}


# Plotting TnTx Curves ----------------------------------------------------


    ylim.range <- sapply(1:length(OSL.Curves.ID.Tx) ,function(x){
  
                  range(object@records[[OSL.Curves.ID.Tx[x]]]@data[,2])
                  
                  })


    xlim <- c(object@records[[OSL.Curves.ID.Tx[1]]]@data[1,1], max(object@records[[OSL.Curves.ID.Tx[1]]]@data[,1]))

    #open plot area LnLx
    plot(NA,NA,
         xlab="Time [s]",
         ylab=paste(CWcurve.type ," [cts/",resolution.OSLCurves," s]",sep=""),
           xlim=xlim,     
           ylim=range(ylim.range),
     
           main=expression(paste(T[n],",",T[x]," curves",sep="")),
           log=log
    )

      ##plot curves and get legend values
      sapply(1:length(OSL.Curves.ID.Tx) ,function(x){
  
         lines(object@records[[OSL.Curves.ID.Tx[[x]]]]@data,col=col[x])
  
      })

      ##mark integration limit
      abline(v=(min(xlim) + min(signal.integral)*resolution.OSLCurves), lty=2, col="gray")
      abline(v=(min(xlim) + max(signal.integral)*resolution.OSLCurves), lty=2, col="gray")
      abline(v=(min(xlim) + min(background.integral)*resolution.OSLCurves), lty=2, col="gray")
      abline(v=(min(xlim) + max(background.integral)*resolution.OSLCurves), lty=2, col="gray")


# Plotting Legend ----------------------------------------


plot(c(1:(length(OSL.Curves.ID)/2)),
     rep(8,length(OSL.Curves.ID)/2),
     type = "p", 
     axes=FALSE, 
     xlab="", 
     ylab="",
     pch=15,
     col=unique(col[1:length(OSL.Curves.ID)]),
     cex=2,
     ylim=c(0,10)
     )

##add text
text(c(1:(length(OSL.Curves.ID)/2)), 
     rep(4,length(OSL.Curves.ID)/2),
     legend.text)

##add line
abline(h=10,lwd=0.5)

##set failed text and mark De as failed
if(length(grep("FAILED", RejectionCriteria$status))>0){
  
  mtext("[FAILED]", col="red")

  
}

par(par.default)
rm(par.default)

}##end output.plot == TRUE

# Plotting  GC  ----------------------------------------

temp.sample <- data.frame(Dose=LnLxTnTx$Dose, 
                          LxTx=LnLxTnTx$LxTx,
                          LxTx.Error=LnLxTnTx$LxTx.Error,
                          TnTx=LnLxTnTx$Net_TnTx
                          )

 temp.GC <- get_RLum.Results(plot_GrowthCurve(temp.sample,
                                              output.plot = output.plot,
                                              ...))[,c("De","De.Error")]



  

# Provide Rejection Criteria for Palaedose error --------------------------

  palaeodose.error.calculated <- ifelse(is.na(temp.GC[,1]) == FALSE, 
                                        round(temp.GC[,2]/temp.GC[,1], digits = 5),
                                        NA)

  palaeodose.error.threshold <- paste("+/- ",  
                                      rejection.criteria$palaeodose.error/100,
                                      sep = "")
  
  palaeodose.error.status <- ifelse(
    palaeodose.error.calculated <= rejection.criteria$palaeodose.error,
                                    "OK", "FAILED")


  palaeodose.error.data.frame <- data.frame(criteria = "palaeodose.error", 
                                   value = palaeodose.error.calculated, 
                                   threshold = palaeodose.error.threshold,
                                   status =  palaeodose.error.status)
  
  ##add to RejectionCriteria data.frame
  RejectionCriteria <- rbind(RejectionCriteria, palaeodose.error.data.frame)


 ##add recjection status
 if(length(grep("FAILED",RejectionCriteria$status))>0){
  
   temp.GC <- data.frame(temp.GC, RC.Status="FAILED")
   
  }else{
    
   temp.GC <- data.frame(temp.GC, RC.Status="OK") 
    
  }


# Return Values -----------------------------------------------------------

  temp.return <- new("RLum.Results",
                     originator = "analyse_SAR.CWOSL",
                     data = list(
                       De.values = temp.GC, 
                       LnLxTnTx.table = LnLxTnTx, 
                       rejection.criteria = RejectionCriteria))

  return(temp.return)
  
  # DOCUMENTATION - INLINEDOC LINES -----------------------------------------

  ##details<<
  ## The function performs an analysis for a standard SAR protocol measurements 
  ## introduced by Murray and Wintle (2000) with CW-OSL curves. 
  ## For the calculation of the Lx/Tx value the function \link{calc_OSLLxTxRatio} 
  ## is used. \cr\cr
  ##
  ## \bold{Working with IRSL data}\cr\cr
  ##
  ## The function was originally designed to work just for 'OSL' curves, 
  ## following the principles of the SAR protocol. An IRSL measurement protocol
  ## may follow this procedure, e.g., post-IR IRSL protocol (Thomsen et al., 2008).
  ## Therefore this functions has been enhanced to work with IRSL data, however, 
  ## the function is only capable to analyse curves that follow the SAR protocol structure, i.e., 
  ## to analyse a post-IR IRSL protocol curve data have to be pre-selected by the user to 
  ## fit the standards of the SAR protocol, i.e., Lx,Tx,Lx,Tx and so on. \cr
  ##
  ## Example: Imagine the measurement contains pIRIR50 and pIRIR225 IRSL curves. Only
  ## one curve type can be analysed at the same time: 
  ## The pIRIR50 curves or the pIRIR225 curves.\cr\cr
  ## 
  ## \bold{Provided rejection criteria}\cr\cr
  ## \sQuote{recyling.ratio}: calculated for every repeated regeneration dose point.\cr
  ## \sQuote{recuperation.rate}: recuperation rate calculated by comparing the 
  ## Lx/Tx values of the zero regeneration point with the Ln/Tn value 
  ## (the Lx/Tx ratio of the natural signal). 
  ## For methodological background see Aitken and Smith (1988)

  ##value<<
  ## A plot (optional) and an \code{\linkS4class{RLum.Results}} object is 
  ## returned containing the following elements: 
  ## \item{De.values}{\link{data.frame} containing De-values, 
  ## De-error and further parameters}
  ## \item{LnLxTnTx.values}{\link{data.frame} of all calculated Lx/Tx values 
  ## including signal, background counts and the dose points.}
  ## \item{rejection.criteria}{\link{data.frame} with values that might by 
  ## used as rejection criteria. NA is produced if no R0 dose point 
  ## exists.}\cr
  ##
  ## The output should be accessed using the function 
  ## \code{\link{get_RLum.Results}}
  
  ##references<<
  ## Aitken, M.J. & Smith, B.W., 1988. Optical dating: recuperation after bleaching. 
  ## Quaternary Science Reviews, 7, pp. 387-393.
  ##
  ## Duller, G., 2003. Distinguishing quartz and feldspar in single grain luminescence
  ## measurements. Radiation Measurements, 37 (2), pp. 161-165. 
  ##
  ## Murray, A.S. & Wintle, A.G.,  2000. Luminescence dating of quartz using an 
  ## improved single-aliquot regenerative-dose protocol. Radiation Measurements, 
  ## 32, pp. 57-73. 
  ##
  ## Thomsen, K.J., Murray, A.S., Jain, M., Boetter-Jensen, L., 2008. 
  ## Laboratory fading rates of various luminescence signals from feldspar-rich 
  ## sediment extracts. Radiation Measurements 43, 1474-1486. 
  ## doi:10.1016/j.radmeas.2008.06.002

  ##note<<
  ## This function must not be mixed up with the function 
  ## \code{\link{Analyse_SAR.OSLdata}}, which works with \link{Risoe.BINfileData-class}
  ## objects.\cr
  ## 
  ## \bold{The function currently does only support 'OSL' or 'IRSL' data!}

  ##seealso<<
  ## \code{\link{calc_OSLLxTxRatio}}, \code{\link{plot_GrowthCurve}}, 
  ## \code{\linkS4class{RLum.Analysis}}, \code{\linkS4class{RLum.Results}}
  ## \code{\link{get_RLum.Results}}

  ##keyword<<
  ## datagen
  ## plot


}, ex=function(){
  
  ##load data
  ##ExampleData.BINfileData contains two BINfileData objects 
  ##CWOSL.SAR.Data and TL.SAR.Data
  data(ExampleData.BINfileData, envir = environment())
  
  ##transform the values from the first position in a RLum.Analysis object
  object <- Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data, pos=1)
  
  ##perform SAR analysis
  results <- analyse_SAR.CWOSL(object, 
                    signal.integral.min = 1,
                    signal.integral.max = 2,
                    background.integral.min = 900,
                    background.integral.max = 1000,
                    log = "x",
                    fit.method = "EXP")
  
  ##show results 
  get_RLum.Results(results)
  
})#END OF STRUCTURE