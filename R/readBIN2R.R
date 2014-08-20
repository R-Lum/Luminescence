readBIN2R <- structure(function(#Import Risoe BIN-file into R
  ### Import a *.bin or a *.binx file produced by a Risoe DA15 and DA20 
  ### TL/OSL reader into R.
  
  # ===========================================================================
  ##author<<
  ## Sebastian Kreutzer, JLU Giessen (Germany), 
  ## Margret C. Fuchs, AWI Postdam (Germany),\cr
  
  ##section<<
  ## version 0.7
  # ===========================================================================

  file,
  ###  \link{character} (\bold{required}): bin-file name (including path), e.g. \cr
  ### [WIN]: \code{readBIN2R("C:/Desktop/test.bin")}, \cr  
  ### [MAC/LINUX]: \code{readBIN2R("/User/test/Desktop/test.bin")}
  
  show.raw.values = FALSE,  
  ### \link{logical} (with default): shows raw values from BIN file for 
  ### \code{LTYPE}, \code{DTYPE} and \code{LIGHTSOURCE} without translation in characters. 
  
  n.records,
  ###  \link{raw} (optional): limits the number of imported records. Can be used in 
  ###  combination with \code{show.record.number} for debugging purposes, e.g. corrupt BIN files.
  
  show.record.number = FALSE,
  ### \link{logical} (with default): shows record number of the imported record, 
  ### for debugging usage only. 
  
  txtProgressBar = TRUE,
  ### \link{logical} (with default): enables or disables 
  ### \code{\link{txtProgressBar}}.
  
  forced.VersionNumber
  ### \link{integer} (optional): allows to cheat the version number check in the 
  ### function by own values for cases where the BIN-file version is not supported.\cr 
  ### Note: The usage is at own risk, only supported BIN-file versions have been tested.
){

  
# Config ------------------------------------------------------------------  
  
  ##set supported BIN format version
  VERSION.supported <- as.raw(c(03, 04, 06))
  
# Set Translation Matrices ------------------------------------------------

##LTYPE
LTYPE.TranslationMatrix <- matrix(NA, nrow=14, ncol=2)
LTYPE.TranslationMatrix[,1] <- 0:13 
LTYPE.TranslationMatrix[,2] <- c("TL",
                                 "OSL",
                                 "IRSL",
                                 "M-IR",
                                 "M-VIS",
                                 "TOL",
                                 "TRPOSL",
                                 "RIR",
                                 "RBR",
                                 "USER",
                                 "POSL",
                                 "SGOSL",
                                 "RL",
                                 "XRF")

##DTYPE
DTYPE.TranslationMatrix <- matrix(NA, nrow=8, ncol=2)
DTYPE.TranslationMatrix[,1] <- 0:7
DTYPE.TranslationMatrix[,2] <- c("Natural","N+dose","Bleach",
                                 "Bleach+dose","Natural (Bleach)",
                                 "N+dose (Bleach)","Dose","Background")  


##LIGHTSOURCE
LIGHTSOURCE.TranslationMatrix <- matrix(NA, nrow=8, ncol=2)
LIGHTSOURCE.TranslationMatrix[,1] <- 0:7
LIGHTSOURCE.TranslationMatrix[,2] <- c("None",
                                       "Lamp",
                                       "IR diodes/IR Laser",
                                       "Calibration LED",
                                       "Blue Diodes",
                                       "White light",
                                       "Green laser (single grain)",
                                       "IR laser (single grain)"
                                       ) 
 
##PRESET VALUES
  CURVENO <- NA
  FNAME <- NA  
  MEASTEMP <- NA
  IRR_UNIT <- NA
  IRR_DOSERATE <- NA
  IRR_DOSERATEERR <- NA
  TIMESINCEIRR <- NA
  TIMETICK <- NA     
  ONTIME <- NA   
  STIMPERIOD <- NA
  GATE_ENABLED <- raw(length = 1)      
  GATE_START <- NA
  GATE_STOP <- NA
  PTENABLED <- raw(length = 1)
  DTENABLED <- raw(length = 1)
  DEADTIME <- NA   
  MAXLPOWER <- NA
  XRF_ACQTIME <- NA
  XRF_HV <- NA
  XRF_CURR <- NA
  XRF_DEADTIMEF <- NA
  SEQUENCE <- NA

# Open Connection ---------------------------------------------------------

##show warinig of version number check has been cheated
  
  if(missing(forced.VersionNumber) == FALSE){
    warning("[readBIN2R.R]: Argument 'forced.VersionNumber' has been used. BIN-file version might not be supported!")
  }

#open connection
con<-file(file, "rb")

   ##get information about file size
   file.size<-file.info(file)

   ##output
   cat(paste("\n[readBIN2R.R]\n\t >> ",file,sep=""), fill=TRUE)
    
   ##set progressbar
   if(txtProgressBar==TRUE){
    pb<-txtProgressBar(min=0,max=file.size$size, char="=", style=3)
   }

##read data up to the end of con

##set ID
ID<-0
  

# LOOP --------------------------------------------------------------------

##start loop for import BIN data
while(length(VERSION<-readBin(con, what="raw", 1, size=1, endian="litte"))>0) {

      ##force version number
      if(missing(forced.VersionNumber) == FALSE){
        VERSION <- as.raw(forced.VersionNumber)
      }    
  
      ##stop input if wrong VERSION    
      if((VERSION%in%VERSION.supported) == FALSE){
    
        ##close connection 
        close(con)
          
        ##show error message
        error.text <- paste("[readBIN2R] Error: The BIN format version (",VERSION,") of this file is currently not supported! Supported version numbers are: ",paste(VERSION.supported,collapse=", "),".",sep="")
        
        stop(error.text)
      
      }    
  
      ##print record ID for debugging purposes
      if(show.record.number == TRUE){
        
        cat(ID,", ")
        if(ID%%10==0){
          cat("\n")    
        }
      }     
      
      
      #empty byte position
      EMPTY<-readBin(con, what="raw", 1, size=1, endian="litte")
      
  # ==========================================================================      
  # BINX FORMAT SUPPORT -----------------------------------------------------
  if(VERSION==06){
    
    ##(1) Header size and strucutre
    ##LENGTH, PREVIOUS, NPOINTS, LTYPE
    temp <- readBin(con, what="int", 3, size=4, endian="little") 
    
    LENGTH <- temp[1]
    PREVIOUS <- temp[2]
    NPOINTS <- temp[3]
    
    ##(2) Sample characteristics
    ##RUN, SET, POSITION, GRAINNUMBER, CURVENO, XCOORD, YCOORD
    temp <- readBin(con, what="int", 7, size=2, endian="little") 
    
      RUN <- temp[1]
      SET <- temp[2]
      POSITION <- temp[3]
      GRAINNUMBER <- temp[4]
      CURVENO <- temp[5]
      XCOORD <- temp[6]
      YCOORD <- temp[7]
    
    ##SAMPLE, COMMENT
    ##SAMPLE
    SAMPLE_SIZE<-readBin(con, what="int", 1, size=1, endian="little")
    SAMPLE<-readChar(con, SAMPLE_SIZE, useBytes=TRUE) 
    #however it should be set to 20
    
    #step forward in con
    if(20-c(SAMPLE_SIZE)>0){
        STEPPING<-readBin(con, what="raw", (20-c(SAMPLE_SIZE)), 
                          size=1, endian="little")
    }
    
    ##COMMENT
    COMMENT_SIZE<-readBin(con, what="int", 1, size=1, endian="little")
    COMMENT<-readChar(con, COMMENT_SIZE, useBytes=TRUE) #set to 80 (manual)
    
    #step forward in con
    if(80-c(COMMENT_SIZE)>0){
      STEPPING<-readBin(con, what="raw", (80-c(COMMENT_SIZE)), 
                        size=1, endian="little")
    }
    
    ##(3) Instrument and sequence characteristic
    ##SYSTEMID
    SYSTEMID <- readBin(con, what="int", 1, size=2, endian="little") 
    
    ##FNAME
    FNAME_SIZE<-readBin(con, what="int", 1, size=1, endian="little")
    FNAME<-readChar(con, FNAME_SIZE, useBytes=TRUE) #set to 100 (manual)
    
    #step forward in con
    if(100-c(FNAME_SIZE)>0){
      STEPPING<-readBin(con, what="raw", (100-c(FNAME_SIZE)), 
                        size=1, endian="little")
    }
    
    ##USER
    USER_SIZE<-readBin(con, what="int", 1, size=1, endian="little")
    USER<-readChar(con, USER_SIZE, useBytes=TRUE) #set to 30 (manual)
    
    #step forward in con
    if(30-c(USER_SIZE)>0){
      STEPPING<-readBin(con, what="raw", (30-c(USER_SIZE)), 
                        size=1, endian="little")
    }
    
    ##TIME
    TIME_SIZE<-readBin(con, what="int", 1, size=1, endian="little")
    
    ##time size corrections for wrong time formats; set n to 6 for all values 
    ##accoording the handbook of Geoff Duller, 2007
    TIME_SIZE<-6
    TIME<-readChar(con, TIME_SIZE, useBytes=TRUE)
    
    
    ##DATE
    DATE_SIZE<-readBin(con, what="int", 1, size=1, endian="little")
    
    ##date size corrections for wrong date formats; set n to 6 for all values 
    ##accoording the handbook of Geoff Duller, 2007  
    DATE_SIZE<-6      
    DATE<-readChar(con, DATE_SIZE, useBytes=TRUE)
    
    
    ##(4) Analysis
    
    ##DTYPE
    DTYPE<-readBin(con, what="int", 1, size=1, endian="little")
    
    ##BL_TIME    
    BL_TIME<-readBin(con, what="double", 1, size=4, endian="little")
    
    ##BL_UNIT    
    BL_UNIT<-readBin(con, what="int", 1, size=1, endian="little")
    
    ##NORM1, NORM2, NORM3, BG
    temp <- readBin(con, what="double", 4, size=4, endian="little") 
  
      NORM1 <- temp[1]
      NORM2 <- temp[2]
      NORM3 <- temp[3]
      BG <- temp[4]
    
    ##SHIFT    
    SHIFT<- readBin(con, what="integer", 1, size=2, endian="little")
    
    ##TAG
    TAG <- readBin(con, what="int", 1, size=1, endian="little")
    
    ##RESERVED    
    RESERVED<-readBin(con, what="raw", 20, size=1, endian="little")
    
    ##(5) Measurement characteristics
    
    ##LTYPE    
    LTYPE <- readBin(con, what="int", 1, size=1, endian="little")
    
    ##LTYPESOURCE    
    LIGHTSOURCE <- readBin(con, what="int", 1, size=1, endian="little")
    
    ##LIGHTPOWER, LOW, HIGH, RATE
    temp <- readBin(con, what="double", 4, size=4, endian="little")
      
      LIGHTPOWER <- temp[1]
      LOW <- temp[2]
      HIGH <- temp[3]
      RATE <- temp[4]
    
    ##TEMPERATURE
    TEMPERATURE <- readBin(con, what="int", 1, size=2, endian="little")
    
    ##MEASTEMP
    MEASTEMP <- readBin(con, what="integer", 1, size=2, endian="little")
    
    ##AN_TEMP
    AN_TEMP <- readBin(con, what="double", 1, size=4, endian="little")
    
    ##AN_TIME
    AN_TIME <- readBin(con, what="double", 1, size=4, endian="little")
    
    ##DELAY, ON, OFF
    temp <- readBin(con, what="int", 3, size=2, endian="little")
    
      TOLDELAY <- temp[1]
      TOLON <- temp[2]
      TOLOFF <- temp[3]
    
    ##IRR_TIME
    IRR_TIME <- readBin(con, what="double", 1, size=4, endian="little")
    
    ##IRR_TYPE
    IRR_TYPE <- readBin(con, what="int", 1, size=1, endian="little")
    
    ##IRR_DOSERATE
    IRR_DOSERATE <- readBin(con, what="double", 1, size=4, endian="little")
    
    ##IRR_DOSERATEERR
    IRR_DOSERATEERR <- readBin(con, what="double", 1, size=4, endian="little")
    
    ##TIMESINCEIRR
    TIMESINCEIRR <- readBin(con, what="integer", 1, size=4, endian="little")
    
    ##TIMETICK
    TIMETICK <- readBin(con, what="double", 1, size=4, endian="little")
    
    ##ONTIME
    ONTIME <- readBin(con, what="integer", 1, size=4, endian="little")
    
    ##STIMPERIOD
    STIMPERIOD <- readBin(con, what="integer", 1, size=4, endian="little")
    
    ##GATE_ENABLED
    GATE_ENABLED <- readBin(con, what="raw", 1, size=1, endian="little")
    
    ##GATE_START
    GATE_START <- readBin(con, what="integer", 1, size=4, endian="little")
    
    ##GATE_STOP
    GATE_STOP <- readBin(con, what="integer", 1, size=4, endian="little")
    
    ##PTENABLED
    PTENABLED <- readBin(con, what="raw", 1, size=1, endian="little")
    
    ##DTENABLED
    DTENABLED <- readBin(con, what="raw", 1, size=1, endian="little")
    
    ##DEADTIME, MAXLPOWER, XRF_ACQTIME, XRF_HV
    temp <- readBin(con, what="double", 4, size=4, endian="little")
    
      DEADTIME <- temp[1]
      MAXLPOWER <- temp[2]
      XRF_ACQTIME <- temp[3]
      XRF_HV <- temp[4]
    
    ##XRF_CURR
    XRF_CURR <- readBin(con, what="integer", 1, size=4, endian="little")
    
    ##XRF_DEADTIMEF
    XRF_DEADTIMEF <- readBin(con, what="double", 1, size=4, endian="little")
    
    ##RESERVED    
    RESERVED<-readBin(con, what="raw", 24, size=1, endian="little")
    
    #DPOINTS
    DPOINTS<-readBin(con, what="integer", NPOINTS, size=4, endian="little")
    
  }else{
  ## =========================================================================    
  ##START BIN FILE FORMAT SUPPORT  
 
  ##LENGTH, PREVIOUS, NPOINTS, LTYPE
  temp <- readBin(con, what="int", 3, size=2, endian="little") 
   
        LENGTH <- temp[1]
        PREVIOUS <- temp[2]
        NPOINTS <- temp[3]
   
  ##LTYPE    
  LTYPE<-readBin(con, what="int", 1, size=1, endian="little")


  ##LOW, HIGH, RATE
  temp <- readBin(con, what="double", 3, size=4, endian="little")
      
      LOW <- temp[1]
      HIGH <- temp[2]
      RATE <- temp[3]

      
  TEMPERATURE<-readBin(con, what="integer", 1, size=2, endian="little")
      
  ##XCOORD, YCOORD, TOLDELAY, TOLON, TOLOFF
  temp <- readBin(con, what="integer", 5, size=2, endian="little")
      
      XCOORD <- temp[1]
      YCOORD <- temp[2]
      TOLDELAY <- temp[3]
      TOLON <- temp[4]
      TOLOFF <- temp[5]
      
  
  ##POSITION
  POSITION<-readBin(con, what="int", 1, size=1, endian="little")
      
  ##RUN    
  RUN<-readBin(con, what="int", 1, size=1, endian="little")
    
  ##TIME
  TIME_SIZE<-readBin(con, what="int", 1, size=1, endian="little")
   
    ##time size corrections for wrong time formats; set n to 6 for all values 
    ##accoording the handbook of Geoff Duller, 2007
    TIME_SIZE<-6
    TIME<-readChar(con, TIME_SIZE, useBytes=TRUE)
 
   
  ##DATE
  DATE_SIZE<-readBin(con, what="int", 1, size=1, endian="little")
        
    ##date size corrections for wrong date formats; set n to 6 for all values 
    ##accoording the handbook of Geoff Duller, 2007  
    DATE_SIZE<-6      
    DATE<-readChar(con, DATE_SIZE, useBytes=TRUE)
     
     
  ##SEQUENCE
  SEQUENCE_SIZE<-readBin(con, what="int", 1, size=1, endian="little")
  SEQUENCE<-readChar(con, SEQUENCE_SIZE, useBytes=TRUE)
  
     #step forward in con
     if(8-SEQUENCE_SIZE>0){
      STEPPING<-readBin(con, what="raw", (8-c(SEQUENCE_SIZE)),size=1, endian="little")
      }
   
      
  ##USER
  USER_SIZE<-readBin(con, what="int", 1, size=1, endian="little")   
  USER<-readChar(con, USER_SIZE, useBytes=FALSE)
      
    #step forward in con
    if(8-c(USER_SIZE)>0){
      STEPPING<-readBin(con, what="raw", (8-c(USER_SIZE)), size=1, endian="little")
    }
    
  ##DTYPE
  DTYPE<-readBin(con, what="int", 1, size=1, endian="little")
      
  ##IRR_TIME
  IRR_TIME<-readBin(con, what="double", 1, size=4, endian="little") 
      
  ##IRR_TYPE    
  IRR_TYPE<-readBin(con, what="int", 1, size=1, endian="little")
    
  ##IRR_UNIT    
  IRR_UNIT<-readBin(con, what="int", 1, size=1, endian="little")
      
  ##BL_TIME    
  BL_TIME<-readBin(con, what="double", 1, size=4, endian="little")
      
  ##BL_UNIT    
  BL_UNIT<-readBin(con, what="int", 1, size=1, endian="little")
      
  ##AN_TEMP, AN_TIME, NORM1, NORM2, NORM3, BG
  temp <- readBin(con, what="double", 6, size=4, endian="little")  
      
      AN_TEMP <- temp[1]
      AN_TIME <- temp[2]
      NORM1 <- temp[3]
      NORM2 <- temp[4]
      NORM3 <- temp[5]
      BG <- temp[6]
              
  ##SHIFT    
  SHIFT<-readBin(con, what="integer", 1, size=2, endian="little")

  ##SAMPLE
  SAMPLE_SIZE<-readBin(con, what="int", 1, size=1, endian="little")
  SAMPLE<-readChar(con, SAMPLE_SIZE, useBytes=TRUE) #however it should be set to 20
          
    #step forward in con
    if(20-c(SAMPLE_SIZE)>0){
     STEPPING<-readBin(con, what="raw", (20-c(SAMPLE_SIZE)), size=1, endian="little")
    }
      
  ##COMMENT
  COMMENT_SIZE<-readBin(con, what="int", 1, size=1, endian="little")
  COMMENT<-readChar(con, COMMENT_SIZE, useBytes=TRUE) #set to 80 (manual)
  
    #step forward in con
    if(80-c(COMMENT_SIZE)>0){
     STEPPING<-readBin(con, what="raw", (80-c(COMMENT_SIZE)), size=1, endian="little")
    }
      
  ##LIGHTSOURCE, SET, TAG
  temp <- readBin(con, what="int", 3, size=1, endian="little")
      
      LIGHTSOURCE <- temp[1]
      SET <- temp[2]
      TAG <- temp[3]
            
    
  ##GRAIN    
  GRAIN<-readBin(con, what="integer", 1, size=2, endian="little")
  
  ##LPOWER    
  LPOWER<-readBin(con, what="double", 1, size=4, endian="little")  

  ##SYSTEMID    
  SYSTEMID<-readBin(con, what="integer", 1, size=2, endian="little")
  
  ##RESERVED    
  RESERVED<-readBin(con, what="raw", 54, size=1, endian="little")

  #DPOINTS
  DPOINTS<-readBin(con, what="integer", NPOINTS, size=4, endian="little")
      
  
  
  }#endif:format support       
  ##END BIN FILE FORMAT SUPPORT
  ## ==========================================================================#
  
  
  #SET UNIQUE ID
  ID<-ID+1    
  
  ##update progress bar
  if(txtProgressBar==TRUE){
    setTxtProgressBar(pb, seek(con,origin="current"))
  }
      
  ##set for equal values with different names
  if(exists("GRAINNUMBER") == TRUE){GRAIN <- GRAINNUMBER}
  if(exists("GRAIN") == TRUE){GRAINNUMBER <- GRAIN}
  
  if(exists("LIGHTPOWER") == TRUE){LPOWER <- LIGHTPOWER}
  if(exists("LPOWER") == TRUE){LIGHTPOWER <- LPOWER}    
           
  ##set data.frame for output or append data on data.frame    
  if(exists("results")==FALSE) {
    results<-data.frame(ID=ID,
                        SEL=TRUE,
                        VERSION=VERSION,
                        LENGTH=LENGTH,
                        PREVIOUS=PREVIOUS,
                        NPOINTS=NPOINTS,
                        
                        RUN=RUN,
                        SET=SET,
                        POSITION=POSITION,
                        GRAIN = GRAIN  ,
                        GRAINNUMBER = GRAIN,
                        CURVENO = CURVENO,
                        XCOORD=XCOORD,
                        YCOORD=YCOORD,
                        SAMPLE=SAMPLE,
                        COMMENT=COMMENT,
                        
                        SYSTEMID=SYSTEMID,
                        FNAME = FNAME,
                        USER=USER,
                        TIME=TIME,
                        DATE=DATE,
                        
                        DTYPE=DTYPE,
                        BL_TIME=BL_TIME,
                        BL_UNIT=BL_UNIT,
                        NORM1=NORM1,
                        NORM2=NORM2,
                        NORM3=NORM3,
                        BG=BG,
                        SHIFT=SHIFT,
                        TAG=TAG,
                        
                        LTYPE = LTYPE,
                        LIGHTSOURCE = LIGHTSOURCE,
                        LPOWER = LPOWER,
                        LIGHTPOWER = LIGHTPOWER,
                        LOW = LOW,
                        HIGH = HIGH,
                        RATE = RATE,
                        TEMPERATURE = TEMPERATURE,
                        MEASTEMP = MEASTEMP,
                        AN_TEMP=AN_TEMP,
                        AN_TIME=AN_TIME,                        
                        TOLDELAY=TOLDELAY,
                        TOLON=TOLON,
                        TOLOFF=TOLOFF,
                        IRR_TIME = IRR_TIME,
                        IRR_TYPE = IRR_TYPE,
                        IRR_UNIT = IRR_UNIT,
                        IRR_DOSERATE = IRR_DOSERATE,
                        IRR_DOSERATEERR = IRR_DOSERATEERR,
                        TIMESINCEIRR = TIMESINCEIRR,
                        TIMETICK = TIMETICK, 
                        ONTIME = ONTIME, 
                        STIMPERIOD = STIMPERIOD, 
                        GATE_ENABLED = GATE_ENABLED, 
                        GATE_START  = GATE_START, 
                        GATE_STOP = GATE_STOP, 
                        PTENABLED = PTENABLED, 
                        DTENABLED = DTENABLED, 
                        DEADTIME = DEADTIME, 
                        MAXLPOWER = MAXLPOWER, 
                        XRF_ACQTIME = XRF_ACQTIME, 
                        XRF_HV = XRF_HV, 
                        XRF_CURR = XRF_CURR, 
                        XRF_DEADTIMEF = XRF_DEADTIMEF,  
                        
                        SEQUENCE=SEQUENCE
                      
                      ) #end set data.frame
                      
                      #set variable for DPOINTS handling
                      DATA<-list(DPOINTS)
  }else{
  temp<-data.frame(ID=ID,
                   SEL=TRUE,
                   VERSION=VERSION,
                   LENGTH=LENGTH,
                   PREVIOUS=PREVIOUS,
                   NPOINTS=NPOINTS,
                   
                   RUN=RUN,
                   SET=SET,
                   POSITION=POSITION,
                   GRAIN = GRAIN  ,
                   GRAINNUMBER = GRAIN,
                   CURVENO = CURVENO,
                   XCOORD=XCOORD,
                   YCOORD=YCOORD,
                   SAMPLE=SAMPLE,
                   COMMENT=COMMENT,
                   
                   SYSTEMID=SYSTEMID,
                   FNAME = FNAME,
                   USER=USER,
                   TIME=TIME,
                   DATE=DATE,
                   
                   DTYPE=DTYPE,
                   BL_TIME=BL_TIME,
                   BL_UNIT=BL_UNIT,
                   NORM1=NORM1,
                   NORM2=NORM2,
                   NORM3=NORM3,
                   BG=BG,
                   SHIFT=SHIFT,
                   TAG=TAG,
                   
                   LTYPE = LTYPE,
                   LIGHTSOURCE = LIGHTSOURCE,
                   LPOWER = LPOWER,
                   LIGHTPOWER = LIGHTPOWER,
                   LOW = LOW,
                   HIGH = HIGH,
                   RATE = RATE,
                   TEMPERATURE = TEMPERATURE,
                   MEASTEMP = MEASTEMP,
                   AN_TEMP=AN_TEMP,
                   AN_TIME=AN_TIME,                        
                   TOLDELAY=TOLDELAY,
                   TOLON=TOLON,
                   TOLOFF=TOLOFF,
                   IRR_TIME = IRR_TIME,
                   IRR_TYPE = IRR_TYPE,
                   IRR_UNIT = IRR_UNIT,
                   IRR_DOSERATE = IRR_DOSERATE, 
                   IRR_DOSERATEERR = IRR_DOSERATEERR,
                   TIMESINCEIRR = TIMESINCEIRR,
                   TIMETICK = TIMETICK, 
                   ONTIME = ONTIME, 
                   STIMPERIOD = STIMPERIOD, 
                   GATE_ENABLED = GATE_ENABLED, 
                   GATE_START  = GATE_START, 
                   GATE_STOP = GATE_STOP, 
                   PTENABLED = PTENABLED, 
                   DTENABLED = DTENABLED, 
                   DEADTIME = DEADTIME, 
                   MAXLPOWER = MAXLPOWER, 
                   XRF_ACQTIME = XRF_ACQTIME, 
                   XRF_HV = XRF_HV, 
                   XRF_CURR = XRF_CURR, 
                   XRF_DEADTIMEF = XRF_DEADTIMEF,  
                   
                   SEQUENCE=SEQUENCE)
                
                #combine DPOINT values and the rest of the data
                DATA<-c(DATA,list(DPOINTS))
                results<-rbind(results,temp)
  }#end else  
  
      
  ##BREAK    
  ##stop loop if record limit is reached  
  if(missing(n.records)==FALSE){
        
      if(n.records==ID){break()}
        
  }  
      
  
}#endwhile::end lopp 


##close con 
close(con)

##close
if(txtProgressBar==TRUE){close(pb)}

##output
cat(paste("\t >> ",ID," records have been read successfully!\n\n", sep=""))

##produce S4 object for output
object <- new("Risoe.BINfileData",
                    METADATA=results,
                    DATA=DATA
                    )

##============================================================================##
# Convert Translation Matrix Values ---------------------------------------


if(show.raw.values == FALSE) {
##LTYPE
object@METADATA[,"LTYPE"]<- sapply(1:length(object@METADATA[,"LTYPE"]),function(x){
   
  as.character(LTYPE.TranslationMatrix[object@METADATA[x,"LTYPE"]==LTYPE.TranslationMatrix[,1],2])
  
})

##TIME CONVERSION

object@METADATA[,"TIME"]<- sapply(1:length(object@METADATA[,"TIME"]),function(x){

  format(strptime(as.character(object@METADATA[x,"TIME"]),"%H%M%S"),"%H:%M:%S")   

})

##DTYPE CONVERSION
object@METADATA[,"DTYPE"]<- sapply(1:length(object@METADATA[,"DTYPE"]),function(x){
  
  as.character(DTYPE.TranslationMatrix[object@METADATA[x,"DTYPE"]==DTYPE.TranslationMatrix[,1],2])
  
})

##LIGHTSOURCE CONVERSION
object@METADATA[,"LIGHTSOURCE"]<- sapply(1:length(object@METADATA[,"LIGHTSOURCE"]),function(x){
  
  as.character(LIGHTSOURCE.TranslationMatrix[object@METADATA[x,"LIGHTSOURCE"]==LIGHTSOURCE.TranslationMatrix[,1],2])
  
})
}

##return values
return(object)
  # DOCUMENTATION - INLINEDOC LINES -----------------------------------------

  ##details<<
  ## The binary data file is parsed byte by byte following the data structure 
  ## published in the Appendices of the Analyst manual p. 42.\cr\cr
  ## For the general BIN-file structure, the reader is referred to the Risoe
  ## website: \code{http://www.nutech.dtu.dk/}

  ##value<<
  ## Returns an S4 \link{Risoe.BINfileData-class} object containing two slots:\cr
  ## \item{METADATA}{A \link{data.frame} containing all variables stored in the bin-file.}
  ## \item{DATA}{A \link{list} containing a numeric \link{vector} of the measured data. 
  ## The ID corresponds to the record ID in METADATA.}

  ##references<<
  ## Duller, G., 2007. Analyst. \url{http://www.nutech.dtu.dk/english/~/media/Andre_Universitetsenheder/Nutech/Produkter%20og%20services/Dosimetri/radiation_measurement_instruments/tl_osl_reader/Manuals/analyst_manual_v3_22b.ashx}

  ##note<<
  ## The function has been successfully tested for BIN format versions 03, 04 and 06. 
  ## The version number depends on the used Sequence Editor.\cr\cr
  ## \bold{Other BIN format versions are currently not supported}. 

  ##seealso<<
  ## \code{\link{writeR2BIN}}, \code{\linkS4class{Risoe.BINfileData}},
  ## \code{\link{readBin}}, \code{\link{merge_Risoe.BINfileData}}, \code{\link{txtProgressBar}}

  ##keyword<<
  ## IO

}, ex=function(){
  
  ##(1) import Risoe BIN-file to R (uncomment for usage)

  #FILE <- file.choose()
  #temp <- readBIN2R(FILE)
  #temp
  
})  
