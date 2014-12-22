plot_GrowthCurve <- structure(function(# Fit and plot a growth curve for luminescence data (Lx/Tx against dose)
  ### A dose response curve is produced for luminescence measurements using a 
  ### regenerative protocol.
  
  # ===========================================================================
  ##author<<
  ## Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France), \cr
  ## Michael Dietze, GFZ Potsdam (Germany), \cr
  
  ##section<<
  ##version 1.2.15
  # ===========================================================================
  
  sample,
  ### \code{\link{data.frame}} (\bold{required}): data frame with three columns
  ### for x=Dose,y=LxTx,z=LxTx.Error, y1=TnTx. The column for the test dose 
  ### response is optional, but requires 'TnTx' as column name if used.

  na.rm = TRUE,
  ### \code{\link{logical}} (with default): excludes \code{NA} values from the data 
  ### set prior to any further operations.
  
  main = "Growth curve",
  ### \code{\link{character}} (with default): header of the plot.
  
  fit.method = "EXP", 
  ### \code{\link{character}} (with default): function used for fitting. 
  ### Possible options are: \code{LIN}, \code{EXP}, \code{EXP OR LIN}, 
  ### \code{EXP+LIN} or \code{EXP+EXP}. See details.
  
  fit.weights = TRUE,
  ### \code{\link{logical}} (with default): option whether the fitting is 
  ### done with or without weights. See details.
	
  fit.includingRepeatedRegPoints = TRUE, 
  ### \code{\link{logical}} (with default): includes repeated points for 
  ### fitting (\code{TRUE}/\code{FALSE}).
	
  fit.NumberRegPoints,
  ### \code{\link{integer}} (optional): set number of regeneration points 
  ### manually. By default the number of all (!) regeneration points is 
  ### used automatically.
	
  fit.NumberRegPointsReal,
  ### \code{\link{integer}} (optional): if the number of regeneration points 
  ### is provided manually, the value of the real, regeneration points = 
  ### all points (repeated points) including reg 0, has to be inserted.
  
  fit.bounds = TRUE,
  ### \code{\link{logical}} (with default): set lower fit bounds for all fitting 
  ### parameters to 0. Limited for the use with the fit methods \code{EXP}, 
  ### \code{EXP+LIN} and \code{EXP OR LIN}. Argument to be inserted for 
  ### experimental application only!
	
  NumberIterations.MC = 100, 
  ### \code{\link{integer}} (with default): number of Monte Carlo simulations 
  ### for error estimation. See details.
		
  output.plot = TRUE, 
  ### \code{\link{logical}} (with default): plot output (\code{TRUE/FALSE}).
  
  output.plotExtended = TRUE,
  ### \code{\link{logical}} (with default): If \code{TRUE}, 3 plots on one plot 
  ### area are provided: (1) growth curve, (2) histogram from Monte 
  ### Carlo error simulation and (3) a test dose response plot. If \code{FALSE}, 
  ### just the growth curve will be plotted. \bold{Requires:} 
  ### \code{output.plot = TRUE}.
  
  output.plotExtended.single = FALSE,
  ### \code{\link{logical}} (with default): 
  ### single plot output (\code{TRUE/FALSE}) to allow for plotting the results 
  ### in single plot windows. 
  ### Requires \code{output.plot = TRUE} and \code{output.plotExtended = TRUE}.
  
  cex.global = 1,
  ### \code{\link{numeric}} (with default): global scaling factor.
  
  ...
  ### Further arguments and graphical parameters to be passed. Note: Standard 
  ### arguments will only be passed to the growth curve plot
) {

  ##1. check if sample is data.frame
  if(is.data.frame(sample)==FALSE){
    stop("\n [plot_GrowthCurve()] Sample has to be of type data.fame!")
  }
  
  ##2. check if sample contains a least three rows 
  if(length(sample[,1])<3){
    stop("\n [plot_GrowthCurve()] At least two regeneration points are needed!")
  }
  
  ## optionally, count and exclude NA values and print result
  if(na.rm == TRUE) {
    n.NA <- sum(!complete.cases(sample))
    if(n.NA == 1) {cat("\n [plot_GrowthCurve()] 1 NA value excluded.")
    } else if(n.NA > 1) {cat(paste("\n [plot_GrowthCurve()]", n.NA, "NA values excluded."))}
  
    sample <- na.exclude(sample)
    
    ##Check if anything is left after removal
    if(nrow(sample) == 0){
      
      stop("[plot_GrowthCurve()] Sorry, after NA removal nothing is left from the data set!")
      
    }
  
  }
  
  
  ##NULL values in the data.frame are not allowed for the y-column
    if(length(sample[sample[,2]==0,2])>0){
      cat("\n[plot_GrowthCurve()] Warning:",
          length(sample[sample[,2]==0,2]),"values with 0 for Lx/Tx detected; replaced by 0.0001.\n")
      sample[sample[,2]==0,2]<-0.0001 
    }
  
##1. INPUT
  
  #1.0.1 calculate number of reg points if not set
  if(missing(fit.NumberRegPoints)==TRUE){fit.NumberRegPoints<-length(sample[-1,1])}
  if(missing(fit.NumberRegPointsReal)==TRUE){
    
    fit.RegPointsReal <- as.integer(
      rownames(sample[-which(duplicated(sample[,1]) | sample[,1]==0),]))

    fit.NumberRegPointsReal <- length(fit.RegPointsReal)
    
  }
 
  #1.1 Produce dataframe from input values
  xy<-data.frame(x=sample[2:(fit.NumberRegPoints+1),1],y=sample[2:(fit.NumberRegPoints+1),2])
	y.Error<-sample[2:(fit.NumberRegPoints+1),3]
  
  ##1.1.1 produce weights for weighted fitting
  if(fit.weights==TRUE){
    fit.weights<-1/y.Error/(sum(1/y.Error))
  }else{
    fit.weights<-NULL
  }

  # Deal with extra arguments -----------------------------------------------
  ##deal with addition arguments 
  extraArgs <- list(...) 

  main <- if("main" %in% names(extraArgs)) {extraArgs$main} else 
  {"Growth curve"}

  xlab <- if("xlab" %in% names(extraArgs)) {extraArgs$xlab} else 
  {"Dose [s]"}

  ylab <- if("ylab" %in% names(extraArgs)) {extraArgs$ylab} else
  {expression(L[x]/T[x])}

  if("cex" %in% names(extraArgs)) {cex.global <- extraArgs$cex} 

  ylim <- if("ylim" %in% names(extraArgs)) {extraArgs$ylim} else 
  {c(min(xy$y)-max(y.Error),(max(xy$y)+if(max(xy$y)*0.1>1.5){1.5}else{max(xy$y)*0.2}))}

  xlim <- if("xlim" %in% names(extraArgs)) {extraArgs$xlim} else 
  {c(0,(max(xy$x)+if(max(xy$x)*0.4>50){50}else{max(xy$x)*0.4}))}

  fun   <- if("fun" %in% names(extraArgs)) {extraArgs$fun} else {FALSE}


	#1.2 Prepare datasets for Monte Carlo Simulation

		data.MC<-t(matrix(sapply(seq(2,fit.NumberRegPoints+1,by=1), 
									function(x){sample(rnorm(10000,mean=sample[x,2], sd=abs(sample[x,3])), 
                            NumberIterations.MC, replace=TRUE)}), nrow=NumberIterations.MC
						     )#end matrix
					)#end transpose matrix
									
	#1.3 set x.natural
		x.natural<-as.vector(seq(1:NumberIterations.MC))
  
  ##1.4 set initialise variables
  De <- NA
  De.Error <- NA
 
##============================================================================##
# FITTING ----------------------------------------------------------------------
##============================================================================##
##3. Fitting values with nonlinear least-squares estimation of the parameters
 
  ##set functions for fitting

  #EXP
	fit.functionEXP<-function(a,b,c,x) {a*(1-exp(-(x+c)/b))}

	#EXP+LIN
	fit.functionEXPLIN<-function(a,b,c,g,x) {a*(1-exp(-(x+c)/b)+(g*x))}
 
  #EXP+EXP
  fit.functionEXPEXP<-function(a1,a2,b1,b2,x){(a1*(1-exp(-(x)/b1)))+(a2*(1-exp(-(x)/b2)))}
  
	##input data for fitting; exclude repeated RegPoints 
  if(fit.includingRepeatedRegPoints==FALSE){
    data<-data.frame(x=xy[-which(duplicated(xy[,1])),1],y=xy[-which(duplicated(xy[,1])),2])
   }else{data<-data.frame(xy)}
  

  ##START PARAMETER ESTIMATION
  ##--------------------------------------------------------------------------##
  ##general setting of start parameters for fitting
  
	   ##a - estimation for a a the maxium of the y-values (Lx/Tx)
     a <- max(data[,2])

     ##b - get start parameters from a linear fit of the log(y) data
     ##    (suppress the warning in case one parameter is negative)

     fit.lm <- lm(suppressWarnings(log(data$y))~data$x)
     b <- as.numeric(1/fit.lm$coefficients[2])   

	   ##c - get start parameters from a linear fit - offset on x-axis
	   fit.lm<-lm(data$y~data$x)
     c <- as.numeric(abs(fit.lm$coefficients[1]/fit.lm$coefficients[2]))
       
     #take slope from x - y scaling
		 g <- max(data[,2]/max(data[,1]))
  
     #set D01 and D02 (in case of EXp+EXP)
     D01 <- NA; D02 <- NA
  
  ##--------------------------------------------------------------------------##
  ##to be a little bit more flexible the start parameters varries within a normal distribution
  
      ##draw 50 start values from a normal distribution a start values
      a.MC<-rnorm(50,mean=a,sd=a/100)
      b.MC<-rnorm(50,mean=b,sd=b/100)
      c.MC<-rnorm(50,mean=c,sd=c/100)
      g.MC<-rnorm(50,mean=g,sd=g/1)
    
      ##set start vector (to avoid errors witin the loop)
      a.start<-NA; b.start<-NA; c.start<-NA; g.start<-NA
  

  
  ##--------------------------------------------------------------------------##
    
	#===========================================================================##
	#EXP#

	if (fit.method=="EXP" | fit.method=="EXP OR LIN" | fit.method=="LIN"){
      
        if(fit.method!="LIN" & length(data[,1])>3){
  
					##FITTING on GIVEN VALUES##
					#	--use classic R fitting routine to fit the curve
          
          ##try to create some start parameters from the input values to make 
          ## the fitting more stable
          for(i in 1:50){
         
            a<-a.MC[i];b<-b.MC[i];c<-c.MC[i]
          
            fit<-try(nls(y~fit.functionEXP(a,b,c,x),
                         data=data,
                         start=c(a=a,b=b,c=c),
                         trace=FALSE,
                         algorithm="port",
                         lower=c(a=0,b>0,c=0),
                         nls.control(maxiter=100,warnOnly=FALSE,minFactor=1/2048) #increase max. iterations
                         ),silent=TRUE)
           
            if(class(fit)!="try-error"){
              #get parameters out of it
              parameters<-(coef(fit)) 
              b.start[i]<-as.vector((parameters["b"]))
              a.start[i]<-as.vector((parameters["a"])) 
              c.start[i]<-as.vector((parameters["c"]))
            }
          }
          
          ##used mean as start parameters for the final fitting
          a<-median(na.exclude(a.start));b<-median(na.exclude(b.start));c<-median(na.exclude(c.start))
          
          #FINAL Fit curve on given values
					fit<-try(nls(y~fit.functionEXP(a,b,c,x),
									data=data,
									start=c(a=a,b=b,c=c),
                  weights=fit.weights,
									trace=FALSE,
									algorithm="port",
                  lower = if(fit.bounds==TRUE){lower=c(a=0,b=0,c=0)}else{c()},
									nls.control(maxiter=500)
          				))#end nls
              
							if (class(fit)=="try-error"){
                
							  writeLines("[plot_GrowthCurve()] >> try-error for EXP fit")
                
							}else{
							  #get parameters out of it
							  parameters<-(coef(fit)) 
							  b<-as.vector((parameters["b"]))
							  a<-as.vector((parameters["a"])) 
							  c<-as.vector((parameters["c"]))
					
                
                
							#calculate De 
							De<-round(-c-b*log(1-sample[1,2]/a), digits=2)
                
              #print D01 value
							D01<-round(b,digits=2) 
              writeLines(paste0("[plot_GrowthCurve()] >> D01 = ",D01, " | De = ", De)) 
            

						##Monte Carlo Simulation
						#	--Fit many curves and calculate a new De +/- De_Error
						#	--take De_Error

						#set variables
						var.b<-vector(mode="numeric", length=NumberIterations.MC)
						var.a<-vector(mode="numeric", length=NumberIterations.MC)
						var.c<-vector(mode="numeric", length=NumberIterations.MC)
						
						#start loop
						for (i in 1:NumberIterations.MC) { 
							
							data<-data.frame(x=xy$x,y=data.MC[,i])

							fit.MC<-try(nls(y~fit.functionEXP(a,b,c,x),
								data=data,
								start=c(a=a,b=b,c=c),
                weights=fit.weights,
								trace=FALSE,
								algorithm="port",
								nls.control(maxiter=500) #increase max. iterations
							),silent=TRUE) #end nls

							#get parameters out of it including error handling
							if (class(fit.MC)=="try-error") {
								
								x.natural[i]<-NA

							}else {
								
								#get parameters out 
								parameters<-coef(fit.MC) 
								var.b[i]<-as.vector((parameters["b"]))
								var.a[i]<-as.vector((parameters["a"])) #Imax
								var.c[i]<-as.vector((parameters["c"]))
							
								#calculate x.natural
								x.natural[i]<-round(-var.c[i]-var.b[i]*log(1-sample[1,2]/var.a[i]), digits=2)
                
						  }

						}#end for loop             
					}#endif::try-error fit    
        }#endif:fit.method!="LIN"
       #========================================================================
	     #LIN#
			 ##two options: just linear fit or LIN fit after the EXP fit failed
                
         #set fit object, if fit objekt was not set before
         if(exists("fit")==FALSE){fit<-NA}
        
		   if ((fit.method=="EXP OR LIN" & class(fit)=="try-error") | 
             fit.method=="LIN" | length(data[,1])<3) {
		     
            #calculate De 
					  De <- round((sample[1,2]-fit.lm$coefficients[1])/fit.lm$coefficients[2], digits=2)
            
            ##remove vector labels
            De <- as.numeric(as.character(De))
            
            #start loop for Monte Carlo Error estimation
            for (i in 1:NumberIterations.MC) { 
  
                data <- data.frame(x=xy$x, y=data.MC[,i])
                fit.lmMC <- lm(data$y~data$x, weights=abs(fit.weights))
  
                #calculate x.natural
                x.natural[i]<-round((sample[1,2]-fit.lmMC$coefficients[1])/
                                      fit.lmMC$coefficients[2], digits=2)
            }#endfor::loop for MC
            
            #correct for fit.method
            fit.method<-"LIN"
            
            ##set fit object 
            if(fit.method=="LIN"){fit<-fit.lm}
                        
      }else{fit.method<-"EXP"}#endif::LIN
    }#end if EXP (this includes the LIN fit option)
		#===========================================================================
    #===========================================================================
		#EXP+LIN#
		else if (fit.method=="EXP+LIN") {
      
             
            ##try some start parameters from the input values to makes the fitting more stable
		        for(i in 1:length(a.MC)){
                    
                   a<-a.MC[i];b<-b.MC[i];c<-c.MC[i];g<-g.MC[i]
                   
                   ##---------------------------------------------------------##
                   ##start: with EXP function
                   fit.EXP<-try(nls(y~fit.functionEXP(a,b,c,x),
                                data=data,
                                start=c(a=a,b=b,c=c),
                                trace=FALSE,
                                algorithm="port",
                                lower=c(a=0,b>10,c=0),
                                nls.control(maxiter=100,warnOnly=FALSE,minFactor=1/1048)
                   ),silent=TRUE)
                 
                   
                    if(class(fit.EXP)!="try-error"){
                       #get parameters out of it
                       parameters<-(coef(fit.EXP)) 
                       b<-as.vector((parameters["b"]))
                       a<-as.vector((parameters["a"])) 
                       c<-as.vector((parameters["c"]))
                      
                   ##end: with EXP function
                   ##---------------------------------------------------------##
                   }
                    
                   
                    fit<-try(nls(y~fit.functionEXPLIN(a,b,c,g,x),
		                  data=data,
		                  start=c(a=a,b=b,c=c,g=g),
		                  trace=FALSE,
		                  algorithm="port",
                      lower = if(fit.bounds==TRUE){lower=c(a=0,b>10,c=0,g=0)}else{c()},             
		                  nls.control(maxiter=500,warnOnly=FALSE,minFactor=1/2048) #increase max. iterations
		                 ),silent=TRUE)
                   
                     if(class(fit)!="try-error"){
                     #get parameters out of it
                     parameters<-(coef(fit)) 
                     b.start[i]<-as.vector((parameters["b"]))
                     a.start[i]<-as.vector((parameters["a"])) 
                     c.start[i]<-as.vector((parameters["c"]))
                     g.start[i]<-as.vector((parameters["g"])) 
                     }
                
    
                   
             }##end for loop
            
           
            ##used mean as start parameters for the final fitting
            a<-median(na.exclude(a.start))
            b<-median(na.exclude(b.start))
            c<-median(na.exclude(c.start))
            g<-median(na.exclude(g.start))
                    
            
            fit<-try(nls(y~fit.functionEXPLIN(a,b,c,g,x),
  			  		data=data,
					  	start=c(a=a,b=b,c=c,g=g),
						  trace=FALSE,
              weights=fit.weights,
						  algorithm="port",
              lower = if(fit.bounds==TRUE){lower=c(a=0,b>10,c=0,g=0)}else{c()},           
						  nls.control(maxiter=500,warnOnly=FALSE,minFactor=1/2048) #increase max. iterations
						  ))

        #if try error stop calculation        
        if(class(fit)!="try-error"){
              
						#get parameters out of it
						parameters<-(coef(fit)) 
						b<-as.vector((parameters["b"]))
						a<-as.vector((parameters["a"])) 
						c<-as.vector((parameters["c"]))
						g<-as.vector((parameters["g"]))
      
						#problem: analytically it is not easy to calculate x, here a simple approximation is made
						
              #calculate absolut differences from LnTn
							differences <- data.frame(dose=xy$x,differences=(sample[1,2]-
                (round(fit.functionEXPLIN(a,b,c,g,x=xy$x),digits=3))))
		         
							#set upper and lower boundary for searching (really timesaving)
							boundary.upper<-unique(differences[differences[,2]==max(differences[differences[,2]<=0,2]),])
							boundary.lower<-unique(differences[differences[,2]==min(differences[differences[,2]>=0,2]),])
            
							#case that there is no upper regeneration point...set a artificial point 20% above the highest reg Point 
							if (length(boundary.upper[,1])==0){
											artificialRegPoint<-max(xy$x)+max(xy$x)*0.2
											boundary.upper[1,1]<-artificialRegPoint									
											}
         
							#write all boundary values in a vector
							i <- seq(boundary.lower[,1],boundary.upper[,1], by=0.01)
					    
							#produce an iteration matrix 
							iteration.matrix <- matrix(c(i,(round(fit.functionEXPLIN(a,b,c,g,x=i),
                                                    digits=3))),
                                         ncol=2)
					                 
							#select dose if Ln/Tn fits the values in the matrix
							De <- round(mean(iteration.matrix[iteration.matrix[,2]==round(sample[1,2],digits=3),1]),digits=2)
					   
						##Monte Carlo Simulation for error estimation
						#	--Fit many curves and calculate a new De +/- De_Error
						#	--take De_Error

						#set variables
						var.b <- vector(mode="numeric", length=NumberIterations.MC)
						var.a <- vector(mode="numeric", length=NumberIterations.MC)
						var.c <- vector(mode="numeric", length=NumberIterations.MC)
						var.g <- vector(mode="numeric", length=NumberIterations.MC)
					           
            ##terminal output fo MC
            cat("\n\t Execute Monte Carlo loops for error estimation of the EXP+LIN fit\n")
            
						##set progressbar
						pb<-txtProgressBar(min=0,max=NumberIterations.MC, char="=", style=3)
            
						#start Monto Carlo loops
						for (i in 1:NumberIterations.MC) { 
							
							data <- data.frame(x=xy$x,y=data.MC[,i])
							 
							fit.MC<-try(nls(y~fit.functionEXPLIN(a,b,c,g,x),
								data=data,
								start=c(a=a,b=b,c=c,g=g),
								trace=FALSE,
                weights=fit.weights,
								algorithm="port",
								nls.control(maxiter=500) #increase max. iterations
							),silent=TRUE)
              
							#get parameters out of it including error handling
							if (class(fit.MC)=="try-error") {
								
								x.natural[i]<-NA

							}else {

							parameters <- coef(fit.MC) 
							var.b[i]<-as.vector((parameters["b"]))
							var.a[i]<-as.vector((parameters["a"]))
							var.c[i]<-as.vector((parameters["c"]))
							var.g[i]<-as.vector((parameters["g"]))
					
							#problem: analytical it is not easy to calculate x, 
              #here a simple approximation is made
						
							#calculate absolute differences from LnTn
							differences <- data.frame(dose=xy$x,
                               differences=(sample[1,2]-(fit.functionEXPLIN(a=var.a[i],b=var.b[i],c=var.c[i],g=var.g[i],x=xy$x))))
            
           		#set upper and lower boundary for searching (really timesaving)
							boundary.upper <- round(
                unique(differences[differences[,2]==max(differences[differences[,2]<=0,2]),]), 
                digits = 3)
							boundary.lower <- round(
                unique(differences[differences[,2]==min(differences[differences[,2]>=0,2]),]),
                digits = 3)

							#case that there is no upper regeneration point...
              # set a artificial point 20% above the highest reg Point 
							if (length(boundary.upper[,1]) == 0){
											artificialRegPoint<-max(xy$x)+max(xy$x)*0.2
											boundary.upper[1,1]<-round(artificialRegPoint, digits = 2)								
											}

							#write all boundary values in a vector (if cases to prevent error)				
						  if(boundary.lower[,1] < boundary.upper[,1]){
               j <- seq(boundary.lower[,1], boundary.upper[,1], by=0.01)
						  }else{
						   j <- seq(boundary.upper[,1], boundary.lower[,1], by=0.01)   
						  }
              
						
							#produce an iteration matrix 
							iteration.matrix<-matrix(
                c(j,(round(var.a[i]*(1-exp(-(j+var.c[i])/var.b[i])+var.g[i]*j),digits=3))),ncol=2)
					
							#select dose when Ln/Tn fits the values in the matrix
							x.natural[i]<-mean(
                iteration.matrix[iteration.matrix[,2]==round(sample[1,2],digits=3),1])
							}

				  ##update progress bar
				  setTxtProgressBar(pb, i)
              
          }#end for loop	
            
					##close
					close(pb)
            
      }#end if try-error 				
		} #End if EXP+LIN
		#==========================================================================
		#===========================================================================
		#EXP+EXP#
		else if (fit.method=="EXP+EXP") {
      
      a1.start<-NA
      a2.start<-NA
      b1.start<-NA
      b2.start<-NA
      
		  ## try to create some start parameters from the input values to make the fitting more stable
		  for(i in 1:50){
		    
		    a1<-a.MC[i];b1<-b.MC[i];
        a2<-a.MC[i]/2; b2<-b.MC[i]/2
		    
		    fit<-try(nls(y~fit.functionEXPEXP(a1,a2,b1,b2,x),
		                 data=data,
		                 start=c(a1=a1,a2=a2,b1=b1,b2=b2),
		                 trace=FALSE,
		                 algorithm="port",
		                 lower=c(a1>0,a2>0,b1>0,b2>0),
		                 nls.control(maxiter=500,warnOnly=FALSE,minFactor=1/2048) #increase max. iterations
		    ),silent=TRUE)
		    
      
		     if(class(fit)!="try-error"){
		        #get parameters out of it
		        parameters<-(coef(fit)) 
		        a1.start[i]<-as.vector((parameters["a1"]))
		        b1.start[i]<-as.vector((parameters["b1"])) 
		        a2.start[i]<-as.vector((parameters["a2"]))
		        b2.start[i]<-as.vector((parameters["b2"]))
		     }        
        }
      
        ##use obtained parameters for fit input
		    a1<-median(na.exclude(a1.start))
        b1<-median(na.exclude(b1.start))
		    a2<-median(na.exclude(a2.start))
		    b2<-median(na.exclude(b2.start))
   
 
							#Fit curve on given values
							fit<-try(nls(y~fit.functionEXPEXP(a1,a2,b1,b2,x),
									data=data,
									start=c(a1=a1,a2=a2,b1=b1,b2=b2),
									trace=FALSE,
                  weights=fit.weights,
									algorithm="port",
									nls.control(maxiter=500), #increase max. iterations
                  lower=c(a1>0,a2>0,b1>0,b2>0)
								))#end nls
             
              ##insert if for try-error      
              if (class(fit)!="try-error") {
                    
							#get parameters out of it
							parameters<-(coef(fit)) 
							b1<-as.vector((parameters["b1"]))
              b2<-as.vector((parameters["b2"]))
							a1<-as.vector((parameters["a1"])) 
              a2<-as.vector((parameters["a2"]))
              
              ##set D0 values
              D01<-round(b1,digits=2)
              D02<-round(b2,digits=2)
       
              #print D0 values
              writeLines(paste0("\n [plot_GrowthCurve()] >> D01 = ",D01, " | D02 = ",D02))
                            
        #problem: analytic it is not easy to calculate x, here an simple approximation is made
						
              #calculate absolut diffrences from LnTn
							differences <- data.frame(dose=xy$x,differences=(sample[1,2]-
                (round(fit.functionEXPEXP(a1,a2,b1,b2,x=xy$x),digits=3))))
		
              
							#set upper and lower boundary for searching (really timesaving)
							boundary.upper<-unique(differences[differences[,2]==max(differences[differences[,2]<=0,2]),])
							boundary.lower<-unique(differences[differences[,2]==min(differences[differences[,2]>=0,2]),])
						 
							#case that there is no upper regeneration point...set a artificial point 20% above the highest reg Point 
							if (length(boundary.upper[,1])==0){
											artificialRegPoint<-max(xy$x)+max(xy$x)*0.2
											boundary.upper[1,1]<-artificialRegPoint									
											}

							#write all boundary values in a vector
							i<-seq(boundary.lower[,1],boundary.upper[,1],by=0.01)
									
							#produce an iteration matrix 
							iteration.matrix<-matrix(c(i,(round(fit.functionEXPEXP(a1,a2,b1,b2,x=i),digits=3))),ncol=2)
		              
							#select dose if Ln/Tn fits the values in the matrix
							De<-mean(iteration.matrix[iteration.matrix[,2]==round(sample[1,2],digits=3),1])
							De<-round(De,digits=2)
              
            ##Monte Carlo Simulation for error estimation
						#	--Fit many curves and calculate a new De +/- De_Error
						#	--take De_Error from the simulation
            # --comparison of De from the MC and original fitted De gives a value for quality

						#set variables
						var.b1<-vector(mode="numeric", length=NumberIterations.MC)
            var.b2<-vector(mode="numeric", length=NumberIterations.MC)
						var.a1<-vector(mode="numeric", length=NumberIterations.MC)
            var.a2<-vector(mode="numeric", length=NumberIterations.MC)
					
													
						##terminal output fo MC
						cat("\n\t Execute Monte Carlo loops for error estimation of the EXP+EXP fit\n")  
            
						##progress bar
            pb<-txtProgressBar(min=0,max=NumberIterations.MC, initial=0, char="=", style=3)  
          
            #start Monto Carlo loops
						for (i in 1:NumberIterations.MC) { 
							
						  #update progress bar
						  setTxtProgressBar(pb,i)
              
							data<-data.frame(x=xy$x,y=data.MC[,i])
					
							fit.MC<-try(nls(y~fit.functionEXPEXP(a1,a2,b1,b2,x),
								data=data,
								start=c(a1=a1,a2=a2,b1=b1,b2=b2),
								trace=FALSE,
                weights=fit.weights,
								algorithm="port",
								nls.control(maxiter=500),
							  lower=c(a1>0,a2>0,b1>0,b2>0)#increase max. iterations
							),silent=TRUE)
              
							#get parameters out of it including error handling
							if (class(fit.MC)=="try-error") {
								
								x.natural[i]<-NA
                
							}else {

							parameters<-(coef(fit.MC)) 
							var.b1[i]<-as.vector((parameters["b1"]))
              var.b2[i]<-as.vector((parameters["b2"]))
       				var.a1[i]<-as.vector((parameters["a1"]))
              var.a2[i]<-as.vector((parameters["a2"]))
																	
							#problem: analytic it is not easy to calculat x, here an simple approximation is made
						
							#calculate absolut differences from LnTn
							differences <- data.frame(dose=xy$x,differences=(sample[1,2]-(round(fit.functionEXPEXP(
                a1=var.a1[i],
                a2=var.a2[i],
                b1=var.b1[i],
                b2=var.b2[i],
                x=xy$x),digits=3))))
					
							#set upper and lower boundary for searching (really timesaving)
							boundary.upper<-unique(differences[differences[,2]==max(differences[differences[,2]<=0,2]),])
							boundary.lower<-unique(differences[differences[,2]==min(differences[differences[,2]>=0,2]),])

							#case that there is no upper regeneration point...set a artificial point 20% above the highest reg Point 
							if (length(boundary.upper[,1])==0){
											artificialRegPoint<-max(xy$x)+max(xy$x)*0.2
											boundary.upper[1,1]<-artificialRegPoint									
											}

							#write all boundary values in a vector
							j<-seq(boundary.lower[,1],boundary.upper[,1],by=0.01)
									
							#produce an iteration matrix 
							iteration.matrix<-matrix(c(j,(round(
                                           (var.a1[i]*(1-exp(-(j)/var.b1[i])))+
                                           (var.a2[i]*(1-exp(-(j)/var.b2[i]))) ,
                                           digits=3))),ncol=2)
					
							#select dose if Ln/Tn fits the values in the matrix
							x.natural[i]<-mean(iteration.matrix[iteration.matrix[,2]==round(sample[1,2],digits=3),1])
              
              
							} #end if "try-error" MC simulation
                                       
						} #end for loop
      
          } #end if "try-error" Fit Method
      
        ##close
        if(exists("pb")){close(pb)}    
    #===========================================================================
		} #End if Fit Method  
      
      
      
		#Get De values from Monto Carlo simulation
		
			#calculate mean and sd (ignore NaN values)
			De.MonteCarlo<-round(mean(na.exclude(x.natural)),digits=2)		
  
			#De.Error is Error of the whole De (ignore NaN values)
			De.Error <- sd(na.exclude(x.natural))
      
      ##choose format in dependency of the size of the error
      De.Error <- ifelse(De.Error <= 0.01,
                          format(De.Error, scientific = TRUE, digits = 2),
                          round(De.Error, digits = 2))
      

# Formula creation --------------------------------------------------------
  
  if(is(fit,"try-error") == FALSE){
  
  if(fit.method == "EXP") {
    f <- parse(text = paste0(round(coef(fit)[1], 5), " * (1 - exp( - (x + ", 
                          round(coef(fit)[3], 5), ")/", 
                          round(coef(fit)[2], 5), "))"))
    
  }
  
  if(fit.method == "EXP+LIN") {
    f <- parse(text = paste0(round(coef(fit)[1], 5), "* (1-exp(-(x+", 
                          round(coef(fit)[3], 5), ") /", 
                          round(coef(fit)[2], 5), ")+(",
                          round(coef(fit)[4], 5), "*x))"))
  }
  
  if(fit.method == "EXP+EXP") {
    f <- parse(text = paste0(round(coef(fit)[1], 5), " * (1 - exp( -x / ", 
                          round(coef(fit)[3], 5), ")) + ", 
                          round(coef(fit)[2], 5), " * (1 - exp(-x / ", 
                          round(coef(fit)[4], 5), "))"))
  }
  
  if(fit.method == "LIN") {
    f <- parse(text = paste0(round(fit.lm$coefficients[2], 5),
                          "* x + ", round(fit.lm$coefficients[1], 5)))
    
  }
  }else{
    
    f <- NA
    
  }

##============================================================================##
# PLOTTING ---------------------------------------------------------------------
##============================================================================##

##5. Plotting if plotOutput==TRUE
if(output.plot==TRUE) {
  
    
      ##cheat the R check
      x<-NULL; rm(x)

#PAR	#open plot area
      if(output.plot== TRUE & 
           output.plotExtended== TRUE & 
           output.plotExtended.single == FALSE ){
      
        ####grep recent plot parameter for later reset
        par.default.complex <- par(no.readonly = TRUE)
      
        ##set new parameter
			  layout(matrix(c(1,1,1,1,2,3), 3, 2, byrow=TRUE), respect=TRUE)
			  par(cex=0.8*cex.global)
        
      }else{
        
        par.default.single <- par(no.readonly = TRUE)$cex
        par(cex=cex.global)
        
      }

#PLOT		#Plot input values

      ##Make selection to support manual number of reg points input
      if(exists("fit.RegPointsReal")==TRUE){
         
          ##here the object sample has to be used otherwise the first regeneration point is not plotted.
          temp.xy.plot  <- sample[fit.RegPointsReal,]

      }else{
          
          temp.xy.plot  <- xy[1:fit.NumberRegPointsReal]
        
      }

			plot(temp.xy.plot[,1:2],
				ylim=ylim,
				xlim=xlim,
				pch=19,
				xlab=xlab,
				ylab=ylab)

#ADD HEADER
      title(main=main,line=3)

#CURVE	#plot fitted curve
			if (fit.method=="EXP+LIN") {try(curve(a*(1-exp(-(x+c)/b)+(g*x)), lwd=1.5, add=TRUE))}
      else if (fit.method=="LIN") {curve(fit.lm$coefficients[2]*x+fit.lm$coefficients[1],lwd=1.5, add=TRUE)}
			else if (fit.method=="EXP") {try(curve(fit.functionEXP(a,b,c,x), lwd=1.5, add=TRUE))}
      else if (fit.method=="EXP+EXP") {try(curve(fit.functionEXPEXP(a1,a2,b1,b2,x),lwd=1.5,add=TRUE))}

##POINTS	#Plot Reg0 and Repeated Points

			#Repeated Point
      points(xy[which(duplicated(xy[,1])),1],xy[which(duplicated(xy[,1])),2], 
             pch=2)
      
      #Reg Point 0
      points(xy[which(xy==0),1],xy[which(xy==0),2], pch=1, cex = 1.5*cex.global)
   
##ARROWS	#y-error Bars

      segments(xy$x,xy$y-y.Error,xy$x,xy$y+y.Error)
	
##LINES	#Insert Ln/Tn
      if(is.na(De)){
        
        lines(c(0,max(sample[,1])*2),c(sample[1,2],sample[1,2]), col="red", lty=2,lwd=1.25)
        
      }else{
        
        try(lines(c(0,De),c(sample[1,2],sample[1,2]), col="red", lty=2,lwd=1.25),silent=TRUE)
        
      }
		
			try(lines(c(De,De),c(0,sample[1,2]), col="red", lty=2, lwd=1.25),silent=TRUE)
			try(points(De,sample[1,2], col="red", pch=19),silent=TRUE)

## check/set mtext
mtext <- if("mtext" %in% names(list(...))) {
  list(...)$mtext
  } else {
    substitute(D[e] == De, 
               list(De=paste(De,"\u00B1",De.Error, " | fit: ",fit.method)))
  }



##TEXT		#Insert fit and result
			try(mtext(side=3, mtext, line=0.5, cex=0.8*cex.global),silent=TRUE)
	
			#write error message in plot if De is NaN
			try(if (De=="NaN") {
				text(sample[2,1],0,"Error: De could not be calculated!", 
         adj=c(0,0), cex=0.8, col="red")
			},silent=TRUE)
	
##LEGEND	#plot legend
			
			legend("topleft", c("REG points", "REG point repeated", "REG point 0"),
          pch=c(19,2,1), cex=0.8*cex.global, bty="n")

##plot only if wanted
      if(output.plot==TRUE & output.plotExtended==TRUE){
        
##HIST		#try to plot histogramm of De values from the Monte Carlo simulation
			
      if(output.plotExtended.single != TRUE){
                
        par(cex=0.7*cex.global)
        
      }
   
            
			##(A) Calculate histogram data 
      try(histogram <- hist(x.natural, plot = FALSE), silent = TRUE)
                
			#to avoid errors plot only if histogram exists
			if (exists("histogram")) {
        
			##calculate normal distribution curves for overlay
			norm.curve.x <- seq(min(x.natural, na.rm = TRUE), 
			                    max(x.natural, na.rm = TRUE),
			                    length = 101)
			
			norm.curve.y <- dnorm(norm.curve.x,
			                      mean=mean(x.natural, na.rm = TRUE),
			                      sd=sd(x.natural, na.rm = TRUE))
      
      ##plot histogram 
			histogram <- hist(x.natural,
  				xlab = xlab,
  				ylab = "Frequency",
  				main=expression(paste(D[e], " from MC simulation")),
          freq=FALSE,
          border = "white",
          axes = FALSE,
          ylim = c(0,max(norm.curve.y)),
  				sub = 
            paste("n = ", NumberIterations.MC, ", valid fits =", length(na.exclude(x.natural))),
  				col="grey")

        ##add axes 
        axis(side = 1)
        axis(side = 2, 
             at = seq(min(histogram$density),max(histogram$density), length = 5),
             labels = round(
               seq(min(histogram$counts),max(histogram$counts), length = 5), 
               digits = 0))
        
        ##add norm curve
        lines(norm.curve.x, norm.curve.y, col = "red")
        
			  ##add rug
			  rug(x.natural)
                   
			##write De + Error from Monte Carlo simulation + write quality of error estimation
			try(mtext(side=3,substitute(D[e[MC]] == De, 
              list(De=paste(De.MonteCarlo,"\u00B1",De.Error,
              " | quality = ",round((1-abs(De-De.MonteCarlo)/De)*100,
              digits=1),"%"))),cex=0.6*cex.global),silent=TRUE)
          
			} else {
    
        plot(NA,NA,
             xlim=c(0,10), 
             ylim=c(0,10), 
             main=expression(paste(D[e], " from Monte Carlo simulation")))
				text(5,5,"not available")
        
			}#end ifelse

		
##PLOT		#PLOT test dose response curve if available if not plot not available
			#plot Tx/Tn value for sensitiviy change
		
			if ("TnTx" %in% colnames(sample)==TRUE) {
		
				plot(1:length(sample[,"TnTx"]),sample[1:(length(sample[,"TnTx"])),"TnTx"]/sample[1,"TnTx"],
				xlab="SAR cycle",
				ylab=expression(paste(T[n]/T[x])),
				main="Test dose response",
				type="o",
				pch=20,
				)
	
##LINES		#plot 1 line
				lines(c(1,length(sample[,"TnTx"])),c(1,1), lty=2, col="gray")
				} else {
			
			 	plot(NA,NA,xlim=c(0,10), ylim=c(0,10), main="Test dose response")
				text(5,5,"not available\n no TnTx column")
			}#end if else


## FUN by R Luminescence Team
if(fun==TRUE){sTeve()}

##END lines
  }#endif::output.plotExtended

 
  ##reset only the parameter that have been changed!  
  if(exists("par.default.single")){
   
    par(cex = par.default.single)
    rm(par.default.single)
  
  }

  if(exists("par.default.complex")){
   
   par(par.default.complex)
   rm(par.default.complex)
   
  }

}#end if plotOutput	

    ##RETURN - return De values and parameter

	  output <- try(data.frame(De=De,De.Error=De.Error, D01=D01, D02=D02, 
                             Fit=fit.method),
                  silent=TRUE)
    output <- set_RLum.Results(data=list(De=output,Fit=fit, Formula=f))
    invisible(output)

  ### \code{RLum.Results} object containing the De (De, De Error, D01 value, D02 value and Fit 
  ### type) and fit object \link{nls} object for \code{EXP}, \code{EXP+LIN} 
  ### and \code{EXP+EXP}. In case of a resulting linear fit when using 
  ### \code{EXP OR LIN}, a \link{lm} object is returned. \cr
  ### The formula \code{Formula} is returned as R expression for further evaluation.
  ### Additionally a plot is returned.
  
  ##details<<
  ## \bold{Fitting methods} \cr\cr
  ## For all options (except for the \code{LIN} and the \code{EXP OR LIN}), 
  ## the \link{nls} function with the \code{port} algorithm is used. \cr
  ## \code{LIN}: fits a linear function to the data using \link{lm}:
  ## \deqn{y = m*x+n}
  ## \code{EXP}: try to fit a function of the form 
  ## \deqn{y = a*(1-exp(-(x+c)/b))}
  ## Parameters b and c are approximated by a linear fit using \link{lm}.
  ## Note: b = D0\cr
  ##
  ## \code{EXP OR LIN}: works for some cases where an \code{EXP} fit fails. 
  ## If the \code{EXP} fit fails, a \code{LIN} fit is done instead. \cr
  ## \code{EXP+LIN}: tries to fit an exponential plus linear function of the 
  ## form: \deqn{y = a*(1-exp(-(x+c)/b)+(g*x))}
  ## The De is calculated by iteration.\cr
  ## \bold{Note:} In the context of luminescence dating, this function has 
  ## no physical meaning. Therefore, no D0 value is returned.\cr
  ## \code{EXP+EXP}: tries to fit a double exponential function of the form
  ## \deqn{y = (a1*(1-exp(-(x)/b1)))+(a2*(1-exp(-(x)/b2)))}
  ## This fitting procedure is not robust against wrong start parameters 
  ## and should be further improved.\cr\cr
  ## \bold{Fit weighting}\cr
  ## If the option \code{fit.weights = TRUE} is chosen, weights are calculated 
  ## using provided signal errors (Lx/Tx error):
  ## \deqn{fit.weights = 1/error/(sum(1/error))}
  ##
  ## \bold{Error estimation using Monte Carlo simulation}\cr
  ## Error estimation is done using a Monte Carlo (MC) simulation approach. A 
  ## set of values is constructed by randomly drawing curve data from a normal 
  ## distribution. The normal distribution is defined by the input values 
  ## (mean = value, sd = value.error). Then, a growth curve fit is attempted 
  ## for each dataset which results in new distribution of values. The 
  ## \link{sd} of this distribution is the error of the De. With increasing 
  ## iterations, the error value is becoming more stable. \bold{Note:} It may 
  ## take some calculation time with increasing MC runs, especially for the 
  ## composed functions (\code{EXP+LIN} and \code{EXP+EXP}).\cr
  ## Each error estimation is done with the function of the chosen fitting 
  ## method. \cr
  ##
  ## \bold{Subtitle information}\cr
  ## To avoid plotting the subtitle information, provide an empty user mtext
  ## \code{mtext = ""}. To plot any other subtitle text, use \code{mtext}.

  ##<<seealso
  ## \code{\link{hist}}, \code{\link{plot}},  \code{\link{nls}}, 
  ## \code{\link{lm}
  
  ##<<references
  ## Duller, G.A.T., 2007. Assessing the error on equivalent dose estimates 
  ## derived from single aliquot regenerative dose measurements. Ancient TL, 
  ## 25, 15-24.\cr
  ## Galbraith, R.F. & Roberts, R.G., 2012. Statistical aspects of equivalent 
  ## dose and error calculation and display in OSL dating: An overview and 
  ## some recommendations. Quaternary Geochronology, 11, 1-27.
  
  ##<<note
  ## No D01 is returned for the fit functions \code{EXP+LIN} and \code{LIN}. 
  ## A D02 value is provided for the function \code{EXP+EXP} only.

}, ex=function(){
  ##(1) plot growth curve for a dummy data.set and show De value
  data(ExampleData.LxTxData, envir = environment())
  temp <- plot_GrowthCurve(LxTxData)
  get_RLum.Results(temp)
  
  ##(1a) to access the fitting value try
  get_RLum.Results(temp, data.object = "Fit")
    
  ##(2) plot the growth curve only - uncomment to use
  ##pdf(file = "~/Desktop/Growth_Curve_Dummy.pdf", paper = "special")
  plot_GrowthCurve(LxTxData)
  ##dev.off()                                   
  
  ##(3) plot growth curve with pdf output - uncomment to use, single output
  ##pdf(file = "~/Desktop/Growth_Curve_Dummy.pdf", paper = "special")
  plot_GrowthCurve(LxTxData, output.plotExtended.single = TRUE)
  ##dev.off()  
})
