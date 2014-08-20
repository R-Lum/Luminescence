calc_FiniteMixture<- structure(function( # Apply the finite mixture model (FMM) after Galbraith (2005) to a given De distribution
  ### This function fits a k-component mixture to a De distribution with 
  ### differing known standard errors. Parameters (doses and mixing proportions)
  ### are estimated by maximum likelihood assuming that the log dose estimates 
  ### are from a mixture of normal distributions.
  
  # ===========================================================================
  ##author<< 
  ## Christoph Burow, University of Cologne (Germany) \cr
  ## Based on a rewritten S script of Rex Galbraith, 2006. \cr\cr
  
  ##section<<
  ## version 0.32
  # ===========================================================================
  
  input.data,
  ### \code{\linkS4class{RLum.Results}} or \link{data.frame} (\bold{required}):
  ### for \code{data.frame}: two columns with De \code{(input.data[,1])} and
  ### De error \code{(values[,2])}
  sigmab,
  ### \code{\link{numeric}}  (\bold{required}): spread in De values given as a
  ### fraction (e.g. 0.2). This value represents the expected overdispersion in
  ### the data should the sample be well-bleached (Cunningham & Wallinga 2012, 
  ### p. 100).
  n.components, 
  ### \code{\link{numeric}}  (\bold{required}): number of components to be 
  ### fitted. If a vector is provided (e.g. \code{c(2:8)}) the finite
  ### mixtures for 2, 3 ... 8 components are calculated and a plot and a
  ### statistical evaluation of the model performance (BIC score and maximum
  ### log-likelihood) is provided.
  sample.id="unknown sample",
  ### \code{\link{character}} (with default): sample id
  n.iterations = 200, 
  ### \code{\link{numeric}}  (with default): number of iterations for maximum 
  ### log likelihood estimates
  grain.probability = FALSE, 
  ### \code{\link{logical}} (with default): prints the estimated probabilities
  ### of which component each grain is in
  main = "Finite Mixture Model",
  ### \code{\link{character}} (with default): plot main title
  dose.scale,
  ### \code{\link{numeric}}: manually set the scaling of the y-axis of  
  ### the first plot with a vector in the form of \code{c(min,max)}
  pdf.weight = TRUE,
  ### \code{\link{logical}} (with default): weight the probability density 
  ### functions by the components proportion (applies only when a vector
  ### is provided for \code{n.components})
  pdf.sigma = "sigmab",
  ### \code{\link{character}} (with default): if \code{"sigmab"} the components
  ### normal distributions are plotted with a common standard deviation (i.e.
  ### \code{sigmab}) as assumed by the FFM. Alternatively,
  ### \code{"se"} takes the standard error of each
  ### component for the sigma parameter of the normal distribution
  pdf.colors = "gray",
  ### \code{\link{character}} (with default): color coding of the components 
  ### in the the plot. Possible options are "gray", "colors" and "none"
  pdf.scale,
  ### \code{\link{numeric}}: manually set the max density value for
  ### proper scaling of the x-axis of the first plot
  plot.proportions = TRUE
  ### \code{\link{logical}} (with default): plot barplot showing the 
  ### proportions of components
){
  
##============================================================================##
## CONSISTENCY CHECK OF INPUT DATA
##============================================================================##
  
  if(missing(input.data)==FALSE){
    
    if(is(input.data, "data.frame") == FALSE & is(input.data,
                                                  "RLum.Results") == FALSE){
      
      stop("[calc_FiniteMixture] Error: 'input.data' object has to be of type 
           'data.frame' or 'RLum.Results'!")
      
    }else{
      
      if(is(input.data, "RLum.Results") == TRUE){
        
        input.data <- get_RLum.Results(input.data, 
                                       signature(object = "De.values"))
      }
    }
  }  
  
  try(colnames(input.data)<- c("ED","ED_Error"),silent=TRUE)
  
  if(colnames(input.data[1])!="ED"||colnames(input.data[2])!="ED_Error") { 
    cat(paste("Columns must be named 'ED' and 'ED_Error'"), fill = FALSE)
    stop(domain=NA) 
  }
  
  if(sigmab <0 | sigmab >1) { 
    cat(paste("sigmab needs to be given as a fraction between", 
              "0 and 1 (e.g. 0.2)"), fill = FALSE)
    stop(domain=NA)
  }
  
  if(any(n.components<2) == TRUE) { 
    cat(paste("Atleast two components need to be fitted"), fill = FALSE)
    stop(domain=NA)
  }
  
  if(pdf.sigma!="se" ) {
    if(pdf.sigma!="sigmab") {
    cat(paste("Only 'se' or 'sigmab' allowed for the pdf.sigma argument"),
        fill = FALSE)
    stop(domain=NA)
    }
  }
  
  if(n.iterations<1 | n.iterations>10000) { 
    cat(paste("Only integers between 1:10000 allowed for n.iterations"),
        fill = FALSE)
    stop(domain=NA)
  }
  
##============================================================================##
## CALCULATIONS
##============================================================================##
  
  ## create storage variables if more than one k is provided
  if(length(n.components)>1) {
    
    # counter needed for various purposes 
    cnt<- 1
    
    # create summary matrix containing DE, standard error (se) and proportion
    # for each component
    comp.n<- matrix(data = NA, ncol = length(n.components),
                    nrow = n.components[length(n.components)] * 3,
                    byrow = TRUE)
    
    # create empty vector as storage for BIC and LLIK scores
    BIC.n<- vector(mode = "double")
    LLIK.n<- vector(mode = "double")
    
    # create empty vectors of type "lists" as storage for mle matrices and
    # grain probabilities
    vmat.n<- vector(mode = "list", length = length(n.components))
    grain.probability.n<- vector(mode = "list", length = length(n.components))
    
  }  
  
  ## start actual calculation (loop) for each provided maximum components to
  ## be fitted.
  for(i in 1:length(n.components)) {
    
    k<- n.components[i]
    
    # calculate yu = log(ED),  su = se(logED),  n = number of grains	
    yu<- log(input.data$ED)
    su<- input.data$ED_Error/input.data$ED
    n<- length(yu)
    
    # compute starting values
    fui<- matrix(0,n,k)
    pui<- matrix(0,n,k)
    nui<- matrix(0,n,k)
    pii<- rep(1/k,k)
    mu<- min(yu) + (max(yu)-min(yu))*(1:k)/(k+1)
    
    # remove the # in the line below to get alternative starting values
    # (useful to check that the algorithm converges to the same values)
    #	mu<- quantile(yu,(1:k)/(k+1))
    
    # compute maximum log likelihood estimates
    nit<- n.iterations
    wu<- 1/(sigmab^2 + su^2)
    rwu<- sqrt(wu)
    
    for(j in 1:nit){
      for(i in 1:k)
      {
        fui[,i]<-  rwu*exp(-0.5*wu*(yu-mu[i])^2)
        nui[,i]<-  pii[i]*fui[,i]
      }            
      pui<- nui/apply(nui,1,sum)
      mu<- apply(wu*yu*pui,2,sum)/apply(wu*pui,2,sum)
      pii<- apply(pui,2,mean)
    }
    
    # calculate the log likelihood and BIC	
    llik<- sum( log( (1/sqrt(2*pi))*apply(nui,1,sum) ))
    bic<- -2*llik + (2*k - 1)*log(n)
    
    # calculate the covariance matrix and standard errors of the estimates
    # i.e., the dose estimtes in Gy and relative standard errors, and
    # the mixing proportions and standard errors.		
    aui<- matrix(0,n,k)
    bui<- matrix(0,n,k)
    for(i in 1:k)
    {
      aui[,i]<- wu*(yu-mu[i])
      bui[,i]<- -wu + (wu*(yu-mu[i]))^2
    }
    delta<- diag(rep(1,k))
    
    Au<- matrix(0,k-1,k-1)
    Bu<- matrix(0,k-1,k)
    Cu<- matrix(0,k,k)
    
    for(i in 1:(k-1)){ for(j in 1:(k-1)){
      Au[i,j]<- sum( (pui[,i]/pii[i] - pui[,k]/pii[k])*(pui[,j]/pii[j] - 
                                                          pui[,k]/pii[k]) )}} 
    
    for(i in 1:(k-1)){ for(j in 1:k){
      Bu[i,j]<- sum( pui[,j]*aui[,j]*(pui[,i]/pii[i] - pui[,k]/pii[k] - 
                                        delta[i,j]/pii[i] + delta[k,j]/pii[k] ) )}}
    
    for(i in 1:k){ for(j in 1:k){
      Cu[i,j]<- sum( pui[,i]*pui[,j]*aui[,i]*aui[,j] - delta[i,j]*bui[,i]*
                       pui[,i] ) }}
    
    invvmat<- rbind(cbind(Au,Bu),cbind(t(Bu),Cu))
    vmat<- solve(invvmat, tol=.Machine$double.xmin)
    rek<- sqrt(sum(vmat[1:(k-1),1:(k-1)]))
    
    
    # calculate DE, relative standard error, standard error
    dose<- exp(mu)
    re<- sqrt(diag(vmat))[-c(1:(k-1))]
    sed<- dose*re
    estd<- rbind(dose,re,sed)
    
    # rename proportion
    prop<- pii
    
    # this calculates the proportional standard error of the proportion of grains
    # in the fitted components. However, the calculation is most likely erroneous.
    # sep<-  c(sqrt(diag(vmat))[c(1:(k-1))],rek)
    
    # rename proportion
    estp<- prop
    
    # merge results to a data frame
    blk<- rep("    ",k)
    comp<- rbind(blk,round(estd,4),blk,round(estp,4))
    comp<- data.frame(comp,row.names=c("","dose (Gy)    ","rse(dose)    ",
                                       "se(dose)(Gy)"," ","proportion   "))
    
    # label results data frame
    cp<- rep("comp",k)
    cn<- c(1:k)
    names(comp)<- paste(cp,cn,sep="")
    
    # calculate the log likelihood and BIC for a single component -- can
    # be useful to see if there is evidence of more than one component
    mu0<- sum(wu*yu)/sum(wu)
    fu0<-  rwu*exp(-0.5*wu*(yu-mu0)^2)
    L0<- sum( log((1/sqrt(2*pi))*fu0 ) )
    bic0<- -2*L0 + log(n)
    comp0<- round(c(exp(mu0),sigmab,L0,bic0),4)
    
    
    ## save results for k components in storage variables
    if(length(n.components)>1) {
      
      # vector of indices needed for finding the dose rows of the summary
      # matrix - position 1,4,7...n 
      pos.n<- seq(from = 1, to = n.components[cnt]*3, by = 3)
      
      # save results of each iteration to summary matrix
      for(i in 1:n.components[cnt]) {
        comp.n[pos.n[i], cnt]<- round(dose[i], 2) #De 
        comp.n[pos.n[i]+1, cnt]<- round(sed[i], 2) #SE
        comp.n[pos.n[i]+2, cnt]<- round(estp[i], 2) #Proportion
      }
      
      # save BIC and llik of each iteration to corresponding vector
      BIC.n[cnt]<- bic
      LLIK.n[cnt]<- llik
      
      # merge BIC and llik scores to a single data frame
      results.n<- rbind(BIC = round(BIC.n, 3),
                        llik = round(LLIK.n, 3))
      
      # save mle matrix and grain probabilities to corresponding vector
      vmat.n[[cnt]]<- vmat
      grain.probability.n[[cnt]]<- grain.probability
      
      # increase counter by one for next iteration
      cnt<- cnt+1
    }#EndOf::save intermediate results
  }##EndOf::calculation loop
  
##============================================================================##
## STATISTICAL CHECK
##============================================================================##
  
  if(length(n.components)>1) {
    
    ## Evaluate maximum log likelihood estimates
    LLIK.significant<- vector(mode = "logical")
    
    # check if llik is at least three times greater when adding a further 
    # component
    for(i in 1:c(length(LLIK.n)-1)) {
      LLIK.significant[i]<- (LLIK.n[i+1]/LLIK.n[i])>3
    }
    
    ## Find lowest BIC score
    BIC.lowest<- n.components[which.min(BIC.n)]
  }
  
##============================================================================##
## OUTPUT
##============================================================================##
  
  ## HEADER (always printed)
  cat("\n [calc_FiniteMixture]")
  
  ##----------------------------------------------------------------------------
  ## OUTPUT WHEN ONLY ONE VALUE FOR n.components IS PROVIDED
  
  if(length(n.components) == 1) {
    
    # covariance matrix
    cat(paste("\n\n--- covariance matrix of mle's ---\n\n"))
    print(round(vmat,6))
    
    # general information on sample and model performance
    cat(paste("\n----------- meta data ------------"))                                 
    cat(paste("\n Sample ID:            ",sample.id))
    cat(paste("\n n:                    ",n))
    cat(paste("\n sigmab:               ",sigmab))
    cat(paste("\n number of components: ",k))
    cat(paste("\n llik:                 ",round(llik,4)))
    cat(paste("\n BIC:                   ",round(bic,3)))
    
    # fitted components
    cat(paste("\n\n----------- components -----------\n\n"))
    print(comp)
    
    
    # print (to 2 decimal places) the estimated probabilities of which component 
    # each grain is in -- sometimes useful for diagnostic purposes			
    if(grain.probability==TRUE) {	
      cat(paste("\n-------- grain probability -------\n\n"))
      print(round(pui,2))                            
    }
    
    # output for single component
    cat(paste("\n-------- single component --------"))                   
    cat(paste("\n mu:                    ", comp0[1]))
    cat(paste("\n sigmab:                ", comp0[2]))
    cat(paste("\n llik:                  ", comp0[3]))
    cat(paste("\n BIC:                   ", comp0[4]))
    cat(paste("\n----------------------------------\n\n"))
    
  }#EndOf::Output for length(n.components) == 1
  
  ##----------------------------------------------------------------------------
  ## OUTPUT WHEN ONLY >1 VALUE FOR n.components IS PROVIDED
  if(length(n.components) > 1) {
    
    ## final labeling of component and BIC/llik matrices
    # create labels
    dose.lab<- paste("c", 1:n.components[length(n.components)],"_dose", sep="")
    se.lab<- paste("c", 1:n.components[length(n.components)],"_se", sep="")
    prop.lab<- paste("c", 1:n.components[length(n.components)],"_prop", sep="")
    
    # empty vector which stores the labeles in correct order (dose, se, prop)
    n.lab<- vector(mode = "expression",
                   n.components[length(n.components)]*3)
    
    # loop to store the labels in correct order (dose, se, prop)
    cnt<- 1
    for(i in pos.n) {
      n.lab[i]<- dose.lab[cnt]
      n.lab[i+1]<- se.lab[cnt]
      n.lab[i+2]<- prop.lab[cnt]
      cnt<- cnt+1
    }
    
    # label columns and rows of summary matrix and BIC/LLIK data frame
    colnames(comp.n)<- n.components[1]:n.components[length(n.components)]
    rownames(comp.n)<- n.lab
    colnames(results.n)<- n.components[1]:n.components[length(n.components)]
    
    ## CONSOLE OUTPUT
    # general information on sample and model performance
    cat(paste("\n\n----------- meta data ------------"))                                 
    cat(paste("\n Sample ID:            ",sample.id))
    cat(paste("\n n:                    ",n))
    cat(paste("\n sigmab:               ",sigmab))
    cat(paste("\n number of components:  ",n.components[1],"-",
              n.components[length(n.components)], sep=""))
    
    # output for single component
    cat(paste("\n\n-------- single component --------"))                   
    cat(paste("\n mu:                    ", comp0[1]))
    cat(paste("\n sigmab:                ", comp0[2]))
    cat(paste("\n llik:                  ", comp0[3]))
    cat(paste("\n BIC:                   ", comp0[4]))
    
    # print component matrix
    cat(paste("\n\n----------- k components -----------\n"))
    print(comp.n, na.print="")
    
    # print BIC scores and LLIK estimates
    cat(paste("\n----------- statistical criteria -----------\n"))
    print(results.n)
    
    ## print evaluation of statistical criteria
    # lowest BIC score
    cat(paste("\n Lowest BIC score for k =", BIC.lowest))
    
    # first significant increase in LLIK estimates
    if(any(LLIK.significant)!=TRUE) {
      cat(paste("\n No significant increase in maximum log",
                "likelihood estimates. \n"))
    } else { 
      cat(paste("\n First significant increase in maximum log likelihood for",
                "k =", which(LLIK.significant==TRUE)[1], "\n\n"))
    }
    
    cat(paste("\n"))
  }#EndOf::Output for length(n.components) > 1
  
##============================================================================##
## PLOTTING
##============================================================================##
  
  ## plot only for length(n.components) > 1
  if(length(n.components) > 1) {
    
    # save previous plot parameter and set new ones
    .pardefault<- par(no.readonly = TRUE)
    
    ## DEVICE AND PLOT LAYOUT
    n.plots<- length(n.components) #number of PDF plots in plotarea #1
    seq.vertical.plots<- seq(from = 1, to = n.plots, by = 1) #indices
    ID.plot.two<- n.plots+if(plot.proportions==TRUE){1}else{0} #ID of second plot area
    ID.plot.three<- n.plots+if(plot.proportions==TRUE){2}else{1} #ID of third plot area
    
    #empty vector for plot indices
    seq.matrix<- vector(mode="integer", length=4*n.plots)
    
    #fill vector with plot indices in correct order
    cnt<- 1
    seq<- seq(1,length(seq.matrix),4)
    for(i in seq) {
      seq.matrix[i]<- cnt
      seq.matrix[i+1]<- cnt
      seq.matrix[i+2]<- if(plot.proportions==TRUE){ID.plot.two}else{cnt}
      seq.matrix[i+3]<- ID.plot.three
      
      cnt<- cnt+1
    }
    
    # create device layout
    layout(matrix(c(seq.matrix),4,n.plots))
    
    # outer margins (bottom, left, top, right)
    par(oma=c(2.5,5,3,5))
    
    # general plot parameters (global scaling, allow overplotting)
    par(cex = 0.8, xpd = NA)
    
    # define color palette for prettier output
    if(pdf.colors == "colors") {
      col.n<- c("red3", "slateblue3", "seagreen", "tan3", "yellow3",
                "burlywood4", "magenta4", "mediumpurple3", "brown4","grey",
                "aquamarine")
      poly.border<- FALSE
    }
    if(pdf.colors == "gray" || pdf.colors == "grey") {
      col.n<- gray.colors(length(n.components)*2)
      poly.border<- FALSE
    }
    if(pdf.colors == "none") {
      col.n<- NULL
      poly.border<- TRUE
    }

    ##--------------------------------------------------------------------------
    ## PLOT 1: EQUIVALENT DOSES OF COMPONENTS
    
    ## create empty plot without x-axis
    for(i in 1:n.plots) {
      
      # set margins (bottom, left, top, right)
      par(mar=c(1,0,2,0))
      
      # empty plot area
      plot(NA, NA,
           xlim=c(min(n.components)-0.2, max(n.components)+0.2),
           ylim=c(min(comp.n[pos.n,]-comp.n[pos.n+1,], na.rm = TRUE),
                  max((comp.n[pos.n,]+comp.n[pos.n+1,])*1.1, na.rm = TRUE)),
           ylab="",
           xaxt="n",
           yaxt="n",
           xlab="")
      
      # add text in upper part of the plot ("k = 1,2..n")
      mtext(bquote(italic(k) == .(n.components[i])),
            side = 3, line = -2, cex=0.8)
      
      # add y-axis label (only for the first plot)
      if(i==1) {
        mtext(expression(paste("D"[e]," [Gy]")), side=2,line=2.7, cex=1)
      }
      
      # empty list to store normal distribution densities
      sapply.storage<- list()
      
      ## NORMAL DISTR. OF EACH COMPONENT
      options(warn=-1) #supress warnings for NA values
      
      # LOOP - iterate over number of components
      for(j in 1:max(n.components)) { 
        
        # draw random values of the ND to check for NA values 
        comp.nd.n<- sort(rnorm(n = length(input.data[,1]),
                               mean = comp.n[pos.n[j],i],
                               sd = comp.n[pos.n[j]+1,i]))    
        
        # proceed if no NA values occured
        if(length(comp.nd.n)!=0) {
          
          # weight - proportion of the component
          wi<- comp.n[pos.n[j]+2,i]
          
          # calculate density values with(out) weights 
          fooX<- function(x) {
            dnorm(x, mean = comp.n[pos.n[j],i], 
                  sd = if(pdf.sigma=="se"){comp.n[pos.n[j]+1,i]}
                  else{if(pdf.sigma=="sigmab"){comp.n[pos.n[j],i]*sigmab}}
                  )*
            if(pdf.weight==TRUE){wi}else{1}  
          }
          
          # x-axis scaling - determine highest dose in first cycle
          if(i==1 && j==1){
            
            max.dose<- max(input.data[,1])+sd(input.data[,1])/2
                        
            min.dose<- min(input.data[,1])-sd(input.data[,1])/2
            
            
            # density function to determine y-scaling if no weights are used
            fooY<- function(x) {
              dnorm(x, mean = na.exclude(comp.n[pos.n,]), 
                    sd = na.exclude(comp.n[pos.n+1,]))
            }
            
            # set y-axis scaling
            dens.max<-max(sapply(0:max.dose, fooY))            
            

          }##EndOfIf::first cycle settings
          
          
          # override y-axis scaling if weights are used
          if(pdf.weight==TRUE){
            sapply.temp<- list()
              for(b in 1:max(n.components)){
                
                # draw random values of the ND to check for NA values 
                comp.nd.n<- sort(rnorm(n = length(input.data[,1]),
                                       mean = comp.n[pos.n[b],i],
                                       sd = comp.n[pos.n[b]+1,i])) 
                
                # proceed if no NA values occured
                if(length(comp.nd.n)!=0) {
                  
                  # weight - proportion of the component
                  wi.temp<- comp.n[pos.n[b]+2,i]            
                  
                  fooT<- function(x) {
                    dnorm(x, mean = comp.n[pos.n[b],i], 
                          sd = if(pdf.sigma=="se"){comp.n[pos.n[b]+1,i]}
                          else{if(pdf.sigma=="sigmab"){comp.n[pos.n[b],i]*sigmab}}
                    )*wi.temp
                  }
                  sapply.temp[[b]]<- sapply(0:max.dose, fooT)
                }
              }
            dens.max<- max(Reduce('+', sapply.temp))
          }
          
          # calculate density values for 0 to maximum dose
          sapply<- sapply(0:max.dose, fooX)
          
          # save density values in list for sum curve of gaussians
          sapply.storage[[j]]<- sapply
          
          ## determine axis scaling
          # x-axis (dose)
          if(missing(dose.scale)==FALSE) {
            y.scale<- dose.scale
          } else {
            y.scale<- c(min.dose,max.dose)
          }
          # y-axis (density)
          if(missing(pdf.scale)==FALSE) {
            x.scale<- pdf.scale
          } else {
            x.scale<- dens.max*1.1
          }
          
          ## PLOT Normal Distributions
          par(new=TRUE)
          plot(sapply, 1:length(sapply)-1, 
               type="l", yaxt="n", xaxt="n", col=col.n[j], lwd=1, 
               ylim=y.scale, 
               xlim=c(0,x.scale),
               xaxs="i", yaxs="i", 
               ann=FALSE, xpd = FALSE)
          
          # draw colored polygons under curve
          polygon(x=c(min(sapply), sapply,  min(sapply)),
                  y=c(0, 0:max.dose,  0), 
                  col = adjustcolor(col.n[j], alpha.f = 0.66),
                  yaxt="n", border=poly.border, xpd = FALSE, lty = 2, lwd = 1.5) 
          
        }
      }##EndOf::Component loop
      
      #turn warnings on again
      options(warn=0)

      # Add sum of gaussians curve
      par(new = TRUE)
      
      plot(Reduce('+', sapply.storage),1:length(sapply)-1,
           type="l", yaxt="n", xaxt="n", col="black", 
           lwd=1.5, lty = 1, 
           ylim=y.scale, 
           xlim=c(0,x.scale),
           xaxs="i", yaxs="i", ann=FALSE, xpd = FALSE)
           
      # draw additional info during first k-cycle
      if(i == 1) {
        
        # plot title
        mtext("Normal distributions", 
              side = 3, font = 2, line = 0, adj = 0, cex = 0.8)
        
        # main title
        mtext(main, 
              side = 3, font = 2, line = 3.5, adj = 0.5, 
              at = grconvertX(0.5, from = "ndc", to = "user"))
        
        # subtitle
        mtext(as.expression(bquote(italic(sigma[b]) == .(sigmab) ~ 
                                     "|" ~ n == .(length(input.data[,1])))),           
              side = 3, font = 1, line = 2.2, adj = 0.5, 
              at = grconvertX(0.5, from = "ndc", to = "user"), cex = 0.9)
        
        # x-axis label
        mtext("Density [a.u.]", 
              side = 1, line = 0.5, adj = 0.5, 
              at = grconvertX(0.5, from = "ndc", to = "user"))
        
        # draw y-axis with proper labels
        axis(side=2, labels = TRUE)
      }
      
      if(pdf.colors == "colors") {
        # create legend labels
        dose.lab.legend<- paste("c", 1:n.components[length(n.components)], sep="")
        
        if(max(n.components)>8) {
          ncol.temp<- 8
          yadj<- 1.025
        } else {
          ncol.temp<- max(n.components)
          yadj<- 0.93
        }
        
        # add legend
        if(i==n.plots) {
          legend(grconvertX(0.55, from = "ndc", to = "user"),
                 grconvertY(yadj, from = "ndc", to = "user"),
                 legend = dose.lab.legend,
                 col = col.n[1:max(n.components)],
                 pch = 15, adj = c(0,0.2), pt.cex=1.4,
                 bty = "n", ncol=ncol.temp, x.intersp=0.4)
          
          mtext("Components: ", cex = 0.8,
                at = grconvertX(0.5, from = "ndc", to = "user"))
        }
      }
      
    }##EndOf::k-loop and Plot 1
    
    ##--------------------------------------------------------------------------
    ## PLOT 2: PROPORTION OF COMPONENTS
    if(plot.proportions==TRUE) {
    # margins for second plot
    par(mar=c(2,0,2,0))
    
    # create matrix with proportions from a subset of the summary matrix
    prop.matrix<- comp.n[pos.n+2,]*100
    
    # stacked barplot of proportions without x-axis
    barplot(prop.matrix,
            width=1,
            xlim=c(0.2, length(n.components)-0.2),
            ylim=c(0,100),
            axes=TRUE,
            space=0,
            col=col.n,
            xpd=FALSE,
            xaxt="n")
    
    # y-axis label
    mtext("Proportion [%]", 
          side=2,line=3, cex=1)
    
    
    # add x-axis with corrected tick positions
    axis(side = 1, labels = n.components, at = n.components+0.5-n.components[1])
    
    # draw a box (not possible with barplot())
    box(lty=1, col="black")
    
    # add subtitle
    mtext("Proportion of components", 
          side = 3, font = 2, line = 0, adj = 0, cex = 0.8)
    
    }
    ##--------------------------------------------------------------------------
    ## PLOT 3: BIC & LLIK
    
    # margins for third plot
    par(mar=c(2,0,2,0))
    
    # prepare scaling for both y-axes
    BIC.scale<- c(min(BIC.n)*if(min(BIC.n)<0){1.2}else{0.8},
                  max(BIC.n)*if(max(BIC.n)<0){0.8}else{1.2})
    LLIK.scale<- c(min(LLIK.n)*if(min(LLIK.n)<0){1.2}else{0.8},
                   max(LLIK.n)*if(max(LLIK.n)<0){0.8}else{1.2})
    
    # plot BIC scores
    plot(n.components, BIC.n,
         main= "",
         type="b",
         pch=22,
         cex=1.5,
         xlim=c(min(n.components)-0.2, max(n.components)+0.2),
         ylim=BIC.scale,
         xaxp=c(min(n.components), max(n.components), length(n.components)-1),
         xlab=expression(paste(italic(k), " Components")),
         ylab=expression(paste("BIC")),
         cex.lab=1.25)
    
    # following plot should be added to previous
    par(new = TRUE)
    
    # plot LLIK estimates
    plot(n.components, LLIK.n,
         xlim=c(min(n.components)-0.2, max(n.components)+0.2),
         xaxp=c(min(n.components), max(n.components), length(n.components)-1),
         ylim=LLIK.scale,
         yaxt="n", type="b", pch=16, xlab="", ylab="", lty=2, cex = 1.5)
    
    # subtitle
    mtext("Statistical criteria", 
          side = 3, font = 2, line = 0, adj = 0, cex = 0.8)
    
    # second y-axis with proper scaling
    axis(side = 4, ylim=c(0,100))
    
    # LLIK axis label
    mtext(bquote(italic(L)[max]),
          side=4,line=3, cex=1.3)
    
    # legend  
    legend(grconvertX(0.75, from = "nfc", to = "user"),
           grconvertY(0.96, from = "nfc", to = "user"),
           legend = c("BIC", as.expression(bquote(italic(L)[max]))),
           pch = c(22,16), pt.bg=c("white","black"), 
           adj = 0, pt.cex=1.3, lty=c(1,2),
           bty = "n", horiz = TRUE, x.intersp=0.5)
    
    
    ## restore previous plot parameters
    par(.pardefault)
    
  }##EndOf::PLOTTING
  

##============================================================================##
## RETURN VALUES
##============================================================================##
  
  # .@data$meta
  meta<- data.frame(id=sample.id,n=n,sigmab=sigmab,n.components=k,
                    llik=llik,bic=bic)
  if(length(n.components)>1) {
    meta.n<- data.frame(id=sample.id, n=n, sigmab=sigmab, n.components=n.components,
                        llik=LLIK.n, bic=BIC.n)
  }
  
  # .@data$single.comp
  single.comp<- data.frame(id=sample.id,mu=comp0[1],sigmab=comp0[2],
                           llik=comp0[3],BIC=comp0[4])
  
  # .@data$components
  comp.re<- rbind(round(estd,4),round(estp,4))
  comp.re<- data.frame(comp.re,row.names=c("dose (Gy)    ","rse(dose)    ",
                                           "se(dose)(Gy)","proportion   "))
  names(comp.re)<- paste(cp,cn,sep="")
  
  # .@data$grain.probability
  grain.probability<- round(pui, 2)
  
  # create S4 object
  newRLumResults.calc_FiniteMixture <- set_RLum.Results(
    data = list(
      mle.matrix=if(length(n.components)==1){vmat}else{vmat.n}, # .@data$mle.matrix
      grain.probability=if(length(n.components)==1){grain.probability}else{grain.probability.n},
      meta=if(length(n.components)==1){meta}else{meta.n},
      components=if(length(n.components)==1){comp.re}else{comp.n},
      single.comp=single.comp))
  
  # Return values
  invisible(newRLumResults.calc_FiniteMixture)
  ### Returns a terminal output. In addition a list
  ### is returned containing the following elements:
  ### \item{mle.matrix}{\link{matrix} covariance matrix of maximum likelihood
  ### estimates.}
  ### \item{grain.probability}{\link{matrix} with estimated probabilities 
  ### of which component each grain is in.}
  ### \item{meta}{\link{data.frame} containing model parameters 
  ### (sample.id, sigmab, n.components, llik, bic).}
  ### \item{components}{\link{data.frame} containing fitted components.}
  ### \item{single.comp}{\link{data.frame} containing log likelihood and 
  ### BIC for a single component.}
  ###
  ### If a vector for \code{n.components} is provided (e.g. 
  ### \code{c(2:8)}), \code{mle.matrix, grain.probability} and \code{meta} are
  ### lists containing matrices of the results for each iteration of the model.
  ###
  ### The output should be accessed using the function 
  ### \code{\link{get_RLum.Results}}
  
  
  ##details<<
  ## This model uses the maximum likelihood and Bayesian Information Criterion 
  ## (BIC) approaches. \cr\cr
  ## Indications of overfitting are: \cr\cr
  ## - increasing BIC \cr
  ## - repeated dose estimates \cr
  ## - covariance matrix not positive definite \cr
  ## - covariance matrix produces NaNs\cr
  ## - convergence problems \cr\cr
  ## \bold{Plot} \cr\cr
  ## If a vector (\code{c(k.min:k.max)}) is provided for \code{n.components}
  ## a plot is generated showing the the k components equivalent doses as 
  ## normal distributions. By default \code{pdf.weight} is set to \code{FALSE},
  ## so that the area under each normal distribution is always 1. If \code{TRUE},
  ## the probability density functions are weighted by the components proportion
  ## for each iteration of k components, so the sum of areas of each component 
  ## equals 1. While the density values are on the same scale when no weights are
  ## used, the y-axis are individually scaled if the probability density 
  ## are weighted by the components proportion. \cr 
  ## The standard deviation (sigma) of the normal distributions is by default
  ## determined by a common \code{sigmab} (see \code{pdf.sigma}). For
  ## \code{pdf.sigma = "se"} the standard error of each component is taken 
  ## instead.\cr
  ## The stacked barplot shows the proportion of each component (in per cent)
  ## calculated by the FFM. The last plot shows the achieved BIC scores
  ## and maximum log-likelihood estimates for each iteration of k.

  ##references<<
  ## Galbraith, R.F. & Green, P.F., 1990. Estimating the component ages in a 
  ## finite mixture. Nuclear Tracks and Radiation Measurements, 17, pp. 197-206.
  ## \cr\cr
  ## Galbraith, R.F. & Laslett, G.M., 1993. Statistical models for mixed fission
  ## track ages. Nuclear Tracks Radiation Measurements, 4, pp. 459-470.\cr\cr
  ## Galbraith, R.F. & Roberts, R.G., 2012. Statistical aspects of equivalent 
  ## dose and error calculation and display in OSL dating: An overview and some
  ## recommendations. Quaternary Geochronology, 11, pp. 1-27.\cr\cr
  ## Roberts, R.G., Galbraith, R.F., Yoshida, H., Laslett, G.M. & Olley, J.M., 
  ## 2000. Distinguishing dose populations in sediment mixtures: a test of 
  ## single-grain optical dating procedures using mixtures of laboratory-dosed
  ## quartz. Radiation Measurements, 32, pp. 459-465.\cr\cr
  ## Galbraith, R.F., 2005. Statistics for Fission Track Analysis, Chapman & 
  ## Hall/CRC, Boca Raton.\cr\cr
  ## \bold{Further reading}\cr\cr
  ## Arnold, L.J. & Roberts, R.G., 2009. Stochastic modelling of multi-grain 
  ## equivalent dose (De) distributions: Implications for OSL dating of sediment
  ## mixtures. Quaternary Geochronology, 4, pp. 204-230.\cr\cr
  ## Cunningham, A.C. & Wallinga, J., 2012. Realizing the potential of fluvial
  ## archives using robust OSL chronologies. Quaternary Geochronology, 12, 
  ## pp. 98-106.\cr\cr
  ## Rodnight, H., Duller, G.A.T., Wintle, A.G. & Tooth, S., 2006. Assessing the
  ## reproducibility and accuracy of optical dating of fluvial deposits. 
  ## Quaternary Geochronology, 1, pp. 109-120.\cr\cr
  ## Rodnight, H. 2008. How many equivalent dose values are needed to obtain a 
  ## reproducible distribution?. Ancient TL, 26, pp. 3-10.
  
  ##seealso<<
  ## \code{\link{calc_CentralDose}},
  ## \code{\link{calc_CommonDose}}, \code{\link{calc_FuchsLang2001}},
  ## \code{\link{calc_MinDose3}}, \code{\link{calc_MinDose4}}    
}, ex=function(){
  ## load example data
  data(ExampleData.DeValues, envir = environment())
  
  ## (1) apply the finite mixture model
  ## NOTE: the data set is not suitable for the finite mixture model,
  ## which is why a very small sigmab is necessary
  calc_FiniteMixture(ExampleData.DeValues,
                     sigmab = 0.08, n.components = 2,
                     grain.probability = TRUE)
  
  ## (2) repeat the finite mixture model for 2, 3 and 4 maximum number of fitted
  ## components and save results
  ## NOTE: The following example is computationally intensive. Please un-comment
  ## the following lines to make the example work.
  #res<- calc_FiniteMixture(ExampleData.DeValues,
  #                         sigmab = 0.01, n.components = c(2:4),
  #                         pdf.weight = TRUE, dose.scale = c(2200,4500))
  
  ## show structure of the results
  #res
  
  ## show the results on equivalent dose, standard error and proportion of
  ## fitted components
  #get_RLum.Results(object=res, data.object="components")
  
  
})#END OF STRUCTURE