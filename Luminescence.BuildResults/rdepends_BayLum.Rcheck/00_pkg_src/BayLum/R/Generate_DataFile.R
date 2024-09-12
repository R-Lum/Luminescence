#' @title Generates, from one (or several) BIN-file(s) of Single-grain OSL measurements, a list of luminescence data and information before statistical analysis (DEPRECATED)
#'
#' @description
#' This function is used to generate, from the BIN file(s), a list of values of:
#' \bold{Single-grain} OSL intensities and associated uncertainties, regenerative doses, etc., which will be the input of the Bayesian models.
#' To be easy-to-use, this function requires a rigorous organisation - all needed files should be arranged in one folder -
#' of informations concerning each BIN file. \cr
#' It is possible to process data for various samples simultaneously and to consider more than one BIN file per sample.
#'
#' @param Path [character] (**required**): the path to the project folder, containing one or more sub folders in which the BIN files
#' are located. If it is not equal to "", it must be terminated by "/".
#'
#' @param FolderNames [character] (**required**): list of names of the sub-folders containing the BIN files
#' - each sub folder must contain a BIN file and associated csv files.
#' See details for more informations on associated csv files required in the sub folders.
#' If there is more than one BIN file per sample, see the details section for instructions regarding how to correctly fill the
#' \code{FolderNames} vector.
#'
#' @param Nb_sample [integer] (**required**): number of samples.
#'
#' @param Nb_binfile [integer] (with default): number of BIN files. It must be equal to, or greater than \code{Nb_sample}.
#' @param BinPerSample [integer] vector (with default): vector with the number of BIN files per sample.
#' The length of this vector must be equal to \code{Nb_sample} and the sum of entries of this vector must be equal to \code{Nb_binfile}.
#' If there is more than one BIN file per sample, see the details section for instructions regarding how to correctly
#' fill \code{BinPerSample} vector.
#' Otherwise, this vector must contain a list of 1 values.
#'
#' @param sepDP [character] (with default): column separator in the `DiscPose.csv` files.
#'
#' @param sepDE [character] (with default): column separator in the `DoseEnv.csv` files.
#'
#' @param sepDS [character] (with default): column separator in the `DoseLab.csv`` files.
#'
#' @param sepR [character] (with default): column separator in the Rule.csv files.
#'
#' @param verbose [logical] (with default): enable/disable verbose mode
#'
#' @param ... further arguments that can be passed to [Luminescence::read_BIN2R].
#'
#' @details
#' With \code{Path} and \code{FolderNames}, this function goes to the sub folders containing the BIN files and associated information to compute
#' the luminescence data.\cr
#'
#' \bold{** What are the required files in each subfolder? **}\cr
#'
#' Each sub folder can be named, for example, as the sample name followed by a number; it must contain:
#' \itemize{
#'   \item \bold{bin.bin}: the bin file renamed as bin.BIN (note: the name of all files matters);
#'   \item \bold{DiscPos.csv}: a two columns csv file containing the list of disc and grain position number of the previously selected grains
#'   (typically this list will include the position of grains based on their sensitivity, recycling or other properties);
#'   \item \bold{DoseEnv.csv}: a two columns file containing the observation of the natural (or environmental),
#'   dose rate, and its non-shared variance (i.e. after removing all shared errors), both in Gy. Note: the user shall provide the squared value of the error associated
#'   with the dose rate experienced by the sample grains in nature;
#'   \item \bold{DoseSourve.csv}: a two columns file containing the observation of the laboratory dose rate,
#'   and its variance (squared error) both in Gy;
#'   \item \bold{rule.csv}: a csv file containing information on
#'   \itemize{
#'     \item \code{beginSignal=}  the first channel for summing the natural or regenerative OSL signal (typically 1 or 6);
#'     \item \code{endSignal=} the last channel for summing the natural or regenerative OSL signal (typically 5 or 10);
#'     \item \code{beginBackground=} the first channel for background estimation of the natural or regenerative OSL signal (typically 76 or 81);
#'     \item \code{endBackground=} the last channel for background estimation of the natural or regenerative OSL signal (typically 95 or 100);
#'     \item \code{beginTest=},
#'     \item \code{endTest=},
#'     \item \code{beginTestBackground=},
#'     \item \code{endTestBackground=} same values as above, for the test dose response (typically the same values should be used);
#'     \item \code{inflatePercent=} uncertainty arising from the instrument reproducibility (typically 0.02, i.e. 2\%);
#'     \item \code{nbOfLastCycleToRemove=} number of cycles at the end of the SAR protocol which should not be included in the dose response curve fitting
#'     (typically 1 if only a recycling test is performed, or 2 if both recycling and IR depletion are tested).\cr
#'   }
#' }
#'
#' \bold{** How to fill the} \code{FolderNames} \bold{vector? **}\cr
#'
#' \code{FolderNames} is a vector of length \code{Nb_binfile}. \code{FolderNames[i]} is the name (e.g., Sample1-File1, or successive names separated by "/" signs,
#' if BIN files are in subfolders, e.g. Sample1/File1) of the subfolder containing all informations on the BIN file of ID number \code{i}.
#' The names in \code{FolderNames} are ordered following two rules:
#' \itemize{
#'   \item The names in the \code{FolderNames} vector must be ordered following the sample order
#'   (the names of subfolders containing BIN files for the same sample should follow each other in the \code{FolderNames} vector, e.g.
#'   Sample1, Sample2-File1, Sample2-File2, etc.).
#'   \item If stratigraphic constraints apply to samples, and so a \bold{Bayesian model with stratigraphic constraints} is implemented,
#'   then the names in the \code{FolderNames} vector must be ordered by order of increasing ages. \cr
#'   For example, \code{FolderNames=c(noun1,noun2)}, in which case \code{noun1} (respectively, \code{noun2}) corresponds to the subfolder
#'   name containing the BIN file of sample 1 (respectively of sample 2).
#'   In addition, if we know that sample 1 is younger than sample 2, then \code{FolderNames} vector is correctly filled. \cr
#'   If conversely, \code{FolderNames=c(noun2,noun1)}, the analysis performed by \code{\link{AgeS_Computation}} would not be consistent.\cr
#' }
#'
#' \bold{** How to fill the} \code{BinPerSample} \bold{vector? **}\cr
#'
#' \code{BinPerSample[i]} correponds to the number of BIN files for the sample whose number ID is equal to \code{i}.\cr
#' For example, let us consider a case with two samples (Sample1 and Sample2), with 2 BIN files for Sample1 and 1 for Sample2.
#' In this case, \code{Nb_binfile}=3 and \code{Nb_sample}=2.
#' The user may then set \code{FolderNames=c("Sample1-File1", "Sample1-File2", "Sample2-File1")}, in which case \code{"Sample1-File1"} is the name of the subfolder
#' containing the first BIN file for Sample1, \code{"Sample1-File2"} the name of the  subfolder for the second BIN file of Sample1; eventually,
#' \code{"Sample2-File1"} is the name of the subfolder containing the BIN file for the second sample. In this case, \code{BinPerSample=c(2,1)}.
#'
#'
#' For the general BIN-file structure, the reader is referred to the following website:
#' \code{http://www.nutech.dtu.dk/}
#'
#' The function \code{\link{read_BIN2R}} developped in \code{\link{Luminescence}} package is used to read the BIN files.
#'
#' @return A list containing the following objects:
#' \itemize{
#'   \item \bold{LT} (one list per sample); each list contains all L/T values for the corresponding sample;
#'   \item \bold{sLT} (one list per sample); each list contains all uncertainties on L/T values for the corresponding sample;
#'   \item \bold{ITimes} (one list per sample); each list contains irradiation time values for the corresponding sample;
#'   \item \bold{dLab}, a matrix containing in line \code{i}, the laboratory dose rate and its variance for sample \code{i};
#'   \item \bold{ddot_env}, a matrix containing in line \code{i}, the environmental dose rate and its variance (excluding the common error terms) for sample \code{i};
#'   \item \bold{regDose} (one list per sample); each list contains all regenerated doses;
#'   \item \bold{J}, a vector giving, for each BIN file, the number of aliquots selected for the analysis;
#'   \item \bold{K}, a vector giving, for each BIN file, the number of regenerative doses in the SAR protocol;
#'   \item \bold{Nb_measurement}, a vector giving, for each BIN file, the number of measurements.\cr
#' }
#'
#' \bold{** How to save this list **}\cr
#'
#' You can save this list in a .RData object. To do this, you can use the fonction \code{\link{save}}.
#' Then, to load this list you can use the function \code{\link{load}} (see example section fore more details).
#'
#' @author Claire Christophe, Sebastian Kreutzer, Anne Philippe, Guillaume Guerin
#'
#' @seealso \code{\link{read_BIN2R}}, \code{\link{combine_DataFiles}}, \code{\link{Generate_DataFile_MG}}, \code{\link{LT_RegenDose}}
#' \code{\link{Age_Computation}}, \code{\link{AgeS_Computation}}, \code{\link{Palaeodose_Computation}}
#'
#' @examples
#'
#' \dontrun{
#' ## Example for one sample with one Bin File
#' path<- system.file("extdata/samp1", "", package="BayLum")
#' folder=""
#' nbsample=1  # give the number of sample
#' Data <- Generate_DataFile(
#'  Path = path,
#'  FolderNames = folder,
#'  Nb_sample = nbsample,
#'  verbose = FALSE)
#' str(Data)
#'
#' ## to save information in RData object in folder containing bin file
#' # save(Data,file=c(paste(path,folder,'Data.RData',sep="")))
#' ## to load information containing Data.RData object
#' # load(file=c(paste(path,folder,"Data.RData",sep="")))
#' }
#'
#' @name Generate_DataFile-deprecated
#' @md
#' @export
Generate_DataFile <- function(
  Path,
  FolderNames,
  Nb_sample,
  Nb_binfile = length(FolderNames),
  BinPerSample = rep(1, Nb_sample),
  sepDP = c(","),
  sepDE = c(","),
  sepDS = c(","),
  sepR = c("="),
  verbose = TRUE,
  ...
){

  .Deprecated("create_DataFile()")

  #--- create object needed
  #---------------------------------------
  # BaSAR observations for samples
  LT = list()     # corresponding to observation of natural and regenerated luminescence signal : N_{k,j}^(i) per sample
  sLT = list()    # correspondind to error of observation of L : sigma_{N_{K,j}^(i)} per sample
  ITimes = list() # corresponding to obsevation : t_{k,j}^(i) per sample

  # information on bin file
  dLab = matrix(1, ncol = Nb_binfile, nrow = 2)  # corresponding to dose source rate of the lab : d_{lab} per sample
  regDose = list()          # computed regenerated dose multiplying the 2 aboves lines per sample
  J = rep(0, Nb_binfile)     # aliquot number per sample
  Nb_measurement = rep(0, Nb_binfile) # measurement number per aliquot
  K = rep(0, Nb_binfile)     # point number considered for the growth curve
  ddot = matrix(1, ncol = Nb_binfile, nrow = 2)   # the dose rate recieved by the sample during the time,
  #   if there is various bin file for one sample, they must have the same ddot

  # Fetch additional arguments for read_BIN2R()----------------------------------------------------

  ##the internal preset
  read_BIN2R.settings <- list(
    duplicated.rm = TRUE,
    verbose = verbose
  )

  ##overwrite the preset if needed
  read_BIN2R.settings <- modifyList(x = read_BIN2R.settings, val = list(...))

  ##combine list and remove duplicated entries
  read_BIN2R.settings <- c(read_BIN2R.settings, list(...))
  read_BIN2R.settings <- read_BIN2R.settings[!duplicated(names(read_BIN2R.settings))]

  #--- read informations
  #---------------------------------------
  bf=0
  for(i in 1:Nb_sample){
    for(nb in 1:BinPerSample[i]){
      bf=bf+1
      if(verbose) print(paste("File being read:",FolderNames[bf]))

      # read files....
      XLS_file <- read.csv(file=paste(Path,FolderNames[bf],"/DiscPos.csv",sep=""),sep=sepDP)
      DL <- read.csv(file=paste(Path,FolderNames[bf],"/DoseSource.csv",sep=""),sep=sepDS)
      dd <- read.csv(file=paste(Path,FolderNames[bf],"/DoseEnv.csv",sep=""),sep=sepDE)
      rule <- read.csv(file=paste(Path,FolderNames[bf],"/rule.csv",sep=""),sep=sepR)

      # BIN file analysis
      object <- Luminescence::read_BIN2R(
        file = paste0(Path, FolderNames[bf]),
        duplicated.rm = read_BIN2R.settings$duplicated.rm,
        verbose =  read_BIN2R.settings$verbose
      )[[1]]

      # csv file indicating position and disc selection and preparation to be red
      XLS_file[[3]]<-XLS_file[[2]]
      XLS_file[[2]]<-XLS_file[[1]]
      XLS_file[[1]] <- object@METADATA$FNAME[1:length(XLS_file[[1]])]
      names(XLS_file) <- c("BIN_FILE","DISC","GRAIN")

      # aliquot number
      J[bf] <- length(XLS_file[,1])

      # data d_lab
      dLab[,bf] <- c(DL$obs[1],DL$var[1])

      # data ddot
      ddot[,bf] <- c(dd[[1]],dd[[2]])

      # information for Lx/Tx
      prop=(length(c(rule[3,1]:rule[4,1]))/length(c(rule[1,1]:rule[2,1])))

      #--- selection of measurement corresponding to the selection done
      #---------------------------------------
      ind=c()
      for(j in 1:J[bf]){
        ind=c(ind,object@METADATA[object@METADATA[,"POSITION"]== XLS_file[j,2] & object@METADATA[,"GRAIN"]== XLS_file[j,3],1])
      }
      # what is ind...
      (object@METADATA[ind[1:20],c("POSITION","GRAIN","IRR_TIME")])

      # regeneration dose number
      Nb_measurement[bf]=length(ind)/J[bf]
      K[bf]=Nb_measurement[bf]/2-(rule[10,1]+1)

      #--- check if K, Nb_measurement, prop are integer
      #---------------------------------------
      # if(is.integer(prop)==FALSE){
      #   warning(paste("Problem folder: ",FolderNames[bf],
      #                 ". Check in rule.csv file if (endBackground-beginBackground+1)/(endSignal-beginSignal+1) is integer.",sep=""),
      #           call. = FALSE)
      # }

      if((Nb_measurement[bf]-floor(Nb_measurement[bf]))!=0){
        warning(paste("Problem folder: ",FolderNames[bf],
                      ". Check in bin.bin file if for all aliquots the measurement of Lx and Tx is the same.
                      If not you can create a new subfolder with the same bin.bin, DoseEnv.csv, DoseSource.csv, rule.csv
                      and a new DiscPos.csv corresponding to these aliquots that had different number of measurement than the previous aliquot.
                      That means considered an other bin file for your sample.",sep=""),
                call. = FALSE)
        nb_mes=floor(Nb_measurement)
        while(nb_mes<=J[bf]){
          (u=object@METADATA[ind[c(floor(Nb_measurement),nb_mes)],c("POSITION","GRAIN","IRR_TIME")])
          if(u[1,2]==u[2,2]){
            nb_mes=nb_mes+1
          }else{break}
        }
        nb_mes=nb_mes-1
        ii=1
        while(ii <= J[bf]){
          (u=object@METADATA[ind[((ii-1)*nb_mes+1):(ii*nb_mes)],c("POSITION","GRAIN","IRR_TIME")])
          if(u[1,2]==u[16,2]){
            ii=ii+1
          }else{
            warning(paste("problem aliquot: position:",XLS_file[ii,2], 'and grain:',XLS_file[ii,3]))
            ii=J[bf]+1}
        }
      }

      if(K[bf]<0){
        warning(paste("Problem folder: ",FolderNames[bf],
                      ". Check in rule.csv file if the (nbOfLastCycleToRemove + 1) is not hihger than the (measurement number of Lx and Tx divided by 2).",sep=""),
                call. = FALSE)
      }

      #--- computation of irradiation time
      #---------------------------------------
      ITemps=matrix(data=NA,nrow=J[bf],ncol=K[bf])
      for(j in 1:J[bf]){
        indices=ind[seq(((j-1)*Nb_measurement[bf]+3),((j-1)*Nb_measurement[bf]+2*(K[bf]+1)),by=2)]
        ITemps[j,]=object@METADATA[indices,"IRR_TIME"]
      }
      # in memory
      if(nb==1){
        ITimes[[i]]=ITemps
      }else{
        t1=dim(ITimes[[i]])
        t2=dim(ITemps)
        if(t2[2]==t1[2]){
          ITimes[[i]]=rbind(ITimes[[i]],ITemps)
        }else{
          if(t2[2]<t1[2]){
            T1=matrix(data=NA,ncol=t1[2],nrow=t2[1])
            T1[,1:t2[2]]=ITemps
            ITimes[[i]]=rbind(ITimes[[i]],T1)
          }else{
            T2=matrix(data=NA,ncol=t2[2],nrow=t1[1])
            T2[,1:t1[2]]=ITimes[[i]]
            T2=rbind(T2,ITemps)
            ITimes[[i]]=T2
          }
        }
      }
      regDose[[i]]=dLab[1,bf]*ITimes[[i]]

      #--- computation of luminescence signals
      #---------------------------------------
      lt=matrix(data=NA,nrow=J[bf],ncol=K[bf]+1)
      slt=matrix(data=NA,nrow=J[bf],ncol=K[bf]+1)
      for(j in 1:J[bf]){
        #-- natural signal
        Lx=sum(object@DATA[ind[(j-1)*Nb_measurement[bf]+1]][[1]][rule[1,1]:rule[2,1]])
        Bx=sum(object@DATA[ind[(j-1)*Nb_measurement[bf]+1]][[1]][rule[3,1]:rule[4,1]])/prop
        Tx=sum(object@DATA[ind[(j-1)*Nb_measurement[bf]+2]][[1]][rule[5,1]:rule[6,1]])
        BTx=sum(object@DATA[ind[(j-1)*Nb_measurement[bf]+2]][[1]][rule[7,1]:rule[8,1]])/prop
        # computation of regenerated luminescence
        lt[j,1]=(Lx-Bx)/(Tx-BTx)
        # computation of error of regenerated luminescence
        slt[j,1]=lt[j,1]*sqrt(((Lx*(1+rule[9,1]^2*Lx)+Bx*(1+rule[9,1]^2*Bx))/(Lx-Bx)^2)+((Tx*(1+rule[9,1]^2*Tx)+BTx*(1+rule[9,1]^2*BTx))/(Tx-BTx)^2))

        #-- regenerated signal
        for(k in 1:K[bf]){
          Lx=sum(object@DATA[ind[(j-1)*Nb_measurement[bf]+2*k+1]][[1]][rule[1,1]:rule[2,1]])
          Bx=sum(object@DATA[ind[(j-1)*Nb_measurement[bf]+2*k+1]][[1]][rule[3,1]:rule[4,1]])/prop
          Tx=sum(object@DATA[ind[(j-1)*Nb_measurement[bf]+2*(k+1)]][[1]][rule[5,1]:rule[6,1]])
          BTx=sum(object@DATA[ind[(j-1)*Nb_measurement[bf]+2*(k+1)]][[1]][rule[7,1]:rule[8,1]])/prop
          # computation of regenerated luminescence
          lt[j,k+1]=(Lx-Bx)/(Tx-BTx)
          # computation of error of regenerated luminescence
          slt[j,k+1]=lt[j,k+1]*sqrt(((Lx*(1+rule[9,1]^2*Lx)+Bx*(1+rule[9,1]^2*Bx))/(Lx-Bx)^2)+((Tx*(1+rule[9,1]^2*Tx)+BTx*(1+rule[9,1]^2*BTx))/(Tx-BTx)^2))
        }
      }
      # in memory
      if(nb==1){
        LT[[i]]=lt
        sLT[[i]]=slt
      }else{
        t1=dim(LT[[i]])
        t2=dim(lt)
        if(t2[2]==t1[2]){
          LT[[i]]=rbind(LT[[i]],lt)
          sLT[[i]]=rbind(sLT[[i]],slt)
        }else{
          if(t2[2]<t1[2]){
            T1=matrix(data=NA,ncol=t1[2],nrow=t2[1])
            T1[,1:t2[2]]=lt
            LT[[i]]=rbind(LT[[i]],T1)
            T1=matrix(data=NA,ncol=t1[2],nrow=t2[1])
            T1[,1:t2[2]]=slt
            sLT[[i]]=rbind(sLT[[i]],T1)
          }else{
            T2=matrix(data=NA,ncol=t2[2],nrow=t1[1])
            T2[,1:t1[2]]=LT[[i]]
            T2=rbind(T2,lt)
            LT[[i]]=T2
            T2=matrix(data=NA,ncol=t2[2],nrow=t1[1])
            T2[,1:t1[2]]=sLT[[i]]
            T2=rbind(T2,slt)
            sLT[[i]]=T2
          }
        }
      }

    }
  }


  #--- return information needed to compute BaSAR analysis
  #---------------------------------------
  Liste=list("LT"=LT,"sLT"=sLT,"ITimes"=ITimes,"dLab"=dLab,"ddot_env"=ddot,"regDose"=regDose,"J"=J,"K"=K,"Nb_measurement"=Nb_measurement)
  return(Liste)
}

