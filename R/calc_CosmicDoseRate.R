#' @title Calculate the cosmic dose rate
#'
#' @description
#' This function calculates the cosmic dose rate taking into account the soft-
#' and hard-component of the cosmic ray flux and allows corrections for
#' geomagnetic latitude, altitude above sea-level and geomagnetic field
#' changes.
#'
#' This function calculates the total cosmic dose rate considering both the
#' soft- and hard-component of the cosmic ray flux.
#'
#' **Internal calculation steps**
#'
#' (1)
#' Calculate total depth of all absorber in hg/cm² (1 hg/cm² = 100 g/cm²)
#'
#' \deqn{absorber = depth_1*density_1 + depth_2*density_2 + ... + depth_n*density_n}
#'
#'
#' (2)
#' If `half.depth = TRUE`
#'
#' \deqn{absorber = absorber/2}
#'
#'
#' (3)
#' Calculate cosmic dose rate at sea-level and 55 deg. latitude
#'
#' a) If absorber is > 167 g/cm² (only hard-component; Allkofer et al.  1975):
#' apply equation given by Prescott & Hutton (1994) (c.f. Barbouti & Rastin
#' 1983)
#'
#' \deqn{D0 = C/(((absorber+d)^\alpha+a)*(absober+H))*exp(-B*absorber)}
#'
#' b) If absorber is < 167 g/cm² (soft- and hard-component): derive D0 from
#' Fig. 1 in Prescott & Hutton (1988).
#'
#'
#' (4)
#' Calculate geomagnetic latitude (Prescott & Stephan 1982, Prescott &
#' Hutton 1994)
#'
#' \deqn{\lambda = arcsin(0.203*cos(latitude)*cos(longitude-291)+0.979*
#' sin(latitude))}
#'
#'
#' (5)
#' Apply correction for geomagnetic latitude and altitude above sea-level.
#' Values for F, J and H were read from Fig. 3 shown in Prescott & Stephan
#' (1982) and fitted with 3-degree polynomials for lambda < 35 degree and a
#' linear fit for lambda > 35 degree.
#'
#' \deqn{Dc = D0*(F+J*exp((altitude/1000)/H))}
#'
#'
#' (6)
#' Optional: Apply correction for geomagnetic field changes in the last
#' 0-80 ka (Prescott & Hutton 1994). Correction and altitude factors are given
#' in Table 1 and Fig. 1 in Prescott & Hutton (1994). Values for altitude
#' factor were fitted with a 2-degree polynomial. The altitude factor is
#' operated on the decimal part of the correction factor.
#'
#' \deqn{Dc' = Dc*correctionFactor}
#'
#'
#' **Usage of `depth` and `density`**
#'
#' (1) If only one value for depth and density is provided, the cosmic dose
#' rate is calculated for exactly one sample and one absorber as overburden
#' (i.e. `depth*density`).
#'
#' (2) In some cases it might be useful to calculate the cosmic dose rate for a
#' sample that is overlain by more than one absorber, e.g. in a profile with
#' soil layers of different thickness and a distinct difference in density.
#' This can be calculated by providing a matching number of values for
#' `depth` and `density` (e.g. `depth = c(1, 2), density = c(1.7, 2.4)`)
#'
#' (3) Another possibility is to calculate the cosmic dose rate for more than
#' one sample of the same profile. This is done by providing more than one
#' values for `depth` and only one for `density`. For example,
#' `depth = c(1, 2, 3)` and `density = 1.7` will calculate the cosmic dose rate
#' for three samples in 1, 2 and 3 m depth in a sediment of density 1.7 g/cm³.
#'
#' @param depth [numeric] (**required**):
#' depth of overburden (m). For more than one absorber use \cr
#' `c(depth_1, depth_2, ..., depth_n)`
#'
#' @param density [numeric] (**required**):
#' average overburden density (g/cm³). For more than one absorber use \cr
#' `c(density_1, density_2, ..., density_n)`
#'
#' @param latitude [numeric] (**required**):
#' latitude (decimal degree), N positive
#'
#' @param longitude [numeric] (**required**):
#' longitude (decimal degree), E positive
#'
#' @param altitude [numeric] (**required**):
#' altitude (m above sea-level)
#'
#' @param corr.fieldChanges [logical] (*with default*):
#' correct for geomagnetic field changes after Prescott & Hutton (1994).
#' Apply only when justified by the data.
#'
#' @param est.age [numeric] (*with default*):
#' estimated age range (ka) for geomagnetic field change correction (0-80 ka allowed)
#'
#' @param half.depth [logical] (*with default*):
#' How to overcome with varying overburden thickness. If `TRUE` only half the
#' depth is used for calculation. Apply only when justified, i.e. when a constant
#' sedimentation rate can safely be assumed.
#'
#' @param error [numeric] (*with default*):
#' general error (percentage) to be implemented on corrected cosmic dose rate estimate
#'
#' @param ... further arguments (`verbose` to disable/enable console output).
#'
#' @return
#' Returns a terminal output. In addition an
#' [RLum.Results-class] object is returned containing the
#' following element:
#'
#' \item{summary}{[data.frame] summary of all relevant calculation results.}
#' \item{args}{[list] used arguments}
#' \item{call}{[call] the function call}
#'
#' The output should be accessed using the function [get_RLum].
#'
#' @note
#' Despite its universal use, the equation to calculate the cosmic dose
#' rate provided by Prescott & Hutton (1994) is falsely stated to be valid from
#' the surface to 10^4 hg/cm² of standard rock. The original expression by
#' Barbouti & Rastin (1983) only considers the muon flux (i.e. hard-component)
#' and is, by their own definition, only valid for depths between 10-10^4
#' hg/cm².
#'
#' Thus, for near-surface samples (i.e. for depths < 167 g/cm²) the equation
#' of Prescott & Hutton (1994) underestimates the total cosmic dose rate, as it
#' neglects the influence of the soft-component of the cosmic ray flux. For
#' samples at zero depth and at sea-level the underestimation can be as large
#' as ~0.1 Gy/ka. In a previous article, Prescott & Hutton (1988) give another
#' approximation of Barbouti & Rastin's equation in the form of
#'
#' \deqn{D = 0.21*exp(-0.070*absorber+0.0005*absorber^2)}
#'
#' which is valid for depths between 150-5000 g/cm². For shallower depths (<
#' 150 g/cm²) they provided a graph (Fig. 1) from which the dose rate can be
#' read.
#'
#' As a result, this function employs the equation of Prescott & Hutton (1994)
#' only for depths > 167 g/cm², i.e. only for the hard-component of the cosmic
#' ray flux. Cosmic dose rate values for depths < 167 g/cm² were obtained from
#' the "AGE" program (Gruen 2009) and fitted with a 6-degree polynomial curve
#' (and hence reproduces the graph shown in Prescott & Hutton 1988). However,
#' these values assume an average overburden density of 2 g/cm³.
#'
#' It is currently not possible to obtain more precise cosmic dose rate values
#' for near-surface samples as there is no equation known to the author of this
#' function at the time of writing.
#'
#'
#' @section Function version: 0.5.3
#'
#' @author
#' Christoph Burow, University of Cologne (Germany)
#'
#' @seealso [BaseDataSet.CosmicDoseRate]
#'
#' @references
#' Allkofer, O.C., Carstensen, K., Dau, W.D., Jokisch, H., 1975.
#' Letter to the editor. The absolute cosmic ray flux at sea level. Journal of
#' Physics G: Nuclear and Particle Physics 1, L51-L52.
#'
#' Barbouti, A.I., Rastin, B.C., 1983. A study of the absolute intensity of muons at sea level
#' and under various thicknesses of absorber. Journal of Physics G: Nuclear and
#' Particle Physics 9, 1577-1595.
#'
#' Crookes, J.N., Rastin, B.C., 1972. An
#' investigation of the absolute intensity of muons at sea-level. Nuclear
#' Physics B 39, 493-508.
#'
#' Gruen, R., 2009. The "AGE" program for the
#' calculation of luminescence age estimates. Ancient TL 27, 45-46.
#'
#' Prescott, J.R., Hutton, J.T., 1988. Cosmic ray and gamma ray dosimetry for
#' TL and ESR. Nuclear Tracks and Radiation Measurements 14, 223-227.
#'
#' Prescott, J.R., Hutton, J.T., 1994. Cosmic ray contributions to dose rates
#' for luminescence and ESR dating: large depths and long-term time variations.
#' Radiation Measurements 23, 497-500.
#'
#' Prescott, J.R., Stephan, L.G., 1982. The contribution of cosmic radiation to the environmental dose for
#' thermoluminescence dating. Latitude, altitude and depth dependences. PACT 6, 17-25.
#'
#' @examples
#'
#' ##(1) calculate cosmic dose rate (one absorber)
#' calc_CosmicDoseRate(depth = 2.78, density = 1.7,
#'                     latitude = 38.06451, longitude = 1.49646,
#'                     altitude = 364, error = 10)
#'
#' ##(2a) calculate cosmic dose rate (two absorber)
#' calc_CosmicDoseRate(depth = c(5.0, 2.78), density = c(2.65, 1.7),
#'                     latitude = 38.06451, longitude = 1.49646,
#'                     altitude = 364, error = 10)
#'
#' ##(2b) calculate cosmic dose rate (two absorber) and
#' ##correct for geomagnetic field changes
#' calc_CosmicDoseRate(depth = c(5.0, 2.78), density = c(2.65, 1.7),
#'                     latitude = 12.04332, longitude = 4.43243,
#'                     altitude = 364, corr.fieldChanges = TRUE,
#'                     est.age = 67, error = 15)
#'
#'
#' ##(3) calculate cosmic dose rate and export results to .csv file
#' #calculate cosmic dose rate and save to variable
#' results<- calc_CosmicDoseRate(depth = 2.78, density = 1.7,
#'                               latitude = 38.06451, longitude = 1.49646,
#'                               altitude = 364, error = 10)
#'
#' # the results can be accessed by
#' get_RLum(results, "summary")
#'
#' #export results to .csv file - uncomment for usage
#' #write.csv(results, file = "c:/users/public/results.csv")
#'
#' ##(4) calculate cosmic dose rate for 6 samples from the same profile
#' ##    and save to .csv file
#' #calculate cosmic dose rate and save to variable
#' results<- calc_CosmicDoseRate(depth = c(0.1, 0.5 , 2.1, 2.7, 4.2, 6.3),
#'                               density = 1.7, latitude = 38.06451,
#'                               longitude = 1.49646, altitude = 364,
#'                               error = 10)
#'
#' #export results to .csv file - uncomment for usage
#' #write.csv(results, file = "c:/users/public/results_profile.csv")
#'
#' @export
calc_CosmicDoseRate<- function(
  depth,
  density,
  latitude,
  longitude,
  altitude,
  corr.fieldChanges = FALSE,
  est.age = NA,
  half.depth = FALSE,
  error = 10,
  ...
) {
  .set_function_name("calc_CosmicDoseRate")
  on.exit(.unset_function_name(), add = TRUE)

  ## Integrity checks -------------------------------------------------------

  .validate_class(depth, "numeric")
  .validate_not_empty(depth)
  .validate_class(density, "numeric")
  .validate_not_empty(density)
  .validate_class(latitude, "numeric")
  .validate_not_empty(latitude)
  .validate_class(longitude, "numeric")
  .validate_not_empty(longitude)
  .validate_class(altitude, "numeric")
  .validate_not_empty(altitude)

  if(any(depth < 0) || any(density < 0)) {
    .throw_error("No negative values allowed for 'depth' and 'density'")
  }

  if(corr.fieldChanges == TRUE) {
    if(is.na(est.age) == TRUE) {
      .throw_error("Correction for geomagnetic field ",
                   "changes requires an age estimate")
    }
    if(est.age > 80) {
      cat("\nCAUTION: No geomagnetic field change correction for samples",
          "older than 80 ka possible, 'corr.fieldChanges' set to FALSE")
      corr.fieldChanges<- FALSE
    }
  }

  if (length(depth) < length(density) ||
      (length(depth) > length(density) && length(density) > 1)) {
    .throw_error("The number of values for 'density' should either be 1 ",
                 "or correspond to the number of values for 'depth'")
  }

  settings <- list(verbose = TRUE)
  settings <- modifyList(settings, list(...))

  ##============================================================================##
  ## CALCULATIONS
  ##============================================================================##


  # initialize parameter for Prescott & Hutton (1994) equation

  C<- 6072
  B<- 0.00055
  d<- 11.6
  alpha<- 1.68
  a<- 75
  H<- 212

  #variable needed to check if cosmic dose rate is calculated for more
  #than one sample

  profile.mode<- FALSE

  ## calculate total absorber of n depths and n densities [single sample]
  ## the calculation is still valid if there is only one depth and one density
  if(length(depth)==length(density)){
    hgcm<- 0
    for(i in 1:length(depth)) {
      hgcm<- hgcm + depth[i]*density[i]
    }
    if(half.depth == TRUE) {
      hgcm<- hgcm/2
    }
  }

  #if there are >1 depths and only one density, calculate
  #absorber for each sample [multi sample]
  if(length(depth) > length(density) & length(density) == 1) {
    profile.mode<- TRUE
    hgcm<- 1:length(depth)
    for(i in 1:length(depth)) {
      hgcm[i]<- depth[i]*density
    }
    if(half.depth == TRUE) {
      hgcm<- hgcm/2
    }
    profile.results<- data.frame(rbind(c(1:3)),cbind(1:length(depth)))
    colnames(profile.results)<- c("depth (m)", "d0 (Gy/ka)",
                                  "dc (Gy/ka)","dc_error (Gy/ka)")
  }


  for(i in 1:length(hgcm)) {

    # calculate cosmic dose rate at sea-level for geomagnetic latitude 55 degrees
    d0 <- (C / ((((hgcm[i] + d)^alpha) + a) * (hgcm[i] + H))) * exp(-B * hgcm[i])

    temp.hgcm <- hgcm[i] * 100
    if (temp.hgcm < 167) {
      d0.ph <- d0

      if (temp.hgcm < 40) {
        d0<- -6*10^-8*temp.hgcm^3+2*10^-5*temp.hgcm^2-0.0025*temp.hgcm+0.2969
      }
      else {
        d0<- 2*10^-6*temp.hgcm^2-0.0008*temp.hgcm+0.2535
      }
      d0 <- max(d0, d0.ph)
    }
    # Calculate geomagnetic latitude
    gml.temp<- 0.203*cos((pi/180)*latitude)*
      cos(((pi/180)*longitude)-(291*pi/180))+0.979*
      sin((pi/180)*latitude)
    true.gml<- asin(gml.temp)/(pi/180)
    gml<- abs(asin(gml.temp)/(pi/180))

    # Find values for F, J and H from graph shown in Prescott & Hutton (1994)
    # values were read from the graph and fitted with 3 degree polynomials and a
    # linear part

    if(gml < 36.5) { # Polynomial fit

      F_ph<- -7*10^-7*gml^3-8*10^-5*gml^2-0.0009*gml+0.3988
    }
    else { # Linear fit

      F_ph<- -0.0001*gml + 0.2347
    }

    if(gml < 34) { # Polynomial fit

      J_ph<- 5*10^-6*gml^3-5*10^-5*gml^2+0.0026*gml+0.5177
    }
    else { # Linear fit
      J_ph<- 0.0005*gml + 0.7388
    }

    if(gml < 36) { # Polynomial fit

      H_ph<- -3*10^-6*gml^3-5*10^-5*gml^2-0.0031*gml+4.398
    }
    else { # Linear fit

      H_ph<- 0.0002*gml + 4.0914
    }

    # Apply correction for geomagnetic latitude and altitude according to
    # Prescott & Hutton (1994)

    dc<- d0*(F_ph + J_ph*exp((altitude/1000)/H_ph))


    ## Additional correction for geomagnetic field change

    if(corr.fieldChanges==TRUE) {

      if(gml <= 35) {

        # Correction matrix for geomagnetic field changes at
        # sea-level (Prescott & Hutton (1994), Table 1)

        corr.matrix<- data.frame(rbind(1:5),1:7)
        colnames(corr.matrix)<- c(0, 10, 20, 30, 35, ">35")
        rownames(corr.matrix)<- c("0-5","5-10","10-15","15-20","20-35","35-50",
                                  "50-80")

        corr.matrix[1,]<- c(0.97, 0.97, 0.98, 0.98, 0.98, 1.00)
        corr.matrix[2,]<- c(0.99, 0.99, 0.99, 0.99, 0.99, 1.00)
        corr.matrix[3,]<- c(1.00, 1.00, 1.00, 1.00, 1.00, 1.00)
        corr.matrix[4,]<- c(1.01, 1.01, 1.01, 1.00, 1.00, 1.00)
        corr.matrix[5,]<- c(1.02, 1.02, 1.02, 1.01, 1.00, 1.00)
        corr.matrix[6,]<- c(1.03, 1.03, 1.02, 1.01, 1.00, 1.00)
        corr.matrix[7,]<- c(1.02, 1.02, 1.02, 1.01, 1.00, 1.00)

        ## Find the correction factor for the given geomagnetic latitude
        row.idx <- cut(est.age, c(0, 5, 10, 15, 20, 35, 50, 80),
                       labels = FALSE)
        col.idx <- cut(gml, c(0, 5, 15, 25, 32.5, 35, Inf),
                       labels = FALSE)
        corr.fac <- corr.matrix[row.idx, col.idx]

        # Find altitude factor via fitted function 2-degree polynomial
        # This factor is only available for positive altitudes
        if(altitude > 0) {

          alt.fac<- -0.026*(altitude/1000)^2 + 0.6628*altitude/1000 + 1.0435

          # Combine geomagnetic latitude correction with altitude
          # correction (figure caption of Fig. 1 in Precott and Hutton (1994))
          diff.one<- corr.fac - 1
          corr.fac<- corr.fac + diff.one * alt.fac
        }

        # Final correction of cosmic dose rate

        dc<- dc * corr.fac

        if (settings$verbose)
          print(paste("corr.fac",corr.fac,"diff.one",diff.one,"alt.fac",alt.fac))

      } else {
        if (settings$verbose)
          cat("\n No geomagnetic field change correction necessary for geomagnetic latitude >35 degrees!")
      }
    }

    # calculate error
    dc.err<- dc*error/100

    # save intermediate results before next sample is calculated
    if(profile.mode==TRUE) {
      profile.results[i,1]<- round(depth[i],2)
      profile.results[i,2]<- round(d0,4)
      profile.results[i,3]<- round(dc,4)
      profile.results[i,4]<- round(dc.err,4)
    }

  }#END.OF.LOOP

  call<- sys.call()
  args<- list(depth = depth, density = density, latitude = latitude, longitude = longitude,
              altitude = altitude, corr.fieldChanges = corr.fieldChanges, est.age = est.age,
              half.depth = half.depth, error = error)

  if(length(hgcm)==1) {

    ##============================================================================##
    ##TERMINAL OUTPUT
    ##============================================================================##
    if (settings$verbose) {
      cat("\n\n [calc_CosmicDoseRate]")
      cat("\n\n ---------------------------------------------------------")
      cat("\n depth (m)              :", depth)
      cat("\n density (g cm^-3)      :", density)
      cat("\n latitude (N deg.)      :", latitude)
      cat("\n longitude (E deg.)     :", longitude)
      cat("\n altitude (m)           :", altitude)
      cat("\n ---------------------------------------------------------")
      cat("\n total absorber (g cm^-2)       :", round(hgcm[i] * 100, 3))
      cat("\n")
      cat("\n cosmic dose rate (Gy ka^-1)    :", round(d0, 4))
      cat("\n  [@sea-level & 55 deg. N G.lat]")
      cat("\n")
      cat("\n geomagnetic latitude (deg.)    :", round(true.gml, 1))
      cat("\n")
      cat("\n cosmic dose rate (Gy ka^-1)    :", round(dc, 4), "+-", round(dc.err, 4))
      cat("\n  [corrected]                 ")
      cat("\n ---------------------------------------------------------\n\n")
    }
    ##============================================================================##
    ##RETURN VALUES
    ##============================================================================##

    if(length(depth)==1) {
      temp1<- data.frame(depth=depth,density=density)
    } else {

      temp1a<- data.frame(rbind(c(1:length(depth))))
      tmpcoln1<- 1:length(depth)

      for(i in 1:length(depth)) {
        temp1a[i]<- depth[i]
        tmpcoln1[i]<- paste("depth",i)
      }

      temp1b<- data.frame(rbind(c(1:length(density))))
      tmpcoln2<- 1:length(density)

      for(i in 1:length(density)) {
        temp1b[i]<- density[i]
        tmpcoln2[i]<- paste("density",i)
      }

      colnames(temp1a)<- tmpcoln1
      colnames(temp1b)<- tmpcoln2
      temp1<- cbind(temp1a,temp1b)
    }

    temp2<- data.frame(latitude=latitude,longitude=longitude,
                       altitude=altitude,total_absorber.gcm2=hgcm*100,
                       d0=d0,geom_lat=true.gml,dc=dc)

    summary<- data.frame(cbind(temp1,temp2))

  } else {

    #terminal output
    if (settings$verbose) {
      cat("\n\n [calc_CosmicDoseRate]")
      cat("\n\n Calculating cosmic dose rate for", length(depth), "samples.\n\n")
      print(profile.results)
    }

    #return value
    add.info<- data.frame(latitude=latitude,longitude=longitude,
                          altitude=altitude,total_absorber.gcm2=hgcm*100,
                          geom_lat=true.gml)
    add.info<- rbind(add.info*length(i))
    colnames(profile.results)<- c("depth","d0","dc","dc_err")

    summary<- data.frame(cbind(profile.results,add.info))
  }

  newRLumResults.calc_CosmicDoseRate <- set_RLum(
      class = "RLum.Results",
      data = list(summary=summary,
                  args=args,
                  call=call))
  invisible(newRLumResults.calc_CosmicDoseRate)
}
