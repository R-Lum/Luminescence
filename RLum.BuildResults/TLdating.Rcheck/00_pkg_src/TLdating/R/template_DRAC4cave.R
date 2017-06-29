#' Creation of a template for \link{use_DRAC4cave} input.
#'
#' This function generates a template that can be fill in and use with \link{use_DRAC4cave}.
#'
#' @author David Strebler
#'
#' @export template_DRAC4cave

template_DRAC4cave <- function(

){

  info <- list(project = NULL,
               sample = NULL,
               date = NULL,
               mineral = NULL,
               conversion.factors = NULL,
               alpha.size.attenuation = NULL,
               beta.size.attenuation = NULL,
               beta.etch.attenuation = NULL)

  De <- list(De = NULL,
             De.err = NULL)

  grain.Dr <- list(U = NULL,
                   U.err = NULL,
                   Th = NULL,
                   Th.err = NULL,
                   K = NULL,
                   K.err = NULL,
                   Rb = NULL,
                   Rb.err = NULL,
                   K2Rb = NULL,
                   alpha = NULL,
                   alpha.err = NULL,
                   beta = NULL,
                   beta.err = NULL,
                   gamma = NULL,
                   gamma.err = NULL)

  grain.info <- list(grain.size.min = NULL,
                     grain.size.max = NULL,
                     grain.etch.min = NULL,
                     grain.etch.max = NULL,
                     a.value = NULL,
                     a.value.err = NULL)

  grain <- list(Dr = grain.Dr,
                info = grain.info)

  sediment.Dr <- list(U = NULL,
                      U.err = NULL,
                      Th = NULL,
                      Th.err = NULL,
                      K = NULL,
                      K.err = NULL,
                      Rb = NULL,
                      Rb.err = NULL,
                      K2Rb = NULL,
                      alpha = NULL,
                      alpha.err = NULL,
                      beta = NULL,
                      beta.err = NULL,
                      gamma = NULL,
                      gamma.err = NULL)

  sediment.info <- list(water.content = NULL,
                        water.content.err =NULL,
                        density = NULL,
                        density.err = NULL,
                        scale4shallow.depth = FALSE)

  sediment <- list(Dr = sediment.Dr,
                   info = sediment.info)

  rock.Dr <- list(U = NULL,
                  U.err = NULL,
                  Th = NULL,
                  Th.err = NULL,
                  K = NULL,
                  K.err = NULL,
                  Rb = NULL,
                  Rb.err = NULL,
                  K2Rb = NULL,
                  alpha = NULL,
                  alpha.err = NULL,
                  beta = NULL,
                  beta.err = NULL,
                  gamma = NULL,
                  gamma.err = NULL)

  rock.info <- list(water.content = NULL,
                    water.content.err = NULL,
                    density = NULL,
                    density.err = NULL,
                    ratio = NULL,
                    ratio.err = NULL)

  rock <- list(Dr = rock.Dr,
               info = rock.info)

  cosmic.Dr <- list(depth = NULL,
                    depth.err = NULL,
                    latitude = NULL,
                    longitude = NULL,
                    altitude = NULL,
                    Dr = NULL,
                    Dr.err = NULL,
                    corr.fieldChanges = FALSE)

  template <- list(info=info,
                   De = De,
                   grain= grain,
                   sediment = sediment,
                   rock = rock,
                   cosmic = cosmic.Dr)

  class(template) <- c("DRAC4cave.list","list")
  return(template)
}
