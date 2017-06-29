#' Creation of a template for \link{use_DRAC4flint} input.
#'
#' This function generates a template that can be fill in and use with \link{use_DRAC4flint}.
#'
#' @author David Strebler
#'
#' @export template_DRAC4flint

template_DRAC4flint <- function(

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

  flint.Dr <- list(U = NULL,
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

  flint.info <- list(grain.size.min = NULL,
                     grain.size.max = NULL,
                     grain.etch.min = NULL,
                     grain.etch.max = NULL,
                     a.value = NULL,
                     a.value.err = NULL,
                     water.content = NULL,
                     water.content.err = NULL,
                     density = NULL,
                     density.err = NULL)

  flint <- list(Dr = flint.Dr,
                info= flint.info)

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
                   flint = flint,
                   sediment = sediment,
                   cosmic = cosmic.Dr)

  class(template) <- c("DRAC4flint.list","list")

  return(template)
}
