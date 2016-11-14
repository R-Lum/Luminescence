#' Creation of a template for \link{use_DRAC4flint} input.
#'
#' This function generates a template that can be fill in and use with \link{use_DRAC4ceramic}.
#'
#' @author David Strebler
#'
#' @export template_DRAC4ceramic

template_DRAC4ceramic <- function(

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


  ceramic.Dr <- list(U = NULL,
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

  ceramic.info <- list(water.content = NULL,
                       water.content.err = NULL,
                       density = NULL,
                       density.err = NULL)

  ceramic <- list(Dr = ceramic.Dr,
                  info = ceramic.info)

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
                   grain= grain,
                   ceramic = ceramic,
                   sediment = sediment,
                   cosmic = cosmic.Dr)

  class(template) <- c("DRAC4ceramic.list","list")
  return(template)
}
