#' @title Convert between phi units and micrometers
#' 
#' @description The function converts values from the phi-scale (Krumbein 1934, 1938) to 
#' the micrometer-scale and vice versa.
#' 
#' @details 
#' 
#' \deqn{
#' \phi = -log2(D/D_{0})}
#' 
#' with \eqn{D} the diameter in µm and \eqn{D_{0}} the reference diameter. 
#' Herer 1000 µm.
#' 
#' @param phi [numeric] vector, grain-size class values 
#' in phi to be converted
#' 
#' @param mu [numeric] vector, grain-size class values 
#' in micrometres to be converted
#' 
#' @return [numeric] vector, converted grain-size class values
#' 
#' @author Michael Dietze, GFZ Potsdam (Germany)
#' 
#' @references 
#' 
#' Krumbein, W.C., 1938. Size frequency distributions of sediments and the normal phi curve. 
#' Journal of Sedimentary Research 8, 84–90. \doi{10.1306/D4269008-2B26-11D7-8648000102C1865D}
#' 
#' Krumbein, W.C., 1934. Size frequency distributions of sediments. 
#' Journal of Sedimentary Research 4, 65–77. \doi{10.1306/D4268EB9-2B26-11D7-8648000102C1865D}
#' 
#' @examples
#' ## load example data set
#' ## generate phi-values
#' phi <- -2:5
#' 
#' ## convert and show phi to mu
#' mu  <- convert_units(phi = phi)
#' mu
#' 
#' ## convert and show mu to phi
#' convert_units(mu = mu)
#' 
#' @md
#' @export convert_units
convert_units <- function(
  phi, 
  mu
) {
  if (missing(mu)) return(1000 * 2 ^ -phi)
  if (missing(phi)) return(-log2(mu / 1000))

  stop("[convert_units()] No correct variables provided!", call. = FALSE)
}