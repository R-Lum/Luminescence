#' Create a DRAC input data template (v1.1)
#'
#' This function returns a DRAC input template (v1.1) to be used in conjunction
#' with the use_DRAC() function
#' 
#' @param nrows \code{\link{integer}} (with default): specifies the number of rows
#' of the template (i.e., the number of data sets you want to submit)
#' 
#' @param notification \code{\link{logical}} (with default): show or hide the
#' notification
#'
#' @return A list.
#' 
#' @export
#'
#' @examples
#' 
#' # create a new DRAC input template
#' template <- template_DRAC()
#' 
#' # show content of the template
#' print(template)
#' print(template$`Project ID`)
#' print(template[[4]])
#' 
#' # fill in data
#' 
#' 
template_DRAC <- function(nrows = 1, notification = TRUE) {
  
  ## TODO:
  # 1 - allow mineral specific presets; new argument 'mineral'
  # 2 - add option to return the DRAC example data set
  
  ## LEGAL NOTICE ----
  messages <- list("\n",
                   "\t-------------------- IMPORTANT NOTE ------------------------\n",
                   "\t This function returns a DRAC input template to be used in ",
                   "\t conjunction with the use_DRAC() function.  \n",
                   "\t The template was reproduced with great care, but we do not",
                   "\t take any responsibility and we are not liable for any ",
                   "\t mistakes or unforeseen misbehaviour.",
                   "\t Note that this template is only compatible with DRAC",
                   "\t version 1.1. Before using this template make sure that",
                   "\t this is the correct version, otherwise expect unspecified",
                   "\t errors.\n",
                   "\t Please ensure you cite the use of DRAC in your work,",
                   "\t published or otherwise. Please cite the website name and",
                   "\t version (e.g. DRAC v1.1) and the accompanying journal",
                   "\t article:",
                   "\t Durcan, J.A., King, G.E., Duller, G.A.T., 2015.",
                   "\t DRAC: Dose rate and age calculation for trapped charge",
                   "\t dating. Quaternary Geochronology 28, 54-61. \n",
                   "\t Set 'notification = FALSE' to hide this message. \n",
                   "\t-------------------- IMPORTANT NOTE ------------------------",
                   "\n")
  
  if (notification) lapply(messages, message)
  
  # CREATE TEMPLATE ----
  template <- list(
    
    `Project ID` = 
      structure(rep("RLum", nrows), required = TRUE, key = "TI:1",
                description = "Inputs can be alphabetic, numeric or selected symbols (/ - () [] _). Spaces are not permitted."), # 
    
    `Sample ID` = 
      structure(rep("999", nrows), required = TRUE, key = "TI:2",
                description = "Inputs can be alphabetic, numeric or selected symbols (/ - () [] _). Spaces are not permitted."), #
    
    `Mineral` = 
      structure(factor(rep("Q", nrows), c("Q", "F", "PM")), required = TRUE,
                description = "The mineral used for dating: quartz, feldspar or polymineral. Input must be 'Q', 'F' or 'PM'."), #
    
    `Conversion factors` = 
      structure(factor(rep("Liritzisetal2013", nrows), c("AdamiecAitken1998", "Guerinetal2011", "Liritzisetal2013", "X")), required = FALSE, key = "TI:4",
                description = "The conversion factors required to calculate dose rates from radionuclide concentrations. Users have the option of datasets from Adamiec and Aitken (1998), Guerin et al. (2011) or Liritzis et al. (2013). Input must be 'AdamiecAitken1998', 'Guerinetal2011', 'Liritzisetal2013' or 'X' if conversion factors are not required."), #
    
    `ExternalU (ppm)` = 
      structure(rep(0, nrows), required = FALSE, key = "TI:5",
                description = "Radionuclide concentrations in parts per million for Uranium, Thorium and Rubidium and % for Potassium. Inputs must be 0 or positive and should not be left blank."), # 
    
    `errExternal U (ppm)` = 
      structure(rep(0, nrows), required = FALSE, key = "TI:6",
                description = "Radionuclide concentrations in parts per million for Uranium, Thorium and Rubidium and % for Potassium. Inputs must be 0 or positive and should not be left blank."), #
    
    `External Th (ppm)` = 
      structure(rep(0, nrows), required = FALSE, key = "TI:7",
                description = "Radionuclide concentrations in parts per million for Uranium, Thorium and Rubidium and % for Potassium. Inputs must be 0 or positive and should not be left blank."), #
    
    `rrExternal Th (ppm)` = 
      structure(rep(0, nrows), required = FALSE, key = "TI:8",
                description = "Radionuclide concentrations in parts per million for Uranium, Thorium and Rubidium and % for Potassium. Inputs must be 0 or positive and should not be left blank."), #
    
    `External K (%)` = 
      structure(rep(0, nrows), required = FALSE, key = "TI:9",
                description = "Radionuclide concentrations in parts per million for Uranium, Thorium and Rubidium and % for Potassium. Inputs must be 0 or positive and should not be left blank."), #
    
    `errExternal K (%)` = 
      structure(rep(0, nrows), required = FALSE, key = "TI:10",
                description = "Radionuclide concentrations in parts per million for Uranium, Thorium and Rubidium and % for Potassium. Inputs must be 0 or positive and should not be left blank."), #
    
    `External Rb (ppm)` = 
      structure(rep(0, nrows), required = FALSE, key = "TI:11",
                description = "Radionuclide concentrations in parts per million for Uranium, Thorium and Rubidium and % for Potassium. Inputs must be 0 or positive and should not be left blank."), #
    
    `errExternal Rb (ppm)` = 
      structure(rep(0, nrows), required = FALSE, key = "TI:12",
                description = "Radionuclide concentrations in parts per million for Uranium, Thorium and Rubidium and % for Potassium. Inputs must be 0 or positive and should not be left blank."), #
    
    `Calculate external Rb from K conc?` = 
      structure(factor(rep("Y", nrows), c("Y", "N")), required = FALSE, key = "TI:13",
                description = "Option to calculate a Rubidium concentration from Potassium, using the 270:1 ratio suggested by Mejdahl (1987). Input should be yes 'Y' or no 'N'."), #
    
    `Internal U (ppm)` = 
      structure(rep(0, nrows), required = FALSE, key = "TI:14",
                description = "Internal radionuclide concentrations in parts per million for Uranium, Thorium and Rubidium and % for Potassium. Inputs must be 0 or positive and should not be left blank."), #
    
    `errInternal U (ppm)` = 
      structure(rep(0, nrows), required = FALSE, key = "TI:15",
                description = "Internal radionuclide concentrations in parts per million for Uranium, Thorium and Rubidium and % for Potassium. Inputs must be 0 or positive and should not be left blank."), #
    
    `Internal Th (ppm)` = 
      structure(rep(0, nrows), required = FALSE, key = "TI:16",
                description = "Internal radionuclide concentrations in parts per million for Uranium, Thorium and Rubidium and % for Potassium. Inputs must be 0 or positive and should not be left blank."), #
    
    `errInternal Th (ppm)` = 
      structure(rep(0, nrows), required = FALSE, key = "TI:17",
                description = "Internal radionuclide concentrations in parts per million for Uranium, Thorium and Rubidium and % for Potassium. Inputs must be 0 or positive and should not be left blank."), #
    
    `Internal K (%)` = 
      structure(rep(0, nrows), required = FALSE, key = "TI:18",
                description = "Internal radionuclide concentrations in parts per million for Uranium, Thorium and Rubidium and % for Potassium. Inputs must be 0 or positive and should not be left blank."), #
    
    `errInternal K (%)` = 
      structure(rep(0, nrows), required = FALSE, key = "TI:19",
                description = "Internal radionuclide concentrations in parts per million for Uranium, Thorium and Rubidium and % for Potassium. Inputs must be 0 or positive and should not be left blank."), #
    
    `Rb (ppm)` = 
      structure(rep(0, nrows), required = FALSE, key = "TI:20",
                description = "Internal radionuclide concentrations in parts per million for Uranium, Thorium and Rubidium and % for Potassium. Inputs must be 0 or positive and should not be left blank."), #
    
    `errRb (ppm)` = 
      structure(rep(0, nrows), required = FALSE, key = "TI:21",
                description = "Internal radionuclide concentrations in parts per million for Uranium, Thorium and Rubidium and % for Potassium. Inputs must be 0 or positive and should not be left blank."), #
    
    `Calculate internal Rb from K conc?` = 
      structure(factor(rep("Y", nrows), c("Y", "N")), required = FALSE, key = "TI:22",
                description = "Option to calculate an internal Rubidium concentration from Potassium, using the 270:1 ratio suggested by Mejdahl (1987). Input should be yes 'Y' or no 'N'."), #
    
    `User external alphadoserate (Gy.ka-1)` = 
      structure(rep(0, nrows), required = FALSE, key = "TI:23",
                description = "Users may input directly measured values for external alpha, beta and gamma dose rates (in Gy.ka-1). Any positive inputs in these fields will override dose rates calculated from radionuclide concentrations. Inputs should be 0 or positive and should not be left blank"), #
    
    `errUser external alphadoserate (Gy.ka-1)` = 
      structure(rep(0, nrows), required = FALSE, key = "TI:24",
                description = "Users may input directly measured values for external alpha, beta and gamma dose rates (in Gy.ka-1). Any positive inputs in these fields will override dose rates calculated from radionuclide concentrations. Inputs should be 0 or positive and should not be left blank"), #
    
    `User external betadoserate (Gy.ka-1)` = 
      structure(rep(0, nrows), required = FALSE, key = "TI:25",
                description = "Users may input directly measured values for external alpha, beta and gamma dose rates (in Gy.ka-1). Any positive inputs in these fields will override dose rates calculated from radionuclide concentrations. Inputs should be 0 or positive and should not be left blank"), #
    
    `errUser external betadoserate (Gy.ka-1)` = 
      structure(rep(0, nrows), required = FALSE, key = "TI:26",
                description = "Users may input directly measured values for external alpha, beta and gamma dose rates (in Gy.ka-1). Any positive inputs in these fields will override dose rates calculated from radionuclide concentrations. Inputs should be 0 or positive and should not be left blank"), #
    
    `User external gamma doserate (Gy.ka-1)` = 
      structure(rep(0, nrows), required = FALSE, key = "TI:27",
                description = "Users may input directly measured values for external alpha, beta and gamma dose rates (in Gy.ka-1). Any positive inputs in these fields will override dose rates calculated from radionuclide concentrations. Inputs should be 0 or positive and should not be left blank"), #
    
    `errUser external gammadoserate (Gy.ka-1)` = 
      structure(rep(0, nrows), required = FALSE, key = "TI:28",
                description = "Users may input directly measured values for external alpha, beta and gamma dose rates (in Gy.ka-1). Any positive inputs in these fields will override dose rates calculated from radionuclide concentrations. Inputs should be 0 or positive and should not be left blank"), #
    
    `User internal doserate (Gy.ka-1)` = 
      structure(rep(0, nrows), required = FALSE, key = "TI:29",
                description = "Users may input an internal dose rate (either alpha, beta or the sum of the two; in Gy.ka-1). DRAC will assume that this value has already been corrected for attenuation. Inputs in this field will override dose rates calculated from radionuclide concentrations. Inputs should be 0 or positive and not left blank."), #
    
    `errUser internal doserate (Gy.ka-1)` = 
      structure(rep(0, nrows), required = FALSE, key = "TI:30",
                description = "Users may input an internal dose rate (either alpha, beta or the sum of the two; in Gy.ka-1). DRAC will assume that this value has already been corrected for attenuation. Inputs in this field will override dose rates calculated from radionuclide concentrations. Inputs should be 0 or positive and not left blank."), #
    
    `Scale gammadoserate at shallow depths?` = 
      structure(factor(rep("Y", nrows), c("Y", "N")), required = FALSE, key = "TI:31",
                description = "Users may choose to scale gamma dose rates for samples taken within 0.3 m of the ground surface. The scaling factors of Aitken (1985) are used. Input should be yes 'Y' or no 'N'."), #
    
    `Grain size min (microns)` = 
      structure(rep(100, nrows), required = TRUE, key = "TI:32",
                description = "The grain size range analysed. DRAC can be used for the grain size ranges between 1 and 1000 µm. Inputs should range between 1 and 1000 and not be left blank."), #
    
    `Grain size max (microns)` = 
      structure(rep(150, nrows), required = TRUE, key = "TI:33",
                description = "The grain size range analysed. DRAC can be used for the grain size ranges between 1 and 1000 µm. Inputs should range between 1 and 1000 and not be left blank."), #
    
    `alpha-Grain size attenuation` = 
      structure(factor(rep("Brennanetal1991", nrows), c("Bell1980", "Brennanetal1991")), required = TRUE, key = "TI:34",
                description = "The grain size attenuation factors for the alpha dose rate. Users have the option of datasets from Bell (1980) and Brennan et al. (1991). Input must be 'Bell1980' or 'Brennanetal1991'."), #
    
    `beta-Grain size attenuation ` = 
      structure(factor(rep("Guerinetal2012-Q", nrows), c("Mejdahl1979", "Brennan2003", "Guerinetal2012-Q", "Guerinetal2012-F")), required = TRUE, key = "TI:35",
                description = "The grain size attenuation factors for the beta dose rate. Users have the option of datasets from Mejdahl (1979), Brennan (2003) and Guerin et al. (2012) for quartz or feldspar. Input must be 'Mejdahl1979', 'Brennan2003', 'Guerinetal2012-Q' or 'Guerinetal2012-F' ."), #
    
    `Etch depth min (microns)` = 
      structure(rep(0, nrows), required = TRUE, key = "TI:36",
                description = "The user defined etch depth range (µm). Inputs should range between 0 and 30 and not be left blank."), #
    
    `Etch depth max (microns)` = 
      structure(rep(10, nrows), required = TRUE, key = "TI:37",
                description = "The user defined etch depth range (µm). Inputs should range between 0 and 30 and not be left blank."), #
    
    `beta-Etch depth attenuation factor` = 
      structure(factor(rep("Bell1979", nrows), c("Bell1979", "Brennan2003")), required = FALSE, key = "TI:38",
                description = "The etch depth attenuation factors for the beta dose rate. Users have the option of datasets from Bell (1979) and Brennan (2003). Input must be 'Bell1979' or 'Brennan2003'. Note: only the dataset of Bell (1980) is provided for attenuation of the alpha dose rate by etching."), #
    
    `a-value` = 
      structure(rep(0, nrows), required = FALSE, key = "TI:39",
                description = "Alpha track efficiency value and uncertainty defined by the user. Inputs should be 0 or positive and not left blank."), #
    
    `erra-value` = 
      structure(rep(0, nrows), required = TRUE, key = "TI:40",
                description = "Alpha track efficiency value and uncertainty defined by the user. Inputs should be 0 or positive and not left blank."), #
    
    `Water content ((wet weight - dry weight)/dry weight) %` = 
      structure(rep(0, nrows), required = TRUE, key = "TI:41",
                description = "Sediment water content (%) over the burial period. Inputs should be 0 or positive and not be left blank."), #
    
    `errWater content %` = 
      structure(rep(0, nrows), required = FALSE, key = "TI:42",
                description = "Sediment water content (%) over the burial period. Inputs should be 0 or positive and not be left blank."), #
    
    `Depth (m)` = 
      structure(rep("X", nrows), required = FALSE, key = "TI:43",
                description = "Depth and uncertainty from which sample was extracted beneath the ground surface. Inputs should be 0 or positive and not left blank. If user defined Dc will be used then an 'X' must be input."), #
    
    `errDepth (m)` = 
      structure(rep("X", nrows), required = FALSE, key = "TI:44",
                description = "Depth and uncertainty from which sample was extracted beneath the ground surface. Inputs should be 0 or positive and not left blank. If user defined Dc will be used then an 'X' must be input."), #
    
    `Overburden density (g cm-3)` = 
      structure(rep("X", nrows), required = FALSE, key = "TI:45",
                description = "Density of the overlying sediment matrix from which the sample was taken. Inputs should be 0 or positive and not be left blank. If user defined Dc will be used then an 'X' must be input."), #
    
    `errOverburden density (g cm-3)` = 
      structure(rep("X", nrows), required = FALSE, key = "TI:46",
                description = "Density of the overlying sediment matrix from which the sample was taken. Inputs should be 0 or positive and not be left blank. If user defined Dc will be used then an 'X' must be input."), #
    
    `Latitude (decimal degrees)` = 
      structure(rep("X", nrows), required = FALSE, key = "TI:47",
                description = "Latitude and longitude of sample location (in degree decimals). Positive values should be used for northern latitudes and eastern longitudes and negative values for southern latitudes and western longitudes. Inputs should range from – 90 to 90 degrees for latitudes and -180 to 180 degrees for longitude. If user defined Dc will be used then an 'X' must be input."), # 
    
    `Longitude (decimal degrees)` = 
      structure(rep("X", nrows), required = FALSE, key = "TI:48",
                description = "Latitude and longitude of sample location (in degree decimals). Positive values should be used for northern latitudes and eastern longitudes and negative values for southern latitudes and western longitudes. Inputs should range from – 90 to 90 degrees for latitudes and -180 to 180 degrees for longitude. If user defined Dc will be used then an 'X' must be input."), # 
    
    `Altitude (m)` = 
      structure(rep("X", nrows), required = FALSE, key = "TI:49",
                description = "Altitude of sample location in metres above sea level. Input should be less than 5000 and not left blank. If user defined Dc will be used then an 'X' must be input."), #
    
    `User cosmicdoserate (Gy.ka-1)` = 
      structure(rep("X", nrows), required = FALSE, key = "TI:50",
                description = "Users may input a cosmic dose rate (in Gy.ka-1). Inputs in these fields will override the DRAC calculated cosmic dose rate. Inputs should be positive or 'X' if not required, and not left blank."), #
    
    `errUser cosmicdoserate (Gy.ka-1)` = 
      structure(rep("X", nrows), required = FALSE, key = "TI:51",
                description = "Users may input a cosmic dose rate (in Gy.ka-1). Inputs in these fields will override the DRAC calculated cosmic dose rate. Inputs should be positive or 'X' if not required, and not left blank."), #
    
    `De (Gy)` = 
      structure(rep("X", nrows), required = FALSE, key = "TI:52",
                description = "Sample De and uncertainty (in Gy). Inputs should be positive or 'X' if not required, and not left blank."), #
    
    `errDe (Gy)` = 
      structure(rep("X", nrows), required = FALSE, key = "TI:53",
                description = "Sample De and uncertainty (in Gy). Inputs should be positive or 'X' if not required, and not left blank.") #
  )   
  
  
  ## RETURN VALUE ---
  # add an additional DRAC class so we can define our own S3 method for as.data.frame
  class(template) <- c("DRAC.list", "list")
  invisible(template)
}



as.data.frame.DRAC.list <- function(x, row.names = NULL, optional = FALSE, ...) {
  DF <- as.data.frame.list(x)
  colnames(DF) <- paste0("TI:", 1:ncol(DF))
  class(DF) <- c("data.frame", "DRAC.data.frame")
  return(DF)
}

