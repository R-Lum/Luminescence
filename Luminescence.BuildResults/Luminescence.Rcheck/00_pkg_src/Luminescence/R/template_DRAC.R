#' Create a DRAC input data template (v1.2)
#'
#' This function returns a DRAC input template (v1.2) to be used in conjunction
#' with the use_DRAC() function
#'
#' @param nrow [integer] (*with default*):
#' specifies the number of rows of the template (i.e., the number of data
#' sets you want to submit).
#'
#' @param preset [character] (*optional*):
#' By default, all values of the template are set to `NA`, which means that
#' the user needs to fill in **all** data first before submitting to DRAC
#' using `use_DRAC()`. To reduce the number of values that need to be
#' provided, `preset` can be used to create a template with at least
#' a minimum of reasonable preset values.
#'
#' `preset` can be one of the following:
#' - `quartz_coarse`
#' - `quartz_fine`
#' - `feldspar_coarse`
#' - `polymineral_fine`
#' - `DRAC-example_quartz`
#' - `DRAC-example_feldspar`
#' - `DRAC-example_polymineral`
#'
#' Note that the last three options can be used to produce a template
#' with values directly taken from the official DRAC input .csv file.
#'
#' @param notification [logical] (*with default*):
#' show or hide the notification
#'
#' @return A list.
#'
#' @author
#' Christoph Burow, University of Cologne (Germany), Sebastian Kreutzer, IRAMAT-CRP2A, Université Bordeaux Montaigne (France)
#'
#' @references
#' Durcan, J.A., King, G.E., Duller, G.A.T., 2015. DRAC: Dose Rate and Age Calculator for trapped charge dating.
#' Quaternary Geochronology 28, 54-61. doi:10.1016/j.quageo.2015.03.012
#'
#' @seealso [as.data.frame], [list]
#'
#' @examples
#'
#' # create a new DRAC input input
#' input <- template_DRAC(preset = "DRAC-example_quartz")
#'
#' # show content of the input
#' print(input)
#' print(input$`Project ID`)
#' print(input[[4]])
#'
#'
#' ## Example: DRAC Quartz example
#' # note that you only have to assign new values where they
#' # are different to the default values
#' input$`Project ID` <- "DRAC-Example"
#' input$`Sample ID` <- "Quartz"
#' input$`Conversion factors` <- "AdamiecAitken1998"
#' input$`External U (ppm)` <- 3.4
#' input$`errExternal U (ppm)` <- 0.51
#' input$`External Th (ppm)` <- 14.47
#' input$`errExternal Th (ppm)` <- 1.69
#' input$`External K (%)` <- 1.2
#' input$`errExternal K (%)` <- 0.14
#' input$`Calculate external Rb from K conc?` <- "N"
#' input$`Calculate internal Rb from K conc?` <- "N"
#' input$`Scale gammadoserate at shallow depths?` <- "N"
#' input$`Grain size min (microns)` <- 90
#' input$`Grain size max (microns)` <- 125
#' input$`Water content ((wet weight - dry weight)/dry weight) %` <- 5
#' input$`errWater content %` <- 2
#' input$`Depth (m)` <- 2.2
#' input$`errDepth (m)` <- 0.22
#' input$`Overburden density (g cm-3)` <- 1.8
#' input$`errOverburden density (g cm-3)` <- 0.1
#' input$`Latitude (decimal degrees)` <- 30.0000
#' input$`Longitude (decimal degrees)` <- 70.0000
#' input$`Altitude (m)` <- 150
#' input$`De (Gy)` <- 20
#' input$`errDe (Gy)` <- 0.2
#'
#' # use DRAC
#' \dontrun{
#' output <- use_DRAC(input)
#' }
#'
#' @md
#' @export
template_DRAC <- function(
  nrow = 1,
  preset = NULL,
  notification = TRUE
){

  ## TODO:
  # 1 - allow mineral specific presets; new argument 'mineral'
  # 2 - add option to return the DRAC example data set

  if (nrow < 0 | nrow > 33)
    stop("'nrow' must be a number between 0 and 33.", call. = FALSE)

  ## PRESETS ----
  valid_presets <- c("quartz_coarse", "quartz_fine", "feldspar_coarse", "polymineral_fine",
                     "DRAC-example_quartz", "DRAC-example_feldspar", "DRAC-example_polymineral")

  if (!is.null(preset)) {
    if (length(preset) != 1 || !is.character(preset))
      stop("\n[template_DRAC()]: Argument 'preset' must be a 'character' of length 1.",
           call. = FALSE)

    if (!preset %in% valid_presets)
      stop("\n[template_DRAC()]: Invalid preset. Please use on of the following: ",
           paste(valid_presets, collapse = ", "), call. = FALSE)
  }

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
      structure(rep(NA_character_, nrow), required = TRUE, allowsX = FALSE, key = "TI:1",
                description = "Inputs can be alphabetic, numeric or selected symbols (/ - () [] _). Spaces are not permitted."), #

    `Sample ID` =
      structure(rep(NA_character_, nrow), required = TRUE, allowsX = FALSE, key = "TI:2",
                description = "Inputs can be alphabetic, numeric or selected symbols (/ - () [] _). Spaces are not permitted."), #

    `Mineral` =
      structure(factor(rep(NA_character_, nrow), c("Q", "F", "PM")), required = TRUE, allowsX = FALSE, key = "TI:3",
                description = "The mineral used for dating: quartz, feldspar or polymineral. Input must be 'Q', 'F' or 'PM'."), #

    `Conversion factors` =
      structure(factor(rep(NA_character_, nrow), c("AdamiecAitken1998", "Guerinetal2011", "Liritzisetal2013", "X")), required = FALSE, allowsX = TRUE, key = "TI:4",
                description = "The conversion factors required to calculate dose rates from radionuclide concentrations. Users have the option of datasets from Adamiec and Aitken (1998), Guerin et al. (2011) or Liritzis et al. (2013). Input must be 'AdamiecAitken1998', 'Guerinetal2011', 'Liritzisetal2013' or 'X' if conversion factors are not required."), #

    `External U (ppm)` =
      structure(rep(NA_real_, nrow), required = FALSE, allowsX = TRUE, key = "TI:5",
                description = "Radionuclide concentrations in parts per million for Uranium, Thorium and Rubidium and % for Potassium. Inputs must be 0 or positive and should not be left blank."), #

    `errExternal U (ppm)` =
      structure(rep(NA_real_, nrow), required = FALSE, allowsX = TRUE, key = "TI:6",
                description = "Radionuclide concentrations in parts per million for Uranium, Thorium and Rubidium and % for Potassium. Inputs must be 0 or positive and should not be left blank."), #

    `External Th (ppm)` =
      structure(rep(NA_real_, nrow), required = FALSE, allowsX = TRUE, key = "TI:7",
                description = "Radionuclide concentrations in parts per million for Uranium, Thorium and Rubidium and % for Potassium. Inputs must be 0 or positive and should not be left blank."), #

    `errExternal Th (ppm)` =
      structure(rep(NA_real_, nrow), required = FALSE, allowsX = TRUE, key = "TI:8",
                description = "Radionuclide concentrations in parts per million for Uranium, Thorium and Rubidium and % for Potassium. Inputs must be 0 or positive and should not be left blank."), #

    `External K (%)` =
      structure(rep(NA_real_, nrow), required = FALSE, allowsX = TRUE, key = "TI:9",
                description = "Radionuclide concentrations in parts per million for Uranium, Thorium and Rubidium and % for Potassium. Inputs must be 0 or positive and should not be left blank."), #

    `errExternal K (%)` =
      structure(rep(NA_real_, nrow), required = FALSE, allowsX = TRUE, key = "TI:10",
                description = "Radionuclide concentrations in parts per million for Uranium, Thorium and Rubidium and % for Potassium. Inputs must be 0 or positive and should not be left blank."), #

    `External Rb (ppm)` =
      structure(rep(NA_real_, nrow), required = FALSE, allowsX = TRUE, key = "TI:11",
                description = "Radionuclide concentrations in parts per million for Uranium, Thorium and Rubidium and % for Potassium. Inputs must be 0 or positive and should not be left blank."), #

    `errExternal Rb (ppm)` =
      structure(rep(NA_real_, nrow), required = FALSE, allowsX = TRUE, key = "TI:12",
                description = "Radionuclide concentrations in parts per million for Uranium, Thorium and Rubidium and % for Potassium. Inputs must be 0 or positive and should not be left blank."), #

    `Calculate external Rb from K conc?` =
      structure(factor(rep(NA_character_, nrow), c("Y", "N")), required = FALSE, allowsX = FALSE, key = "TI:13",
                description = "Option to calculate a Rubidium concentration from Potassium, using the 270:1 ratio suggested by Mejdahl (1987). Input should be yes 'Y' or no 'N'."), #

    `Internal U (ppm)` =
      structure(rep(NA_real_, nrow), required = FALSE, allowsX = TRUE, key = "TI:14",
                description = "Internal radionuclide concentrations in parts per million for Uranium, Thorium and Rubidium and % for Potassium. Inputs must be 0 or positive and should not be left blank."), #

    `errInternal U (ppm)` =
      structure(rep(NA_real_, nrow), required = FALSE, allowsX = TRUE, key = "TI:15",
                description = "Internal radionuclide concentrations in parts per million for Uranium, Thorium and Rubidium and % for Potassium. Inputs must be 0 or positive and should not be left blank."), #

    `Internal Th (ppm)` =
      structure(rep(NA_real_, nrow), required = FALSE, allowsX = TRUE, key = "TI:16",
                description = "Internal radionuclide concentrations in parts per million for Uranium, Thorium and Rubidium and % for Potassium. Inputs must be 0 or positive and should not be left blank."), #

    `errInternal Th (ppm)` =
      structure(rep(NA_real_, nrow), required = FALSE, allowsX = TRUE, key = "TI:17",
                description = "Internal radionuclide concentrations in parts per million for Uranium, Thorium and Rubidium and % for Potassium. Inputs must be 0 or positive and should not be left blank."), #

    `Internal K (%)` =
      structure(rep(NA_real_, nrow), required = FALSE, allowsX = TRUE, key = "TI:18",
                description = "Internal radionuclide concentrations in parts per million for Uranium, Thorium and Rubidium and % for Potassium. Inputs must be 0 or positive and should not be left blank."), #

    `errInternal K (%)` =
      structure(rep(NA_real_, nrow), required = FALSE, allowsX = TRUE, key = "TI:19",
                description = "Internal radionuclide concentrations in parts per million for Uranium, Thorium and Rubidium and % for Potassium. Inputs must be 0 or positive and should not be left blank."), #

    `Rb (ppm)` =
      structure(rep(NA_real_, nrow), required = FALSE, allowsX = TRUE, key = "TI:20",
                description = "Internal radionuclide concentrations in parts per million for Uranium, Thorium and Rubidium and % for Potassium. Inputs must be 0 or positive and should not be left blank."), #

    `errRb (ppm)` =
      structure(rep(NA_real_, nrow), required = FALSE, allowsX = TRUE, key = "TI:21",
                description = "Internal radionuclide concentrations in parts per million for Uranium, Thorium and Rubidium and % for Potassium. Inputs must be 0 or positive and should not be left blank."), #

    `Calculate internal Rb from K conc?` =
      structure(factor(rep(NA_character_, nrow), c("Y", "N", "X")), required = FALSE, allowsX = TRUE, key = "TI:22",
                description = "Option to calculate an internal Rubidium concentration from Potassium, using the 270:1 ratio suggested by Mejdahl (1987). Input should be yes 'Y' or no 'N'."), #

    `User external alphadoserate (Gy.ka-1)` =
      structure(rep(NA_real_, nrow), required = FALSE, allowsX = TRUE, key = "TI:23",
                description = "Users may input directly measured values for external alpha, beta and gamma dose rates (in Gy.ka-1). Any positive inputs in these fields will override dose rates calculated from radionuclide concentrations. Inputs should be 0 or positive and should not be left blank"), #

    `errUser external alphadoserate (Gy.ka-1)` =
      structure(rep(NA_real_, nrow), required = FALSE, allowsX = TRUE, key = "TI:24",
                description = "Users may input directly measured values for external alpha, beta and gamma dose rates (in Gy.ka-1). Any positive inputs in these fields will override dose rates calculated from radionuclide concentrations. Inputs should be 0 or positive and should not be left blank"), #

    `User external betadoserate (Gy.ka-1)` =
      structure(rep(NA_real_, nrow), required = FALSE, allowsX = TRUE, key = "TI:25",
                description = "Users may input directly measured values for external alpha, beta and gamma dose rates (in Gy.ka-1). Any positive inputs in these fields will override dose rates calculated from radionuclide concentrations. Inputs should be 0 or positive and should not be left blank"), #

    `errUser external betadoserate (Gy.ka-1)` =
      structure(rep(NA_real_, nrow), required = FALSE, allowsX = TRUE, key = "TI:26",
                description = "Users may input directly measured values for external alpha, beta and gamma dose rates (in Gy.ka-1). Any positive inputs in these fields will override dose rates calculated from radionuclide concentrations. Inputs should be 0 or positive and should not be left blank"), #

    `User external gamma doserate (Gy.ka-1)` =
      structure(rep(NA_real_, nrow), required = FALSE, allowsX = TRUE, key = "TI:27",
                description = "Users may input directly measured values for external alpha, beta and gamma dose rates (in Gy.ka-1). Any positive inputs in these fields will override dose rates calculated from radionuclide concentrations. Inputs should be 0 or positive and should not be left blank"), #

    `errUser external gammadoserate (Gy.ka-1)` =
      structure(rep(NA_real_, nrow), required = FALSE, allowsX = TRUE, key = "TI:28",
                description = "Users may input directly measured values for external alpha, beta and gamma dose rates (in Gy.ka-1). Any positive inputs in these fields will override dose rates calculated from radionuclide concentrations. Inputs should be 0 or positive and should not be left blank"), #

    `User internal doserate (Gy.ka-1)` =
      structure(rep(NA_real_, nrow), required = FALSE, allowsX = TRUE, key = "TI:29",
                description = "Users may input an internal dose rate (either alpha, beta or the sum of the two; in Gy.ka-1). DRAC will assume that this value has already been corrected for attenuation. Inputs in this field will override dose rates calculated from radionuclide concentrations. Inputs should be 0 or positive and not left blank."), #

    `errUser internal doserate (Gy.ka-1)` =
      structure(rep(NA_real_, nrow), required = FALSE, allowsX = TRUE, key = "TI:30",
                description = "Users may input an internal dose rate (either alpha, beta or the sum of the two; in Gy.ka-1). DRAC will assume that this value has already been corrected for attenuation. Inputs in this field will override dose rates calculated from radionuclide concentrations. Inputs should be 0 or positive and not left blank."), #

    `Scale gammadoserate at shallow depths?` =
      structure(factor(rep(NA_character_, nrow), c("Y", "N")), required = FALSE, allowsX = FALSE, key = "TI:31",
                description = "Users may choose to scale gamma dose rates for samples taken within 0.3 m of the ground surface. The scaling factors of Aitken (1985) are used. Input should be yes 'Y' or no 'N'."), #

    `Grain size min (microns)` =
      structure(rep(NA_real_, nrow), required = TRUE, allowsX = FALSE, key = "TI:32",
                description = "The grain size range analysed. DRAC can be used for the grain size ranges between 1 and 1000 microns. Inputs should range between 1 and 1000 and not be left blank."), #

    `Grain size max (microns)` =
      structure(rep(NA_real_, nrow), required = TRUE, allowsX = FALSE, key = "TI:33",
                description = "The grain size range analysed. DRAC can be used for the grain size ranges between 1 and 1000 microns. Inputs should range between 1 and 1000 and not be left blank."), #

    `alpha-Grain size attenuation` =
      structure(factor(rep(NA_character_, nrow), c("Bell1980", "Brennanetal1991")), required = TRUE, allowsX = FALSE, key = "TI:34",
                description = "The grain size attenuation factors for the alpha dose rate. Users have the option of datasets from Bell (1980) and Brennan et al. (1991). Input must be 'Bell1980' or 'Brennanetal1991'."), #

    `beta-Grain size attenuation ` =
      structure(factor(rep(NA_character_, nrow), c("Mejdahl1979", "Brennan2003", "Guerinetal2012-Q", "Guerinetal2012-F")), required = TRUE, allowsX = FALSE, key = "TI:35",
                description = "The grain size attenuation factors for the beta dose rate. Users have the option of datasets from Mejdahl (1979), Brennan (2003) and Guerin et al. (2012) for quartz or feldspar. Input must be 'Mejdahl1979', 'Brennan2003', 'Guerinetal2012-Q' or 'Guerinetal2012-F' ."), #

    `Etch depth min (microns)` =
      structure(rep(NA_real_, nrow), required = TRUE, allowsX = FALSE, key = "TI:36",
                description = "The user defined etch depth range (microns). Inputs should range between 0 and 30 and not be left blank."), #

    `Etch depth max (microns)` =
      structure(rep(NA_real_, nrow), required = TRUE, allowsX = FALSE, key = "TI:37",
                description = "The user defined etch depth range (microns). Inputs should range between 0 and 30 and not be left blank."), #

    `beta-Etch depth attenuation factor` =
      structure(factor(rep(NA_character_, nrow), c("Bell1979", "Brennan2003", "X")), required = FALSE, allowsX = TRUE, key = "TI:38",
                description = "The etch depth attenuation factors for the beta dose rate. Users have the option of datasets from Bell (1979) and Brennan (2003). Input must be 'Bell1979' or 'Brennan2003'. Note: only the dataset of Bell (1980) is provided for attenuation of the alpha dose rate by etching."), #

    `a-value` =
      structure(rep(NA_real_, nrow), required = FALSE, allowsX = TRUE, key = "TI:39",
                description = "Alpha track efficiency value and uncertainty defined by the user. Inputs should be 0 or positive and not left blank."), #

    `erra-value` =
      structure(rep(NA_real_, nrow), required = TRUE, allowsX = TRUE, key = "TI:40",
                description = "Alpha track efficiency value and uncertainty defined by the user. Inputs should be 0 or positive and not left blank."), #

    `Water content ((wet weight - dry weight)/dry weight) %` =
      structure(rep(NA_real_, nrow), required = TRUE, allowsX = FALSE, key = "TI:41",
                description = "Sediment water content (%) over the burial period. Inputs should be 0 or positive and not be left blank."), #

    `errWater content %` =
      structure(rep(NA_real_, nrow), required = FALSE, allowsX = FALSE, key = "TI:42",
                description = "Sediment water content (%) over the burial period. Inputs should be 0 or positive and not be left blank."), #

    `Depth (m)` =
      structure(rep(NA_character_, nrow), required = FALSE, allowsX = TRUE, key = "TI:43",
                description = "Depth and uncertainty from which sample was extracted beneath the ground surface. Inputs should be 0 or positive and not left blank."), #

    `errDepth (m)` =
      structure(rep(NA_character_, nrow), required = FALSE, allowsX = TRUE, key = "TI:44",
                description = "Depth and uncertainty from which sample was extracted beneath the ground surface. Inputs should be 0 or positive and not left blank."), #

    `Overburden density (g cm-3)` =
      structure(rep(NA_real_, nrow), required = TRUE, allowsX = FALSE, key = "TI:45",
                description = "Density of the overlying sediment matrix from which the sample was taken. Inputs should be 0 or positive and not be left blank. The scaling calculation will use the overburden density and uncertainty provided."), #

    `errOverburden density (g cm-3)` =
      structure(rep(NA_real_, nrow), required = TRUE, allowsX = FALSE, key = "TI:46",
                description = "Density of the overlying sediment matrix from which the sample was taken. Inputs should be 0 or positive and not be left blank. The scaling calculation will use the overburden density and uncertainty provided."), #

    `Latitude (decimal degrees)` =
      structure(rep(NA_character_, nrow), required = FALSE, allowsX = TRUE, key = "TI:47",
                description = "Latitude and longitude of sample location (in degree decimals). Positive values should be used for northern latitudes and eastern longitudes and negative values for southern latitudes and western longitudes. Inputs should range from -90 to 90 degrees for latitudes and -180 to 180 degrees for longitude."), #

    `Longitude (decimal degrees)` =
      structure(rep(NA_character_, nrow), required = FALSE, allowsX = TRUE, key = "TI:48",
                description = "Latitude and longitude of sample location (in degree decimals). Positive values should be used for northern latitudes and eastern longitudes and negative values for southern latitudes and western longitudes. Inputs should range from -90 to 90 degrees for latitudes and -180 to 180 degrees for longitude."), #

    `Altitude (m)` =
      structure(rep(NA_character_, nrow), required = FALSE, allowsX = TRUE, key = "TI:49",
                description = "Altitude of sample location in metres above sea level. Input should be less than 5000 and not left blank."), #

    `User cosmicdoserate (Gy.ka-1)` =
      structure(rep(NA_character_, nrow), required = FALSE, allowsX = TRUE, key = "TI:50",
                description = "Users may input a cosmic dose rate (in Gy.ka-1). Inputs in these fields will override the DRAC calculated cosmic dose rate. Inputs should be positive or 'X' if not required, and not left blank."), #

    `errUser cosmicdoserate (Gy.ka-1)` =
      structure(rep(NA_character_, nrow), required = FALSE, allowsX = TRUE, key = "TI:51",
                description = "Users may input a cosmic dose rate (in Gy.ka-1). Inputs in these fields will override the DRAC calculated cosmic dose rate. Inputs should be positive or 'X' if not required, and not left blank."), #

    `De (Gy)` =
      structure(rep(NA_character_, nrow), required = FALSE, allowsX = TRUE, key = "TI:52",
                description = "Sample De and uncertainty (in Gy). Inputs should be positive or 'X' if not required, and not left blank."), #

    `errDe (Gy)` =
      structure(rep(NA_character_, nrow), required = FALSE, allowsX = TRUE, key = "TI:53",
                description = "Sample De and uncertainty (in Gy). Inputs should be positive or 'X' if not required, and not left blank.") #
  )

  ## RETURN VALUE ----
  # add an additional DRAC class so we can define our own S3 method for as.data.frame
  class(template) <- c("DRAC.list", "list")
  # set preset
  if (!is.null(preset))
    template <- .preset_DRAC(template, preset)

  invisible(template)
}

.preset_DRAC <- function(x, preset) {

  preset_list <- list(
    ## DRAC COLUMNS (TI:xx) ---       TI:1               2             3         4              5    6      7     8     9     10  11 12  13   14   15   16   17    18   19   20   21   22   23   24   25    26   27   28   29   30   31    32   33                 34                 35  36  37         38      39      40  41 42    43    44   45   46  47   48   49   50   51      52    53
    "quartz_coarse" = list("RLum_preset", "quartz_coarse", "Q", "Guerinetal2011", "X", "X", "X",
                           "X", "X", "X", "X", "X", "N", "X", "X", "X", "X", "X", "X", "X", "X",
                           "X", "X", "X", "X", "X", "X", "X", "X", "X", "Y", 100, 200,
                           "Brennanetal1991", "Guerinetal2012-Q", 20, 5, "Bell1979",
                           0.035, 0.01, 0, 0, 0, 0, 1.8, 0.1, "X", "X", 0, "X", "X", "X", "X"),
    "quartz_fine" = list("RLum_preset", "quartz_fine", "Q", "Guerinetal2011", "X", "X", "X", "X",
                         "X", "X", "X", "X", "N", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X",
                         "X", "X", "X", "X", "X", "X", "X", "Y", 4, 11, "Brennanetal1991",
                         "Guerinetal2012-Q", 0, 0, "Bell1979", 0.035, 0.01, 0, 0, 0, 0, 1.8, 0.1,
                         "X", "X", 0, "X", "X", "X", "X"),
    "feldspar_coarse" = list("RLum_preset", "feldspar_coarse", "F", "Guerinetal2011", "X", "X", "X",
                             "X", "X", "X", "X", "X", "Y", "X", "X", "X", "X", 12.5, 0.5, "X", "X",
                             "X", "X", "X", "X", "X", "X", "X", "X", "X", "Y", 100, 200,
                             "Brennanetal1991", "Guerinetal2012-F", 0, 0, "Bell1979", 0.08, 0.01,
                             0, 0, 0, 0, 1.8, 0.1, "X", "X", 0, "X", "X", "X", "X"),
    "polymineral_fine" = list("RLum_preset", "polymineral_fine", "PM", "Guerinetal2011", "X", "X",
                              "X", "X", "X", "X", "X", "X", "Y", "X", "X", "X", "X", 12.5, 0.5,
                              "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "Y", 4, 11,
                              "Brennanetal1991", "Guerinetal2012-F", 0, 0, "Bell1979", 0.08, 0.01,
                              0, 0, 0, 0, 1.8, 0.1, "X", "X", 0, "X", "X", "X", "X"),
    "DRAC-example_quartz" = list("DRAC-example",	"Quartz",       "Q", "Guerinetal2011",	 3.4,	0.51,	14.47, 1.69,	1.2, 0.14, 0,	0, "N", "X", "X", "X", "X",  "X",	"X", "X", "X", "X", "X", "X", "X",  "X", "X", "X", "X", "X", "N",  90, 125, "Brennanetal1991", "Guerinetal2012-Q", 8,	10, "Bell1979",	    0,      0,  5, 2, 2.22, 0.05, 1.8, 0.1, 30,  70, 150, "X", "X",     20,  0.2),
    "DRAC-example_feldspar" = list(   "DRAC-example", "Feldspar",     "F", "AdamiecAitken1998",  2,  0.2,     8,  0.4, 1.75, 0.05, 0, 0, "Y", "X", "X", "X", "X", 12.5, 0.5, "X", "X", "N", "X", "X", "X",  "X", "X", "X", "X", "X", "Y",	180, 212,        "Bell1980",      "Mejdahl1979", 0,  0, "Bell1979",  0.15,   0.05, 10, 3, 0.15, 0.02, 1.8, 0.1, 60, 100, 200, "X", "X",     15,  1.5),
    "DRAC-example_polymineral" = list("DRAC-example", "Polymineral", "PM", "AdamiecAitken1998",  4,  0.4,    12, 0.12, 0.83, 0.08, 0, 0, "Y", "X", "X", "X", "X", 12.5, 0.5, "X", "X", "N", "X", "X", 2.5, 0.15, "X", "X", "X", "X", "Y",   4,	11,        "Bell1980",	    "Mejdahl1979", 0,  0, "Bell1979", 0.086, 0.0038, 10, 5,  0.2, 0.02, 1.8, 0.1, 46, 118, 200, 0.2, 0.1, 204.47, 2.69)
  )

  n <- length(x[[1]])
  for (i in 1:length(x))
    x[[i]] <- rep(preset_list[[preset]][[i]], n)
  return(x)
}
