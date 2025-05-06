#' Calculate the gamma dose deposited within a sample taking layer-to-layer
#' variations in radioactivity into account (according to Aitken, 1985)
#'
#' This function calculates the gamma dose deposited in a luminescence sample
#' taking into account layer-to-layer variations in sediment radioactivity.
#' The function scales user inputs of uranium, thorium and potassium based on
#' input parameters for sediment density, water content and given layer
#' thicknesses and distances to the sample.
#'
#' **User Input**
#'
#' To calculate the gamma dose which is deposited in a sample, the user needs
#' to provide information on those samples influencing the luminescence sample.
#' As a rule of thumb, all sediment layers within at least 30 cm radius from
#' the luminescence sample taken should be taken into account when calculating
#' the gamma dose rate. However, the actual range of gamma radiation might be
#' different, depending on the emitting radioelement, the water content and the
#' sediment density of each layer (Aitken, 1985). Therefore the user is
#' advised to provide as much detail as possible and physically sensible.
#'
#' The function requires a [data.frame] that is to be structured
#' in columns and rows, with samples listed in rows. The first column contains
#' information on the layer/sample ID, the second on the thickness (in cm) of
#' each layer, whilst column 3 should contain `NA` for all layers that are not
#' sampled for OSL/TL. For the layer the OSL/TL sample was taken from a numerical
#' value must be provided, which is the distance (in cm) measured from **bottom**
#' of the layer of interest. If the whole layer was sampled insert `0`. If the
#' sample was taken from *within* the layer, insert a numerical value `>0`,
#' which describes the distance from the middle of the sample to the bottom of
#' the layer in cm. Columns 4 to 9 should contain radionuclide concentrations
#' and their standard errors for
#' potassium (in %), thorium (in ppm) and uranium (in ppm). Columns 10 and 11
#' give information on the water content and its uncertainty (standard error)
#' in %. The layer density (in g/cm3) should be given in column 12. No cell
#' should be left blank. Please ensure to keep the column titles as given in
#' the example dataset (`data('ExampleData.ScaleGammaDose')`, see examples).
#'
#' The user can decide which dose rate
#' conversion factors should be used to calculate the gamma dose rates.
#' The options are:
#' - `"Cresswelletal2018"` (Cresswell et al., 2018)
#' - `"Liritzisetal2013"` (Liritzis et al., 2013)
#' - `"Guerinetal2011"` (Guerin et al., 2011)
#' - `"AdamiecAitken1998"` (Adamiec and Aitken, 1998)
#'
#'
#' **Water content**
#'
#' The water content provided by the user should be calculated according to:
#'
#' \deqn{ ( Wet weight [g] - Dry weight [g] ) / Dry weight [g] * 100 }
#'
#'
#' **Calculations**
#'
#' After converting the radionuclide concentrations into dose rates, the
#' function will scale the dose rates based on the thickness of the layers,
#' the distances to the sample, the water content and the density of the sediment.
#' The calculations are based on Aitken (1985, Appendix H). As an example
#' (equivalent to Aitken, 1985), assuming three layers of sediment, where **L** is
#' inert and positioned in between the infinite thick and equally active
#' layers **A** and **B**, the dose in **L** and **B** due to **A** is given by
#'
#' \deqn{ {1-f(x)}D_A }
#'
#' Where `x` is the distance into the inert medium, so `f(x)` is the weighted
#' average fractional dose at `x` and `D_A` denotes that the dose is delivered by **A**.
#' `f(x)` is derived from table H1 (Aitken, 1985), when setting `z = x`.
#' Consequently, the dose in **A** and **L** due to **B** is given by
#'
#' \deqn{ {1 - f(t-x)}D_B }
#'
#' Here `t` is the thickness of **L** and the other parameters are denoted as above,
#' just for the dose being delivered by B. `f(t-x)` is derived from table H1
#' (Aitken, 1985), when setting `z` equal to `t-x`. Following this, the dose in **L**
#' delivered by **A** and **B** is given by
#'
#' \deqn{ {2 - f(x) - f(t-x)}D_{AB} }
#'
#' Since **A** and **B** are equally active `D_{AB} = D_A = D_B`.
#'
#' The function uses the value of the fractional dose rate at the layer
#' boundary to start the calculation for the next layer. This way, the function
#' is able to scale the gamma dose rate accurately for distant layers when the
#' density and water content is not constant for the entire section.
#'
#' @param data [data.frame] (**required**):
#' A table containing all relevant information for each individual layer. The
#' table must have the following named columns:
#'
#' - `id` ([character]): an arbitrary id or name of each layer
#' - `thickness` ([numeric]): vertical extent of each layer in cm
#' - `sample_offset` ([logical]): distance of the sample in cm,
#'  **measured from the BOTTOM OF THE TARGET LAYER**. Except for the target layer
#'  all values must be `NA`.
#' - `K` ([numeric]): K nuclide content in %
#' - `K_se` ([numeric]): error on the K content
#' - `Th` ([numeric]): Th nuclide content in ppm
#' - `Th_se` ([numeric]): error on the Th content
#' - `U` ([numeric]): U nuclide content in ppm
#' - `U_se` ([numeric]): error on the U content
#' - `water_content` ([numeric]): water content of each layer in %
#' - `water_content_se` ([numeric]):  error on the water content
#' - `density` ([numeric]): bulk density of each layer in g/cm^-3
#'
#' @param conversion_factors [character] (*optional*):
#' The conversion factors used to calculate the dose rate from sediment
#' nuclide contents. Valid options are:
#'
#' - `"Cresswelletal2018"` (default)
#' - `"Liritzisetal2013"`
#' - `"Guerinetal2011"`
#' - `"AdamiecAitken1998"`
#'
#' @param fractional_gamma_dose [character] (*optional*):
#' Factors to scale gamma dose rate values. Valid options are:
#'
#' - `"Aitken1985"` (default): Table H1 in the appendix
#'
#' @param verbose [logical] (*with default*):
#' enable/disable output to the terminal.
#'
#' @param plot [logical] (*with default*):
#' enable/disable the plot output.
#'
#' @param plot_singlePanels [logical] (*with default*):
#' enable/disable single plot mode, i.e. one plot window per plot.
#'
#' @param ... Further parameters passed to [barplot].
#'
#' @return
#'
#' After performing the calculations the user is provided with different outputs.
#' 1. The total gamma dose rate received by the sample (+/- uncertainties) as a
#' print in the console.
#' 2. A plot showing the sediment sequence, the user input sample information
#' and the contribution to total gamma dose rate.
#' 3. RLum Results. If the user wishes to save these results, writing a script
#' to run the function and to save the results would look like this:
#'
#' ```
#' mydata <- read.table("c:/path/to/input/file.txt")
#' results <- scale_GammaDose(mydata)
#' table <- get_RLum(results)
#' write.csv(table, "c:/path/to/results.csv")
#' ```
#'
#' -----------------------------------\cr
#' `[ NUMERICAL OUTPUT ]`\cr
#' -----------------------------------\cr
#'
#' **`RLum.Results`**-object
#'
#' **slot:** **`@data`**
#'
#' \tabular{lll}{
#'  **Element** \tab **Type** \tab **Description**\cr
#'  `$summary` \tab `data.frame` \tab summary of the model results \cr
#'  `$data` \tab `data.frame` \tab the original input data \cr
#'  `$dose_rates` \tab `list` \tab two `data.frames` for the scaled and infinite matrix dose rates \cr
#'  `$tables` \tab `list` \tab several `data.frames` containing intermediate results \cr
#'  `$args` \tab `character` \tab arguments of the call \cr
#'  `$call` \tab `call` \tab the original function call \cr
#' }
#'
#' **slot:** **`@info`**
#'
#' Currently unused.
#'
#' ------------------------\cr
#' `[ PLOT OUTPUT ]`\cr
#' ------------------------\cr
#'
#' Three plots are produced:
#'
#' - A visualisation of the provided sediment layer structure to quickly
#' assess whether the data was provided and interpreted correctly.
#' - A scatter plot of the nuclide contents per layer (K, Th, U) as well as the
#' water content. This may help to correlate the dose rate contribution of
#' specific layers to the layer of interest.
#' - A barplot visualising the contribution of each layer to the total dose rate
#' received by the sample in the target layer.
#'
#' @section Function version: 0.1.4
#'
#' @keywords datagen
#'
#' @note
#' **This function has BETA status. If possible, results should be**
#' **cross-checked.**
#'
#' @author Svenja Riedesel, Aberystwyth University (United Kingdom) \cr
#' Martin Autzen, DTU NUTECH Center for Nuclear Technologies (Denmark) \cr
#' Christoph Burow, University of Cologne (Germany) \cr
#' Based on an excel spreadsheet and accompanying macro written by Ian Bailiff.
#'
#' @seealso [ExampleData.ScaleGammaDose],
#' [BaseDataSet.ConversionFactors], [approx], [barplot]
#'
#' @references
#'
#' Aitken, M.J., 1985. Thermoluminescence Dating. Academic Press, London.
#'
#' Adamiec, G., Aitken, M.J., 1998. Dose-rate conversion factors: update.
#' Ancient TL 16, 37-46.
#'
#' Cresswell., A.J., Carter, J., Sanderson, D.C.W., 2018.
#' Dose rate conversion parameters: Assessment of nuclear data.
#' Radiation Measurements 120, 195-201.
#'
#' Guerin, G., Mercier, N., Adamiec, G., 2011. Dose-rate conversion
#' factors: update. Ancient TL, 29, 5-8.
#'
#' Liritzis, I., Stamoulis, K., Papachristodoulou, C., Ioannides, K., 2013.
#' A re-evaluation of radiation dose-rate conversion factors. Mediterranean
#' Archaeology and Archaeometry 13, 1-15.
#'
#'
#' @section Acknowledgements:
#'
#' We thank Dr Ian Bailiff for the provision of an excel spreadsheet, which has
#' been very helpful when writing this function.
#'
#' @examples
#'
#' # Load example data
#' data("ExampleData.ScaleGammaDose", envir = environment())
#' x <- ExampleData.ScaleGammaDose
#'
#' # Scale gamma dose rate
#' results <- scale_GammaDose(data = x,
#'                            conversion_factors = "Cresswelletal2018",
#'                            fractional_gamma_dose = "Aitken1985",
#'                            verbose = TRUE,
#'                            plot = TRUE)
#'
#' get_RLum(results)
#'
#' @md
#' @export
scale_GammaDose <- function(
  data,
  conversion_factors = c("Cresswelletal2018", "Guerinetal2011", "AdamiecAitken1998", "Liritzisetal2013")[1],
  fractional_gamma_dose = c("Aitken1985")[1],
  verbose = TRUE,
  plot = TRUE,
  plot_singlePanels = FALSE,
  ...
) {
  .set_function_name("scale_GammaDose")
  on.exit(.unset_function_name(), add = TRUE)

  ## HELPER FUNCTION ----
  # Wrapper for formatC to enforce precise digit printing
  f <- function(x, d = 3) formatC(x, digits = d, format = "f")

  ## ------------------------------------------------------------------------ ##
  ## LOAD TABLES
  ## ------------------------------------------------------------------------ ##

  # To satisfy CRAN check ('no visible global binding')
  BaseDataSet.ConversionFactors <- BaseDataSet.FractionalGammaDose <- NA

  load(system.file("data", "BaseDataSet.ConversionFactors.rda",
                   package = "Luminescence"))
  load(system.file("data", "BaseDataSet.FractionalGammaDose.rda",
                   package = "Luminescence"))


  ## ------------------------------------------------------------------------ ##
  ## DEFAULT SETTINGS
  ## ------------------------------------------------------------------------ ##
  settings <- list(
    main = "Contributions of each layer to the total \n gamma dose rate received by the sample",
    xlab = "Contribution to total gamma dose rate (%) received by the sample",
    cex = 1.0,
    col = "grey",
    info = list()
  )
  # overwrite and append default values
  settings <- modifyList(settings, list(...))


  ## ------------------------------------------------------------------------ ##
  ## CHECK INPUT
  ## ------------------------------------------------------------------------ ##

  ## Input data
  # basic class and length check
  .validate_class(data, "data.frame")
  if (ncol(data) != 12)
    .throw_error("'data' should have 12 columns (currently ", ncol(data), ")")

  # make sure that it has the correct column names
  colnames_expected <- c("id","thickness","sample_offset","K","K_se","Th","Th_se","U","U_se",
                         "water_content","water_content_se", "density")
  if (is.null(names(data)) || any(names(data) != colnames_expected)) {
    if (verbose)
      .throw_warning("Unexpected column names for 'data', new names were ",
                     "automatically assigned. See documentation to ensure ",
                     "that columns are in proper order")
    colnames(data) <- colnames_expected
  }
  # check if there is only one target layer
  if (sum(!is.na(data$sample_offset)) != 1)
    .throw_error("Only one layer must be contain a numeric value in column ",
                 "'sample_offset', all other rows must be NA")
  if (!is.numeric(data$sample_offset[which(!is.na(data$sample_offset))]))
    .throw_error("Non-numeric value in the row of the target layer")
  if (data$sample_offset[which(!is.na(data$sample_offset))] < 0)
    .throw_error("The numeric value in 'sample_offset' must be positive")
  if (data$sample_offset[which(!is.na(data$sample_offset))] > data$thickness[which(!is.na(data$sample_offset))])
    .throw_error("Sample offset larger than the target-layer's thickness")

  ## conversion factors: we do not use BaseDataSet.ConversionFactors directly
  ## as it is in alphabetical level, but we want to have 'Cresswelletal2018'
  ## in first position, as that is our default value
  valid_conversion_factors <- c("Cresswelletal2018", "Guerinetal2011",
                                "AdamiecAitken1998", "Liritzisetal2013")
  stopifnot(all(names(BaseDataSet.ConversionFactors) %in%
                valid_conversion_factors))
  conversion_factors <- .validate_args(conversion_factors,
                                       valid_conversion_factors)
  fractional_gamma_dose <- .validate_args(fractional_gamma_dose,
                                          names(BaseDataSet.FractionalGammaDose))
  .validate_logical_scalar(verbose)
  .validate_logical_scalar(plot)
  .validate_logical_scalar(plot_singlePanels)

  ## ------------------------------------------------------------------------ ##
  ## Select tables
  ## ------------------------------------------------------------------------ ##
  conv_fac <- BaseDataSet.ConversionFactors[[conversion_factors]]$gamma
  frac_dose <- BaseDataSet.FractionalGammaDose[[fractional_gamma_dose]]

  ## ------------------------------------------------------------------------ ##
  ## CALCULATION
  ## ------------------------------------------------------------------------ ##
  dose_rate <- data.frame(
    K = data$K * conv_fac$K[1],
    K_re = sqrt( (data$K_se / data$K)^2 + conv_fac$K[2]^2 ),
    Th = data$Th * conv_fac$Th[1],
    Th_re = sqrt( (data$Th_se / data$Th)^2 + conv_fac$Th[2]^2 ),
    U = data$U * conv_fac$U[1],
    U_re = sqrt( (data$U_se / data$U)^2 + conv_fac$U[2]^2 )
  )

  dose_rate$sum <- dose_rate$K + dose_rate$Th + dose_rate$U
  dose_rate$sum_re <- sqrt(dose_rate$K_re^2 + dose_rate$Th_re^2 + dose_rate$U_re^2)
  dose_rate$K_frac <- dose_rate$K / dose_rate$sum
  dose_rate$K_frac_re <- sqrt(dose_rate$K_re^2 + dose_rate$sum_re^2 )
  dose_rate$Th_frac <- dose_rate$Th / dose_rate$sum
  dose_rate$Th_frac_re <- sqrt(dose_rate$Th_re^2 + dose_rate$sum_re^2 )
  dose_rate$U_frac <- dose_rate$U / dose_rate$sum
  dose_rate$U_frac_re <- sqrt(dose_rate$U_re^2 + dose_rate$sum_re^2 )

  ## weighted fractional dose
  z_scale <- do.call(cbind, Map(function(d, wc) {
    (frac_dose$z * 2) / (d + ( (wc / 100) * d))
  }, data$density, data$water_content))

  layer_fracDoseRate <- do.call(cbind, Map(function(K, Th, U, K_re, Th_re, U_re) {
    data.frame(
      val = frac_dose$K * K + frac_dose$Th * Th + frac_dose$U * U,
      err = sqrt( K_re^2 + Th_re^2 + U_re^2 )
    )
  }, dose_rate$K_frac, dose_rate$Th_frac, dose_rate$U_frac,
  dose_rate$K_frac_re, dose_rate$Th_frac_re, dose_rate$U_frac_re))

  ## TODO: LEGACY CODE
  target <- which(!is.na(data$sample_offset))
  distance <- data.frame(upper = c(rev(cumsum(data$thickness[target:1])[-1]) - data$sample_offset[target],
                                   abs(data$sample_offset[target]),
                                   cumsum(data$thickness[(target+1):nrow(data)]) + data$sample_offset[target]))
  distance$lower <- abs(distance$upper - data$thickness)

  ## Calculate infitite dose rate and dose received by the sample
  ## MAP: iterate over LAYERS
  Inf_frac <- as.data.frame(do.call(rbind, Map(function(z, n) {

    interpol <- Map(function(x) {
      approx(z, x, n = 1000, method = "linear")
    }, frac_dose[, c("K", "Th", "U")])

    x1 = data$thickness[n]
    x2 = 0
    C1 = which.min(abs(interpol$K$x - x1))
    C2 = which.min(abs(interpol$K$x - x2))

    ## MAP: iterate over NUCLIDE
    do.call(cbind, Map(function(x) {

      y1 = interpol[[x]]$y[C1]
      y2 = interpol[[x]]$y[C2]

      ### ----
      if (n != target) {

        if (n < target) {
          k <-  n + 1
          seq <- k:target
        } else if (n > target) {
          k <- n - 1
          seq <- target:k
        }

        for (j in seq) {
          fit <- approx(z_scale[ ,j], frac_dose[ , x], n = 1000, method = "linear")
          x1_temp <- which.min(abs(fit$y - y1))
          x2_temp <- which.min(abs(fit$y - y2))

          if (j != target) {
            x1 <- fit$x[x1_temp] + data$thickness[j]
            x2 <- fit$x[x2_temp] + data$thickness[j]
          }
          if (j == target) {

            if (n < target) {
              x1 <- fit$x[x1_temp] + data$thickness[target] - data$sample_offset[target]
              x2 <- fit$x[x2_temp] + data$thickness[target] - data$sample_offset[target]
            }

            if (n > target) {
              x1 <- fit$x[x1_temp] + data$sample_offset[target]
              x2 <- fit$x[x2_temp] + data$sample_offset[target]
            }
          }

          C1_temp <- which.min(abs(fit$x - x1))
          C2_temp <- which.min(abs(fit$x - x2))

          y1 <- fit$y[C1_temp]
          y2 <- fit$y[C2_temp]
        }
        r <- y1 - y2
      }

      ### ----
      if (n == target) {
        x1 <- data$sample_offset[target]
        x2 <- abs(data$thickness[target] - data$sample_offset[target])

        C1_temp <- which.min(abs(interpol[[x]]$x - x1))
        C2_temp <- which.min(abs(interpol[[x]]$x - x2))

        r <- interpol[[x]]$y[C1_temp] + interpol[[x]]$y[C2_temp] - 1
      }

      return(r)
    }, c("K", "Th", "U")))

  }, as.data.frame(z_scale), 1:nrow(data))))


  ## Generate output object
  op <- setNames(vector(mode = "list", length = 17),
                 nm = c("K","K_se","Th","Th_se","U","U_se","sum","sum_se",
                        "K_inf","K_inf_se","Th_inf","Th_inf_se","U_inf","U_inf_se","sum_inf","sum_inf_se",
                        "contrib"))

  # fractional dose rate
  op$K <- Inf_frac$K * dose_rate$K / (1 + 1.14 * data$water_content / 100)
  op$K_se <- op$K * sqrt(dose_rate$K_re^2 + (data$water_content_se / data$water_content)^2)
  op$Th <- Inf_frac$Th * dose_rate$Th / (1 + 1.14 * data$water_content / 100)
  op$Th_se <- op$Th * sqrt(dose_rate$Th_re^2 + (data$water_content_se / data$water_content)^2)
  op$U <- Inf_frac$U * dose_rate$U / (1 + 1.14 * data$water_content / 100)
  op$U_se <- op$U * sqrt(dose_rate$U_re^2 + (data$water_content_se / data$water_content)^2)
  op$sum <- op$K + op$Th + op$U
  op$sum_se <- sqrt(op$K_se^2 + op$Th_se^2 + op$U_se^2)

  # infinite matrix dose rate
  op$K_inf <- op$K / Inf_frac$K
  op$K_inf_se <- op$K_inf * sqrt(dose_rate$K_re^2 + (data$water_content_se / data$water_content)^2)
  op$Th_inf <- op$Th / Inf_frac$Th
  op$Th_inf_se <- op$Th_inf * sqrt(dose_rate$Th_re^2 + (data$water_content_se / data$water_content)^2)
  op$U_inf <- op$U / Inf_frac$U
  op$U_inf_se <- op$U_inf * sqrt(dose_rate$U_re^2 + (data$water_content_se / data$water_content)^2)
  op$sum_inf <- op$K_inf + op$Th_inf + op$U_inf
  op$sum_inf_se <- sqrt(op$K_inf_se^2 + op$Th_inf_se^2 + op$U_inf_se^2)

  ## Calculate the total dose rates
  for(i in seq(1, length(op)-1, 2)) {
    op[[i]] <- c(op[[i]], sum(op[[i]]))
    op[[i+1]] <- c(op[[i+1]], sqrt(sum(op[[i+1]]^2)))
  }

  ## Calculate contributions for each layer
  op$contrib <- op$sum[1:nrow(data)] / op$sum[nrow(data)+1] * 100
  op$contrib <- c(op$contrib, sum(op$contrib))


  # Cast to data.frame
  op <- as.data.frame(do.call(cbind, op))

  ## ------------------------------------------------------------------------ ##
  ## CONSOLE OUTPUT
  ## ------------------------------------------------------------------------ ##
  if (verbose) {
    cat(paste0("\n [scale_GammaDose()]\n\n"))
    cat(" ----\n")
    cat(" Conversion factors:", conversion_factors, "\n")
    cat(" Gamma dose fractions:", fractional_gamma_dose, "\n")
    cat(" Target layer:", data$id[target], "\n\n")

    cat(" ---- Infinite matrix gamma dose rate per layer ----\n\n")
    print(data.frame(ID = data$id,
                     `K (Gy/ka)` = paste0(f(op$K_inf[-(nrow(data)+1)]), "\u00b1", f(op$K_inf_se[-(nrow(data)+1)])),
                     `Th (Gy/ka)` = paste0(f(op$Th_inf[-(nrow(data)+1)]), "\u00b1", f(op$Th_inf_se[-(nrow(data)+1)])),
                     `U (Gy/ka)` = paste0(f(op$U_inf[-(nrow(data)+1)]), "\u00b1", f(op$U_inf_se[-(nrow(data)+1)])),
                     `Total (Gy/ka)` = f(op$sum_inf[-(nrow(data)+1)]),
                     check.names = FALSE
    ))
    cat("\n")
    cat(sprintf(" ---- Scaled gamma dose rate for target layer: %s  ----\n\n", data$id[target]))
    print(data.frame(ID = c(data$id, "TOTAL"),
                     `K (Gy/ka)` = paste0(f(op$K), "\u00b1", f(op$K_se)),
                     `Th (Gy/ka)` = paste0(f(op$Th), "\u00b1", f(op$Th_se)),
                     `U (Gy/ka)` = paste0(f(op$U), "\u00b1", f(op$U_se)),
                     `Contribution (%)` = round(op$contrib, 1),
                     check.names = FALSE
    ))
    cat("\n ----\n")
    cat(" Infinite matrix gamma dose rate:\t",
        f(op$sum_inf[target]), "\u00b1",
        f(op$sum_inf_se[target]), "Gy/ka \n")
    cat(" Scaled gamma dose rate:\t\t",
        f(op$sum[length(op$sum)]), "\u00b1",
        f(op$sum_se[length(op$sum_se)]), "Gy/ka")
    cat("\n\n")
  }


  ## ------------------------------------------------------------------------ ##
  ## PLOT
  ## ------------------------------------------------------------------------ ##
  if (plot) {

    # save and recover plot parameters
    par.old <- par(no.readonly = TRUE)
    on.exit(par(par.old), add = TRUE)

    if (!plot_singlePanels)
      graphics::layout(matrix(
                    c(1,1, 2, 3, 4, 5,
                      1,1, 2, 3, 4, 5,
                      1,1, 6, 6, 6, 6,
                      1,1, 6, 6, 6, 6), ncol = 6, byrow = TRUE))

    ## Plot 1 - Layer setup
    ## --------------------------------------------------------------

    ## Global plot settings
    par(mar = c(2, 5, ifelse(plot_singlePanels, 4, 1), 4) + 0.1)

    plot(NA, NA,
         main = ifelse(plot_singlePanels, "Profile structure", ""),
         xlim = c(0, 1),
         ylim = rev(range(pretty(c(sum(data$thickness), 0)))),
         xaxt = "n",
         xlab = "",
         ylab = "Depth below surface of uppermost layer (cm)",
         bty = "n",
         xaxs = "i")

    # x-axis label
    title(xlab = "Horizontal extent (a.u.)", line = 0)

    # horizontal layer lines
    abline(h = c(0, cumsum(data$thickness), sum(data$thickness)), lty = 1, col = "grey50", xpd = FALSE)

    # layer names
    mtext(side = 2, at = c(0, cumsum(data$thickness) - data$thickness / 2, sum(data$thickness)),
          text = c("", data$id, ""), las = 1, line = -5, cex = 0.75, padj = 0.3,
          col = "#428bca")

    # right y-axis
    axis(side = 4, at = c(0, cumsum(data$thickness), sum(data$thickness)),
         labels = FALSE, tck = -0.01)

    # right y-axis labels
    mtext(side = 4, at = c(0, cumsum(data$thickness) - data$thickness / 2, sum(data$thickness)),
          text = c("", paste(data$thickness, "cm"), ""), las = 1,
          line = ifelse(plot_singlePanels, 0.5, -4),
          cex = 0.8,
          col = "#b22222")

    # fill gap between lowermost layer and max range of pretty xlim
    polygon(x = c(0, 1, 1, 0),
            y = c(sum(data$thickness), sum(data$thickness),
                  max(range(pretty(c(sum(data$thickness), 0)))), max(range(pretty(c(sum(data$thickness), 0))))),
            density = 10, angle = 45
    )

    # add sample
    points(x = 0.5, y = sum(data$thickness[1:target]) - data$sample_offset[target],
           pch = 13, col = "#b22222", cex = 3, lwd = 2)


    ## PLOT 2 - Nuclide contents
    ## --------------------------------------------------------------

    # global plot settings
    if (!plot_singlePanels) {
      par(
        mar = c(4, 2, 3, 0.5) + 0.1,
        cex = 0.6,
        # oma = c(1, 1, 1, 1) + 0.1,
        pch = 16)
    } else {
      par(par.old)
      par(
        mfrow = c(1, 4),
        mar = c(4, 5, 0, 0) + 0.1,
        oma = c(1, 1, 1, 1) + 0.1,
        pch = 16)
    }

    # calculate fancy x-axis limits by considering the error bars
    calc_xlim <- function(x, se) {
      range(pretty(c(x - se, x + se)))
    }

    # horizontal error bars
    plot_error <- function(x, se) {
      segments(x - se, length(x):1, x + se, length(x):1)
      epsilon <- 0.1
      segments(x - se, length(x):1 - epsilon, x - se, length(x):1 + epsilon)
      segments(x + se, length(x):1 - epsilon, x + se, length(x):1 + epsilon)
    }

    # plot labels
    cols <- c("K", "Th", "U", "water_content")
    xlabs <- c("K content (%)", "Th content (ppm)", "U content (ppm)", "Water content (%)")

    # main plot
    for (i in 1:length(cols)) {

      # main
      plot(NA, NA,
           xlim = calc_xlim(data[[cols[i]]], data[[paste0(cols[i], "_se")]]),
           ylim = c(1, nrow(data)),
           ylab = "",
           xlab = xlabs[i],
           yaxt = "n")

      # vertical orientation lines
      abline(v = axTicks(side = 1), col = "grey", lty = 3, xpd = FALSE)

      # data points
      points(data[[cols[i]]], nrow(data):1, cex = 1.5)

      # errors
      plot_error(data[[cols[i]]], data[[paste0(cols[i], "_se")]])

      # y-axis label for the first plot
      if (i == 1)
        axis(2, at = nrow(data):1, labels = data$id, las = 1)
    }


    ## PLOT 3 - Contribution
    ## --------------------------------------------------------------

    ## Global plot settings
    # recover standard plot settings first
    if (!plot_singlePanels) {
      par(mar = c(5, 5, 1, 6) + 0.1,
          cex = 0.7)
    } else {
      par(par.old)
      par(mar = c(5, 8, 4, 4) + 0.1,
          cex = settings$cex)
    }

    ## Set colors: target layer is blue, all other grey
    cols <- c("grey", "#428bca")
    pos <- rev(as.numeric(!is.na(data$sample_offset)) + 1)

    ## Contributions of each layer
    bp <- graphics::barplot(height = op$contrib[(nrow(op)-1):1],
                  horiz = TRUE,
                  main = ifelse(plot_singlePanels, settings$main, ""),
                  xlab = settings$xlab,
                  xlim = range(pretty(op$contrib[1:(nrow(op)-1)])),
                  col = cols[pos])

    # layer names
    mtext(side = 2, at = bp,
          line = ifelse(plot_singlePanels, 3, 3.5),
          las = 1,
          cex = ifelse(plot_singlePanels, 0.8, 0.7),
          text = rev(data$id))

    # contribution percentage
    mtext(side = 2, at = rev(bp),
          text = paste(signif(op$contrib[1:(nrow(op) - 1)], 2), "%"),
          col = "#b22222",
          las = 1, line = 0.5, cex = 0.7)

    # absolute dose rate values (right side)
    mtext(side = 4, at = rev(bp),
          text = paste(c("  ", rep("+", nrow(op) - 2)), f(op$sum[1:(nrow(op) - 1)]), "Gy/ka"),
          col = "black",
          las = 1, line = -0.5, cex = 0.7)

    # sum of absolute dose rate values (-> scaled total gamma dose rate)
    mtext(side = 4,
          at = min(bp) - diff(bp)[1] / 2,
          text = paste("=", f(op$sum[nrow(op)]), "Gy/ka"),
          col = "#b22222", las = 1,
          line = ifelse(plot_singlePanels, -1, -0.5),
          cex = ifelse(plot_singlePanels, 0.8, 0.7))

    # recover old plot parameters
    par(par.old)
  }

  ## ------------------------------------------------------------------------ ##
  ## RETURN VALUE
  ## ------------------------------------------------------------------------ ##

  ## Infinity matrix dose rate table
  infinite_matrix <- data.frame(
    ID = data$id,
    K = op$K_inf[-(nrow(data)+1)],
    K_err = op$K_inf_se[-(nrow(data)+1)],
    Th = op$Th_inf[-(nrow(data)+1)],
    Th_err = op$Th_inf_se[-(nrow(data)+1)],
    U = op$U_inf[-(nrow(data)+1)],
    U_err = op$U_inf_se[-(nrow(data)+1)],
    Total = op$sum_inf[-(nrow(data)+1)]
  )

  ## Scaled dose rate table
  scaled_dose_rate <- data.frame(
    ID = c(data$id, "TOTAL"),
    K = op$K,
    K_err = op$K_se,
    Th = op$Th,
    Th_err = op$Th_se,
    U = op$U,
    U_err = op$U_se,
    Contribution = op$contrib
  )

  ## Summary table with the most important results
  summary <- data.frame(
    id = data$id[target],
    dose_rate_K = op$K[nrow(op)],
    dose_rate_K_err = op$K_se[nrow(op)],
    dose_rate_Th = op$Th[nrow(op)],
    dose_rate_Th_err = op$Th_se[nrow(op)],
    dose_rate_U = op$U[nrow(op)],
    dose_rate_U_err = op$U_se[nrow(op)],
    dose_rate_total = op$sum[length(op$sum)],
    dose_rate_total_err = op$sum_se[length(op$sum_se)]
  )

  ## Create RLum.Results object (return object)
  results <- set_RLum(class = "RLum.Results",
                      originator = "scale_GammaDose",
                      data = list(summary = summary,
                                  data = data,
                                  dose_rates = list(
                                    infinite_matrix = infinite_matrix,
                                    scaled_dose_rate = scaled_dose_rate
                                  ),
                                  tables = list(
                                    conversion_factors = conv_fac,
                                    distances = distance,
                                    layer_fractional_dose_rate = layer_fracDoseRate,
                                    dose_rates = dose_rate,
                                    infnite_matrix_dose_fractions = Inf_frac,
                                    z_scale = z_scale
                                  ),
                                  args = as.list(sys.call()[-1]),
                                  call = sys.call()),
                      info = settings$info
  )

  return(results)
}
