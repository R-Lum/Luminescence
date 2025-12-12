# Create a DRAC input data template (v1.2)

This function returns a DRAC input template (v1.2) to be used in
conjunction with the
[use_DRAC](https://r-lum.github.io/Luminescence/reference/use_DRAC.md)
function

## Usage

``` r
template_DRAC(nrow = 1L, preset = NULL, file_input = NULL, notification = TRUE)
```

## Arguments

- nrow:

  [integer](https://rdrr.io/r/base/integer.html) (*with default*):
  specifies the number of rows of the template (i.e., the number of data
  sets you want to submit).

- preset:

  [character](https://rdrr.io/r/base/character.html) (*optional*): By
  default, all values of the template are set to `NA`, which means that
  the user needs to fill in **all** data first before submitting to DRAC
  using
  [`use_DRAC()`](https://r-lum.github.io/Luminescence/reference/use_DRAC.md).
  To reduce the number of values that need to be provided, `preset` can
  be used to create a template with at least a minimum of reasonable
  preset values.

  `preset` can be one of the following:

  - `quartz_coarse`

  - `quartz_fine`

  - `feldspar_coarse`

  - `polymineral_fine`

  - `DRAC-example_quartz`

  - `DRAC-example_feldspar`

  - `DRAC-example_polymineral`

  Note that the last three options can be used to produce a template
  with values directly taken from the official DRAC input `.csv` file.

- file_input:

  [character](https://rdrr.io/r/base/character.html) file connection to
  a DRAC `.csv` file, the file will be imported and translated to the
  template that can be used by
  [use_DRAC](https://r-lum.github.io/Luminescence/reference/use_DRAC.md).
  Please note that there is not check on validity of the `.csv` file.

- notification:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*): show
  or hide the notification

## Value

A list of class `DRAC.list`.

## Function version

0.1.1

## How to cite

Burow, C., Kreutzer, S., 2025. template_DRAC(): Create a DRAC input data
template (v1.2). Function version 0.1.1. In: Kreutzer, S., Burow, C.,
Dietze, M., Fuchs, M.C., Schmidt, C., Fischer, M., Friedrich, J.,
Mercier, N., Philippe, A., Riedesel, S., Autzen, M., Mittelstrass, D.,
Gray, H.J., Galharret, J., Colombo, M., Steinbuch, L., Boer, A.d., 2025.
Luminescence: Comprehensive Luminescence Dating Data Analysis. R package
version 1.1.2. https://r-lum.github.io/Luminescence/

## References

Durcan, J.A., King, G.E., Duller, G.A.T., 2015. DRAC: Dose Rate and Age
Calculator for trapped charge dating. Quaternary Geochronology 28,
54-61. doi:10.1016/j.quageo.2015.03.012

## See also

[as.data.frame](https://rdrr.io/r/base/as.data.frame.html),
[list](https://rdrr.io/r/base/list.html)

## Author

Christoph Burow, University of Cologne (Germany), Sebastian Kreutzer,
Institute of Geography, Heidelberg University (Germany) , RLum Developer
Team

## Examples

``` r
# create a new DRAC input input
input <- template_DRAC(preset = "DRAC-example_quartz")
#> 
#>  -------------------- IMPORTANT NOTE ------------------------
#>   This function returns a DRAC input template to be used in 
#>   conjunction with the use_DRAC() function.  
#>   The template was reproduced with great care, but we do not
#>   take any responsibility and we are not liable for any 
#>   mistakes or unforeseen misbehaviour.
#>   Note that this template is only compatible with DRAC
#>   version 1.1. Before using this template make sure that
#>   this is the correct version, otherwise expect unspecified
#>   errors.
#>   Please ensure you cite the use of DRAC in your work,
#>   published or otherwise. Please cite the website name and
#>   version (e.g. DRAC v1.1) and the accompanying journal
#>   article:
#>   Durcan, J.A., King, G.E., Duller, G.A.T., 2015.
#>   DRAC: Dose rate and age calculation for trapped charge
#>   dating. Quaternary Geochronology 28, 54-61. 
#>   Set 'notification = FALSE' to hide this message. 
#>  -------------------- IMPORTANT NOTE ------------------------
#> 

# show content of the input
print(input)
#> TI:1 => Project ID 
#>       VALUES = DRAC-example 
#>       ALLOWS 'X' =  FALSE 
#>       REQUIRED = TRUE 
#>       DESCRIPTION =  Inputs can be alphabetic, numeric or selected symbols (/ - () [] _). Spaces are 
#>          not permitted. 
#>           
#>  
#> TI:2 => Sample ID 
#>       VALUES = Quartz 
#>       ALLOWS 'X' =  FALSE 
#>       REQUIRED = TRUE 
#>       DESCRIPTION =  Inputs can be alphabetic, numeric or selected symbols (/ - () [] _). Spaces are 
#>          not permitted. 
#>           
#>  
#> TI:3 => Mineral 
#>       VALUES = Q 
#>       ALLOWS 'X' =  FALSE 
#>       REQUIRED = TRUE 
#>       DESCRIPTION =  The mineral used for dating: quartz, feldspar or polymineral. Input must be 'Q' 
#>          , 'F' or 'PM'. 
#>           
#>       OPTIONS =  Q, F, PM 
#> 
#> TI:4 => Conversion factors 
#>       VALUES = Guerinetal2011 
#>       ALLOWS 'X' =  TRUE 
#>       REQUIRED = FALSE 
#>       DESCRIPTION =  The conversion factors required to calculate dose rates from radionuclide conce 
#>          ntrations. Users have the option of datasets from Adamiec and Aitken (1998), Gue 
#>          rin et al. (2011), Liritzis et al. (2013) or Cresswell et al. (2018). Input must 
#>          be 'AdamiecAitken1998', 'Guerinetal2011', 'Liritzisetal2013', 'Cresswelletal201 
#>          8', or 'X' if conversion factors are not required. 
#>           
#>       OPTIONS =  AdamiecAitken1998, Guerinetal2011, Liritzisetal2013, Cresswelletal2018, X 
#> 
#> TI:5 => External U (ppm) 
#>       VALUES = 3.4 
#>       ALLOWS 'X' =  TRUE 
#>       REQUIRED = FALSE 
#>       DESCRIPTION =  Radionuclide concentrations in parts per million for Uranium, Thorium and Rubid 
#>          ium and % for Potassium. Inputs must be 0 or positive and should not be left bla 
#>          nk. 
#>           
#>  
#> TI:6 => errExternal U (ppm) 
#>       VALUES = 0.51 
#>       ALLOWS 'X' =  TRUE 
#>       REQUIRED = FALSE 
#>       DESCRIPTION =  Radionuclide concentrations in parts per million for Uranium, Thorium and Rubid 
#>          ium and % for Potassium. Inputs must be 0 or positive and should not be left bla 
#>          nk. 
#>           
#>  
#> TI:7 => External Th (ppm) 
#>       VALUES = 14.47 
#>       ALLOWS 'X' =  TRUE 
#>       REQUIRED = FALSE 
#>       DESCRIPTION =  Radionuclide concentrations in parts per million for Uranium, Thorium and Rubid 
#>          ium and % for Potassium. Inputs must be 0 or positive and should not be left bla 
#>          nk. 
#>           
#>  
#> TI:8 => errExternal Th (ppm) 
#>       VALUES = 1.69 
#>       ALLOWS 'X' =  TRUE 
#>       REQUIRED = FALSE 
#>       DESCRIPTION =  Radionuclide concentrations in parts per million for Uranium, Thorium and Rubid 
#>          ium and % for Potassium. Inputs must be 0 or positive and should not be left bla 
#>          nk. 
#>           
#>  
#> TI:9 => External K (%) 
#>       VALUES = 1.2 
#>       ALLOWS 'X' =  TRUE 
#>       REQUIRED = FALSE 
#>       DESCRIPTION =  Radionuclide concentrations in parts per million for Uranium, Thorium and Rubid 
#>          ium and % for Potassium. Inputs must be 0 or positive and should not be left bla 
#>          nk. 
#>           
#>  
#> TI:10 => errExternal K (%) 
#>       VALUES = 0.14 
#>       ALLOWS 'X' =  TRUE 
#>       REQUIRED = FALSE 
#>       DESCRIPTION =  Radionuclide concentrations in parts per million for Uranium, Thorium and Rubid 
#>          ium and % for Potassium. Inputs must be 0 or positive and should not be left bla 
#>          nk. 
#>           
#>  
#> TI:11 => External Rb (ppm) 
#>       VALUES = 0 
#>       ALLOWS 'X' =  TRUE 
#>       REQUIRED = FALSE 
#>       DESCRIPTION =  Radionuclide concentrations in parts per million for Uranium, Thorium and Rubid 
#>          ium and % for Potassium. Inputs must be 0 or positive and should not be left bla 
#>          nk. 
#>           
#>  
#> TI:12 => errExternal Rb (ppm) 
#>       VALUES = 0 
#>       ALLOWS 'X' =  TRUE 
#>       REQUIRED = FALSE 
#>       DESCRIPTION =  Radionuclide concentrations in parts per million for Uranium, Thorium and Rubid 
#>          ium and % for Potassium. Inputs must be 0 or positive and should not be left bla 
#>          nk. 
#>           
#>  
#> TI:13 => Calculate external Rb from K conc? 
#>       VALUES = N 
#>       ALLOWS 'X' =  FALSE 
#>       REQUIRED = FALSE 
#>       DESCRIPTION =  Option to calculate a Rubidium concentration from Potassium, using the 270:1 ra 
#>          tio suggested by Mejdahl (1987). Input should be yes 'Y' or no 'N'. 
#>           
#>       OPTIONS =  Y, N 
#> 
#> TI:14 => Internal U (ppm) 
#>       VALUES = X 
#>       ALLOWS 'X' =  TRUE 
#>       REQUIRED = FALSE 
#>       DESCRIPTION =  Internal radionuclide concentrations in parts per million for Uranium, Thorium  
#>          and Rubidium and % for Potassium. Inputs must be 0 or positive and should not be 
#>          left blank. 
#>           
#>  
#> TI:15 => errInternal U (ppm) 
#>       VALUES = X 
#>       ALLOWS 'X' =  TRUE 
#>       REQUIRED = FALSE 
#>       DESCRIPTION =  Internal radionuclide concentrations in parts per million for Uranium, Thorium  
#>          and Rubidium and % for Potassium. Inputs must be 0 or positive and should not be 
#>          left blank. 
#>           
#>  
#> TI:16 => Internal Th (ppm) 
#>       VALUES = X 
#>       ALLOWS 'X' =  TRUE 
#>       REQUIRED = FALSE 
#>       DESCRIPTION =  Internal radionuclide concentrations in parts per million for Uranium, Thorium  
#>          and Rubidium and % for Potassium. Inputs must be 0 or positive and should not be 
#>          left blank. 
#>           
#>  
#> TI:17 => errInternal Th (ppm) 
#>       VALUES = X 
#>       ALLOWS 'X' =  TRUE 
#>       REQUIRED = FALSE 
#>       DESCRIPTION =  Internal radionuclide concentrations in parts per million for Uranium, Thorium  
#>          and Rubidium and % for Potassium. Inputs must be 0 or positive and should not be 
#>          left blank. 
#>           
#>  
#> TI:18 => Internal K (%) 
#>       VALUES = X 
#>       ALLOWS 'X' =  TRUE 
#>       REQUIRED = FALSE 
#>       DESCRIPTION =  Internal radionuclide concentrations in parts per million for Uranium, Thorium  
#>          and Rubidium and % for Potassium. Inputs must be 0 or positive and should not be 
#>          left blank. 
#>           
#>  
#> TI:19 => errInternal K (%) 
#>       VALUES = X 
#>       ALLOWS 'X' =  TRUE 
#>       REQUIRED = FALSE 
#>       DESCRIPTION =  Internal radionuclide concentrations in parts per million for Uranium, Thorium  
#>          and Rubidium and % for Potassium. Inputs must be 0 or positive and should not be 
#>          left blank. 
#>           
#>  
#> TI:20 => Rb (ppm) 
#>       VALUES = X 
#>       ALLOWS 'X' =  TRUE 
#>       REQUIRED = FALSE 
#>       DESCRIPTION =  Internal radionuclide concentrations in parts per million for Uranium, Thorium  
#>          and Rubidium and % for Potassium. Inputs must be 0 or positive and should not be 
#>          left blank. 
#>           
#>  
#> TI:21 => errRb (ppm) 
#>       VALUES = X 
#>       ALLOWS 'X' =  TRUE 
#>       REQUIRED = FALSE 
#>       DESCRIPTION =  Internal radionuclide concentrations in parts per million for Uranium, Thorium  
#>          and Rubidium and % for Potassium. Inputs must be 0 or positive and should not be 
#>          left blank. 
#>           
#>  
#> TI:22 => Calculate internal Rb from K conc? 
#>       VALUES = N 
#>       ALLOWS 'X' =  FALSE 
#>       REQUIRED = FALSE 
#>       DESCRIPTION =  Option to calculate an internal Rubidium concentration from Potassium, using th 
#>          e 270:1 ratio suggested by Mejdahl (1987). Input should be yes 'Y' or no 'N'. 
#>           
#>       OPTIONS =  Y, N 
#> 
#> TI:23 => User external alphadoserate (Gy.ka-1) 
#>       VALUES = X 
#>       ALLOWS 'X' =  TRUE 
#>       REQUIRED = FALSE 
#>       DESCRIPTION =  Users may input directly measured values for external alpha, beta and gamma dos 
#>          e rates (in Gy.ka-1). Any positive inputs in these fields will override dose rat 
#>          es calculated from radionuclide concentrations. Inputs should be 0 or positive a 
#>          nd should not be left blank 
#>           
#>  
#> TI:24 => errUser external alphadoserate (Gy.ka-1) 
#>       VALUES = X 
#>       ALLOWS 'X' =  TRUE 
#>       REQUIRED = FALSE 
#>       DESCRIPTION =  Users may input directly measured values for external alpha, beta and gamma dos 
#>          e rates (in Gy.ka-1). Any positive inputs in these fields will override dose rat 
#>          es calculated from radionuclide concentrations. Inputs should be 0 or positive a 
#>          nd should not be left blank 
#>           
#>  
#> TI:25 => User external betadoserate (Gy.ka-1) 
#>       VALUES = X 
#>       ALLOWS 'X' =  TRUE 
#>       REQUIRED = FALSE 
#>       DESCRIPTION =  Users may input directly measured values for external alpha, beta and gamma dos 
#>          e rates (in Gy.ka-1). Any positive inputs in these fields will override dose rat 
#>          es calculated from radionuclide concentrations. Inputs should be 0 or positive a 
#>          nd should not be left blank 
#>           
#>  
#> TI:26 => errUser external betadoserate (Gy.ka-1) 
#>       VALUES = X 
#>       ALLOWS 'X' =  TRUE 
#>       REQUIRED = FALSE 
#>       DESCRIPTION =  Users may input directly measured values for external alpha, beta and gamma dos 
#>          e rates (in Gy.ka-1). Any positive inputs in these fields will override dose rat 
#>          es calculated from radionuclide concentrations. Inputs should be 0 or positive a 
#>          nd should not be left blank 
#>           
#>  
#> TI:27 => User external gamma doserate (Gy.ka-1) 
#>       VALUES = X 
#>       ALLOWS 'X' =  TRUE 
#>       REQUIRED = FALSE 
#>       DESCRIPTION =  Users may input directly measured values for external alpha, beta and gamma dos 
#>          e rates (in Gy.ka-1). Any positive inputs in these fields will override dose rat 
#>          es calculated from radionuclide concentrations. Inputs should be 0 or positive a 
#>          nd should not be left blank 
#>           
#>  
#> TI:28 => errUser external gammadoserate (Gy.ka-1) 
#>       VALUES = X 
#>       ALLOWS 'X' =  TRUE 
#>       REQUIRED = FALSE 
#>       DESCRIPTION =  Users may input directly measured values for external alpha, beta and gamma dos 
#>          e rates (in Gy.ka-1). Any positive inputs in these fields will override dose rat 
#>          es calculated from radionuclide concentrations. Inputs should be 0 or positive a 
#>          nd should not be left blank 
#>           
#>  
#> TI:29 => User internal doserate (Gy.ka-1) 
#>       VALUES = X 
#>       ALLOWS 'X' =  TRUE 
#>       REQUIRED = FALSE 
#>       DESCRIPTION =  Users may input an internal dose rate (either alpha, beta or the sum of the two 
#>          ; in Gy.ka-1). DRAC will assume that this value has already been corrected for a 
#>          ttenuation. Inputs in this field will override dose rates calculated from radion 
#>          uclide concentrations. Inputs should be 0 or positive and not left blank. 
#>           
#>  
#> TI:30 => errUser internal doserate (Gy.ka-1) 
#>       VALUES = X 
#>       ALLOWS 'X' =  TRUE 
#>       REQUIRED = FALSE 
#>       DESCRIPTION =  Users may input an internal dose rate (either alpha, beta or the sum of the two 
#>          ; in Gy.ka-1). DRAC will assume that this value has already been corrected for a 
#>          ttenuation. Inputs in this field will override dose rates calculated from radion 
#>          uclide concentrations. Inputs should be 0 or positive and not left blank. 
#>           
#>  
#> TI:31 => Scale gammadoserate at shallow depths? 
#>       VALUES = N 
#>       ALLOWS 'X' =  FALSE 
#>       REQUIRED = FALSE 
#>       DESCRIPTION =  Users may choose to scale gamma dose rates for samples taken within 0.3 m of th 
#>          e ground surface. The scaling factors of Aitken (1985) are used. Input should be 
#>          yes 'Y' or no 'N'. 
#>           
#>       OPTIONS =  Y, N 
#> 
#> TI:32 => Grain size min (microns) 
#>       VALUES = 90 
#>       ALLOWS 'X' =  FALSE 
#>       REQUIRED = TRUE 
#>       DESCRIPTION =  The grain size range analysed. DRAC can be used for the grain size ranges betwe 
#>          en 1 and 1000 microns. Inputs should range between 1 and 1000 and not be left bl 
#>          ank. 
#>           
#>  
#> TI:33 => Grain size max (microns) 
#>       VALUES = 125 
#>       ALLOWS 'X' =  FALSE 
#>       REQUIRED = TRUE 
#>       DESCRIPTION =  The grain size range analysed. DRAC can be used for the grain size ranges betwe 
#>          en 1 and 1000 microns. Inputs should range between 1 and 1000 and not be left bl 
#>          ank. 
#>           
#>  
#> TI:34 => alpha-Grain size attenuation 
#>       VALUES = Brennanetal1991 
#>       ALLOWS 'X' =  FALSE 
#>       REQUIRED = TRUE 
#>       DESCRIPTION =  The grain size attenuation factors for the alpha dose rate. Users have the opti 
#>          on of datasets from Bell (1980) and Brennan et al. (1991). Input must be 'Bell19 
#>          80' or 'Brennanetal1991'. 
#>           
#>       OPTIONS =  Bell1980, Brennanetal1991 
#> 
#> TI:35 => beta-Grain size attenuation  
#>       VALUES = Guerinetal2012-Q 
#>       ALLOWS 'X' =  FALSE 
#>       REQUIRED = TRUE 
#>       DESCRIPTION =  The grain size attenuation factors for the beta dose rate. Users have the optio 
#>          n of datasets from Mejdahl (1979), Brennan (2003) and Guerin et al. (2012) for q 
#>          uartz or feldspar. Input must be 'Mejdahl1979', 'Brennan2003', 'Guerinetal2012-Q 
#>          ' or 'Guerinetal2012-F' . 
#>           
#>       OPTIONS =  Mejdahl1979, Brennan2003, Guerinetal2012-Q, Guerinetal2012-F 
#> 
#> TI:36 => Etch depth min (microns) 
#>       VALUES = 8 
#>       ALLOWS 'X' =  FALSE 
#>       REQUIRED = TRUE 
#>       DESCRIPTION =  The user defined etch depth range (microns). Inputs should range between 0 and  
#>          30 and not be left blank. 
#>           
#>  
#> TI:37 => Etch depth max (microns) 
#>       VALUES = 10 
#>       ALLOWS 'X' =  FALSE 
#>       REQUIRED = TRUE 
#>       DESCRIPTION =  The user defined etch depth range (microns). Inputs should range between 0 and  
#>          30 and not be left blank. 
#>           
#>  
#> TI:38 => beta-Etch depth attenuation factor 
#>       VALUES = Bell1979 
#>       ALLOWS 'X' =  TRUE 
#>       REQUIRED = FALSE 
#>       DESCRIPTION =  The etch depth attenuation factors for the beta dose rate. Users have the optio 
#>          n of datasets from Bell (1979) and Brennan (2003). Input must be 'Bell1979' or ' 
#>          Brennan2003'. Note: only the dataset of Bell (1980) is provided for attenuation  
#>          of the alpha dose rate by etching. 
#>           
#>       OPTIONS =  Bell1979, Brennan2003, X 
#> 
#> TI:39 => a-value 
#>       VALUES = 0 
#>       ALLOWS 'X' =  TRUE 
#>       REQUIRED = FALSE 
#>       DESCRIPTION =  Alpha track efficiency value and uncertainty defined by the user. Inputs should 
#>          be 0 or positive and not left blank. 
#>           
#>  
#> TI:40 => erra-value 
#>       VALUES = 0 
#>       ALLOWS 'X' =  TRUE 
#>       REQUIRED = TRUE 
#>       DESCRIPTION =  Alpha track efficiency value and uncertainty defined by the user. Inputs should 
#>          be 0 or positive and not left blank. 
#>           
#>  
#> TI:41 => Water content ((wet weight - dry weight)/dry weight) % 
#>       VALUES = 5 
#>       ALLOWS 'X' =  FALSE 
#>       REQUIRED = TRUE 
#>       DESCRIPTION =  Sediment water content (%) over the burial period. Inputs should be 0 or positi 
#>          ve and not be left blank. 
#>           
#>  
#> TI:42 => errWater content % 
#>       VALUES = 2 
#>       ALLOWS 'X' =  FALSE 
#>       REQUIRED = FALSE 
#>       DESCRIPTION =  Sediment water content (%) over the burial period. Inputs should be 0 or positi 
#>          ve and not be left blank. 
#>           
#>  
#> TI:43 => Depth (m) 
#>       VALUES = 2.22 
#>       ALLOWS 'X' =  TRUE 
#>       REQUIRED = FALSE 
#>       DESCRIPTION =  Depth and uncertainty from which sample was extracted beneath the ground surfac 
#>          e. Inputs should be 0 or positive and not left blank. 
#>           
#>  
#> TI:44 => errDepth (m) 
#>       VALUES = 0.05 
#>       ALLOWS 'X' =  TRUE 
#>       REQUIRED = FALSE 
#>       DESCRIPTION =  Depth and uncertainty from which sample was extracted beneath the ground surfac 
#>          e. Inputs should be 0 or positive and not left blank. 
#>           
#>  
#> TI:45 => Overburden density (g cm-3) 
#>       VALUES = 1.8 
#>       ALLOWS 'X' =  FALSE 
#>       REQUIRED = TRUE 
#>       DESCRIPTION =  Density of the overlying sediment matrix from which the sample was taken. Input 
#>          s should be 0 or positive and not be left blank. The scaling calculation will us 
#>          e the overburden density and uncertainty provided. 
#>           
#>  
#> TI:46 => errOverburden density (g cm-3) 
#>       VALUES = 0.1 
#>       ALLOWS 'X' =  FALSE 
#>       REQUIRED = TRUE 
#>       DESCRIPTION =  Density of the overlying sediment matrix from which the sample was taken. Input 
#>          s should be 0 or positive and not be left blank. The scaling calculation will us 
#>          e the overburden density and uncertainty provided. 
#>           
#>  
#> TI:47 => Latitude (decimal degrees) 
#>       VALUES = 30 
#>       ALLOWS 'X' =  TRUE 
#>       REQUIRED = FALSE 
#>       DESCRIPTION =  Latitude and longitude of sample location (in degree decimals). Positive values 
#>          should be used for northern latitudes and eastern longitudes and negative value 
#>          s for southern latitudes and western longitudes. Inputs should range from -90 to 
#>          90 degrees for latitudes and -180 to 180 degrees for longitude. 
#>           
#>  
#> TI:48 => Longitude (decimal degrees) 
#>       VALUES = 70 
#>       ALLOWS 'X' =  TRUE 
#>       REQUIRED = FALSE 
#>       DESCRIPTION =  Latitude and longitude of sample location (in degree decimals). Positive values 
#>          should be used for northern latitudes and eastern longitudes and negative value 
#>          s for southern latitudes and western longitudes. Inputs should range from -90 to 
#>          90 degrees for latitudes and -180 to 180 degrees for longitude. 
#>           
#>  
#> TI:49 => Altitude (m) 
#>       VALUES = 150 
#>       ALLOWS 'X' =  TRUE 
#>       REQUIRED = FALSE 
#>       DESCRIPTION =  Altitude of sample location in metres above sea level. Input should be less tha 
#>          n 5000 and not left blank. 
#>           
#>  
#> TI:50 => User cosmicdoserate (Gy.ka-1) 
#>       VALUES = X 
#>       ALLOWS 'X' =  TRUE 
#>       REQUIRED = FALSE 
#>       DESCRIPTION =  Users may input a cosmic dose rate (in Gy.ka-1). Inputs in these fields will ov 
#>          erride the DRAC calculated cosmic dose rate. Inputs should be positive or 'X' if 
#>          not required, and not left blank. 
#>           
#>  
#> TI:51 => errUser cosmicdoserate (Gy.ka-1) 
#>       VALUES = X 
#>       ALLOWS 'X' =  TRUE 
#>       REQUIRED = FALSE 
#>       DESCRIPTION =  Users may input a cosmic dose rate (in Gy.ka-1). Inputs in these fields will ov 
#>          erride the DRAC calculated cosmic dose rate. Inputs should be positive or 'X' if 
#>          not required, and not left blank. 
#>           
#>  
#> TI:52 => De (Gy) 
#>       VALUES = 20 
#>       ALLOWS 'X' =  TRUE 
#>       REQUIRED = FALSE 
#>       DESCRIPTION =  Sample De and uncertainty (in Gy). Inputs should be positive or 'X' if not requ 
#>          ired, and not left blank. 
#>           
#>  
#> TI:53 => errDe (Gy) 
#>       VALUES = 0.2 
#>       ALLOWS 'X' =  TRUE 
#>       REQUIRED = FALSE 
#>       DESCRIPTION =  Sample De and uncertainty (in Gy). Inputs should be positive or 'X' if not requ 
#>          ired, and not left blank. 
#>           
#>  
print(input$`Project ID`)
#> [1] "DRAC-example"
#> attr(,"required")
#> [1] TRUE
#> attr(,"allowsX")
#> [1] FALSE
#> attr(,"default_class")
#> [1] "character"
#> attr(,"key")
#> [1] "TI:1"
#> attr(,"description")
#> [1] "Inputs can be alphabetic, numeric or selected symbols (/ - () [] _). Spaces are not permitted."
print(input[[4]])
#> [1] Guerinetal2011
#> attr(,"required")
#> [1] FALSE
#> attr(,"allowsX")
#> [1] TRUE
#> attr(,"default_class")
#> [1] factor
#> attr(,"key")
#> [1] TI:4
#> attr(,"description")
#> [1] The conversion factors required to calculate dose rates from radionuclide concentrations. Users have the option of datasets from Adamiec and Aitken (1998), Guerin et al. (2011), Liritzis et al. (2013) or Cresswell et al. (2018). Input must be 'AdamiecAitken1998', 'Guerinetal2011', 'Liritzisetal2013', 'Cresswelletal2018', or 'X' if conversion factors are not required.
#> 5 Levels: AdamiecAitken1998 Guerinetal2011 ... X


## Example: DRAC Quartz example
# note that you only have to assign new values where they
# are different to the default values
input$`Project ID` <- "DRAC-Example"
input$`Sample ID` <- "Quartz"
input$`Conversion factors` <- "AdamiecAitken1998"
input$`External U (ppm)` <- 3.4
input$`errExternal U (ppm)` <- 0.51
input$`External Th (ppm)` <- 14.47
input$`errExternal Th (ppm)` <- 1.69
input$`External K (%)` <- 1.2
input$`errExternal K (%)` <- 0.14
input$`Calculate external Rb from K conc?` <- "N"
input$`Calculate internal Rb from K conc?` <- "N"
input$`Scale gammadoserate at shallow depths?` <- "N"
input$`Grain size min (microns)` <- 90
#> [[[<-.DRAC.list]]()] Error: Grain size min (microns): found numeric, expected integer -> coercing to integer
input$`Grain size max (microns)` <- 125
#> [[[<-.DRAC.list]]()] Error: Grain size max (microns): found numeric, expected integer -> coercing to integer
input$`Water content ((wet weight - dry weight)/dry weight) %` <- 5
input$`errWater content %` <- 2
input$`Depth (m)` <- 2.2
input$`errDepth (m)` <- 0.22
input$`Overburden density (g cm-3)` <- 1.8
input$`errOverburden density (g cm-3)` <- 0.1
input$`Latitude (decimal degrees)` <- 30.0000
input$`Longitude (decimal degrees)` <- 70.0000
input$`Altitude (m)` <- 150
input$`De (Gy)` <- 20
input$`errDe (Gy)` <- 0.2

# use DRAC
if (FALSE) { # \dontrun{
output <- use_DRAC(input)
} # }
```
