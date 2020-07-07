# Tracking of developement branch to integrate new function by M.A. and S.R. 

## `BaseDateSet.Conversion.rda`
* Replace existing `BaseDateSet.Conversion.rda` by new version
* Update dataset documentation 
* Correct wrong publication year `Cresswelletal2019` >> `Cresswelletal2018`
* Compress dataset with `xz` to reduce file size
* Update `scale_GammaDose()`

## `BaseDateSet.GrainSizeAttenuation.rda`
* Add dataset to package
* Rename from `GSA` to `BaseDataSet.GrainSizeAttenuation`
* Add documentation
* Add news

## `convert_Concentration2DoseRate()`
* Add function to package
* Add integrity tests + RLum.Results return
* Slight code polish
* Add alternative template output if input is missing
* Add docu from Svenja and Martin >> reformat
* Add further mandatory documentation
* Add running example
* TODO: add unit tests 

# NEWS (to be added)

* Add new function `convert_Concentration2DoseRate()` by Svenja Riedesel 
and Martin Autzen. This function converts radionuclide concentrations (K in %, Th and U in ppm) 
into dose rates (Gy/ka). Beta dose rates are also attenuated for the grain size. 
Beta and gamma dose rates are corrected for the water content. 

* `BaseDateSet.Conversion`, `Cresswelletal2019` was changed to `Cresswelletal2018`. 
The corresponding function `scale_GammaDose()` was updated.

* `BaseDateSet.GrainSizeAttenuation.rda`: Grain size correction data for beta-dose 
rates published by Guerin et al. (2012). Dataset contributed by Svenja Riedesel
and Martin Autzen.

* The package new depends on R (>= 3.5.0)
