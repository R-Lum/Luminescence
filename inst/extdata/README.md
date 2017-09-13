## Raw data sets
 
The **R** package `Luminescence` includes a number of raw data files, which are mostly used in the example sections of appropriate functions. They are also used internally for testing corresponding functions using the `testthat` package (see files in `tests\testthat\`) to ensure their operational reliability.

## Accessibility

If the **R** package `Luminescence` is installed correctly the preferred way to access and use these data from within **R** is as follows:

```r
system.file("extdata/<FILENAME>", package = "Luminescence")
```

## Individual file descriptions

#### *Daybreak_TestFile.DAT/.txt*

**Type:** raw measurement data </br>
**Device:** Daybreak OSL/TL reader </br>
**Measurement date:** unknown </br>
**Location:** unknown </br>
**Provided by:** unknown </br>
**Related R function(s):** `read_Daybreak2R()` </br>
**Reference:** unknown

#### *DorNie_0016.psl*

**Type:** raw measurement data </br>
**Device:** SUERC portable OSL reader </br>
**Measurement date:** 19/05/2016 </br>
**Location:** Dormagen-Nievenheim, Germany </br>
**Provided by:** Christoph Burow (University of Cologne) </br>
**Related R function(s):** `read_PSL2R()` </br>
**Reference:** unpublished

**Additional information:** Sample measured at an archaeological site near Dormagen-Nievenheim (Germany) during a practical course on Luminesence dating in 2016.


#### *QNL84_2_bleached.txt*, *QNL84_2_unbleached.txt*

**Type:** Test data for exponential fits </br>
**Reference:** Berger, G.W., Huntley, D.J., 1989. Test data for exponential fits. Ancient TL 7, 43-46.

#### *STRB87_1_bleached.txt*, *STRB87_1_unbleached.txt*

**Type:** Test data for exponential fits </br>
**Reference:** Berger, G.W., Huntley, D.J., 1989. Test data for exponential fits. Ancient TL 7, 43-46.


