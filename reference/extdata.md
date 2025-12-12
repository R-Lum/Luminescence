# Collection of External Data

Description and listing of data provided in the folder `data/extdata`

## Details

The **R** package `Luminescence` includes a number of raw data files,
which are mostly used in the example sections of appropriate functions.
They are also used internally for testing corresponding functions using
the `testthat` package (see files in `tests/testthat/`) to ensure their
operational reliability.

**Accessibility**

If the **R** package `Luminescence` is installed correctly the preferred
way to access and use these data from within **R** is as follows:

`system.file("extdata/<FILENAME>", package = "Luminescence")`

**Individual file descriptions**

*\>\>Daybreak_TestFile.DAT.txt\<\<*

**Type:** raw measurement data  
**Device:** Daybreak OSL/TL reader  
**Measurement date:** unknown  
**Location:** unknown  
**Provided by:** unknown  
**Related R function(s):**
[`read_Daybreak2R()`](https://r-lum.github.io/Luminescence/reference/read_Daybreak2R.md)  
**Reference:** unknown

*\>\>DorNie_0016.psl\<\<*

**Type:** raw measurement data  
**Device:** SUERC portable OSL reader  
**Measurement date:** 19/05/2016  
**Location:** Dormagen-Nievenheim, Germany  
**Provided by:** Christoph Burow (University of Cologne)  
**Related R function(s):**
[`read_PSL2R()`](https://r-lum.github.io/Luminescence/reference/read_PSL2R.md)  
**Reference:** unpublished  
**Additional information:** Sample measured at an archaeological site
near  
Dormagen-Nievenheim (Germany) during a practical course on Luminescence
dating in 2016.  

*\>\>QNL84_2_bleached.txt*, *QNL84_2_unbleached.txt\<\<*

**Type:** Test data for exponential fits  
**Reference:** Berger, G.W., Huntley, D.J., 1989. Test data for
exponential fits. Ancient TL 7, 43-46.
[doi:10.26034/la.atl.1989.150](https://doi.org/10.26034/la.atl.1989.150)  

*\>\>STRB87_1_bleached.txt*, *STRB87_1_unbleached.txt\<\<*

**Type:** Test data for exponential fits  
**Reference:** Berger, G.W., Huntley, D.J., 1989. Test data for
exponential fits. Ancient TL 7, 43-46.
[doi:10.26034/la.atl.1989.150](https://doi.org/10.26034/la.atl.1989.150)

*\>\>XSYG_file.xsyg*

**Type:** XSYG-file stump  
**Info:** XSYG-file with some basic curves to test functions  
**Reference:** no reference available
