## run spelling during R CMD check but not during covr::pacakge_coverage()
if (requireNamespace('spelling', quietly = TRUE) &&
    file.exists('../00_pkg_src'))
  spelling::spell_check_test(
    vignettes = TRUE,
    error = FALSE,
    skip_on_cran = TRUE)
