context("app_RLum")

test_that("Simply check app frame", {
  skip_on_cran()

  if("RLumShiny" %in% installed.packages()){
    expect_message(app_RLum(app = "abc"), regexp = "Invalid app name")

  }else{
    expect_error(app_RLum(app = "abc"), regexp = "Shiny applications require")

  }

})
