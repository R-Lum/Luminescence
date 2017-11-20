context("internals")

test_that("Test internals", {
  testthat::skip_on_cran()

  ##.warningCatcher
  expect_warning(Luminescence:::.warningCatcher(for(i in 1:5){warning("test")}))

  ##.smoothing
  expect_silent(Luminescence:::.smoothing(runif(100), k = 5, method = "median"))
  expect_error(Luminescence:::.smoothing(runif(100), method = "test"))


  ##fancy_scientific
  plot(seq(1e10, 1e20, length.out = 10),1:10, xaxt = "n")
  expect_silent(axis(1, at = axTicks(1),labels = Luminescence:::fancy_scientific(axTicks(1))))

  ##.create_StatisticalSummaryText()
  expect_silent(Luminescence:::.create_StatisticalSummaryText())
  expect_is(
    Luminescence:::.create_StatisticalSummaryText(
      calc_Statistics(data.frame(1:10,1:10)), keywords = "mean"), class = "character")

  ##.unlist_RLum()
  expect_length(Luminescence:::.unlist_RLum(list(a = list(b = list(c = list(d = 1, e = 2))))), 2)

})
