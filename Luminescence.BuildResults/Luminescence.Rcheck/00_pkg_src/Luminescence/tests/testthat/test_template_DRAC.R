##Full check
test_that("Check template creation ", {
  testthat::skip_on_cran()

  ## test output class
  SW({
  expect_message(res <- template_DRAC(),
                 "IMPORTANT NOTE")
  })
  expect_s3_class(res, "DRAC.list")
  expect_s3_class(template_DRAC(notification = FALSE), "DRAC.list")
  expect_s3_class(template_DRAC(nrow = 10, notification = FALSE), "DRAC.list")

  ## test presets
  expect_identical(as.numeric(template_DRAC(notification = FALSE, preset = "quartz_coarse")$`a-value`), 0.035)
  expect_identical(as.numeric(template_DRAC(notification = FALSE, preset = "quartz_fine")$`a-value`), 0.035)
  expect_identical(as.numeric(template_DRAC(notification = FALSE, preset = "feldspar_coarse")$`a-value`), 0.08)
  expect_identical(as.numeric(template_DRAC(notification = FALSE, preset = "polymineral_fine")$`a-value`), 0.08)
  expect_identical(as.numeric(template_DRAC(notification = FALSE, preset = "DRAC-example_quartz")$`De (Gy)`), 20)
  expect_identical(as.numeric(template_DRAC(notification = FALSE, preset = "DRAC-example_feldspar")$`De (Gy)`), 15)
  expect_identical(as.numeric(template_DRAC(notification = FALSE, preset = "DRAC-example_polymineral")$`De (Gy)`), 204.47)

  expect_true(
    do.call(all.equal, as.list(template_DRAC(nrow = 2, notification = FALSE, preset = "DRAC-example_quartz")$`De (Gy)`))
  )

  ## misc tests
  expect_true(all(is.na(template_DRAC(notification = FALSE))))
  expect_true(!all(is.na(template_DRAC(preset = "DRAC-example_polymineral", notification = FALSE))))
  expect_equal(length(template_DRAC(notification = FALSE)), 53)
  expect_equal(length(template_DRAC(nrow = 10, notification = FALSE)[[1]]), 10)
  expect_s3_class(template_DRAC(nrow = -1, notification = FALSE), "DRAC.list")

  ## expect failure
  expect_error(template_DRAC("preset"),
               "'nrow' must be a positive integer scalar")
  expect_warning(template_DRAC(nrow = 5001, notification = FALSE),
                 regexp = "\\[template_DRAC\\(\\)\\] More than 5000 datasets might not be supported!")
  expect_error(template_DRAC(preset = "does_not_exist"),
               "Invalid preset")
  expect_error(template_DRAC(preset = c("does_not_exist", "neither_this_one")),
               "'preset' must be a 'character' of length 1")
  expect_error(template_DRAC(preset = 999),
               "'preset' must be a 'character' of length 1")
})
