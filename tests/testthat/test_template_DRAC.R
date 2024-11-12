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

  ## use the file_input option
  SW({
    tmp_file <- tempfile(fileext = ".csv")
    write.csv(x = as.data.frame(template_DRAC(nrow = 12, preset = 'quartz_coarse')), file = tmp_file, row.names = FALSE)
    t <- expect_s3_class(suppressWarnings(template_DRAC(file_input = tmp_file, notification = FALSE)), "DRAC.list")
    expect_length(t[[1]], 3)
  })

  ## expect failure
  expect_error(template_DRAC(nrow = -1),
               "'nrow' should be a positive integer scalar")
  expect_error(template_DRAC("preset"),
               "'nrow' should be a positive integer scalar")
  expect_warning(template_DRAC(nrow = 5001, notification = FALSE),
                 "[template_DRAC()] More than 5000 datasets might not be supported",
                 fixed = TRUE)
  expect_error(template_DRAC(preset = "does_not_exist"),
               "'preset' should be one of 'quartz_coarse', 'quartz_fine'")
  expect_error(template_DRAC(preset = c("does_not_exist", "neither_this_one")),
               "'preset' contains multiple values but not all of them match 'choices'")
  expect_error(template_DRAC(preset = 999),
               "'preset' should be one of 'quartz_coarse', 'quartz_fine'")
})

