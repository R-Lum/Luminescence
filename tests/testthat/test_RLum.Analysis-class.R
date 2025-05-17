## load data
data(ExampleData.RLum.Analysis, envir = environment())
obj <- IRSAR.RF.Data
data(ExampleData.XSYG, envir = environment())
sar <- OSL.SARMeasurement$Sequence.Object[1:9]

## construct test object
tmp <- set_RLum(
    "RLum.Analysis",
    protocol = "testthat",
    records = lapply(1:20, function(x)  {
      set_RLum(
          "RLum.Data.Curve",
          recordType = "test",
          data = matrix(1:10, ncol = 2),
          info = list(el = as.character(x))
      )
    }),
    info = list(el = "test")
)

## missing info slot to test very old objects (before commit 5226b88, Jan 2016)
old <- tmp
attr(old, "info") <- NULL

test_that("Check the example and the numerical values", {
  testthat::skip_on_cran()

  ## set_RLum()
  expect_s4_class(tmp, "RLum.Analysis")
  expect_s4_class(set_RLum("RLum.Analysis", records = tmp, .pid = "test"), "RLum.Analysis")

  ##overwrite object
  expect_s4_class(set_RLum("RLum.Analysis", records = tmp), "RLum.Analysis")

  ## as()
  expect_type(as(tmp, "list"), "list")
  expect_s4_class(as(list(), "RLum.Analysis"), "RLum.Analysis")

  ## output
  expect_output(print(as(list(), "RLum.Analysis")), regexp = "This is an empty object")
  expect_s4_class(
    set_RLum(
      "RLum.Analysis",
      protocol = "testthat",
      records = set_RLum(
        "RLum.Analysis",
        protocol = "nest",
        records = list(matrix(1:10, ncol = 2))
      ),
      info = list(el = "test")
    ),
    "RLum.Analysis"
  )

  ## show()
  expect_output(print(tmp))
  expect_output(print(old))

  ## names()
  expect_type(names(tmp), "character")
})

test_that("get_RLum", {
  testthat::skip_on_cran()

  ## input validation
  expect_error(get_RLum(obj, subset = 1),
               "[get_RLum()] 'subset' must contain a logical expression",
               fixed = TRUE)
  expect_error(get_RLum(obj, subset = (error == "OSL")),
               "[get_RLum()] Invalid subset expression, valid terms are:",
               fixed = TRUE)
  expect_error(get_RLum(tmp, record.id = "character"),
               "'record.id' should be of class 'integer', 'numeric' or 'logical'")
  expect_error(get_RLum(tmp, recordType = 1L),
               "'recordType' should be of class 'character'")
  expect_error(get_RLum(tmp, curveType = 1L),
               "'curveType' should be of class 'character'")
  expect_error(get_RLum(tmp, RLum.type = 1L),
               "'RLum.type' should be of class 'character'")
  expect_error(get_RLum(tmp, get.index = "a"),
               "'get.index' should be a single logical value")

  ## check functionality
  expect_length(get_RLum(obj, subset = (recordType == "RF")), 2)
  expect_length(get_RLum(obj, subset = "recordType == 'RF'"), 2)
  expect_length(get_RLum(tmp, subset = (el == "2")), 1)
  expect_s4_class(get_RLum(tmp, subset = (el == "2")), "RLum.Analysis")
  expect_type(get_RLum(tmp, info.object = "el"), "character")

  ## check subset for info elements
  t_subset <- IRSAR.RF.Data
  t_subset@records[[1]]@info <- list(TEST = "SUBSET")
  expect_length(get_RLum(t_subset, subset = c(TEST == "SUBSET")), 1)

  expect_type(get_RLum(obj, get.index = FALSE), "list")
  expect_type(get_RLum(obj, get.index = NULL), "list")
  expect_type(get_RLum(obj, get.index = TRUE), "integer")
  expect_s4_class(get_RLum(obj, get.index = FALSE, drop = FALSE),
                  "RLum.Analysis")
  expect_type(get_RLum(obj, get.index = TRUE, drop = FALSE),
              "integer")
  expect_type(get_RLum(tmp, record.id = c(3, 5), get.index = FALSE),
              "list")

  expect_s4_class(get_RLum(obj, record.id = 1),
                  "RLum.Data.Curve")
  expect_s4_class(get_RLum(obj, record.id = 1, drop = FALSE),
                  "RLum.Analysis")
  expect_type(get_RLum(obj, record.id = 1, get.index = TRUE),
              "integer")
  expect_message(expect_null(get_RLum(obj, record.id = 99)),
                 "[get_RLum()] Error: At least one 'record.id' is invalid",
                 fixed = TRUE)

  expect_warning(get_RLum(tmp, info.object = "missing"),
                 "[get_RLum()] Invalid 'info.object' name, valid names are:",
                 fixed = TRUE)
  expect_warning(expect_null(get_RLum(set_RLum("RLum.Analysis"),
                                      info = "test")),
                 "[get_RLum()] This 'RLum.Analysis' object has no info objects",
                 fixed = TRUE)
  SW({
  expect_message(expect_null(get_RLum(obj, subset = (recordType == "error"))),
                 "'subset' expression produced an empty selection, NULL returned")
  expect_message(expect_null(get_RLum(obj, subset = (recordType == NA))),
                 "'subset' expression produced an empty selection, NULL returned")
  })
})

test_that("sort_RLum", {
  testthat::skip_on_cran()

  ## input validation
  expect_error(sort_RLum(sar, slot = NA),
               "'slot' should be of class 'character' or NULL")
  expect_error(sort_RLum(sar, slot = c("curveType", "error")),
               "Invalid 'slot' name, valid names are:")
  expect_error(sort_RLum(sar, info_element = NA),
               "'info_element' should be of class 'character' or NULL")
  expect_error(sort_RLum(sar, info_element = c("position", "error")),
               "Invalid 'info_element' name, valid names are:")
  expect_error(sort_RLum(sar, slot = NULL, info_element = NULL),
               "At least one of 'slot' and 'info_element' should not be NULL")
  expect_error(sort_RLum(sar, slot = "recordType", decreasing = "error"),
               "'decreasing' should be of class 'logical'")
  expect_error(sort_RLum(sar, slot = "recordType", decreasing = NA),
               "'decreasing' should be of class 'logical'")

  ## check empty object
  expect_s4_class(sort_RLum(set_RLum("RLum.Analysis")), class = "RLum.Analysis")

  ## check one curve object
  expect_s4_class(
    sort_RLum(set_RLum("RLum.Analysis", records = list(set_RLum("RLum.Data.Curve"))),
              slot = "recordType"),
                  class = "RLum.Analysis")

  ## sort only using the first field until #605 is done
  expect_message(sort_RLum(sar, slot = c("curveType", "recordType")),
                 "Only the first 'slot' field will be used in sorting")

  ## check functionality
  expect_snapshot_RLum(sort_RLum(sar, slot = "recordType"))
  expect_snapshot_RLum(sort_RLum(sar, info_element = "curveDescripter"))

  ## present a list of those objects
  expect_type(sort_RLum(list(sar, sar), info_element = "X_MIN"), "list")
  expect_type(sort_RLum(list(iris, mtcars), info_element = "X_MIN"), "list")
  expect_snapshot(sort_RLum(list(sar, sar), info_element = "X_MIN"))

  ## sort after three columns
  expect_s4_class(sort_RLum(sar, info_element = c("XY_LENGTH", "NCOL", "X_MIN")), "RLum.Analysis")

  ## now add spectra
  sar_a <- sar
  sar_a@records <- c(sar_a@records, set_RLum("RLum.Data.Spectrum"))
  expect_s4_class(sort_RLum(sar_a, info_element = "X_MIN"), "RLum.Analysis")

  ## try with image
  sar_a <- sar
  sar_a@records <- c(sar_a@records, set_RLum("RLum.Data.Image"))
  expect_s4_class(sort_RLum(sar_a, info_element = "X_MIN"), "RLum.Analysis")

  ## use slot sorting with more than one element in the slot
  ## it should not break
  sar_a <- sar
  sar_a@records[[1]]@.pid <- c("a", "b")
  sort_RLum(sar_a, slot = ".pid")

  empty <- as(list(), "RLum.Analysis")
  expect_equal(sort_RLum(empty, slot = "curveType"),
               empty)

  ## check a special case where individual info elements have a length > 1
  sar@records[[1]]@info <- c(sar@records[[1]]@info, test = list(x = 1:10))
  expect_s4_class(sort_RLum(sar, info_element = "startDate"), class = "RLum.Analysis")
})

test_that("structure_RLum", {
  testthat::skip_on_cran()

  ## input validation
  expect_error(structure_RLum(
      set_RLum("RLum.Analysis",
               records = list(set_RLum("RLum.Data.Image")))),
      "Only 'RLum.Data.Curve' objects are allowed")

  ## full functionality

  ## object with empty info
  expect_s3_class(res <- structure_RLum(obj),
                  "data.frame")
  expect_equal(nrow(res), length(obj@records))
  expect_equal(ncol(res), 13)
  expect_equal(res$n.channels, c(5, 524))
  expect_equal(res$recordType, c("RF", "RF"))
  expect_equal(unlist(res$info), c(NA, NA))

  expect_s3_class(res2 <- structure_RLum(obj, fullExtent = TRUE),
                  "data.frame")
  expect_equal(names(res2), names(res))
  expect_equal(res2$info, c(NA, NA))

  ## special case with longer .pid
  obj_a <- obj
  obj_a@records[[1]]@.pid <- c(obj_a@records[[1]]@.pid, obj_a@records[[1]]@.pid)
  expect_s3_class(structure_RLum(obj_a), "data.frame")

  ## object with some info
  data(ExampleData.BINfileData, envir = environment())
  d1 <- Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data, pos = 1)
  res <- structure_RLum(d1)
  expect_equal(nrow(res), length(d1@records))
  expect_equal(ncol(res), 13)

  res2 <- structure_RLum(d1, fullExtent = TRUE)
  expect_equal(nrow(res2), length(d1@records))
  expect_equal(ncol(res2), 12 + length(d1@records[[1]]@info))
  expect_null(res2$info)
  expect_equal(names(res2)[-c(1:12)],
               paste0("info.", names(d1@records[[1]]@info)))

  ## on an empty object with some info
  res <- structure_RLum(tmp)
  expect_equal(nrow(res), length(tmp@records))
  expect_equal(ncol(res), 13)
  expect_equal(res$n.channels, rep(5, 20))

  res2 <- structure_RLum(tmp, fullExtent = TRUE)
  expect_equal(nrow(res2), length(tmp@records))
  expect_equal(ncol(res2), 12 + length(tmp@records[[1]]@info))
  expect_null(res2$info)                     ## since @info contains only one
  expect_equal(names(res2)[-c(1:12)],        ##  element named `el`, the last
               names(tmp@records[[1]]@info)) ##  column in res2 is also named
  expect_equal(res2$el, as.character(1:20))  ##  `el` rather than `info.el`

  ## on an even emptier object empty info
  empty <- set_RLum("RLum.Analysis",
                    records = list(set_RLum("RLum.Data.Curve")))

  expect_s3_class(res <- structure_RLum(empty),
                  "data.frame")
  expect_equal(nrow(res), length(empty@records))
  expect_equal(ncol(res), 13)
  expect_equal(res$n.channels, 1)
  expect_equal(unlist(res$info), NA)

  res2 <- structure_RLum(empty, fullExtent = TRUE)
  expect_equal(nrow(res2), length(empty@records))
  expect_equal(ncol(res2), 13)
  expect_equal(names(res2), names(res))
  expect_equal(res2$info, NA)

  ## melt
  t <- melt_RLum(tmp)
  expect_type(t, "list")
})

test_that("remove_RLum", {
  testthat::skip_on_cran()

  ## remove all OSL curves
  t <- expect_s4_class(remove_RLum(sar, recordType = "OSL"), "RLum.Analysis")
  expect_length(t@records, n = 4)

  ## provide arguments that are overwritten
  t <- expect_s4_class(remove_RLum(sar, recordType = "OSL", drop = TRUE), "RLum.Analysis")
  expect_length(t@records, n = 4)

  t <- expect_s4_class(remove_RLum(sar, record.id = 8:20), "RLum.Analysis")
  expect_length(t@records, n = 7)

  ## provide as list
  t <- expect_type(remove_RLum(list(sar, sar), recordType = "OSL"), "list")
  expect_length(t, n = 2)

  ## odd wrong element
  t <- expect_type(remove_RLum(list(sar, "error"), recordType = "OSL"), "list")
  expect_length(t, n = 2)

  ## use subset
  ## this produces and error because of the logical expression
  expect_error(remove_RLum(list(sar, sar), subset = recordType == "OSL"))

  ## simple check for info element
  expect_s4_class(remove_RLum(sar, subset = "duration== 118"), "RLum.Analysis")

  ## this works with terminal output
  SW({
  t <- expect_type(remove_RLum(list(sar, sar), subset = "recordType == 'TL'"), "list")
  })

  expect_length(t, n = 2)

})
