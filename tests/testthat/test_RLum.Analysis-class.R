data(ExampleData.RLum.Analysis, envir = environment())
obj <- IRSAR.RF.Data

##construct empty object
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

test_that("Check the example and the numerical values", {
  testthat::skip_on_cran()

  ## set_RLum()
  expect_s4_class(tmp, "RLum.Analysis")

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

  ## names()
  expect_type(names(tmp), "character")
})

test_that("get_RLum", {
  testthat::skip_on_cran()

  ## input validation
  expect_error(get_RLum(obj, subset = "error"),
               "[get_RLum()] 'subset' must contain a logical expression",
               fixed = TRUE)
  expect_error(get_RLum(obj, subset = (error == "OSL")),
               "[get_RLum()] Invalid subset expression, valid terms are:",
               fixed = TRUE)
  expect_error(get_RLum(tmp, record.id = "character"),
               "'record.id' has to be of type 'numeric' or 'logical'")
  expect_error(get_RLum(tmp, recordType = 1L),
               "'recordType' has to be of type 'character'")
  expect_error(get_RLum(tmp, curveType = 1L),
               "'curveType' has to be of type 'character'")
  expect_error(get_RLum(tmp, RLum.type = 1L),
               "'RLum.type' has to be of type 'character'")
  expect_error(get_RLum(tmp, get.index = "a"),
               "'get.index' has to be of type 'logical'")

  ## check functionality
  expect_length(get_RLum(obj, subset = (recordType == "RF")), 2)
  expect_length(get_RLum(tmp, subset = (el == "2")), 1)
  expect_s4_class(get_RLum(tmp, subset = (el == "2")), "RLum.Analysis")
  expect_type(get_RLum(tmp, info.object = "el"), "character")

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
  })
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
  expect_equal(res$info, c(NA, NA))

  expect_s3_class(res2 <- structure_RLum(obj, fullExtent = TRUE),
                  "data.frame")
  expect_equal(names(res2), names(res))
  expect_equal(res2$info, c(NA, NA))

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
  expect_equal(res$info, NA)

  res2 <- structure_RLum(empty, fullExtent = TRUE)
  expect_equal(nrow(res2), length(empty@records))
  expect_equal(ncol(res2), 13)
  expect_equal(names(res2), names(res))
  expect_equal(res2$info, NA)
})
