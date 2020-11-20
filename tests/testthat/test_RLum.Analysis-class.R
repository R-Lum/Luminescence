test_that("Check the example and the numerical values", {
  testthat::skip_on_cran()
  local_edition(3)

  ##load example data
  data("ExampleData.RLum.Analysis")
  obj <- IRSAR.RF.Data

  ## set_RLum()
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

  ## get_RLum
  expect_length(get_RLum(obj, subset = (recordType == "RF")), 2)
  expect_null(get_RLum(obj, subset = (recordType == "")))
  expect_length(get_RLum(tmp, subset = (el == "2")), 1)
  expect_s4_class(get_RLum(tmp, subset = (el == "2")), "RLum.Analysis")
  expect_type(get_RLum(tmp, info.object = "el"), "character")
  expect_warning(get_RLum(tmp, info.object = "missing"), regexp = "Invalid info.object name")
  expect_error(get_RLum(tmp, record.id = "character"), "'record.id' has to be of type 'numeric' or 'logical'!")
  expect_error(get_RLum(tmp, recordType = 1L), "'recordType' has to be of type 'character'!")
  expect_error(get_RLum(tmp, curveType = 1L), "'curveType' has to be of type 'character'!")
  expect_error(get_RLum(tmp, RLum.type = 1L), "'RLum.type' has to be of type 'character'!")
  expect_error(get_RLum(tmp, get.index = "a"), "'get.index' has to be of type 'logical'!")
  expect_null(suppressWarnings(get_RLum(set_RLum("RLum.Analysis"), info = "test")))

  ##structure RLum
  expect_error(
    structure_RLum(set_RLum("RLum.Analysis", records = list(set_RLum("RLum.Data.Image")))),
    "Only 'RLum.Data.Curve' objects are allowed!")

})

