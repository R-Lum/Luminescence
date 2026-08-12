# generate_fixtures.R ---------------------------------------------------------
#
# Exports reference data from the R package for the Python port:
#
#   1. The four BaseDataSet.* lookup tables (load-bearing physical constants)
#      -> src/luminescence/data/datasets/*.csv          (shipped in the wheel)
#   2. R-computed reference outputs for numerical parity tests
#      -> tests/python/fixtures/<subsystem>/<function>__<case>.json
#   3. tests/python/fixtures/MANIFEST.json with full provenance
#
# Run manually from the repository root (never in Python CI):
#
#     Rscript tools/generate_fixtures.R
#
# Uses base R only. Nested lists are flattened into one CSV per data.frame,
# with "__" separating path components (e.g. ConversionFactors__Guerinetal2011__beta.csv).
# Every CSV starts with '#' comment lines (skipped by pandas via comment="#").
# ------------------------------------------------------------------------------

pkg_version <- read.dcf("DESCRIPTION", fields = "Version")[1, 1]
timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
datasets_dir <- "src/luminescence/data/datasets"
fixtures_dir <- "tests/python/fixtures"
dir.create(datasets_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fixtures_dir, recursive = TRUE, showWarnings = FALSE)

written <- character(0)

write_csv_with_provenance <- function(df, file, source_rda) {
  con <- file(file, open = "w", encoding = "UTF-8")
  on.exit(close(con))
  writeLines(sprintf("# source: Luminescence R package v%s, data/%s", pkg_version, source_rda), con)
  writeLines(sprintf("# generated: %s by tools/generate_fixtures.R", timestamp), con)
  has_rownames <- !identical(attr(df, "row.names"), seq_len(nrow(df)))
  write.csv(df, con, row.names = has_rownames)
  written <<- c(written, file)
  message("  wrote ", file)
}

# Recursively export an object: data.frames/matrices/vectors become CSVs,
# lists recurse with their element names appended via "__".
export_object <- function(obj, name, source_rda) {
  if (is.data.frame(obj)) {
    write_csv_with_provenance(obj, file.path(datasets_dir, paste0(name, ".csv")), source_rda)
  } else if (is.matrix(obj)) {
    export_object(as.data.frame(obj), name, source_rda)
  } else if (is.atomic(obj)) {
    df <- data.frame(value = obj)
    if (!is.null(names(obj))) df <- cbind(name = names(obj), df)
    write_csv_with_provenance(df, file.path(datasets_dir, paste0(name, ".csv")), source_rda)
  } else if (is.list(obj)) {
    stopifnot(!is.null(names(obj)))
    for (el in names(obj)) {
      export_object(obj[[el]], paste(name, el, sep = "__"), source_rda)
    }
  } else {
    stop("Cannot export object of class ", paste(class(obj), collapse = "/"), ": ", name)
  }
}

## Part 1: BaseDataSet lookup tables -------------------------------------------

message("Exporting BaseDataSet lookup tables ...")
base_data_files <- list.files("data", pattern = "^BaseDataSet.*\\.rda$")
for (rda in base_data_files) {
  e <- new.env()
  load(file.path("data", rda), envir = e)
  for (obj_name in ls(e)) {
    # strip the redundant "BaseDataSet." prefix from file names
    clean <- sub("^BaseDataSet\\.", "", obj_name)
    export_object(get(obj_name, envir = e), clean, rda)
  }
}

## Part 2: reference outputs for parity tests ----------------------------------
#
# Grown phase by phase. Each block loads its inputs, runs the R implementation
# with a fixed seed, and stores the result as JSON with full double precision.
# Requires library(Luminescence) and jsonlite; skipped with a message when
# either is unavailable so Part 1 keeps working without them.

has_pkg <- requireNamespace("Luminescence", quietly = TRUE) &&
  requireNamespace("jsonlite", quietly = TRUE)
if (!has_pkg) {
  message("Luminescence/jsonlite not installed - skipping Part 2 (parity fixtures)")
} else {
  suppressPackageStartupMessages(library(Luminescence))

  write_fixture <- function(result, subsystem, name, pretty = TRUE) {
    dir.create(file.path(fixtures_dir, subsystem), showWarnings = FALSE, recursive = TRUE)
    path <- file.path(fixtures_dir, subsystem, paste0(name, ".json"))
    jsonlite::write_json(result, path, digits = NA, na = "null", pretty = pretty)
    written <<- c(written, path)
    message("  wrote ", path)
  }

  ## -- Phase 1: SAR vertical slice ------------------------------------------

  data(ExampleData.BINfileData, envir = environment())

  # input: the Risoe object itself, so Python needs no R at test time.
  # Only positions 1-2 are exported (the parity tests use position 1; 2 is
  # headroom) -- the full 24-position payload would add ~2 MB to the repo.
  keep <- CWOSL.SAR.Data@METADATA$POSITION %in% c(1, 2)
  meta_path <- file.path(fixtures_dir, "io", "CWOSL.SAR.Data__METADATA.csv")
  dir.create(file.path(fixtures_dir, "io"), showWarnings = FALSE, recursive = TRUE)
  write.csv(CWOSL.SAR.Data@METADATA[keep, ], meta_path, row.names = FALSE)
  written <- c(written, meta_path)
  write_fixture(CWOSL.SAR.Data@DATA[keep], "io", "CWOSL.SAR.Data__DATA", pretty = FALSE)

  set.seed(1)
  object <- Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data, pos = 1)
  results <- analyse_SAR.CWOSL(
    object,
    signal_integral = 1:2,
    background_integral = 900:1000,
    plot = FALSE,
    verbose = FALSE
  )
  write_fixture(
    list(
      data = get_RLum(results, "data"),
      LnLxTnTx.table = get_RLum(results, "LnLxTnTx.table"),
      rejection.criteria = get_RLum(results, "rejection.criteria")
    ),
    "analysis", "analyse_SAR.CWOSL__pos1_sig1-2_bg900-1000_SSE"
  )

  # calc_OSLLxTxRatio on the first Lx/Tx pair of the same aliquot
  lx <- get_RLum(object, record.id = 2)  # record 1 is TL
  tx <- get_RLum(object, record.id = 4)
  ratio <- calc_OSLLxTxRatio(
    Lx.data = as.data.frame(lx@data),
    Tx.data = as.data.frame(tx@data),
    signal_integral = 1:2,
    background_integral = 900:1000
  )
  write_fixture(
    list(LxTx.table = get_RLum(ratio, "LxTx.table")),
    "analysis", "calc_OSLLxTxRatio__pos1_first_pair"
  )

  # calc_Statistics on ExampleData.DeValues (input exported alongside the result)
  data(ExampleData.DeValues, envir = environment())
  bt998_path <- file.path(fixtures_dir, "models", "ExampleData.DeValues__BT998.csv")
  dir.create(file.path(fixtures_dir, "models"), showWarnings = FALSE, recursive = TRUE)
  write.csv(ExampleData.DeValues$BT998, bt998_path, row.names = FALSE)
  written <- c(written, bt998_path)
  stats <- calc_Statistics(ExampleData.DeValues$BT998, n.MCM = NULL)
  write_fixture(stats, "models", "calc_Statistics__BT998")

  # fit_DoseResponseCurve on the LxTx table produced above (deterministic part)
  set.seed(1)
  lxtx_table <- get_RLum(results, "LnLxTnTx.table")
  fit <- fit_DoseResponseCurve(
    data.frame(
      Dose = lxtx_table$Dose,
      LxTx = lxtx_table$LxTx,
      LxTx.Error = lxtx_table$LxTx.Error
    ),
    fit.method = "SSE", n.MC = 100, verbose = FALSE
  )
  write_fixture(
    list(De = get_RLum(fit)),
    "fitting", "fit_DoseResponseCurve__pos1_SSE"
  )
}

## MANIFEST ---------------------------------------------------------------------

manifest_file <- file.path(fixtures_dir, "MANIFEST.json")
json_escape <- function(x) gsub('"', '\\\\"', gsub("\\\\", "\\\\\\\\", x))
manifest <- c(
  "{",
  sprintf('  "luminescence_r_version": "%s",', json_escape(pkg_version)),
  sprintf('  "r_version": "%s",', json_escape(R.version.string)),
  sprintf('  "generated": "%s",', json_escape(timestamp)),
  '  "generator": "tools/generate_fixtures.R",',
  '  "files": [',
  paste0('    "', json_escape(sort(written)), '"', c(rep(",", max(0, length(written) - 1)), "")),
  "  ]",
  "}"
)
writeLines(manifest, manifest_file)
message("Wrote ", manifest_file, " (", length(written), " files)")
