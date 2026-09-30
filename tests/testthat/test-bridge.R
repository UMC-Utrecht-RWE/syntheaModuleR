# bridge.R: bridge_columns(), read_bridge_codelist().

# bridge_columns() ----

test_that("bridge_columns returns Family-A defaults, overridable per field", {
  defaults <- bridge_columns()
  expect_equal(defaults$coding_system, "coding_system")
  expect_equal(defaults$display, "code_name")

  overridden <- bridge_columns(
    coding_system = "product_identifier",
    display = "product_name"
  )
  expect_equal(overridden$coding_system, "product_identifier")
  expect_equal(overridden$display, "product_name")
  # untouched fields keep their default
  expect_equal(overridden$code, "code")
})

# read_bridge_codelist() -- group_by set (AESI-style) ----

aesi_fixture <- function() {
  data.frame(
    event_abbreviation = c(
      "ANAPHYLAXIS",
      "ANAPHYLAXIS",
      "ANAPHYLAXIS",
      "GBS",
      "ALPS",
      "ALPS"
    ),
    coding_system = "MEDCODEID",
    code = c("1", "2", "3", "4", "5", "6"),
    code_name = c(
      "Chronic orthostatic hypotension",
      "Allergic reaction",
      "Anaphylaxis care",
      "Guillain-Barre syndrome",
      "possible-only concept A",
      "possible-only concept B"
    ),
    tags = c("exclude", "possible", "narrow", "narrow", "possible", "possible"),
    type = "AESI",
    stringsAsFactors = FALSE
  )
}

test_that("read_bridge_codelist prefers narrow-tagged rows over possible/exclude", {
  result <- read_bridge_codelist(
    aesi_fixture(),
    type = "AESI",
    coding_system = "MEDCODEID",
    group_by = "event_abbreviation"
  )
  expect_equal(nrow(result), 3)
  anaph <- result[result$event_abbreviation == "ANAPHYLAXIS", ]
  expect_equal(anaph$display, "Anaphylaxis care") # the narrow row, not the exclude-tagged one
})

test_that("read_bridge_codelist falls back to possible when no narrow row exists", {
  result <- read_bridge_codelist(
    aesi_fixture(),
    type = "AESI",
    coding_system = "MEDCODEID",
    group_by = "event_abbreviation"
  )
  alps <- result[result$event_abbreviation == "ALPS", ]
  expect_equal(nrow(alps), 1)
  expect_true(
    alps$display %in% c("possible-only concept A", "possible-only concept B")
  )
})

test_that("read_bridge_codelist splits compound multiple:x+y tags and matches on any constituent", {
  df <- data.frame(
    event_abbreviation = "COMPOUND",
    coding_system = "MEDCODEID",
    code = "9",
    code_name = "Compound-tagged concept",
    tags = "multiple:narrow+possible",
    type = "AESI",
    stringsAsFactors = FALSE
  )
  result <- read_bridge_codelist(
    df,
    type = "AESI",
    coding_system = "MEDCODEID",
    group_by = "event_abbreviation"
  )
  expect_equal(nrow(result), 1)
  expect_equal(result$display, "Compound-tagged concept")
})

test_that("read_bridge_codelist errors, naming the concept, when every row is exclude/ignore", {
  df <- data.frame(
    event_abbreviation = c("ONLYEXCLUDE", "OK"),
    coding_system = "MEDCODEID",
    code = c("1", "2"),
    code_name = c("x", "y"),
    tags = c("exclude", "narrow"),
    type = "AESI",
    stringsAsFactors = FALSE
  )
  expect_error(
    read_bridge_codelist(
      df,
      type = "AESI",
      coding_system = "MEDCODEID",
      group_by = "event_abbreviation"
    ),
    "ONLYEXCLUDE"
  )
})

test_that("read_bridge_codelist drops compound tags carrying exclude/ignore, even with a preferred part", {
  df <- data.frame(
    event_abbreviation = c("X", "X"),
    coding_system = "MEDCODEID",
    code = c("1", "2"),
    code_name = c("excluded compound", "possible row"),
    tags = c("multiple:exclude+narrow", "possible"),
    type = "AESI",
    stringsAsFactors = FALSE
  )
  result <- read_bridge_codelist(
    df,
    type = "AESI",
    coding_system = "MEDCODEID",
    group_by = "event_abbreviation"
  )
  expect_equal(result$display, "possible row")
})

test_that("read_bridge_codelist treats an NA tags cell as ineligible instead of erroring", {
  df <- data.frame(
    event_abbreviation = c("X", "X"),
    coding_system = "MEDCODEID",
    code = c("1", "2"),
    code_name = c("untagged", "tagged"),
    tags = c(NA, "narrow"),
    type = "AESI",
    stringsAsFactors = FALSE
  )
  result <- read_bridge_codelist(
    df,
    type = "AESI",
    coding_system = "MEDCODEID",
    group_by = "event_abbreviation"
  )
  expect_equal(result$display, "tagged")
})

test_that("read_bridge_codelist treats NA filter fields as non-matching", {
  df <- data.frame(
    event_abbreviation = c("X", "Y"),
    coding_system = c("MEDCODEID", NA),
    code = c("1", "2"),
    code_name = c("kept", "blank system"),
    tags = "narrow",
    type = c("AESI", NA),
    stringsAsFactors = FALSE
  )
  result <- read_bridge_codelist(df, type = "AESI", coding_system = "MEDCODEID")
  expect_equal(nrow(result), 1)
  expect_equal(result$display, "kept")
})

# read_bridge_codelist() -- group_by NULL (exposure-style) ----

test_that("read_bridge_codelist with group_by = NULL returns every filtered row uncollapsed", {
  df <- data.frame(
    event_abbreviation = c("RSV", "RSV", "RSV", "OTHER"),
    product_identifier = "PRODCODEID",
    code = c("A1", "A2", "A3", "B1"),
    product_name = c(
      "Vaccine A product",
      "Vaccine B product",
      "Unspecified RSV product",
      "Not RSV"
    ),
    stringsAsFactors = FALSE
  )
  result <- read_bridge_codelist(
    df,
    event_abbreviation = "RSV",
    coding_system = "PRODCODEID",
    columns = bridge_columns(
      coding_system = "product_identifier",
      display = "product_name"
    )
  )
  expect_equal(nrow(result), 3)
  expect_true(all(grepl(
    "RSV|Vaccine A|Vaccine B",
    result$display,
    ignore.case = TRUE
  )))
})

# Column aliasing / real-world deviations ----

test_that("read_bridge_codelist tolerates a missing `origin` column and a renamed display column", {
  df <- data.frame(
    event_abbreviation = "X",
    coding_system = "MEDCODEID",
    code = "1",
    label = "Renamed display column", # covid19-style: concept_name -> label
    tags = "narrow",
    type = "AESI",
    stringsAsFactors = FALSE
  )
  result <- read_bridge_codelist(
    df,
    type = "AESI",
    coding_system = "MEDCODEID",
    group_by = "event_abbreviation",
    columns = bridge_columns(display = "label")
  )
  expect_equal(result$display, "Renamed display column")
})

test_that("read_bridge_codelist errors clearly on a Family-B-shaped (OMOP-style) input", {
  omop_like <- data.frame(
    omop_concept_id = 1,
    concept_code = "X",
    concept_name = "Y",
    cdm_table_name = "Z",
    stringsAsFactors = FALSE
  )
  expect_error(
    read_bridge_codelist(omop_like, type = "AESI", group_by = "concept_code"),
    "could not be resolved"
  )
})

test_that("read_bridge_codelist reads a file path with every column as character", {
  df <- aesi_fixture()
  path <- tempfile(fileext = ".csv")
  utils::write.csv(df, path, row.names = FALSE)
  result <- read_bridge_codelist(
    path,
    type = "AESI",
    coding_system = "MEDCODEID",
    group_by = "event_abbreviation"
  )
  expect_equal(nrow(result), 3)
  expect_type(result$code, "character")
})
