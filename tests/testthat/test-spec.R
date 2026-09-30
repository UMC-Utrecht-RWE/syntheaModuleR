# spec.R: build_module_spec(), write_module_spec(), read_module_spec(), build_module_from_spec().

exposure_codes_fixture <- function() {
  data.frame(
    option = c("drugA", "drugB"),
    system = c("RxNorm", "RxNorm"),
    code = c("111", "222"),
    display = c("Drug A", "Drug B"),
    stringsAsFactors = FALSE
  )
}

outcome_codes_fixture <- function() {
  data.frame(
    event_abbreviation = c("OUT1", "OUT2"),
    system = c("SNOMED-CT", "SNOMED-CT"),
    code = c("999", "888"),
    display = c("Outcome One", "Outcome Two"),
    stringsAsFactors = FALSE
  )
}

a_spec <- function(inclusion_criteria = NULL, exposure_state = "vaccine") {
  build_module_spec(
    "spec_test",
    exposure_codes = exposure_codes_fixture(),
    exposure_shares = c(drugA = 0.3, drugB = 0.3, comparator = 0.4),
    outcome_codes = outcome_codes_fixture(),
    outcome_probability = 0.1,
    outcome_delay = list(low = 0, high = 100, unit = "days"),
    inclusion_criteria = inclusion_criteria,
    exposure_state = exposure_state
  )
}

# build_module_spec() ----

test_that("build_module_spec assembles exposure options, comparator, and outcome items", {
  spec <- a_spec()
  expect_equal(spec$name, "spec_test")
  expect_equal(length(spec$exposure$options), 2)
  expect_equal(spec$exposure$options[[1]]$option, "drugA")
  expect_equal(spec$exposure$comparator$share, 0.4)
  expect_equal(length(spec$outcomes$items), 2)
  expect_equal(spec$outcomes$items[[1]]$probability, 0.1)
  expect_null(spec$inclusion_criteria)
})

test_that("build_module_spec errors when exposure_shares doesn't sum to 1", {
  expect_error(
    build_module_spec(
      "x",
      exposure_codes = exposure_codes_fixture(),
      exposure_shares = c(drugA = 0.3, drugB = 0.3, comparator = 0.3), # sums to 0.9
      outcome_codes = outcome_codes_fixture(),
      outcome_probability = 0.1,
      outcome_delay = list(low = 0, high = 100, unit = "days")
    ),
    "sum to 1"
  )
})

test_that("build_module_spec errors when exposure_shares is missing the comparator entry", {
  expect_error(
    build_module_spec(
      "x",
      exposure_codes = exposure_codes_fixture(),
      exposure_shares = c(drugA = 0.5, drugB = 0.5),
      outcome_codes = outcome_codes_fixture(),
      outcome_probability = 0.1,
      outcome_delay = list(low = 0, high = 100, unit = "days")
    ),
    "comparator"
  )
})

test_that("build_module_spec errors on an out-of-range outcome probability", {
  expect_error(
    build_module_spec(
      "x",
      exposure_codes = exposure_codes_fixture(),
      exposure_shares = c(drugA = 0.3, drugB = 0.3, comparator = 0.4),
      outcome_codes = outcome_codes_fixture(),
      outcome_probability = 1.5,
      outcome_delay = list(low = 0, high = 100, unit = "days")
    ),
    "\\(0, 1\\)"
  )
})

test_that("build_module_spec rejects a negative arm share even when the total is 1", {
  expect_error(
    build_module_spec(
      "x",
      exposure_codes = exposure_codes_fixture(),
      exposure_shares = c(drugA = -0.1, drugB = 0.6, comparator = 0.5),
      outcome_codes = outcome_codes_fixture(),
      outcome_probability = 0.1,
      outcome_delay = list(low = 0, high = 100, unit = "days")
    ),
    "finite number in \\[0, 1\\]"
  )
})

test_that("build_module_spec errors clearly on an NA outcome probability", {
  codes <- outcome_codes_fixture()
  codes$p <- c(0.05, NA)
  expect_error(
    build_module_spec(
      "x",
      exposure_codes = exposure_codes_fixture(),
      exposure_shares = c(drugA = 0.3, drugB = 0.3, comparator = 0.4),
      outcome_codes = codes,
      outcome_probability = "p",
      outcome_delay = list(low = 0, high = 100, unit = "days")
    ),
    "\\(0, 1\\)"
  )
})

test_that("build_module_spec accepts a column name for per-row outcome probability", {
  codes <- outcome_codes_fixture()
  codes$p <- c(0.05, 0.2)
  spec <- build_module_spec(
    "x",
    exposure_codes = exposure_codes_fixture(),
    exposure_shares = c(drugA = 0.3, drugB = 0.3, comparator = 0.4),
    outcome_codes = codes,
    outcome_probability = "p",
    outcome_delay = list(low = 0, high = 100, unit = "days")
  )
  expect_equal(spec$outcomes$items[[1]]$probability, 0.05)
  expect_equal(spec$outcomes$items[[2]]$probability, 0.2)
  # a per-row-sourced probability has no single meaningful default
  expect_null(spec$outcomes$default_probability)
})

test_that("build_module_spec falls back from `system` to `coding_system` column names", {
  codes <- exposure_codes_fixture()
  names(codes)[names(codes) == "system"] <- "coding_system"
  spec <- build_module_spec(
    "x",
    exposure_codes = codes,
    exposure_shares = c(drugA = 0.3, drugB = 0.3, comparator = 0.4),
    outcome_codes = outcome_codes_fixture(),
    outcome_probability = 0.1,
    outcome_delay = list(low = 0, high = 100, unit = "days")
  )
  expect_equal(spec$exposure$options[[1]]$system, "RxNorm")
})

# write_module_spec() / read_module_spec() ----

test_that("write_module_spec then read_module_spec round-trips the spec", {
  spec <- a_spec(
    inclusion_criteria = list(
      age = list(operator = ">=", quantity = 18, unit = "years")
    )
  )
  path <- tempfile(fileext = ".yaml")
  write_module_spec(spec, path)
  spec2 <- read_module_spec(path)
  expect_equal(spec2$name, spec$name)
  expect_equal(spec2$inclusion_criteria$age$operator, ">=")
  expect_equal(spec2$exposure$options[[1]]$option, "drugA")
  expect_equal(length(spec2$outcomes$items), 2)
})

test_that("write_module_spec creates parent directories", {
  path <- file.path(tempfile(), "nested", "dir", "spec.yaml")
  write_module_spec(a_spec(), path)
  expect_true(file.exists(path))
})

test_that("read_module_spec errors clearly on a YAML file missing required keys", {
  path <- tempfile(fileext = ".yaml")
  yaml::write_yaml(list(name = "incomplete"), path)
  expect_error(read_module_spec(path), "exposure")
})

# build_module_from_spec() ----

exposure_module <- function(spec) {
  module_states(build_module_from_spec(spec)[[spec$name]])
}

test_that("build_module_from_spec omits the Guard entirely when inclusion_criteria is absent", {
  m <- exposure_module(a_spec())
  expect_equal(m$Initial$direct_transition, "Exposure Choice")
})

test_that("build_module_from_spec prepends a Guard when inclusion_criteria is present", {
  spec <- a_spec(
    inclusion_criteria = list(
      age = list(operator = ">=", quantity = 60, unit = "years")
    )
  )
  m <- exposure_module(spec)
  expect_equal(m$Initial$direct_transition, "Inclusion Criteria")
  expect_equal(m[["Inclusion Criteria"]]$type, "Guard")
})

test_that("build_module_from_spec builds a Vaccine state for exposure_state = 'vaccine'", {
  m <- exposure_module(a_spec(exposure_state = "vaccine"))
  expect_equal(m[["Exposure - drugA"]]$type, "Vaccine")
})

test_that("build_module_from_spec builds a bare MedicationOrder for exposure_state = 'medication'", {
  m <- exposure_module(a_spec(
    exposure_state = "medication"
  ))
  expect_equal(m[["Exposure - drugA"]]$type, "MedicationOrder")
  expect_null(m[["Exposure - drugA"]]$reason)
})

test_that("build_module_from_spec builds a ConditionOnset for exposure_state = 'condition'", {
  m <- exposure_module(a_spec(
    exposure_state = "condition"
  ))
  expect_equal(m[["Exposure - drugA"]]$type, "ConditionOnset")
})

test_that("build_module_from_spec returns the exposure module plus one module per outcome item", {
  modules <- build_module_from_spec(a_spec())
  expect_equal(
    names(modules),
    c("spec_test", "spec_test - OUT1", "spec_test - OUT2")
  )
  exposure <- module_states(modules[["spec_test"]])
  expect_false(any(grepl("OUT", names(exposure))))

  out1 <- module_states(modules[["spec_test - OUT1"]])
  expect_equal(out1$Initial$direct_transition, "Wait For Exposure")
  guard <- out1[["Wait For Exposure"]]
  expect_equal(guard$type, "Guard")
  expect_equal(guard$allow$attribute, "exposure_group")
  expect_equal(guard$allow$operator, "is not nil")
  expect_null(guard$allow$value)
  expect_equal(guard$direct_transition, "OUT1 Delay")
  expect_equal(out1[["Outcome - OUT1"]]$type, "ConditionOnset")
})

test_that("build_module_from_spec gives each outcome its own window, not a cumulative chain", {
  out2 <- module_states(build_module_from_spec(a_spec())[["spec_test - OUT2"]])
  expect_false(any(grepl("OUT1", names(out2))))
  expect_equal(out2[["Wait For Exposure"]]$direct_transition, "OUT2 Delay")
})

test_that("build_module_from_spec produces valid modules that round-trip through jsonlite", {
  for (json in build_module_from_spec(a_spec())) {
    parsed <- jsonlite::fromJSON(json, simplifyVector = FALSE)
    expect_true("Terminal" %in% names(parsed$states))
  }
})

test_that("build_module_from_spec(as_json = FALSE) returns R lists", {
  modules <- build_module_from_spec(a_spec(), as_json = FALSE)
  expect_true(all(vapply(modules, is.list, logical(1))))
})

test_that("build_module_from_spec rejects invalid hand-edited probabilities and shares", {
  spec <- a_spec()
  spec$outcomes$items[[1]]$probability <- 1.5
  expect_error(build_module_from_spec(spec), "OUT1' probability")

  spec <- a_spec()
  spec$outcomes$items[[1]]$probability <- "high"
  expect_error(build_module_from_spec(spec), "OUT1' probability")

  spec <- a_spec()
  spec$exposure$options[[1]]$share <- -0.1
  spec$exposure$options[[2]]$share <- 0.7
  expect_error(build_module_from_spec(spec), "finite number in \\[0, 1\\]")
})

test_that("build_module_from_spec requires exposure$attribute", {
  spec <- a_spec()
  spec$exposure$attribute <- NULL
  expect_error(build_module_from_spec(spec), "exposure\\$attribute must be set")
})

test_that("build_module_from_spec accepts a spec list directly (not just a file path)", {
  spec <- a_spec()
  m <- exposure_module(spec)
  expect_true("Exposure Choice" %in% names(m))
})

test_that("build_module_from_spec accepts a YAML path directly", {
  path <- tempfile(fileext = ".yaml")
  write_module_spec(a_spec(), path)
  m <- module_states(build_module_from_spec(path)[["spec_test"]])
  expect_true("Exposure Choice" %in% names(m))
})

test_that("build_module_from_spec honors a per-item probability/delay override over the defaults", {
  spec <- a_spec()
  spec$outcomes$items[[1]]$probability <- 0.9
  spec$outcomes$items[[1]]$delay <- list(low = 5, high = 5, unit = "days")
  m <- module_states(build_module_from_spec(spec)[["spec_test - OUT1"]])
  expect_equal(m[["OUT1 Delay"]]$range$low, 5)
  choice <- m[["OUT1 Onset Choice"]]$distributed_transition
  onset_share <- Filter(function(o) o$transition == "Outcome - OUT1", choice)[[
    1
  ]]$distribution
  expect_equal(onset_share, 0.9)
})
