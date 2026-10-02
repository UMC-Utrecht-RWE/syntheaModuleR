# events.R: build_event_spec(), write_event_spec(), read_event_spec(), build_modules_from_events(),
# write_modules(), summarise_events().

event_codes_fixture <- function() {
  data.frame(
    concept_id = c(
      "D_PAIN_AESI", "D_PAIN_AESI", "C_HF_COV", "C_HF_COV", "C_HF_COV",
      "DP_STAT", "DP_STAT", "DP_ANTIBIO", "VP_INF"
    ),
    source = "fixture",
    source_type = c("AESI", "AESI", "COV", "COV", "COV", "DP", "DP", "DP", "VP"),
    system = c(rep("MEDCODEID", 5), rep("PRODCODEID", 4)),
    code = c("1", "2", "2", "3", "4", "11", "12", "13", "14"),
    display = c("Pain", "Pain 2", "Pain 2", "HF", "Barré", "Statin A", "Statin B", "Antibiotic", "Flu"),
    tag = "narrow",
    stringsAsFactors = FALSE
  )
}

event_exposure_fixture <- function() {
  data.frame(
    option = c("drugA", "drugB"),
    system = "PRODCODEID",
    code = c("111", "222"),
    display = c("Drug A", "Drug B"),
    stringsAsFactors = FALSE
  )
}

an_event_spec <- function(...) {
  build_event_spec(
    "events_test",
    event_codes_fixture(),
    exposure_codes = event_exposure_fixture(),
    exposure_shares = c(drugA = 0.3, drugB = 0.3, comparator = 0.4),
    exposure_delay = list(low = 365, high = 545, unit = "days"),
    cohort_criteria = list(age = list(operator = ">=", quantity = 60, unit = "years")),
    recurring_drugs = "DP_STAT",
    ...
  )
}

leaf_codes <- function(states) {
  leaves <- states[vapply(states, function(s) {
    s$type %in% c("ConditionOnset", "MedicationOrder", "Vaccine")
  }, logical(1))]
  vapply(leaves, function(s) s$codes[[1]]$code, character(1))
}

# build_event_spec() ----

test_that("build_event_spec assigns blocks and patterns from roles", {
  x <- an_event_spec()
  cc <- x$concepts
  aesi <- cc[cc$concept_id == "D_PAIN_AESI", ]
  expect_setequal(aesi$block, c("baseline", "follow_up"))
  expect_equal(unique(aesi$pattern), "acute")
  expect_equal(cc$pattern[cc$concept_id == "C_HF_COV"], "chronic")
  expect_equal(cc$block[cc$concept_id == "DP_STAT"], "chronic")
  expect_equal(cc$pattern[cc$concept_id == "DP_STAT"], "recurring")
  expect_equal(cc$pattern[cc$concept_id == "DP_ANTIBIO"], "course")
  expect_equal(cc$state_type[cc$concept_id == "VP_INF"], "vaccine")
  expect_equal(cc$n_codes[cc$concept_id == "C_HF_COV"], 3L)
  expect_true(all(cc$weight == 1))
  expect_equal(x$spec$spec_version, 2L)
})

test_that("build_event_spec errors on a source_type with no roles entry", {
  codes <- event_codes_fixture()
  codes$source_type[1] <- "LAB"
  expect_error(
    build_event_spec(
      "t",
      codes,
      exposure_codes = event_exposure_fixture(),
      exposure_shares = c(drugA = 0.5, drugB = 0.2, comparator = 0.3),
      exposure_delay = list(low = 365, high = 545, unit = "days")
    ),
    "no `roles` entry for source_type\\(s\\): LAB"
  )
})

test_that("build_event_spec records skipped concepts in provenance", {
  x <- an_event_spec(skipped = c("TP_X_COV", "TP_A_COV"))
  expect_equal(unlist(x$spec$provenance$skipped_concepts), c("TP_A_COV", "TP_X_COV"))
})

# Timing and consistency checks ----

test_that("a baseline window that could run into exposure is rejected", {
  blocks <- default_event_blocks()
  blocks$baseline$window$high <- 330
  expect_error(an_event_spec(blocks = blocks), "baseline records could land after exposure")
})

test_that("recurring therapy that can end before exposure is rejected", {
  patterns <- default_event_patterns()
  patterns$recurring$cycles <- 5
  expect_error(an_event_spec(patterns = patterns), "raise patterns\\$recurring\\$cycles")
})

test_that("a recurring block drawing more than one therapy per module is rejected", {
  blocks <- default_event_blocks()
  blocks$chronic$k <- list(`0` = 0.5, `1` = 0.3, `2` = 0.2)
  expect_error(an_event_spec(blocks = blocks), "use `parallel` for more")
})

test_that("k shares must sum to 1", {
  blocks <- default_event_blocks()
  blocks$follow_up$k <- list(`0` = 0.5, `1` = 0.2)
  expect_error(an_event_spec(blocks = blocks), "k shares must sum to 1")
})

test_that("a pattern a state type can't have is rejected", {
  x <- an_event_spec()
  x$concepts$pattern[x$concepts$concept_id == "C_HF_COV"] <- "recurring"
  expect_error(build_modules_from_events(x), "can't have")
})

test_that("an enabled concept without codes is rejected; a disabled one is fine", {
  x <- an_event_spec()
  x$codes <- x$codes[x$codes$concept_id != "VP_INF", ]
  expect_error(build_modules_from_events(x), "enabled concept\\(s\\) with no codes: VP_INF")

  x$concepts$enabled[x$concepts$concept_id == "VP_INF"] <- FALSE
  modules <- build_modules_from_events(x, as_json = FALSE)
  expect_false("14" %in% leaf_codes(modules[["events_test - baseline"]]$states))
})

test_that("a concept referring to an undefined block is rejected", {
  x <- an_event_spec()
  x$concepts$block[1] <- "nowhere"
  expect_error(build_modules_from_events(x), "not defined in `blocks`: nowhere")
})

# YAML + CSV I/O ----

test_that("write_event_spec/read_event_spec round-trip the spec, concepts and codes", {
  x <- an_event_spec()
  dir <- tempfile()
  path <- write_event_spec(x, dir)
  expect_true(file.exists(file.path(dir, "events_test_concepts.csv")))
  y <- read_event_spec(path)
  expect_equal(y$concepts, x$concepts, ignore_attr = TRUE)
  expect_equal(y$codes, x$codes, ignore_attr = TRUE)
  expect_equal(y$spec$blocks, x$spec$blocks)
  expect_true("Barré" %in% y$codes$display)
})

test_that("read_event_spec fills a blank weight with 1 and honours enabled = FALSE", {
  dir <- tempfile()
  path <- write_event_spec(an_event_spec(), dir)
  cpath <- file.path(dir, "events_test_concepts.csv")
  concepts <- read.csv(cpath, colClasses = "character")
  concepts$weight[1] <- ""
  concepts$enabled[concepts$concept_id == "VP_INF"] <- "FALSE"
  write.csv(concepts, cpath, row.names = FALSE)
  y <- read_event_spec(path)
  expect_equal(y$concepts$weight[1], 1)
  expect_false(y$concepts$enabled[y$concepts$concept_id == "VP_INF"])
})

test_that("read_event_spec rejects unknown keys and columns", {
  dir <- tempfile()
  path <- write_event_spec(an_event_spec(), dir)
  spec <- yaml::read_yaml(path)
  spec$probabilty <- 0.1
  yaml::write_yaml(spec, path)
  expect_error(read_event_spec(path), "unknown top-level key\\(s\\): probabilty")

  path <- write_event_spec(an_event_spec(), dir)
  cpath <- file.path(dir, "events_test_concepts.csv")
  concepts <- read.csv(cpath, colClasses = "character")
  concepts$probability <- "0.1"
  write.csv(concepts, cpath, row.names = FALSE)
  expect_error(read_event_spec(path), "unknown concepts column\\(s\\): probability")
})

test_that("read_module_spec points a spec_version 2 file to read_event_spec", {
  path <- write_event_spec(an_event_spec(), tempfile())
  expect_error(read_module_spec(path), "read it with read_event_spec\\(\\)")
})

# build_modules_from_events() ----

test_that("build_modules_from_events builds the exposure module plus one module per block copy", {
  modules <- build_modules_from_events(an_event_spec(), as_json = FALSE)
  expect_equal(
    names(modules),
    c(
      "events_test", "events_test - baseline", "events_test - chronic 1",
      "events_test - chronic 2", "events_test - follow_up"
    )
  )
  exposure <- modules[["events_test"]]$states
  expect_equal(exposure[["Cohort Criteria"]]$direct_transition, "Cohort Entry")
  expect_equal(exposure[["Cohort Entry"]]$attribute, "cohort_entry")
  expect_equal(exposure[["Cohort Entry"]]$direct_transition, "Exposure Delay")
  expect_equal(exposure[["Exposure Delay"]]$range$high, 545)
})

test_that("every code of every block concept is exactly one leaf state", {
  modules <- build_modules_from_events(an_event_spec(), as_json = FALSE)
  baseline <- leaf_codes(modules[["events_test - baseline"]]$states)
  # baseline: D_PAIN_AESI (1, 2), C_HF_COV (2, 3, 4), DP_ANTIBIO (13), VP_INF (14)
  expect_equal(sort(unname(baseline)), sort(c("1", "2", "2", "3", "4", "13", "14")))
  expect_equal(sort(unname(leaf_codes(modules[["events_test - follow_up"]]$states))), c("1", "2"))
  expect_equal(sort(unname(leaf_codes(modules[["events_test - chronic 1"]]$states))), c("11", "12"))
})

test_that("the K-draw loop sets, checks and decrements a per-module counter", {
  states <- build_modules_from_events(an_event_spec(), as_json = FALSE)[["events_test - follow_up"]]$states
  expect_equal(states[["Wait For Anchor"]]$allow$attribute, "exposure_group")
  k_targets <- vapply(states[["K Choice"]]$distributed_transition, `[[`, character(1), "transition")
  expect_equal(k_targets, c("Terminal", "Set K 1", "Set K 2"))
  expect_equal(states[["Set K 2"]]$attribute, "events_test_follow_up_remaining")
  expect_equal(states[["Set K 2"]]$value, 2)
  check <- states[["Check"]]$conditional_transition
  expect_equal(check[[1]]$condition$operator, ">")
  expect_equal(check[[2]]$transition, "Terminal")
  expect_equal(states[["Decrement"]]$action, "decrement")
  expect_equal(states[["Decrement"]]$direct_transition, "Check")
  # follow-up window 0-1460 days over max(k) = 2 draws
  expect_equal(states[["Step Delay"]]$range$high, 730)
})

test_that("pattern tails end what the leaf recorded", {
  modules <- build_modules_from_events(an_event_spec(), as_json = FALSE)
  baseline <- modules[["events_test - baseline"]]$states
  pain_leaf <- baseline[["D_PAIN_AESI Code 1"]]
  expect_equal(pain_leaf$assign_to_attribute, "events_test_baseline_current")
  expect_null(pain_leaf$target_encounter) # recorded immediately, no wellness wait
  expect_equal(pain_leaf$direct_transition, "Acute Resolve Delay")
  expect_equal(baseline[["Acute End"]]$type, "ConditionEnd")
  expect_equal(baseline[["Acute End"]]$referenced_by_attribute, "events_test_baseline_current")
  expect_equal(baseline[["DP_ANTIBIO Code 1"]]$direct_transition, "Course Delay")
  expect_equal(baseline[["Course End"]]$type, "MedicationEnd")
  expect_equal(baseline[["C_HF_COV Code 1"]]$direct_transition, "Decrement") # chronic
  expect_equal(baseline[["VP_INF Code 1"]]$type, "Vaccine")
})

test_that("a recurring concept loops back to its own code pick until its cycles run out", {
  states <- build_modules_from_events(an_event_spec(), as_json = FALSE)[["events_test - chronic 1"]]$states
  expect_equal(states[["DP_STAT Start"]]$value, 24)
  expect_true(states[["DP_STAT Code 1"]]$chronic)
  expect_equal(states[["DP_STAT Code 1"]]$direct_transition, "DP_STAT Course")
  expect_equal(states[["DP_STAT Course End"]]$type, "MedicationEnd")
  check <- states[["DP_STAT Cycle Check"]]$conditional_transition
  expect_equal(check[[1]]$transition, "DP_STAT Gap")
  expect_equal(check[[2]]$transition, "Decrement")
  expect_equal(states[["DP_STAT Gap"]]$direct_transition, "DP_STAT Code Pick")
  # chronic copies use separate attributes
  expect_equal(states[["DP_STAT Start"]]$attribute, "events_test_chronic_1_cycles")
})

test_that("concept and code weights become normalised, full-precision probabilities", {
  x <- an_event_spec()
  x$concepts$weight[x$concepts$concept_id == "C_HF_COV" & x$concepts$block == "baseline"] <- 2
  json <- build_modules_from_events(x)[["events_test - baseline"]]
  states <- jsonlite::fromJSON(json, simplifyVector = FALSE)$states
  pick <- states[["Concept Pick"]]$distributed_transition
  shares <- vapply(pick, `[[`, numeric(1), "distribution")
  expect_equal(sum(shares), 1)
  expect_equal(shares[vapply(pick, `[[`, character(1), "transition") == "C_HF_COV Code Pick"], 2 / 5)
  hf <- vapply(states[["C_HF_COV Code Pick"]]$distributed_transition, `[[`, numeric(1), "distribution")
  expect_equal(hf, rep(1 / 3, 3)) # not rounded to 0.3333
})

test_that("max_codes_per_concept caps leaves reproducibly without touching the session RNG", {
  set.seed(99)
  before <- .Random.seed
  a <- build_modules_from_events(an_event_spec(), as_json = FALSE, max_codes_per_concept = 1)
  expect_identical(.Random.seed, before)
  b <- build_modules_from_events(an_event_spec(), as_json = FALSE, max_codes_per_concept = 1)
  hf_a <- leaf_codes(a[["events_test - baseline"]]$states)
  expect_equal(hf_a, leaf_codes(b[["events_test - baseline"]]$states))
  expect_equal(sum(grepl("^C_HF_COV", names(hf_a))), 1)
})

test_that("build_modules_from_events reads a YAML path", {
  path <- write_event_spec(an_event_spec(), tempfile())
  expect_length(build_modules_from_events(path), 5)
})

# write_modules() / summarise_events() ----

test_that("write_modules writes flat files and returns matching Synthea arguments", {
  dir <- tempfile()
  dir.create(dir)
  writeLines("{}", file.path(dir, "stale.json"))
  out <- write_modules(build_modules_from_events(an_event_spec()), dir)
  expect_false(file.exists(file.path(dir, "stale.json")))
  expect_setequal(
    basename(out$files),
    c(
      "events_test.json", "events_test_baseline.json", "events_test_chronic_1.json",
      "events_test_chronic_2.json", "events_test_follow_up.json"
    )
  )
  expect_equal(out$synthea_args, c("-d", dir, "-m", "events_test*"))
  expect_true(any(grepl("Barré", readLines(out$files[2], encoding = "UTF-8"), fixed = TRUE)))
})

test_that("summarise_events reports expected events and code overlap", {
  s <- summarise_events(an_event_spec(skipped = "TP_X_COV"))
  baseline <- s$blocks[s$blocks$block == "baseline", ]
  expect_equal(baseline$expected_events, 1.3)
  expect_equal(baseline$share_with_none, 0.4)
  chronic <- s$blocks[s$blocks$block == "chronic", ]
  expect_equal(chronic$modules, 2)
  expect_equal(chronic$share_with_none, 0.49)
  expect_equal(s$overlap$concept_a, "C_HF_COV")
  expect_equal(s$overlap$concept_b, "D_PAIN_AESI")
  expect_equal(s$overlap$shared_codes, 1L)
  expect_equal(s$skipped, "TP_X_COV")
})
