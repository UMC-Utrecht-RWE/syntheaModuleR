# events.R -- the "events" (spec_version 2) form of the bridge -> spec -> module pipeline. Stage 1
# is read_bridge_concepts() (bridge.R): every eligible code of every concept in a codelist. This
# file holds stages 2-4: build_event_spec() (assign each concept to a block/pattern and add the
# timeline parameters), write_event_spec()/read_event_spec() (a YAML for the structure plus two
# hand-editable CSVs for the concepts and their codes), and build_modules_from_events().
#
# Instead of one module per concept, each *block* (e.g. baseline history, chronic therapy,
# follow-up outcomes) is one module that draws K events per patient -- K from a per-block
# distribution -- each draw picking a concept by weight, then one of its codes. That keeps the
# module count small (Synthea re-checks every module for every person at every timestep) and
# makes "how many events does a patient get" a direct parameter. The block modules are built on
# the Engine layer (create_state_settings()/build_module()) rather than the Frontend combinators:
# a loop whose thousands of code leaves converge on a few shared tail states doesn't fit the
# fragment contract (one entry, one exit), and pathways() grows its state list quadratically.
# The exposure module is still Frontend-built (create_population()/pathways()/build_cohort_module()).

.event_unit_days <- c(days = 1, weeks = 7, months = 30, years = 365)
.event_state_patterns <- list(
  condition = c("chronic", "acute"),
  medication = c("course", "recurring"),
  vaccine = c("once")
)
.event_concept_cols <- list(
  required = c("concept_id", "state_type", "pattern", "block"),
  optional = c("source", "source_type", "weight", "enabled", "n_codes", "note")
)
.event_code_cols <- list(
  required = c("concept_id", "system", "code", "display"),
  optional = c("tag", "weight")
)
.event_spec_keys <- list(
  required = c("spec_version", "name", "timeline", "exposure", "blocks", "patterns"),
  optional = c("concepts_file", "codes_file", "provenance")
)

# Defaults ----

#' Default block/state/pattern assignment per codelist category
#'
#' Used by `build_event_spec()` to turn each concept's `source_type` (the codelist's `type`, e.g.
#' `AESI`/`COV`/`CODE`, or the drug-proxy codelist's `system`, `DP`/`VP`) into one or more
#' `concepts.csv` rows. A source type listed twice (AESI) gets one row per block: here an AESI is
#' both prior history (`baseline`) and an outcome (`follow_up`). Drug (`DP`) concepts named in
#' `default_recurring_drugs()` are moved to the recurring block by `build_event_spec()`.
#'
#' @return A `data.frame` with columns `source_type`, `block`, `state_type`, `pattern`.
#' @examples
#' default_event_roles()
#' @export
default_event_roles <- function() {
  data.frame(
    source_type = c("AESI", "AESI", "COV", "CODE", "DP", "VP"),
    block = c("baseline", "follow_up", "baseline", "baseline", "baseline", "baseline"),
    state_type = c("condition", "condition", "condition", "condition", "medication", "vaccine"),
    pattern = c("acute", "acute", "chronic", "chronic", "course", "once"),
    stringsAsFactors = FALSE
  )
}

#' Default list of drug concepts given as repeat prescriptions
#'
#' Long-term therapies (antihypertensives, statins, anticoagulants, immunosuppressants,
#' psychotropics, ...) from the RWE-BRIDGE drug-proxy codelist. Every other drug concept defaults
#' to a single short `course`. Edit `concepts.csv` afterwards to change any of them.
#'
#' @return A character vector of `DP_*` concept ids.
#' @examples
#' default_recurring_drugs()
#' @export
default_recurring_drugs <- function() {
  paste0("DP_", c(
    "ACE", "ANTIARRHYT", "ASPIRIN", "CCB", "CONTRHYPERT", "OTHERANTIHYPERT", "STAT", "NOAC",
    "WAR", "COVCARDIOCEREBROVAS", "COVDIAB", "COVCKD", "COVRESPCHRONIC", "COVMENTHEALTH",
    "COVHIV", "HIVSPECIFIC", "PSYCH", "EPILEPSY", "IMMUNOSUPPR", "IMMUNCHECKPOINT",
    "CANCERHORMONE", "COVCANCER"
  ))
}

#' Default blocks: baseline history, chronic therapy, follow-up outcomes
#'
#' Each block becomes one module that waits for its `anchor` attribute, then draws `k` events
#' (`k` is a named share vector: names are counts, values their probabilities). The draws are
#' spread over `window` (measured from the anchor).
#'
#' - `baseline`: prior history, anchored on cohort entry, finishes before exposure.
#' - `chronic`: at most one recurring therapy per module copy, started early in the baseline
#'   window and repeated through the exposure date; `parallel = 2` runs two independent copies,
#'   so a patient can be on up to two long-term therapies.
#' - `follow_up`: outcomes, anchored on the exposure attribute.
#'
#' @param cohort_attribute The attribute the exposure module sets at cohort entry.
#' @param exposure_attribute The attribute the exposure module tags the arm as.
#' @return A named list of blocks.
#' @examples
#' default_event_blocks()
#' @export
default_event_blocks <- function(
  cohort_attribute = "cohort_entry",
  exposure_attribute = "exposure_group"
) {
  list(
    baseline = list(
      anchor = cohort_attribute,
      # 200 + 5 draws x 30-day acute tail = 350 days, inside a 365-day minimum exposure delay.
      window = list(low = 0, high = 200, unit = "days"),
      k = list(`0` = 0.4, `1` = 0.25, `2` = 0.15, `3` = 0.1, `4` = 0.05, `5` = 0.05)
    ),
    chronic = list(
      anchor = cohort_attribute,
      window = list(low = 0, high = 120, unit = "days"),
      k = list(`0` = 0.7, `1` = 0.3),
      parallel = 2
    ),
    follow_up = list(
      anchor = exposure_attribute,
      window = list(low = 0, high = 1460, unit = "days"),
      k = list(`0` = 0.7, `1` = 0.2, `2` = 0.1)
    )
  )
}

#' Default timing for each event pattern
#'
#' - `acute` conditions resolve (`ConditionEnd`) after `resolves_after`; `chronic` ones never do.
#' - `course` drugs are one prescription ended (`MedicationEnd`) after `course`.
#' - `recurring` drugs repeat `course` + `gap` for `cycles` prescriptions.
#' - `once` (vaccines) has no parameters.
#'
#' Ending each record matters: Synthea won't start a new record for a code that's still active.
#'
#' @return A named list of patterns.
#' @examples
#' default_event_patterns()
#' @export
default_event_patterns <- function() {
  list(
    acute = list(resolves_after = list(low = 7, high = 30, unit = "days")),
    course = list(course = list(low = 5, high = 14, unit = "days")),
    recurring = list(
      course = list(low = 28, high = 28, unit = "days"),
      gap = list(low = 0, high = 28, unit = "days"),
      cycles = 24
    )
  )
}

# Stage 2: build the spec ----

#' Assemble an events (spec_version 2) spec from concept codes and study parameters
#'
#' Turns `read_bridge_concepts()`'s long codes table into the three parts of an events spec:
#' `spec` (the YAML part: timeline, exposure, blocks, patterns), `concepts` (one row per
#' (concept, block), assigned by `roles`) and `codes` (one row per (concept, code)). Like
#' `build_module_spec()`, every parameter is caller-supplied -- nothing is read from a study's
#' pipeline-computed-logic files.
#'
#' @param name Name of the exposure module; block modules are named `"<name> - <block>"`.
#' @param codes A `data.frame` with `concept_id`, `source_type`, `system`, `code`, `display`
#'   (and optionally `source`, `tag`) -- typically several `read_bridge_concepts()` results
#'   `rbind()`-ed together.
#' @param exposure_codes,exposure_shares,exposure_state,exposure_attribute As in
#'   `build_module_spec()`.
#' @param exposure_delay `list(low, high, unit)`: time from cohort entry to exposure. Its random
#'   spread is what spreads exposure dates, and its `low` bounds how long the baseline window
#'   can be.
#' @param cohort_criteria Optional `create_population()` arguments (`age`, `gender`, `race`,
#'   `socioeconomic`, `date`) gating cohort entry.
#' @param cohort_attribute Attribute set at cohort entry (the baseline/chronic blocks' anchor).
#' @param blocks Named list of blocks, see `default_event_blocks()`.
#' @param patterns Named list of patterns, see `default_event_patterns()`.
#' @param roles Per-`source_type` assignment, see `default_event_roles()`. Every `source_type`
#'   in `codes` must appear here.
#' @param recurring_drugs Medication concept ids given the `recurring` pattern, in
#'   `recurring_block`.
#' @param recurring_block Block the `recurring_drugs` go in.
#' @param skipped Concept ids that had no codes (e.g. `read_bridge_concepts()`'s `"skipped"`
#'   attribute), recorded in `provenance$skipped_concepts` for the audit trail.
#' @param provenance Optional free-form list carried into the spec for audit only.
#' @return A list with `spec`, `concepts`, `codes`. Pass it to `write_event_spec()` or straight to
#'   `build_modules_from_events()`.
#' @examples
#' \dontrun{
#' codes <- rbind(
#'   read_bridge_concepts("full_codelist.csv", coding_system = "MEDCODEID"),
#'   read_bridge_concepts("drug_proxies.csv",
#'     coding_system = "PRODCODEID", concept_col = "drug_abbreviation",
#'     source_type_col = "system", fallback_concept = NULL, exclude_concepts = "VP_RSV",
#'     columns = bridge_columns(coding_system = "product_identifier", display = "product_name")
#'   )
#' )
#' x <- build_event_spec("vaccine_safety", codes,
#'   exposure_codes = exposure_codes,
#'   exposure_shares = c(vaccine_a = 0.5, comparator = 0.5),
#'   exposure_delay = list(low = 365, high = 545, unit = "days"),
#'   cohort_criteria = list(age = list(operator = ">=", quantity = 60, unit = "years"))
#' )
#' write_event_spec(x, "spec/")
#' }
#' @export
build_event_spec <- function(
  name,
  codes,
  exposure_codes,
  exposure_shares,
  exposure_delay,
  cohort_criteria = NULL,
  cohort_attribute = "cohort_entry",
  exposure_state = c("vaccine", "medication", "condition"),
  exposure_attribute = "exposure_group",
  blocks = default_event_blocks(cohort_attribute, exposure_attribute),
  patterns = default_event_patterns(),
  roles = default_event_roles(),
  recurring_drugs = default_recurring_drugs(),
  recurring_block = "chronic",
  skipped = NULL,
  provenance = NULL
) {
  exposure_state <- match.arg(exposure_state)
  where <- "build_event_spec()"
  .required_cols(codes, c("concept_id", "source_type", "system", "code", "display"), "codes")
  .required_cols(roles, c("source_type", "block", "state_type", "pattern"), "roles")

  exposure <- .exposure_spec(
    exposure_codes,
    exposure_shares,
    exposure_state,
    exposure_attribute,
    where
  )

  concept_ids <- unique(codes$concept_id)
  first <- match(concept_ids, codes$concept_id)
  source_type <- toupper(codes$source_type[first])
  unknown_types <- setdiff(unique(source_type), toupper(roles$source_type))
  if (length(unknown_types) > 0) {
    stop(sprintf(
      "%s: no `roles` entry for source_type(s): %s",
      where,
      paste(unknown_types, collapse = ", ")
    ))
  }

  n_codes <- as.integer(table(factor(codes$concept_id, levels = concept_ids)))
  concept_rows <- lapply(seq_along(concept_ids), function(i) {
    r <- roles[toupper(roles$source_type) == source_type[i], , drop = FALSE]
    recurring <- r$state_type == "medication" & concept_ids[i] %in% recurring_drugs
    r$block[recurring] <- recurring_block
    r$pattern[recurring] <- "recurring"
    data.frame(
      concept_id = concept_ids[i],
      source = if ("source" %in% names(codes)) codes$source[first[i]] else NA_character_,
      source_type = source_type[i],
      state_type = r$state_type,
      pattern = r$pattern,
      block = r$block,
      weight = NA_real_,
      enabled = TRUE,
      n_codes = n_codes[i],
      note = "",
      stringsAsFactors = FALSE
    )
  })
  concepts <- do.call(rbind, concept_rows)
  rownames(concepts) <- NULL

  codes_out <- data.frame(
    concept_id = codes$concept_id,
    system = codes$system,
    code = codes$code,
    display = codes$display,
    tag = if ("tag" %in% names(codes)) codes$tag else "",
    weight = NA_real_,
    stringsAsFactors = FALSE
  )

  timeline <- list(
    cohort_criteria = cohort_criteria,
    cohort_attribute = cohort_attribute,
    exposure_delay = exposure_delay
  )
  if (length(skipped) > 0) {
    provenance <- c(provenance, list(skipped_concepts = as.list(sort(skipped))))
  }

  x <- list(
    spec = list(
      spec_version = 2L,
      name = name,
      timeline = timeline[!vapply(timeline, is.null, logical(1))],
      exposure = exposure,
      blocks = blocks,
      patterns = patterns,
      provenance = provenance
    ),
    concepts = concepts,
    codes = codes_out
  )
  .check_event_spec(x, where)
}

# Stage 3: YAML + CSV I/O ----

#' Write an events spec: a YAML plus two CSVs
#'
#' Writes `<stem>.yaml` (timeline, exposure, blocks, patterns, provenance),
#' `<stem>_concepts.csv` and `<stem>_codes.csv` into `dir`, all UTF-8 regardless of the session
#' locale. The YAML records the two CSV file names, so `read_event_spec()` only needs its path.
#' A blank `weight` in either CSV means 1; a blank `enabled` means `TRUE`.
#'
#' @param x `build_event_spec()`'s or `read_event_spec()`'s output.
#' @param dir Output directory (created if needed).
#' @param stem File-name stem. Defaults to the spec name.
#' @return The YAML path, invisibly.
#' @export
write_event_spec <- function(x, dir, stem = x$spec$name) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  concepts_file <- paste0(stem, "_concepts.csv")
  codes_file <- paste0(stem, "_codes.csv")
  spec <- x$spec
  spec$concepts_file <- concepts_file
  spec$codes_file <- codes_file

  .write_csv_utf8(x$concepts, file.path(dir, concepts_file))
  .write_csv_utf8(x$codes, file.path(dir, codes_file))
  yaml_path <- file.path(dir, paste0(stem, ".yaml"))
  .write_yaml_utf8(spec, yaml_path)
  invisible(yaml_path)
}

#' Read an events spec written by `write_event_spec()` (or hand-edited)
#'
#' Reads the YAML and the two CSVs it names (relative to the YAML's folder), fills blank
#' `weight`/`enabled` cells with their defaults, and runs the same checks as
#' `build_event_spec()`: unknown keys/columns, pattern/state-type compatibility, block `k` shares
#' summing to 1, every enabled concept having codes, and the timing guarantees (baseline records
#' before exposure, recurring therapy spanning it).
#'
#' @param path Path to the YAML file.
#' @return A list with `spec`, `concepts`, `codes`.
#' @export
read_event_spec <- function(path) {
  spec <- yaml::read_yaml(path, fileEncoding = "UTF-8")
  for (key in c("concepts_file", "codes_file")) {
    if (is.null(spec[[key]])) {
      stop(sprintf("%s: missing `%s`", path, key))
    }
  }
  base <- dirname(path)
  x <- list(
    spec = spec,
    concepts = .read_csv_utf8(file.path(base, spec$concepts_file)),
    codes = .read_csv_utf8(file.path(base, spec$codes_file))
  )
  .check_event_spec(x, path)
}

#' Write a data.frame as UTF-8 CSV regardless of the session locale
#'
#' `utils::write.csv()` re-encodes through the native encoding even with `fileEncoding`, which
#' under a C locale writes e.g. "é" as "<c3><a9>". Every field is quoted; `NA` is written blank.
#' @noRd
.write_csv_utf8 <- function(df, path) {
  quote_col <- function(v) {
    v <- enc2utf8(ifelse(is.na(v), "", as.character(v)))
    paste0("\"", gsub("\"", "\"\"", v, fixed = TRUE, useBytes = TRUE), "\"")
  }
  lines <- paste(quote_col(names(df)), collapse = ",")
  if (nrow(df) > 0) {
    lines <- c(lines, do.call(paste, c(lapply(df, quote_col), sep = ",")))
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(lines, con = path, useBytes = TRUE)
}

#' Read a CSV written by `.write_csv_utf8()`: every column character, UTF-8, blanks kept as ""
#' @noRd
.read_csv_utf8 <- function(path) {
  if (!file.exists(path)) {
    stop(sprintf("file not found: %s", path))
  }
  utils::read.csv(
    path,
    colClasses = "character",
    stringsAsFactors = FALSE,
    check.names = FALSE,
    encoding = "UTF-8",
    na.strings = character(0)
  )
}

# Checks ----

#' Validate and normalise an events spec (`list(spec, concepts, codes)`)
#'
#' Coerces CSV-read character columns (`weight`, `enabled`, `n_codes`) to their types, fills blank
#' `weight` with 1 and blank `enabled` with `TRUE`, and stops on anything that would make
#' `build_modules_from_events()` fail or produce a mistimed module.
#' @return `x`, normalised.
#' @noRd
.check_event_spec <- function(x, where) {
  spec <- x$spec
  .check_keys(names(spec), .event_spec_keys, where, "top-level key")
  if (!identical(as.integer(spec$spec_version), 2L)) {
    stop(sprintf("%s: spec_version must be 2, got %s", where, format(spec$spec_version)))
  }

  timeline <- spec$timeline
  .check_keys(
    names(timeline),
    list(
      required = c("cohort_attribute", "exposure_delay"),
      optional = "cohort_criteria"
    ),
    where,
    "timeline key"
  )
  exposure_delay <- .check_range(timeline$exposure_delay, sprintf("%s: timeline$exposure_delay", where))
  exposure_attribute <- .check_exposure_attribute(spec$exposure$attribute, where)
  if (!spec$exposure$state_type %in% c("vaccine", "medication", "condition")) {
    stop(sprintf("%s: unknown exposure$state_type '%s'", where, spec$exposure$state_type))
  }

  patterns <- spec$patterns
  .check_keys(
    names(patterns),
    list(required = character(0), optional = c("acute", "course", "recurring")),
    where,
    "pattern"
  )
  pattern_fields <- list(
    acute = "resolves_after",
    course = "course",
    recurring = c("course", "gap", "cycles")
  )
  for (p in names(patterns)) {
    .check_keys(
      names(patterns[[p]]),
      list(required = pattern_fields[[p]], optional = character(0)),
      where,
      sprintf("patterns$%s field", p)
    )
  }

  concepts <- .normalise_table(x$concepts, .event_concept_cols, "concepts", where)
  codes <- .normalise_table(x$codes, .event_code_cols, "codes", where)
  concepts$enabled <- if ("enabled" %in% names(concepts)) {
    v <- toupper(trimws(as.character(concepts$enabled)))
    !v %in% c("FALSE", "F", "NO", "0")
  } else {
    rep(TRUE, nrow(concepts))
  }

  bad_state <- !concepts$state_type %in% names(.event_state_patterns)
  if (any(bad_state)) {
    stop(sprintf(
      "%s: unknown concepts$state_type: %s",
      where,
      paste(unique(concepts$state_type[bad_state]), collapse = ", ")
    ))
  }
  ok_pattern <- mapply(
    function(st, p) p %in% .event_state_patterns[[st]],
    concepts$state_type,
    concepts$pattern
  )
  if (!all(ok_pattern)) {
    i <- which(!ok_pattern)[1]
    stop(sprintf(
      "%s: concept '%s' has pattern '%s', which a %s can't have (allowed: %s)",
      where,
      concepts$concept_id[i],
      concepts$pattern[i],
      concepts$state_type[i],
      paste(.event_state_patterns[[concepts$state_type[i]]], collapse = ", ")
    ))
  }
  used_patterns <- setdiff(unique(concepts$pattern[concepts$enabled]), c("chronic", "once"))
  missing_patterns <- setdiff(used_patterns, names(patterns))
  if (length(missing_patterns) > 0) {
    stop(sprintf(
      "%s: concepts use pattern(s) with no `patterns` entry: %s",
      where,
      paste(missing_patterns, collapse = ", ")
    ))
  }
  unknown_blocks <- setdiff(unique(concepts$block), names(spec$blocks))
  if (length(unknown_blocks) > 0) {
    stop(sprintf(
      "%s: concepts refer to block(s) not defined in `blocks`: %s",
      where,
      paste(unknown_blocks, collapse = ", ")
    ))
  }
  dup <- duplicated(concepts[c("concept_id", "block")])
  if (any(dup)) {
    stop(sprintf(
      "%s: concept '%s' appears more than once in block '%s'",
      where,
      concepts$concept_id[dup][1],
      concepts$block[dup][1]
    ))
  }
  no_codes <- setdiff(concepts$concept_id[concepts$enabled], codes$concept_id)
  if (length(no_codes) > 0) {
    stop(sprintf(
      "%s: enabled concept(s) with no codes: %s",
      where,
      paste(no_codes, collapse = ", ")
    ))
  }

  pattern_days <- .pattern_days(patterns)
  for (b in names(spec$blocks)) {
    block <- spec$blocks[[b]]
    label <- sprintf("%s: blocks$%s", where, b)
    .check_keys(
      names(block),
      list(required = c("anchor", "window", "k"), optional = "parallel"),
      where,
      sprintf("blocks$%s field", b)
    )
    if (!block$anchor %in% c(timeline$cohort_attribute, exposure_attribute)) {
      stop(sprintf(
        "%s: anchor '%s' must be the cohort attribute ('%s') or the exposure attribute ('%s')",
        label,
        block$anchor,
        timeline$cohort_attribute,
        exposure_attribute
      ))
    }
    window <- .check_range(block$window, sprintf("%s$window", label))
    k <- .check_k(block$k, label)
    if (!is.null(block$parallel) &&
      (!is.numeric(block$parallel) || block$parallel < 1 || block$parallel %% 1 != 0)) {
      stop(sprintf("%s: parallel must be a whole number >= 1", label))
    }

    in_block <- concepts$enabled & concepts$block == b
    block_patterns <- unique(concepts$pattern[in_block])
    if (!identical(block$anchor, timeline$cohort_attribute) || !any(in_block)) {
      next
    }
    if ("recurring" %in% block_patterns) {
      if (max(k$counts) > 1) {
        stop(sprintf(
          paste0(
            "%s: a block with recurring concepts can draw at most 1 per patient (draws in one ",
            "module run one after another, so a second therapy would only start after the first ",
            "ends) -- use `parallel` for more"
          ),
          label
        ))
      }
      if (window$high >= exposure_delay$low) {
        stop(sprintf(
          "%s: recurring therapy must start before exposure: window high (%s days) must be below exposure_delay low (%s days)",
          label,
          window$high,
          exposure_delay$low
        ))
      }
      cycle_min <- pattern_days$recurring_cycle_min * patterns$recurring$cycles
      if (window$low + cycle_min < exposure_delay$high) {
        stop(sprintf(
          paste0(
            "%s: recurring therapy can end before exposure: window low + cycles x (course + gap) ",
            "low = %s days, but exposure can be as late as %s days -- raise patterns$recurring$cycles"
          ),
          label,
          window$low + cycle_min,
          exposure_delay$high
        ))
      }
    } else {
      tail <- max(c(0, unlist(pattern_days$tail_max[intersect(block_patterns, names(pattern_days$tail_max))])))
      latest <- window$high + max(k$counts) * tail
      if (latest > exposure_delay$low) {
        stop(sprintf(
          paste0(
            "%s: baseline records could land after exposure: window high + %d x longest ",
            "tail (%s days) = %s days, but exposure can be as early as %s days"
          ),
          label,
          max(k$counts),
          tail,
          latest,
          exposure_delay$low
        ))
      }
    }
  }

  x$concepts <- concepts
  x$codes <- codes
  x
}

#' Stop on unknown or missing names
#' @noRd
.check_keys <- function(given, allowed, where, what) {
  unknown <- setdiff(given, c(allowed$required, allowed$optional))
  if (length(unknown) > 0) {
    stop(sprintf(
      "%s: unknown %s(s): %s (allowed: %s)",
      where,
      what,
      paste(unknown, collapse = ", "),
      paste(c(allowed$required, allowed$optional), collapse = ", ")
    ))
  }
  missing <- setdiff(allowed$required, given)
  if (length(missing) > 0) {
    stop(sprintf("%s: missing %s(s): %s", where, what, paste(missing, collapse = ", ")))
  }
}

#' Check a `list(low, high, unit)` and return it converted to days
#' @noRd
.check_range <- function(r, label) {
  if (!is.list(r) || is.null(r$low) || is.null(r$high) || is.null(r$unit)) {
    stop(sprintf("%s must be list(low, high, unit)", label))
  }
  if (!r$unit %in% names(.event_unit_days)) {
    stop(sprintf(
      "%s: unit must be one of %s",
      label,
      paste(names(.event_unit_days), collapse = ", ")
    ))
  }
  if (!is.numeric(r$low) || !is.numeric(r$high) || r$low < 0 || r$low > r$high) {
    stop(sprintf("%s: need 0 <= low <= high", label))
  }
  list(low = r$low * .event_unit_days[[r$unit]], high = r$high * .event_unit_days[[r$unit]])
}

#' Check a block's `k` (names = event counts, values = shares summing to 1)
#' @return `list(counts, shares)`.
#' @noRd
.check_k <- function(k, label) {
  counts <- suppressWarnings(as.integer(names(k)))
  shares <- suppressWarnings(as.numeric(unlist(k)))
  if (length(k) == 0 || anyNA(counts) || any(counts < 0) || anyDuplicated(counts) ||
    length(shares) != length(counts)) {
    stop(sprintf("%s: k must be named by distinct whole numbers >= 0, e.g. list(`0` = 0.5, `1` = 0.5)", label))
  }
  .check_shares(shares, sprintf("%s: k", label))
  if (abs(sum(shares) - 1) > 1e-6) {
    stop(sprintf("%s: k shares must sum to 1, got %s", label, sum(shares)))
  }
  list(counts = counts, shares = shares)
}

#' The pattern durations the timing checks need, in days
#' @noRd
.pattern_days <- function(patterns) {
  out <- list(tail_max = list(), recurring_cycle_min = NA_real_)
  if (!is.null(patterns$acute)) {
    out$tail_max$acute <- .check_range(patterns$acute$resolves_after, "patterns$acute$resolves_after")$high
  }
  if (!is.null(patterns$course)) {
    out$tail_max$course <- .check_range(patterns$course$course, "patterns$course$course")$high
  }
  if (!is.null(patterns$recurring)) {
    rc <- patterns$recurring
    course <- .check_range(rc$course, "patterns$recurring$course")
    gap <- .check_range(rc$gap, "patterns$recurring$gap")
    if (!is.numeric(rc$cycles) || rc$cycles < 1 || rc$cycles %% 1 != 0) {
      stop("patterns$recurring$cycles must be a whole number >= 1")
    }
    out$recurring_cycle_min <- course$low + gap$low
  }
  out
}

#' Check a concepts/codes table's columns and coerce its typed columns
#' @noRd
.normalise_table <- function(df, cols, df_name, where) {
  if (!is.data.frame(df)) {
    stop(sprintf("%s: `%s` must be a data.frame", where, df_name))
  }
  .check_keys(names(df), cols, where, sprintf("%s column", df_name))
  for (nm in c("concept_id", "state_type", "pattern", "block", "system", "code")) {
    if (nm %in% names(df)) {
      df[[nm]] <- trimws(as.character(df[[nm]]))
    }
  }
  if ("weight" %in% names(df)) {
    w <- df$weight
    w <- if (is.numeric(w)) w else suppressWarnings(as.numeric(ifelse(trimws(w) == "", NA, w)))
    if (any(!is.na(w) & (w < 0 | !is.finite(w)))) {
      stop(sprintf("%s: %s$weight must be a finite number >= 0 (blank = 1)", where, df_name))
    }
    df$weight <- ifelse(is.na(w), 1, w)
  } else {
    df$weight <- rep(1, nrow(df))
  }
  if ("n_codes" %in% names(df)) {
    df$n_codes <- suppressWarnings(as.integer(df$n_codes))
  }
  df
}

# Stage 4: build the modules ----

#' Build Synthea modules from an events spec
#'
#' Purely mechanical. Produces:
#'
#' - **The exposure module** (named after the spec): an optional cohort-entry Guard from
#'   `timeline$cohort_criteria`, a `SetAttribute` of `timeline$cohort_attribute`, a Delay of
#'   `timeline$exposure_delay` (which spreads exposure dates), then the exposure arms (as in
#'   `build_module_from_spec()`), tagged as `exposure$attribute`.
#' - **One module per block** (`"<name> - <block>"`, or `"<name> - <block> <i>"` for each of a
#'   block's `parallel` copies), each a K-draw loop: wait for the block's anchor attribute, pick
#'   K from `k`, then K times: a delay (the block window split into `max(k)` slots), a concept
#'   picked by `weight`, one of its codes picked by `weight`, and the concept's pattern tail
#'   (`ConditionEnd`/`MedicationEnd` after `acute`/`course`; a repeat-prescription cycle for
#'   `recurring`).
#'
#' Records are made with no target encounter, so Synthea records them at the exact simulated time
#' (attached to the person's latest encounter) rather than waiting for a wellness visit.
#'
#' @param x A path to an events YAML, or `build_event_spec()`/`read_event_spec()`'s output.
#' @param as_json If TRUE (default), each module is a JSON string; if FALSE, an R list.
#' @param validate Passed to `build_module()`/`build_cohort_module()`.
#' @param max_codes_per_concept Optional cap: keep a random sample of at most this many codes
#'   per concept (reproducible via `seed`), for quick runs. `NULL` (default): every code.
#' @param seed Seed for `max_codes_per_concept` sampling; the session's RNG state is restored.
#' @return A named list of modules, the exposure module first.
#' @examples
#' \dontrun{
#' modules <- build_modules_from_events("spec/vaccine_safety.yaml")
#' write_modules(modules, "modules/vaccine_safety")
#' }
#' @export
build_modules_from_events <- function(
  x,
  as_json = TRUE,
  validate = TRUE,
  max_codes_per_concept = NULL,
  seed = 1L
) {
  where <- "build_modules_from_events()"
  x <- if (is.character(x) && length(x) == 1) read_event_spec(x) else .check_event_spec(x, where)
  spec <- x$spec
  codes <- x$codes
  if (!is.null(max_codes_per_concept)) {
    codes <- .sample_codes(codes, max_codes_per_concept, seed)
  }

  modules <- list()
  modules[[spec$name]] <- build_cohort_module(
    spec$name,
    .event_exposure_fragment(spec, where),
    as_json = as_json,
    validate = validate
  )

  for (b in names(spec$blocks)) {
    concepts <- x$concepts[x$concepts$enabled & x$concepts$block == b, , drop = FALSE]
    if (nrow(concepts) == 0) {
      next
    }
    block <- spec$blocks[[b]]
    n_copies <- if (is.null(block$parallel)) 1 else block$parallel
    for (i in seq_len(n_copies)) {
      suffix <- if (n_copies > 1) paste0(" ", i) else ""
      module_name <- paste0(spec$name, " - ", b, suffix)
      modules[[module_name]] <- build_module(
        module_name,
        .draw_module_states(
          block,
          concepts,
          codes,
          spec$patterns,
          attribute_prefix = .attribute_slug(paste0(spec$name, "_", b, suffix))
        ),
        as_json = as_json,
        validate = validate
      )
    }
  }
  modules
}

#' The exposure module's fragment: cohort Guard -> cohort tag -> exposure delay -> arms
#' @noRd
.event_exposure_fragment <- function(spec, where) {
  timeline <- spec$timeline
  cohort_guard <- if (!is.null(timeline$cohort_criteria)) {
    do.call(create_population, c(list(label = "Cohort Criteria"), timeline$cohort_criteria))
  } else {
    NULL
  }
  chain(
    cohort_guard,
    create_tag("Cohort Entry", attribute = timeline$cohort_attribute, value = TRUE),
    create_delay(
      timeline$exposure_delay$low,
      timeline$exposure_delay$high,
      timeline$exposure_delay$unit,
      label = "Exposure Delay"
    ),
    .exposure_fragment(spec$exposure, where)
  )
}

#' Turn a module name into an attribute-safe slug
#' @noRd
.attribute_slug <- function(x) {
  gsub("^_+|_+$", "", gsub("[^A-Za-z0-9]+", "_", tolower(x)))
}

#' Keep at most `n` codes per concept, sampled reproducibly without touching the session RNG
#' @noRd
.sample_codes <- function(codes, n, seed) {
  if (!is.numeric(n) || length(n) != 1 || n < 1) {
    stop("max_codes_per_concept must be a number >= 1")
  }
  had_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  if (had_seed) {
    old_seed <- get(".Random.seed", envir = globalenv())
    on.exit(assign(".Random.seed", old_seed, envir = globalenv()), add = TRUE)
  } else {
    on.exit(rm(".Random.seed", envir = globalenv()), add = TRUE)
  }
  set.seed(seed)
  keep <- unlist(lapply(split(seq_len(nrow(codes)), codes$concept_id), function(idx) {
    if (length(idx) <= n) idx else sort(sample(idx, n))
  }))
  codes[sort(keep), , drop = FALSE]
}

#' Normalise weights into transition probabilities that sum to 1
#' @noRd
.shares <- function(w) {
  if (sum(w) <= 0) {
    stop("weights must not all be 0")
  }
  w / sum(w)
}

#' A `distributed` transition over `targets`, or a `direct` one when there's only one target
#' @noRd
.pick_transition <- function(targets, shares) {
  keep <- shares > 0
  targets <- targets[keep]
  shares <- shares[keep]
  if (length(targets) == 1) {
    return(create_transition_settings("direct", to = targets))
  }
  create_transition_settings(
    "distributed",
    options = lapply(seq_along(targets), function(i) {
      list(transition = targets[i], distribution = shares[i])
    })
  )
}

#' A Delay state over a `list(low, high, unit)`
#' @noRd
.delay_state <- function(r, to) {
  create_state_settings(
    "Delay",
    range = create_component_settings("range", low = r$low, high = r$high, unit = r$unit),
    transition = create_transition_settings("direct", to = to)
  )
}

#' Build a block module's named state list (the K-draw loop)
#'
#' Layout:
#' `Initial -> Wait For Anchor -> [Window Start] -> K Choice -> Set K <k> -> Check`;
#' `Check -> (remaining > 0) Step Delay -> Concept Pick -> <concept> [Start] -> <concept> Code Pick
#' -> <concept> Code <i> -> <pattern tail> -> Decrement -> Check`, `Check -> (else) Terminal`.
#' Shared tails: `Acute Resolve Delay -> Acute End`, `Course Delay -> Course End`; `chronic` and
#' `once` leaves go straight to `Decrement`. Each recurring concept has its own tail, looping back
#' to its own Code Pick until its cycles run out.
#' @noRd
.draw_module_states <- function(block, concepts, codes, patterns, attribute_prefix) {
  remaining_attr <- paste0(attribute_prefix, "_remaining")
  current_attr <- paste0(attribute_prefix, "_current")
  cycles_attr <- paste0(attribute_prefix, "_cycles")
  k <- .check_k(block$k, "block")
  k_max <- max(k$counts)
  direct <- function(to) create_transition_settings("direct", to = to)

  states <- list()
  states[["Initial"]] <- create_state_settings("Initial", transition = direct("Wait For Anchor"))
  after_wait <- if (block$window$low > 0) "Window Start" else "K Choice"
  states[["Wait For Anchor"]] <- create_state_settings(
    "Guard",
    allow = create_logic_settings("Attribute", attribute = block$anchor, operator = "is not nil"),
    transition = direct(after_wait)
  )
  if (block$window$low > 0) {
    states[["Window Start"]] <- .delay_state(
      list(low = block$window$low, high = block$window$low, unit = block$window$unit),
      "K Choice"
    )
  }

  k_targets <- ifelse(k$counts == 0, "Terminal", paste0("Set K ", k$counts))
  states[["K Choice"]] <- create_state_settings(
    "Simple",
    transition = .pick_transition(k_targets, k$shares)
  )
  for (n in setdiff(k$counts, 0)) {
    states[[paste0("Set K ", n)]] <- create_state_settings(
      "SetAttribute",
      attribute = remaining_attr,
      value = n,
      transition = direct("Check")
    )
  }

  slot <- if (k_max > 0) (block$window$high - block$window$low) / k_max else 0
  after_check <- if (slot > 0) "Step Delay" else "Concept Pick"
  states[["Check"]] <- create_state_settings(
    "Simple",
    transition = create_transition_settings(
      "conditional",
      options = list(
        list(
          condition = create_logic_settings(
            "Attribute",
            attribute = remaining_attr,
            operator = ">",
            value = 0
          ),
          transition = after_check
        ),
        list(transition = "Terminal")
      )
    )
  )
  if (slot > 0) {
    states[["Step Delay"]] <- .delay_state(
      list(low = 0, high = slot, unit = block$window$unit),
      "Concept Pick"
    )
  }

  concept_entry <- ifelse(
    concepts$pattern == "recurring",
    paste0(concepts$concept_id, " Start"),
    paste0(concepts$concept_id, " Code Pick")
  )
  states[["Concept Pick"]] <- create_state_settings(
    "Simple",
    transition = .pick_transition(concept_entry, .shares(concepts$weight))
  )

  tail_entry <- c(
    chronic = "Decrement",
    once = "Decrement",
    acute = "Acute Resolve Delay",
    course = "Course Delay"
  )
  concept_states <- lapply(seq_len(nrow(concepts)), function(ci) {
    cid <- concepts$concept_id[ci]
    pattern <- concepts$pattern[ci]
    cc <- codes[codes$concept_id == cid, , drop = FALSE]
    leaf_names <- paste0(cid, " Code ", seq_len(nrow(cc)))
    after_leaf <- if (pattern == "recurring") paste0(cid, " Course") else tail_entry[[pattern]]

    out <- list()
    if (pattern == "recurring") {
      out[[paste0(cid, " Start")]] <- create_state_settings(
        "SetAttribute",
        attribute = cycles_attr,
        value = patterns$recurring$cycles,
        transition = direct(paste0(cid, " Code Pick"))
      )
    }
    out[[paste0(cid, " Code Pick")]] <- create_state_settings(
      "Simple",
      transition = .pick_transition(leaf_names, .shares(cc$weight))
    )
    leaves <- lapply(seq_len(nrow(cc)), function(i) {
      code <- list(create_component_settings(
        "code",
        system = cc$system[i],
        code = cc$code[i],
        display = cc$display[i]
      ))
      switch(
        concepts$state_type[ci],
        condition = create_state_settings(
          "ConditionOnset",
          codes = code,
          assign_to_attribute = current_attr,
          transition = direct(after_leaf)
        ),
        medication = create_state_settings(
          "MedicationOrder",
          codes = code,
          chronic = identical(pattern, "recurring"),
          assign_to_attribute = current_attr,
          transition = direct(after_leaf)
        ),
        vaccine = create_state_settings(
          "Vaccine",
          codes = code,
          series = 1,
          transition = direct(after_leaf)
        )
      )
    })
    out <- c(out, .named_list(leaves, leaf_names))

    if (pattern == "recurring") {
      rc <- patterns$recurring
      out[[paste0(cid, " Course")]] <- .delay_state(rc$course, paste0(cid, " Course End"))
      out[[paste0(cid, " Course End")]] <- create_state_settings(
        "MedicationEnd",
        referenced_by_attribute = current_attr,
        transition = direct(paste0(cid, " Cycle Count"))
      )
      out[[paste0(cid, " Cycle Count")]] <- create_state_settings(
        "Counter",
        attribute = cycles_attr,
        action = "decrement",
        transition = direct(paste0(cid, " Cycle Check"))
      )
      out[[paste0(cid, " Cycle Check")]] <- create_state_settings(
        "Simple",
        transition = create_transition_settings(
          "conditional",
          options = list(
            list(
              condition = create_logic_settings(
                "Attribute",
                attribute = cycles_attr,
                operator = ">",
                value = 0
              ),
              transition = paste0(cid, " Gap")
            ),
            list(transition = "Decrement")
          )
        )
      )
      out[[paste0(cid, " Gap")]] <- .delay_state(rc$gap, paste0(cid, " Code Pick"))
    }
    out
  })
  states <- c(states, unlist(concept_states, recursive = FALSE))

  used <- unique(concepts$pattern)
  if ("acute" %in% used) {
    states[["Acute Resolve Delay"]] <- .delay_state(patterns$acute$resolves_after, "Acute End")
    states[["Acute End"]] <- create_state_settings(
      "ConditionEnd",
      referenced_by_attribute = current_attr,
      transition = direct("Decrement")
    )
  }
  if ("course" %in% used) {
    states[["Course Delay"]] <- .delay_state(patterns$course$course, "Course End")
    states[["Course End"]] <- create_state_settings(
      "MedicationEnd",
      referenced_by_attribute = current_attr,
      transition = direct("Decrement")
    )
  }
  states[["Decrement"]] <- create_state_settings(
    "Counter",
    attribute = remaining_attr,
    action = "decrement",
    transition = direct("Check")
  )
  states[["Terminal"]] <- create_state_settings("Terminal")

  dup <- unique(names(states)[duplicated(names(states))])
  if (length(dup) > 0) {
    stop(sprintf("duplicate state name(s): %s", paste(head(dup, 5), collapse = ", ")))
  }
  states
}

# Output + review ----

#' Write a list of modules as flat JSON files, ready for Synthea's `-d`/`-m`
#'
#' Writes each module as `<module name, non-alphanumerics -> "_">.json` directly in `dir` -- never
#' in a subfolder, because Synthea loads a module directory's subfolders as *submodules*, which
#' never run on their own.
#'
#' @param modules A named list of modules (`build_modules_from_events()`'s output).
#' @param dir Output directory (created if needed).
#' @param clean If TRUE (default), delete existing `.json` files in `dir` first, so a removed
#'   module can't be left behind.
#' @return Invisibly, a list: `files` (the written paths) and `synthea_args` (the `-d`/`-m`
#'   arguments to run exactly these modules: Synthea's `-m` matches file names relative to `-d`,
#'   without `.json`).
#' @export
write_modules <- function(modules, dir, clean = TRUE) {
  if (!is.list(modules) || is.null(names(modules)) || any(!nzchar(names(modules)))) {
    stop("write_modules(): `modules` must be a named list")
  }
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  if (isTRUE(clean)) {
    unlink(list.files(dir, pattern = "\\.json$", full.names = TRUE))
  }
  stems <- .attribute_slug(names(modules))
  if (anyDuplicated(stems)) {
    stop(sprintf(
      "write_modules(): module names collide as file names: %s",
      paste(unique(stems[duplicated(stems)]), collapse = ", ")
    ))
  }
  files <- file.path(dir, paste0(stems, ".json"))
  for (i in seq_along(modules)) {
    write_module_json(modules[[i]], files[i])
  }
  invisible(list(
    files = files,
    synthea_args = c("-d", dir, "-m", paste0(.common_prefix(stems), "*"))
  ))
}

#' Longest common prefix of a character vector
#' @noRd
.common_prefix <- function(x) {
  prefix <- x[1]
  for (s in x[-1]) {
    while (!startsWith(s, prefix)) {
      prefix <- substr(prefix, 1, nchar(prefix) - 1)
    }
  }
  prefix
}

#' Summarise an events spec for review
#'
#' What the modules will contain and roughly what a patient will get: per block, the number of
#' modules, concepts, codes and expected events per patient; per concept, its share of its
#' block's draws; the concepts skipped for having no codes; and which concepts share codes. A
#' shared code is recorded once but counts toward every concept that lists it, so overlapping
#' concepts end up more frequent than their weight alone suggests.
#'
#' @param x A path to an events YAML, or `build_event_spec()`/`read_event_spec()`'s output.
#' @return A list of `data.frame`s: `blocks`, `concepts`, `overlap`; plus `skipped` (character).
#' @export
summarise_events <- function(x) {
  x <- if (is.character(x) && length(x) == 1) read_event_spec(x) else .check_event_spec(x, "summarise_events()")
  spec <- x$spec
  concepts <- x$concepts[x$concepts$enabled, , drop = FALSE]
  n_codes <- table(x$codes$concept_id)

  blocks <- do.call(rbind, lapply(names(spec$blocks), function(b) {
    block <- spec$blocks[[b]]
    k <- .check_k(block$k, b)
    copies <- if (is.null(block$parallel)) 1 else block$parallel
    in_block <- concepts$concept_id[concepts$block == b]
    data.frame(
      block = b,
      anchor = block$anchor,
      modules = if (length(in_block) > 0) copies else 0,
      concepts = length(in_block),
      codes = sum(n_codes[in_block]),
      expected_events = copies * sum(k$counts * k$shares),
      share_with_none = prod(rep(k$shares[k$counts == 0], copies)),
      stringsAsFactors = FALSE
    )
  }))

  concepts$share_of_block <- stats::ave(concepts$weight, concepts$block, FUN = function(w) w / sum(w))
  concepts$expected_per_patient <- concepts$share_of_block *
    blocks$expected_events[match(concepts$block, blocks$block)]
  concepts$n_codes <- as.integer(n_codes[concepts$concept_id])

  enabled_codes <- unique(x$codes[x$codes$concept_id %in% concepts$concept_id, c("concept_id", "code")])
  by_code <- split(enabled_codes$concept_id, enabled_codes$code)
  by_code <- by_code[lengths(by_code) > 1]
  pairs <- do.call(rbind, lapply(by_code, function(ids) {
    p <- utils::combn(sort(ids), 2)
    data.frame(concept_a = p[1, ], concept_b = p[2, ], stringsAsFactors = FALSE)
  }))
  overlap <- if (is.null(pairs)) {
    data.frame(concept_a = character(0), concept_b = character(0), shared_codes = integer(0))
  } else {
    agg <- stats::aggregate(list(shared_codes = rep(1L, nrow(pairs))), pairs, sum)
    agg[order(-agg$shared_codes), , drop = FALSE]
  }
  rownames(overlap) <- NULL

  list(
    blocks = blocks,
    concepts = concepts[c(
      "concept_id", "block", "state_type", "pattern", "n_codes", "share_of_block",
      "expected_per_patient"
    )],
    overlap = overlap,
    skipped = unlist(spec$provenance$skipped_concepts)
  )
}
