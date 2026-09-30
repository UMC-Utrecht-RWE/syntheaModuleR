# spec.R -- stages 2-4 of the bridge -> spec -> module pipeline (stage 1 is read_bridge_codelist(),
# in bridge.R): build_module_spec() ("add inclusion criteria / exposure / outcome parameters"),
# write_module_spec()/read_module_spec() ("produce/read a YAML that is editable"), and
# build_module_from_spec() ("go from YAML to build a module"). Kept in its own file, separate from
# module.R's GMF-grammar recipes (build_disease_module()/build_cohort_module()), since this is a
# distinct concern -- a study-config-shaped intermediate representation and its YAML I/O -- not
# another layer of the JSON grammar. build_module_from_spec() is built entirely on the existing
# Engine/Frontend layers (pathways()/chain()/create_delay()/create_population()/create_vaccine()/
# create_medication()/create_condition()/build_cohort_module()); it introduces no new GMF concept.

#' Assemble an exposure+outcome module spec from resolved codes and study parameters
#'
#' Combines already-resolved exposure/outcome code tables (typically `read_bridge_codelist()`'s
#' output, though any `data.frame` with the right columns works) with the human-decided
#' parameters a codelist alone can never supply -- arm shares, onset probability/timing, optional
#' inclusion criteria -- into a single nested list, the "spec". Deliberately does not read any
#' pipeline-computed-logic file (e.g. a study's own `study_variables.csv`) to fill these in; they
#' are always caller-supplied, every time.
#'
#' @param name Module name.
#' @param exposure_codes A `data.frame` with columns `option` (the arm name, e.g. `"abrysvo"`),
#'   `code`, `display`, and `system` (or `coding_system`, used as a fallback column name so
#'   `read_bridge_codelist()`'s output can be passed straight through).
#' @param exposure_shares A named numeric vector covering every `exposure_codes$option` plus
#'   `"comparator"` (the no-exposure arm's share); must sum to 1.
#' @param outcome_codes A `data.frame` with columns `event_abbreviation` (or `label`), `code`,
#'   `display`, and `system` (or `coding_system`).
#' @param outcome_probability A single number in (0, 1), applied to every outcome, or the name of
#'   a numeric column in `outcome_codes` to use per row instead.
#' @param outcome_delay A `list(low, high, unit)` applied uniformly to every outcome's arbitrary
#'   placement window. A specific item's delay can still be hand-overridden after writing the
#'   YAML -- see the `outcomes.items[[i]].delay` shape `build_module_from_spec()` reads.
#' @param inclusion_criteria Optional `list(age = list(operator, quantity, unit), gender = ...,
#'   race = ..., socioeconomic = ...)`, passed straight through to `create_population()`'s
#'   matching arguments. `NULL` (default): no Guard at all -- omitted from the module entirely,
#'   not merely a no-op filter.
#' @param exposure_state Which Frontend leaf builds each exposure row: `"vaccine"`
#'   (`create_vaccine()`), `"medication"` (a bare, unconditioned `MedicationOrder` -- exposure
#'   isn't "reason"-linked to a prior condition the way `create_medication()` expects), or
#'   `"condition"` (`create_condition()`).
#' @param exposure_attribute Person attribute name the exposure `pathways()` tags with the chosen
#'   arm. Default `"exposure_group"`.
#' @param provenance Optional free-form list (e.g. source file paths, filters used, a generation
#'   timestamp) carried into the spec purely for audit -- never read back by
#'   `build_module_from_spec()`.
#' @return A nested list (the spec). Pass to `write_module_spec()` to persist it, or straight to
#'   `build_module_from_spec()`.
#' @examples
#' \dontrun{
#' spec <- build_module_spec(
#'   "vaccine_safety",
#'   exposure_codes = exposure_codes, exposure_shares = c(
#'     abrysvo = 0.25, arexvy = 0.25, other_rsv = 0.05, comparator = 0.45
#'   ),
#'   outcome_codes = aesi_codes, outcome_probability = 0.03,
#'   outcome_delay = list(low = 0, high = 1460, unit = "days")
#' )
#' write_module_spec(spec, "vaccine_safety.yaml")
#' }
#' @export
build_module_spec <- function(
  name,
  exposure_codes,
  exposure_shares,
  outcome_codes,
  outcome_probability,
  outcome_delay,
  inclusion_criteria = NULL,
  exposure_state = c("vaccine", "medication", "condition"),
  exposure_attribute = "exposure_group",
  provenance = NULL
) {
  exposure_state <- match.arg(exposure_state)

  .required_cols(
    exposure_codes,
    c("option", "code", "display"),
    "exposure_codes"
  )
  if (!"comparator" %in% names(exposure_shares)) {
    stop(
      "build_module_spec(): exposure_shares must include a \"comparator\" entry"
    )
  }
  missing_shares <- setdiff(exposure_codes$option, names(exposure_shares))
  if (length(missing_shares) > 0) {
    stop(sprintf(
      "build_module_spec(): exposure_shares is missing entries for: %s",
      paste(missing_shares, collapse = ", ")
    ))
  }
  exposure_system <- .fallback_col(exposure_codes, "system", "coding_system")

  exposure_options <- lapply(seq_len(nrow(exposure_codes)), function(i) {
    list(
      option = exposure_codes$option[i],
      system = exposure_system[i],
      code = exposure_codes$code[i],
      display = exposure_codes$display[i],
      share = unname(exposure_shares[[exposure_codes$option[i]]])
    )
  })

  .check_shares(
    c(
      vapply(exposure_options, function(o) o$share, numeric(1)),
      exposure_shares[["comparator"]]
    ),
    "build_module_spec(): exposure_shares"
  )
  total_share <- sum(vapply(
    exposure_options,
    function(o) o$share,
    numeric(1)
  )) +
    exposure_shares[["comparator"]]
  if (!isTRUE(all.equal(unname(total_share), 1))) {
    stop(sprintf(
      "build_module_spec(): exposure_shares must sum to 1, got %s",
      total_share
    ))
  }

  label_col <- if ("event_abbreviation" %in% names(outcome_codes)) {
    "event_abbreviation"
  } else {
    "label"
  }
  .required_cols(
    outcome_codes,
    c(label_col, "code", "display"),
    "outcome_codes"
  )
  outcome_system <- .fallback_col(outcome_codes, "system", "coding_system")

  probability_values <- if (
    is.character(outcome_probability) &&
      length(outcome_probability) == 1 &&
      outcome_probability %in% names(outcome_codes)
  ) {
    outcome_codes[[outcome_probability]]
  } else {
    rep(outcome_probability, nrow(outcome_codes))
  }
  for (p in probability_values) {
    .check_probability(p, "build_module_spec(): outcome onset probabilities")
  }

  outcome_items <- lapply(seq_len(nrow(outcome_codes)), function(i) {
    list(
      event_abbreviation = outcome_codes[[label_col]][i],
      system = outcome_system[i],
      code = outcome_codes$code[i],
      display = outcome_codes$display[i],
      probability = unname(probability_values[i])
    )
  })

  list(
    name = name,
    inclusion_criteria = inclusion_criteria,
    exposure = list(
      attribute = exposure_attribute,
      state_type = exposure_state,
      options = exposure_options,
      comparator = list(
        include = TRUE,
        share = unname(exposure_shares[["comparator"]])
      )
    ),
    outcomes = list(
      default_probability = if (is.numeric(outcome_probability)) {
        outcome_probability[1]
      } else {
        NULL
      },
      default_delay = outcome_delay,
      items = outcome_items
    ),
    provenance = provenance
  )
}

#' Write a module spec to a YAML file
#'
#' @param spec A spec list (`build_module_spec()`'s output, or a hand-edited one).
#' @param path Output file path. Parent directories are created if needed.
#' @return `path`, invisibly.
#' @export
write_module_spec <- function(spec, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  yaml::write_yaml(spec, path)
  invisible(path)
}

#' Read a module spec from a YAML file
#'
#' Checks the required top-level keys are present, erroring clearly (naming which ones are
#' missing) rather than failing deep inside `build_module_from_spec()`.
#'
#' @param path Path to a YAML file written by `write_module_spec()`, or hand-edited.
#' @return The spec, as a nested list.
#' @export
read_module_spec <- function(path) {
  spec <- yaml::read_yaml(path)
  .check_required_spec_keys(spec, path)
  spec
}

#' @noRd
.check_required_spec_keys <- function(spec, source_label) {
  required <- c("name", "exposure", "outcomes")
  missing <- setdiff(required, names(spec))
  if (length(missing) > 0) {
    stop(sprintf(
      "%s is missing required top-level key(s): %s",
      source_label,
      paste(missing, collapse = ", ")
    ))
  }
}

#' Stop if a spec input table lacks any required column
#'
#' @param df A `data.frame` passed to `build_module_spec()`.
#' @param cols Column names `df` must contain.
#' @param df_name The argument's name, used in the error message.
#' @return `NULL`, invisibly; called for its error side effect.
#' @noRd
.required_cols <- function(df, cols, df_name) {
  missing <- setdiff(cols, names(df))
  if (length(missing) > 0) {
    stop(sprintf(
      "build_module_spec(): %s is missing column(s): %s",
      df_name,
      paste(missing, collapse = ", ")
    ))
  }
}

#' Return the first of several alternative column names present in a table
#'
#' Tolerates both `system` (this package's own naming) and `coding_system` (BRIDGE's naming,
#' as returned by `read_bridge_codelist()`) without the caller having to rename first.
#'
#' @param df A `data.frame`.
#' @param ... Candidate column names, in preference order.
#' @return The first matching column of `df`, as a vector.
#' @noRd
.fallback_col <- function(df, ...) {
  for (nm in c(...)) {
    if (nm %in% names(df)) {
      return(df[[nm]])
    }
  }
  stop(sprintf(
    "build_module_spec(): expected one of these columns: %s. Found: %s.",
    paste(c(...), collapse = ", "),
    paste(names(df), collapse = ", ")
  ))
}

#' Stop unless every arm share is a single finite number in [0, 1]
#' @noRd
.check_shares <- function(shares, where) {
  ok <- vapply(
    shares,
    function(x) is.numeric(x) && length(x) == 1 && is.finite(x) && x >= 0 && x <= 1,
    logical(1)
  )
  if (!all(ok)) {
    stop(sprintf("%s: every share must be a finite number in [0, 1]", where))
  }
}

#' Stop unless `p` is a single finite number strictly between 0 and 1
#' @noRd
.check_probability <- function(p, where) {
  if (!is.numeric(p) || length(p) != 1 || !is.finite(p) || p <= 0 || p >= 1) {
    stop(sprintf(
      "%s must be a number in (0, 1), got %s",
      where,
      paste(format(p), collapse = ", ")
    ))
  }
}

#' Build one exposure option's leaf fragment for a spec's `exposure$state_type`
#'
#' @param state_type One of `"vaccine"`, `"medication"`, `"condition"`.
#' @param label The fragment's state label.
#' @param code A `create_component_settings("code", ...)` result.
#' @return A fragment: a `Vaccine`, a chronic `MedicationOrder`, or a wellness-diagnosed
#'   condition.
#' @noRd
.exposure_leaf <- function(state_type, label, code) {
  switch(
    state_type,
    vaccine = create_vaccine(label, code),
    medication = create_step(
      "MedicationOrder",
      codes = list(code),
      chronic = TRUE,
      label = label
    ),
    condition = create_condition(label, code, diagnosis = "wellness"),
    stop(sprintf(
      "build_module_from_spec(): unknown exposure state_type '%s'",
      state_type
    ))
  )
}

#' Build Synthea modules from a spec
#'
#' Purely mechanical -- no codelist access, no parameter decisions, everything it needs is already
#' resolved in `x`. Produces several modules, not one:
#'
#' - **The exposure module** (named `spec$name`): an optional `create_population()` Guard from
#'   `inclusion_criteria` (entirely omitted -- not a no-op -- when absent), then the exposure
#'   `pathways()` block from `exposure$options`/`comparator`, using the leaf matching
#'   `exposure$state_type`. The chosen arm is tagged on the person as `exposure$attribute`.
#' - **One outcome module per `outcomes$items` row** (named `"<spec$name> - <event_abbreviation>"`):
#'   a Guard that waits until `exposure$attribute` is set, then `create_delay(...)`, then
#'   `pathways(onset vs. none)`. A per-item `probability`/`delay` overrides
#'   `outcomes$default_probability`/`default_delay` when present -- the hand-editable escape
#'   hatch, even though `build_module_spec()` itself never writes a per-item delay.
#'
#' Outcomes are separate modules because a person is only ever in one state of a given module at a
#' time: chained inside one module, each outcome's delay would only start after the previous one's
#' had finished, making the windows cumulative and order-dependent. Synthea runs every module for
#' a person side by side, so separate modules give each outcome its own window measured from
#' exposure (to Synthea's time-step resolution). Every module goes through `build_cohort_module()`
#' (which runs `validate_module()`). All of them need to be installed together in the target
#' Synthea checkout.
#'
#' @param x A path to a YAML spec file, or an already-loaded spec list (`build_module_spec()`'s
#'   or `read_module_spec()`'s output).
#' @param as_json If TRUE (default), each module is a pretty-printed JSON string. If FALSE, each
#'   is an R list instead.
#' @param validate Passed to `build_cohort_module()`.
#' @return A named list of modules (JSON strings or R lists, per `as_json`), keyed by module name:
#'   the exposure module first, then one per outcome item.
#' @examples
#' \dontrun{
#' modules <- build_module_from_spec("vaccine_safety.yaml")
#' for (nm in names(modules)) {
#'   write_module_json(modules[[nm]], file.path("modules", paste0(nm, ".json")))
#' }
#' }
#' @export
build_module_from_spec <- function(x, as_json = TRUE, validate = TRUE) {
  spec <- if (is.character(x) && length(x) == 1) {
    read_module_spec(x)
  } else {
    .check_required_spec_keys(x, "spec")
    x
  }

  opt_names <- vapply(
    spec$exposure$options,
    function(opt) opt$option,
    character(1)
  )
  exposure_options <- .named_list(
    lapply(spec$exposure$options, function(opt) {
      code <- create_component_settings(
        "code",
        system = opt$system,
        code = opt$code,
        display = opt$display
      )
      .exposure_leaf(
        spec$exposure$state_type,
        paste0("Exposure - ", opt$option),
        code
      )
    }),
    opt_names
  )
  exposure_shares <- .named_list(
    as.list(vapply(spec$exposure$options, function(opt) opt$share, numeric(1))),
    opt_names
  )
  exposure_shares <- unlist(exposure_shares)

  if (isTRUE(spec$exposure$comparator$include)) {
    exposure_options <- c(exposure_options, list(comparator = NULL))
    exposure_shares <- c(
      exposure_shares,
      c(comparator = spec$exposure$comparator$share)
    )
  }

  .check_shares(exposure_shares, "build_module_from_spec(): exposure shares")
  exposure_attribute <- spec$exposure$attribute
  if (!is.character(exposure_attribute) || length(exposure_attribute) != 1 || !nzchar(exposure_attribute)) {
    stop(
      "build_module_from_spec(): exposure$attribute must be set -- the outcome modules wait on it"
    )
  }

  exposure_fragment <- pathways(
    "Exposure",
    options = exposure_options,
    shares = exposure_shares,
    attribute = exposure_attribute
  )

  inclusion_fragment <- if (!is.null(spec$inclusion_criteria)) {
    do.call(
      create_population,
      c(list(label = "Inclusion Criteria"), spec$inclusion_criteria)
    )
  } else {
    NULL
  }

  modules <- list()
  modules[[spec$name]] <- build_cohort_module(
    spec$name,
    chain(inclusion_fragment, exposure_fragment),
    as_json = as_json,
    validate = validate
  )

  for (item in spec$outcomes$items) {
    abbr <- item$event_abbreviation
    p <- if (!is.null(item$probability)) {
      item$probability
    } else {
      spec$outcomes$default_probability
    }
    delay <- if (!is.null(item$delay)) {
      item$delay
    } else {
      spec$outcomes$default_delay
    }
    if (is.null(p) || is.null(delay)) {
      stop(sprintf(
        "build_module_from_spec(): outcome item '%s' has no probability/delay and no default is set",
        abbr
      ))
    }
    .check_probability(
      p,
      sprintf("build_module_from_spec(): outcome item '%s' probability", abbr)
    )

    code <- create_component_settings(
      "code",
      system = item$system,
      code = item$code,
      display = item$display
    )
    onset <- create_condition(
      paste0("Outcome - ", abbr),
      code,
      diagnosis = "wellness"
    )
    module_name <- paste0(spec$name, " - ", abbr)
    if (!is.null(modules[[module_name]])) {
      stop(sprintf(
        "build_module_from_spec(): duplicate outcome event_abbreviation '%s'",
        abbr
      ))
    }
    modules[[module_name]] <- build_cohort_module(
      module_name,
      chain(
        create_guard(
          create_logic_settings(
            "Attribute",
            attribute = exposure_attribute,
            operator = "is not nil"
          ),
          label = "Wait For Exposure"
        ),
        create_delay(
          delay$low,
          delay$high,
          delay$unit,
          label = paste0(abbr, " Delay")
        ),
        pathways(
          paste0(abbr, " Onset"),
          options = list(onset = onset, none = NULL),
          shares = c(onset = p, none = 1 - p)
        )
      ),
      as_json = as_json,
      validate = validate
    )
  }

  modules
}
