# module.R -- the generic assembler (build_module(), the primary entry point of this toolkit),
# the graph-level validator (validate_module()), the file writer (write_module_json()), and the
# build_disease_module() convenience recipe built on top of the whole toolkit.

#' The generic module builder. Takes whatever `states` the caller assembled (via
#' create_state_settings() and/or raw lists, in any order/number/mix of types) and returns the
#' validated module.
#'
#' @param name Module name.
#' @param states A named list of state settings, keyed by state name. Must include a state named
#'   "Initial" and at least one state of type "Terminal"; everything else -- which states appear,
#'   how many, in what order -- is entirely up to the caller.
#' @param specialty Optional clinician specialty for encounters this module creates.
#' @param remarks Optional character vector of free-text notes.
#' @param gmf_version GMF version number to record in the module header. Default 2.
#' @param as_json If TRUE (default), returns the validated module as a pretty-printed JSON
#'   string. If FALSE, returns the R list instead (e.g. to keep composing before finalizing).
#' @param pretty Passed to jsonlite::toJSON when as_json = TRUE.
#' @param validate If TRUE (default), runs validate_module() and aborts without returning
#'   anything if it fails.
#' @return The validated module: a JSON string (as_json = TRUE) or an R list (as_json = FALSE).
#' @examples
#' states <- list(
#'   Initial = create_state_settings("Initial",
#'     transition = create_transition_settings("direct", to = "Terminal")
#'   ),
#'   Terminal = create_state_settings("Terminal")
#' )
#' build_module("Minimal Example", states)
#' @export
build_module <- function(name, states, specialty = NULL, remarks = NULL, gmf_version = 2,
                         as_json = TRUE, pretty = TRUE, validate = TRUE) {
  stopifnot(is.character(name), length(name) == 1, nzchar(name))
  stopifnot(is.list(states), length(states) > 0)
  if (is.null(names(states)) || any(!nzchar(names(states)))) {
    stop("`states` must be a named list, keyed by state name")
  }

  module <- list(name = name)
  if (!is.null(specialty)) module$specialty <- specialty
  if (!is.null(remarks)) module$remarks <- unname(as.list(as.character(remarks)))
  module$states <- states
  module$gmf_version <- gmf_version

  if (isTRUE(validate)) {
    validate_module(module)
  }

  if (isTRUE(as_json)) {
    jsonlite::toJSON(module, auto_unbox = TRUE, pretty = pretty, null = "null")
  } else {
    module
  }
}

#' Validate a module list against the engine's own load-time invariants
#' (org.mitre.synthea.engine.Module / State), covering every state-name-bearing reference the
#' full state/transition vocabulary can produce:
#'  - states is non-empty and contains exactly one "Initial" state
#'  - at least one state has type "Terminal"
#'  - every state has a "type"
#'  - every non-Terminal state has exactly one transition property
#'  - every state-name reference (direct_transition, distributed_transition[].transition,
#'    conditional_transition[].transition, complex_transition[].transition and
#'    .distributions[].transition, target_encounter, condition_onset, medication_order,
#'    careplan, allergy_onset, device) resolves to a state that exists
#'
#' @param module_list A module list, either freshly built (as_json = FALSE) or round-tripped
#'   through jsonlite::fromJSON(..., simplifyVector = FALSE).
#' @return TRUE (invisibly) if valid; otherwise stops with a descriptive error.
#' @examples
#' states <- list(
#'   Initial = create_state_settings("Initial",
#'     transition = create_transition_settings("direct", to = "Terminal")
#'   ),
#'   Terminal = create_state_settings("Terminal")
#' )
#' module <- build_module("Minimal Example", states, as_json = FALSE)
#' validate_module(module)
#' @export
validate_module <- function(module_list) {
  transition_keys <- c(
    "direct_transition", "distributed_transition", "conditional_transition",
    "complex_transition", "lookup_table_transition", "type_of_care_transition"
  )

  if (is.null(module_list$states) || length(module_list$states) == 0) {
    stop("Module has no states")
  }
  states <- module_list$states
  state_names <- names(states)
  if (is.null(state_names) || any(!nzchar(state_names))) {
    stop("Module 'states' must be a named object keyed by state name")
  }

  initial_count <- sum(state_names == "Initial")
  if (initial_count != 1) {
    stop(sprintf("Module must have exactly one state named 'Initial' (found %d)", initial_count))
  }

  is_terminal <- vapply(states, function(s) identical(s$type, "Terminal"), logical(1))
  if (!any(is_terminal)) {
    stop("Module has no state of type 'Terminal'")
  }

  for (nm in state_names) {
    st <- states[[nm]]
    if (is.null(st$type) || !nzchar(st$type)) {
      stop(sprintf("State '%s' has no 'type'", nm))
    }
    if (!identical(st$type, "Terminal")) {
      present <- transition_keys[transition_keys %in% names(st)]
      if (length(present) == 0) {
        stop(sprintf("State '%s' (type %s) has no transition property", nm, st$type))
      }
      if (length(present) > 1) {
        stop(sprintf(
          "State '%s' has more than one transition property: %s",
          nm, paste(present, collapse = ", ")
        ))
      }
    }
  }

  referenced <- character(0)
  for (nm in state_names) {
    st <- states[[nm]]
    if (!is.null(st$direct_transition)) {
      referenced <- c(referenced, st$direct_transition)
    }
    if (!is.null(st$distributed_transition)) {
      referenced <- c(referenced, vapply(
        st$distributed_transition,
        function(o) o$transition, character(1)
      ))
    }
    if (!is.null(st$conditional_transition)) {
      referenced <- c(referenced, vapply(
        st$conditional_transition,
        function(o) o$transition, character(1)
      ))
    }
    if (!is.null(st$complex_transition)) {
      for (o in st$complex_transition) {
        if (!is.null(o$transition)) referenced <- c(referenced, o$transition)
        if (!is.null(o$distributions)) {
          referenced <- c(referenced, vapply(o$distributions, function(d) d$transition, character(1)))
        }
      }
    }
    if (!is.null(st$target_encounter) && nzchar(st$target_encounter)) {
      referenced <- c(referenced, st$target_encounter)
    }
    if (!is.null(st$condition_onset)) referenced <- c(referenced, st$condition_onset)
    if (!is.null(st$medication_order)) referenced <- c(referenced, st$medication_order)
    if (!is.null(st$careplan)) referenced <- c(referenced, st$careplan)
    if (!is.null(st$allergy_onset)) referenced <- c(referenced, st$allergy_onset)
    if (!is.null(st$device)) referenced <- c(referenced, st$device)
  }
  missing <- setdiff(unique(referenced), state_names)
  if (length(missing) > 0) {
    stop(sprintf("References to undefined state(s): %s", paste(missing, collapse = ", ")))
  }

  invisible(TRUE)
}

#' Write a module to disk.
#'
#' @param x Either a JSON string (build_module()'s default output) or an R list
#'   (build_module(as_json = FALSE)'s output, re-validated before writing).
#' @param path Output file path. Parent directories are created if needed.
#' @return The output path, invisibly.
#' @examples
#' states <- list(
#'   Initial = create_state_settings("Initial",
#'     transition = create_transition_settings("direct", to = "Terminal")
#'   ),
#'   Terminal = create_state_settings("Terminal")
#' )
#' module_json <- build_module("Minimal Example", states)
#' write_module_json(module_json, tempfile(fileext = ".json"))
#' @export
write_module_json <- function(x, path) {
  if (is.character(x) && length(x) == 1) {
    json_text <- x
  } else if (is.list(x)) {
    validate_module(x)
    json_text <- as.character(jsonlite::toJSON(x, auto_unbox = TRUE, pretty = TRUE, null = "null"))
  } else {
    stop("write_module_json(): `x` must be a JSON string or a module list")
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(json_text, con = path)
  invisible(path)
}

#' Convenience recipe: onset -> diagnosis encounter -> optional resolution/death -> Terminal.
#' Kept for backward compatibility with earlier versions of this toolkit, and as a worked
#' example of composing the layers -- the primary interface is build_module() +
#' create_state_settings() (see example_compose.R). Same public signature/behavior as before;
#' still returns an R list (as_json = FALSE), not a JSON string.
#'
#' @param name Module name.
#' @param condition_code A single create_component_settings("code", ...) result -- becomes the
#'   ConditionOnset state's `codes[1]`.
#' @param diagnosis "wellness" (block until the next scheduled wellness visit) or "standalone"
#'   (open a dedicated encounter immediately; requires encounter_class).
#' @param encounter_class Required when diagnosis == "standalone".
#' @param onset_delay Optional list(low, high, unit) -- condition onsets after a random delay.
#' @param age_gate Optional list(operator, quantity, unit) -- module waits until this age test
#'   passes before onset proceeds.
#' @param resolves_after Optional list(low, high, unit) -- condition is ended after a random
#'   delay following diagnosis. If omitted, the condition is left chronic.
#' @param death Optional list(probability, code = NULL) -- chance of death after diagnosis.
#' @param remarks Optional character vector of free-text notes.
#' @param gmf_version GMF version number to record in the module header. Default 2.
#' @return An R list mirroring the module JSON tree.
#' @examples
#' hypertension_code <- create_component_settings("code",
#'   system = "SNOMED-CT",
#'   code = "38341003", display = "Hypertension"
#' )
#' build_disease_module("Hypertension",
#'   condition_code = hypertension_code,
#'   diagnosis = "wellness"
#' )
#' @export
build_disease_module <- function(name,
                                 condition_code,
                                 diagnosis = c("wellness", "standalone"),
                                 encounter_class = NULL,
                                 onset_delay = NULL,
                                 age_gate = NULL,
                                 resolves_after = NULL,
                                 death = NULL,
                                 remarks = NULL,
                                 gmf_version = 2) {
  diagnosis <- match.arg(diagnosis)
  if (diagnosis == "standalone" && (is.null(encounter_class) || !nzchar(encounter_class))) {
    stop("encounter_class is required when diagnosis = \"standalone\"")
  }
  if (!is.null(death)) {
    stopifnot(
      is.list(death), "probability" %in% names(death),
      is.numeric(death$probability), death$probability > 0, death$probability <= 1
    )
  }

  onset_name <- "Condition Onset"
  encounter_name <- "Diagnosis Encounter"
  encounter_end_name <- "End Encounter"
  resolve_delay_name <- "Condition Resolves Delay"
  resolve_name <- "Condition Resolves"

  next_after_encounter <- if (!is.null(resolves_after)) resolve_delay_name else "Terminal"

  chain <- character(0)
  if (!is.null(age_gate)) chain <- c(chain, "Age Gate")
  if (!is.null(onset_delay)) chain <- c(chain, "Onset Delay")
  chain <- c(chain, onset_name)
  full_chain <- c(chain, encounter_name)

  states <- list()

  if (!is.null(age_gate)) {
    nxt <- full_chain[which(chain == "Age Gate") + 1]
    states[["Age Gate"]] <- create_state_settings("Guard",
      allow = create_logic_settings("Age",
        operator = age_gate$operator,
        quantity = age_gate$quantity, unit = age_gate$unit
      ),
      transition = create_transition_settings("direct", to = nxt)
    )
  }
  if (!is.null(onset_delay)) {
    nxt <- full_chain[which(chain == "Onset Delay") + 1]
    states[["Onset Delay"]] <- create_state_settings("Delay",
      range = create_component_settings("range",
        low = onset_delay$low, high = onset_delay$high,
        unit = onset_delay$unit
      ),
      transition = create_transition_settings("direct", to = nxt)
    )
  }

  states[[onset_name]] <- create_state_settings("ConditionOnset",
    codes = list(condition_code),
    target_encounter = encounter_name,
    transition = create_transition_settings("direct", to = encounter_name)
  )

  encounter_fields <- if (diagnosis == "wellness") list(wellness = TRUE) else list(encounter_class = encounter_class)
  states[[encounter_name]] <- do.call(create_state_settings, c(
    list(type = "Encounter"), encounter_fields,
    list(transition = create_transition_settings("direct", to = encounter_end_name))
  ))

  if (!is.null(resolves_after)) {
    states[[resolve_delay_name]] <- create_state_settings("Delay",
      range = create_component_settings("range",
        low = resolves_after$low, high = resolves_after$high,
        unit = resolves_after$unit
      ),
      transition = create_transition_settings("direct", to = resolve_name)
    )
    states[[resolve_name]] <- create_state_settings("ConditionEnd",
      condition_onset = onset_name,
      transition = create_transition_settings("direct", to = "Terminal")
    )
  }

  if (!is.null(death)) {
    death_fields <- if (!is.null(death$code)) list(codes = list(death$code)) else list(condition_onset = onset_name)
    states[["Death"]] <- do.call(create_state_settings, c(
      list(type = "Death"), death_fields,
      list(transition = create_transition_settings("direct", to = "Terminal"))
    ))

    states[[encounter_end_name]] <- create_state_settings("EncounterEnd",
      transition = create_transition_settings("distributed", options = list(
        list(transition = "Death", distribution = death$probability),
        list(transition = next_after_encounter, distribution = 1 - death$probability)
      ))
    )
  } else {
    states[[encounter_end_name]] <- create_state_settings("EncounterEnd",
      transition = create_transition_settings("direct", to = next_after_encounter)
    )
  }

  states[["Initial"]] <- create_state_settings("Initial",
    transition = create_transition_settings("direct", to = chain[1])
  )
  states[["Terminal"]] <- create_state_settings("Terminal")

  build_module(
    name = name, states = states, remarks = remarks, gmf_version = gmf_version,
    as_json = FALSE
  )
}

#' The Frontend layer's finisher: turn a fragment (from chain()/pathways()/classify()/
#' repeat_until()/a leaf in blocks.R) into a validated module. The only place `Initial`/`Terminal`
#' get invented -- always under the literal name "Terminal", the convention pathways()/classify()'s
#' `terminal_options` relies on. No block or combinator upstream needs to know whether it's first
#' or last in the graph.
#'
#' @param name Module name.
#' @param fragment A fragment: `list(states, entry, exit)`, as returned by any Frontend-layer
#'   `create_*()` leaf or `chain()`/`pathways()`/`classify()`/`repeat_until()`.
#' @param remarks Optional character vector of free-text notes.
#' @param gmf_version GMF version number to record in the module header. Default 2.
#' @param as_json If TRUE (default), returns the validated module as a pretty-printed JSON
#'   string. If FALSE, returns the R list instead.
#' @param validate If TRUE (default), runs validate_module() and aborts without returning
#'   anything if it fails.
#' @return The validated module: a JSON string (as_json = TRUE) or an R list (as_json = FALSE).
#' @examples
#' diabetes <- create_component_settings("code",
#'   system = "SNOMED-CT", code = "44054006",
#'   display = "Diabetes mellitus type 2 (disorder)"
#' )
#' build_cohort_module("Diabetes Cohort", create_condition("Diabetes", diabetes))
#' @export
build_cohort_module <- function(name, fragment, remarks = NULL, gmf_version = 2, as_json = TRUE,
                                validate = TRUE) {
  states <- fragment$states
  states[[fragment$exit]]$direct_transition <- "Terminal"
  states[["Initial"]] <- create_state_settings("Initial",
    transition = create_transition_settings("direct", to = fragment$entry)
  )
  states[["Terminal"]] <- create_state_settings("Terminal")

  build_module(
    name = name, states = states, remarks = remarks, gmf_version = gmf_version,
    as_json = as_json, pretty = TRUE, validate = validate
  )
}
