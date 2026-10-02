# blocks.R -- the Frontend layer's leaf constructors. Each wraps one common Engine shape (or a
# handful of Engine states glued together) and returns a "fragment": list(states, entry, exit).
# A fragment is not valid Synthea JSON -- its `exit` state's direct_transition is left as the
# literal placeholder "__PENDING__", patched by whichever combinator (chain()/pathways()/
# classify()/repeat_until()) or build_cohort_module() consumes it next. See the plan at
# validated-floating-garden.md for the full design rationale.
#
# Naming: create_<noun>() (no "_settings" suffix) signals the Frontend layer, distinct from the
# Engine's create_<noun>_settings(). `label` is both a block's external "handle" (the string
# another block references, e.g. create_medication(condition = "Diabetes")) and the exact backend
# JSON state name of its primary state -- not two separate things kept in sync.

#' Name a list of values (base-R equivalent of `stats::setNames()`, avoided to keep this
#' package's only dependency `jsonlite`)
#' @param values A plain list.
#' @param names_ Names to assign, same length as `values`.
#' @return `values`, with `names_` assigned.
.named_list <- function(values, names_) {
  names(values) <- names_
  values
}

# Utility ----

#' Wrap any Engine state type as a one-state fragment (the escape hatch)
#'
#' The universal frontend leaf: builds a single state of any of the 31 `create_state_settings()`
#' types and returns it as a trivial fragment (`entry == exit == label`). Every other leaf in this
#' file hides Engine vocabulary (exact `type` names, exact field names); this one exposes it on
#' purpose, for the state types with no dedicated leaf (`Symptom`, `Device`, `ImagingStudy`,
#' `SupplyList`, `CallSubmodule`, `Physiology`, ...).
#'
#' @param type A `create_state_settings()` type name.
#' @param ... Fields for that state type, exactly as `create_state_settings()` expects them
#'   (excluding `transition`, which this function always sets to a pending placeholder).
#' @param label The state's name -- also this fragment's entry/exit handle.
#' @return A fragment: `list(states, entry, exit)`.
#' @examples
#' create_step("Symptom",
#'   symptom = "Chest Pain", cause = "Heart Disease",
#'   range = create_component_settings("range", low = 10, high = 30),
#'   label = "Chest Pain Symptom"
#' )
#' @export
create_step <- function(type, ..., label) {
  state <- do.call(
    create_state_settings,
    c(
      list(type = type),
      list(...),
      list(
        transition = create_transition_settings("direct", to = "__PENDING__")
      )
    )
  )
  list(states = .named_list(list(state), label), entry = label, exit = label)
}

#' A random-duration wait, as a fragment
#'
#' Shorthand for `create_step("Delay", range = create_component_settings("range", ...), label =
#' label)` -- the most common state in every worked example in this package.
#'
#' @param low,high,unit Passed straight through to `create_component_settings("range", ...)`.
#' @param label The state's name. Defaults to `"Delay"` -- safe even reused across a module,
#'   since a second undistinguished use is caught loudly by a combinator's collision-check, not
#'   silently overwritten.
#' @return A fragment.
#' @examples
#' create_delay(low = 6, high = 18, unit = "months")
#' @export
create_delay <- function(low, high, unit, label = "Delay") {
  create_step(
    "Delay",
    range = create_component_settings(
      "range",
      low = low,
      high = high,
      unit = unit
    ),
    label = label
  )
}

#' A generic wait-until-true block, as a fragment
#'
#' Wraps `Guard` around any `create_logic_settings()` result. `create_population()` below is a
#' named convenience over exactly this, specialized to demographic `Logic`.
#'
#' @param condition A `create_logic_settings()` result.
#' @param label The state's name. Defaults to `"Guard"`.
#' @return A fragment.
#' @examples
#' create_guard(create_logic_settings("Age", operator = ">=", quantity = 18, unit = "years"))
#' @export
create_guard <- function(condition, label = "Guard") {
  state <- create_state_settings(
    "Guard",
    allow = condition,
    transition = create_transition_settings("direct", to = "__PENDING__")
  )
  list(states = .named_list(list(state), label), entry = label, exit = label)
}

# Demographic ----

#' Define a population eligibility gate, as a fragment
#'
#' Combines whichever demographic filters are given into one `Guard` (via `And()` if more than
#' one) -- a named convenience over `create_guard()`. Returns `NULL` (a true no-op, skipped by
#' `chain()`) if nothing is given. This gates *when a simulated patient proceeds through this
#' module*, not who Synthea simulates at all -- the same caveat `build_disease_module()`'s
#' `age_gate` already carries.
#'
#' @param label The `Guard` state's name.
#' @param age Optional `list(operator, quantity, unit)`, same shape as `build_disease_module()`'s
#'   `age_gate`.
#' @param gender Optional, passed straight to `create_logic_settings("Gender", gender = gender)`.
#' @param race Optional, passed straight to `create_logic_settings("Race", race = race)`.
#' @param socioeconomic Optional, one of `"High"`/`"Middle"`/`"Low"` (the `Logic` engine's own
#'   documented closed set for `SocioeconomicStatus`); validated when given, left `NULL` (no
#'   filter) by default.
#' @param date Optional `list(operator, year = , month = , date = )` -- a calendar-time filter,
#'   passed straight to `create_logic_settings("Date", ...)` (exactly one of `year`/`month`/`date`;
#'   `date` is a full `list(year, month, day, hour, minute, second, millisecond)`).
#' @return A fragment, or `NULL` if every filter is omitted.
#' @examples
#' create_population("Adult Filter", age = list(operator = ">=", quantity = 18, unit = "years"))
#' @export
create_population <- function(
  label,
  age = NULL,
  gender = NULL,
  race = NULL,
  socioeconomic = NULL,
  date = NULL
) {
  conditions <- list()
  if (!is.null(age)) {
    conditions <- c(
      conditions,
      list(create_logic_settings(
        "Age",
        operator = age$operator,
        quantity = age$quantity,
        unit = age$unit
      ))
    )
  }
  if (!is.null(gender)) {
    conditions <- c(
      conditions,
      list(create_logic_settings("Gender", gender = gender))
    )
  }
  if (!is.null(race)) {
    conditions <- c(
      conditions,
      list(create_logic_settings("Race", race = race))
    )
  }
  if (!is.null(socioeconomic)) {
    socioeconomic <- match.arg(socioeconomic, c("High", "Middle", "Low"))
    conditions <- c(
      conditions,
      list(create_logic_settings(
        "SocioeconomicStatus",
        category = socioeconomic
      ))
    )
  }

  if (!is.null(date)) {
    conditions <- c(
      conditions,
      list(create_logic_settings(
        "Date",
        operator = date$operator,
        year = date$year,
        month = date$month,
        date = date$date
      ))
    )
  }

  if (length(conditions) == 0) {
    return(NULL)
  }
  condition <- if (length(conditions) == 1) {
    conditions[[1]]
  } else {
    create_logic_settings("And", conditions = conditions)
  }
  create_guard(condition, label = label)
}

# Clinical events ----

#' A condition's onset (+ diagnosis encounter, + optional resolution), as a fragment
#'
#' Onset -> diagnosis encounter -> end encounter, the same chain `build_disease_module()` builds.
#' The onset state is named exactly `label` (not "`label` Onset") specifically so
#' `create_medication()` can pass that same string straight through as `reason`.
#'
#' @param label The onset state's name -- also the handle other blocks reference via `condition =`.
#' @param code A `create_component_settings("code", ...)` result.
#' @param diagnosis `"wellness"` (block until the next scheduled wellness visit) or
#'   `"standalone"` (open a dedicated encounter immediately; requires `encounter_class`).
#' @param encounter_class Required when `diagnosis = "standalone"`.
#' @param onset_delay Optional `list(low, high, unit)` -- the condition begins after a random
#'   delay instead of immediately.
#' @param resolves_after Optional `list(low, high, unit)` -- appends a `Delay -> ConditionEnd`
#'   after the encounter; omit it and the condition stays chronic.
#' @return A fragment.
#' @examples
#' diabetes <- create_component_settings("code",
#'   system = "SNOMED-CT", code = "44054006",
#'   display = "Diabetes mellitus type 2 (disorder)"
#' )
#' create_condition("Diabetes", diabetes, diagnosis = "wellness")
#' @export
create_condition <- function(
  label,
  code,
  diagnosis = c("wellness", "standalone"),
  encounter_class = NULL,
  onset_delay = NULL,
  resolves_after = NULL
) {
  diagnosis <- match.arg(diagnosis)
  if (
    identical(diagnosis, "standalone") &&
      (is.null(encounter_class) || !nzchar(encounter_class))
  ) {
    stop(
      "create_condition(): `encounter_class` is required when diagnosis = \"standalone\""
    )
  }

  encounter_label <- paste0(label, " Encounter")
  encounter_end_label <- paste0(label, " Encounter End")

  onset_state <- create_state_settings(
    "ConditionOnset",
    codes = list(code),
    target_encounter = encounter_label,
    transition = create_transition_settings("direct", to = encounter_label)
  )

  encounter_fields <- if (identical(diagnosis, "wellness")) {
    list(wellness = TRUE)
  } else {
    list(encounter_class = encounter_class)
  }
  encounter_state <- do.call(
    create_state_settings,
    c(
      list(type = "Encounter"),
      encounter_fields,
      list(
        transition = create_transition_settings(
          "direct",
          to = encounter_end_label
        )
      )
    )
  )

  encounter_end_state <- create_state_settings(
    "EncounterEnd",
    transition = create_transition_settings("direct", to = "__PENDING__")
  )

  fragment <- list(
    states = .named_list(
      list(onset_state, encounter_state, encounter_end_state),
      c(label, encounter_label, encounter_end_label)
    ),
    entry = label,
    exit = encounter_end_label
  )

  if (!is.null(onset_delay)) {
    delay <- create_delay(
      onset_delay$low,
      onset_delay$high,
      onset_delay$unit,
      label = paste0(label, " Onset Delay")
    )
    fragment <- chain(delay, fragment)
  }
  if (!is.null(resolves_after)) {
    resolve_delay <- create_delay(
      resolves_after$low,
      resolves_after$high,
      resolves_after$unit,
      label = paste0(label, " Resolves Delay")
    )
    resolve_end <- create_step(
      "ConditionEnd",
      condition_onset = label,
      label = paste0(label, " Resolves")
    )
    fragment <- chain(fragment, resolve_delay, resolve_end)
  }
  fragment
}

#' A medication order (+ optional fixed course), as a fragment
#'
#' `duration = "long"` -> `chronic = TRUE`, no end. `duration = "short"` -> `chronic = FALSE`
#' plus an automatic `Delay(course) -> MedicationEnd`.
#'
#' @param label The order state's name -- also the handle `create_discontinue()`/switch pathways
#'   reference via `medication =`.
#' @param code A `create_component_settings("code", ...)` result.
#' @param condition The exact `create_condition()` `label` this medication treats (resolves as
#'   `reason`).
#' @param duration `"long"` (chronic, default) or `"short"` (a fixed course; `course` required).
#' @param course Required when `duration = "short"`: `list(low, high, unit)`.
#' @return A fragment.
#' @examples
#' metformin <- create_component_settings("code",
#'   system = "RxNorm", code = "860975",
#'   display = "Metformin hydrochloride 500 MG Oral Tablet"
#' )
#' create_medication("Metformin",
#'   metformin,
#'   condition = "Diabetes", duration = "long"
#' )
#' @export
create_medication <- function(
  label,
  code,
  condition,
  duration = c("long", "short"),
  course = NULL
) {
  duration <- match.arg(duration)
  if (identical(duration, "short") && is.null(course)) {
    stop("create_medication(): `course` is required when duration = \"short\"")
  }

  order_state <- create_state_settings(
    "MedicationOrder",
    codes = list(code),
    reason = condition,
    chronic = identical(duration, "long"),
    transition = create_transition_settings("direct", to = "__PENDING__")
  )
  fragment <- list(
    states = .named_list(list(order_state), label),
    entry = label,
    exit = label
  )

  if (identical(duration, "short")) {
    delay <- create_delay(
      course$low,
      course$high,
      course$unit,
      label = paste0(label, " Course")
    )
    end <- create_step(
      "MedicationEnd",
      medication_order = label,
      label = paste0(label, " Course End")
    )
    fragment <- chain(fragment, delay, end)
  }
  fragment
}

#' Stop a medication with nothing started in its place, as a fragment
#'
#' A bare `MedicationEnd`. Peels the single most common `create_step("MedicationEnd", ...)` use
#' off the escape hatch into a readable call (`medication =`, not the backend's
#' `medication_order =`).
#'
#' @param label The state's name.
#' @param medication The exact `create_medication()` `label` being stopped.
#' @return A fragment.
#' @examples
#' create_discontinue("Discontinue Metformin", medication = "Metformin")
#' @export
create_discontinue <- function(label, medication) {
  create_step("MedicationEnd", medication_order = medication, label = label)
}

#' A lab result (or any other coded fact), as a fragment
#'
#' The state type most real cohort definitions key on ("HbA1c > 8"), not just a diagnosis code.
#'
#' @param label The state's name.
#' @param code A `create_component_settings("code", ...)` result.
#' @param value Optional plain number -- convenience for the Engine's `exact` component
#'   (`create_component_settings("exact", quantity = value, unit = unit)`), set on both the
#'   nested `exact` component and the state's own top-level `unit` field (the Engine requires the
#'   latter whenever a numeric `exact`/`range` value is used).
#' @param value_code Optional coded (non-numeric) result.
#' @param unit Optional, used with `value`.
#' @return A fragment.
#' @examples
#' hba1c <- create_component_settings("code",
#'   system = "LOINC", code = "4548-4",
#'   display = "Hemoglobin A1c"
#' )
#' create_observation("HbA1c Reading", hba1c, value = 8.5, unit = "%")
#' @export
create_observation <- function(
  label,
  code,
  value = NULL,
  value_code = NULL,
  unit = NULL
) {
  exact <- if (!is.null(value)) {
    create_component_settings("exact", quantity = value, unit = unit)
  } else {
    NULL
  }
  create_step(
    "Observation",
    codes = list(code),
    exact = exact,
    value_code = value_code,
    unit = unit,
    label = label
  )
}

#' A vital-sign reading, as a fragment
#'
#' `VitalSign` -- a genuinely distinct Engine state from `Observation` (different Java class,
#' keyed on `vital_sign` rather than `codes`). Same `value` convenience as `create_observation()`.
#' Note: the Engine requires exactly one of `exact`/`range`/`expression`/`distribution` on every
#' `VitalSign` state -- omitting `value` here (and not adding one via `create_step()` afterward)
#' will surface that as a clear error from `create_state_settings()` at build time.
#'
#' @param label The state's name.
#' @param vital_sign The Engine's `VitalSign` attribute name (e.g. `"Blood Pressure Systolic"`).
#' @param value Optional plain number -- see `create_observation()`'s `value`.
#' @param unit Optional, used with `value`.
#' @return A fragment.
#' @examples
#' create_vital_sign("High Systolic BP", "Blood Pressure Systolic", value = 145, unit = "mmHg")
#' @export
create_vital_sign <- function(label, vital_sign, value = NULL, unit = NULL) {
  exact <- if (!is.null(value)) {
    create_component_settings("exact", quantity = value, unit = unit)
  } else {
    NULL
  }
  create_step(
    "VitalSign",
    vital_sign = vital_sign,
    exact = exact,
    unit = unit,
    label = label
  )
}

#' A procedure, as a fragment
#'
#' Core to claims-style pipeline testing.
#'
#' @param label The state's name.
#' @param code A `create_component_settings("code", ...)` result.
#' @param condition Optional -- the exact `create_condition()` `label` this procedure treats
#'   (resolves as `reason`).
#' @param length Optional `list(low, high, unit)` -- maps to the Engine's `duration` range
#'   component; omit it for an instantaneous procedure. Named `length`, not `duration`, to avoid
#'   colliding with `create_medication()`'s unrelated `duration = c("long","short")` mode argument.
#' @return A fragment.
#' @examples
#' appendectomy <- create_component_settings("code",
#'   system = "SNOMED-CT", code = "80146002",
#'   display = "Appendectomy"
#' )
#' create_procedure("Appendectomy", appendectomy)
#' @export
create_procedure <- function(label, code, condition = NULL, length = NULL) {
  duration_component <- if (!is.null(length)) {
    create_component_settings(
      "range",
      low = length$low,
      high = length$high,
      unit = length$unit
    )
  } else {
    NULL
  }
  create_step(
    "Procedure",
    codes = list(code),
    reason = condition,
    duration = duration_component,
    label = label
  )
}

#' A mortality endpoint, as a fragment
#'
#' Pairs with `pathways()`/`classify()`'s `terminal_options =` to route a death pathway straight
#' to `Terminal` instead of rejoining.
#'
#' @param label The state's name. Defaults to `"Death"`.
#' @param condition Optional -- the exact `create_condition()` `label` death is attributed to
#'   (resolves as `condition_onset`).
#' @param codes Optional list of `create_component_settings("code", ...)` results -- an explicit
#'   cause-of-death code, instead of `condition`.
#' @param after Optional `list(low, high, unit)` -- delays death; omitted means immediate.
#' @return A fragment.
#' @examples
#' create_death(condition = "Diabetes", after = list(low = 1, high = 10, unit = "years"))
#' @export
create_death <- function(
  label = "Death",
  condition = NULL,
  codes = NULL,
  after = NULL
) {
  range_component <- if (!is.null(after)) {
    create_component_settings(
      "range",
      low = after$low,
      high = after$high,
      unit = after$unit
    )
  } else {
    NULL
  }
  create_step(
    "Death",
    condition_onset = condition,
    codes = codes,
    range = range_component,
    label = label
  )
}

#' A standalone visit not tied to a new diagnosis, as a fragment
#'
#' `Encounter` + paired `EncounterEnd`. `create_condition()` already embeds one of these; this is
#' for every other case (a routine wellness visit, a visit that only hosts an
#' observation/procedure).
#'
#' @param label The encounter state's name.
#' @param wellness Block until the next scheduled wellness visit (default `TRUE`).
#' @param encounter_class Required when `wellness = FALSE`.
#' @return A fragment.
#' @examples
#' create_encounter("Annual Checkup")
#' @export
create_encounter <- function(label, wellness = TRUE, encounter_class = NULL) {
  end_label <- paste0(label, " End")
  wellness_field <- if (isTRUE(wellness)) TRUE else NULL
  start_state <- create_state_settings(
    "Encounter",
    wellness = wellness_field,
    encounter_class = encounter_class,
    transition = create_transition_settings("direct", to = end_label)
  )
  end_state <- create_state_settings(
    "EncounterEnd",
    transition = create_transition_settings("direct", to = "__PENDING__")
  )
  list(
    states = .named_list(list(start_state, end_state), c(label, end_label)),
    entry = label,
    exit = end_label
  )
}

#' An allergy onset (+ optional resolution), as a fragment
#'
#' Same onset(+resolution) shape as `create_condition()`.
#'
#' @param label The onset state's name.
#' @param code A `create_component_settings("code", ...)` result.
#' @param allergy_type,category,reactions Passed straight through to `AllergyOnset`.
#' @param resolves_after Optional `list(low, high, unit)` -- appends `Delay -> AllergyEnd`.
#' @return A fragment.
#' @examples
#' penicillin <- create_component_settings("code",
#'   system = "RxNorm", code = "7980",
#'   display = "Penicillin"
#' )
#' create_allergy("Penicillin Allergy", penicillin)
#' @export
create_allergy <- function(
  label,
  code,
  allergy_type = NULL,
  category = NULL,
  reactions = NULL,
  resolves_after = NULL
) {
  onset <- create_step(
    "AllergyOnset",
    codes = list(code),
    allergy_type = allergy_type,
    category = category,
    reactions = reactions,
    label = label
  )
  if (is.null(resolves_after)) {
    return(onset)
  }
  delay <- create_delay(
    resolves_after$low,
    resolves_after$high,
    resolves_after$unit,
    label = paste0(label, " Resolves Delay")
  )
  end <- create_step(
    "AllergyEnd",
    allergy_onset = label,
    label = paste0(label, " Resolves")
  )
  chain(onset, delay, end)
}

#' A care plan (+ optional resolution), as a fragment
#'
#' Same shape as `create_allergy()`/`create_condition()`.
#'
#' @param label The start state's name.
#' @param code A `create_component_settings("code", ...)` result.
#' @param activities,goals,reason Passed straight through to `CarePlanStart`.
#' @param resolves_after Optional `list(low, high, unit)` -- appends `Delay -> CarePlanEnd`.
#' @return A fragment.
#' @examples
#' diabetes_care <- create_component_settings("code",
#'   system = "SNOMED-CT", code = "698360004",
#'   display = "Diabetes self-management plan"
#' )
#' create_careplan("Diabetes Care Plan", diabetes_care)
#' @export
create_careplan <- function(
  label,
  code,
  activities = NULL,
  goals = NULL,
  reason = NULL,
  resolves_after = NULL
) {
  start <- create_step(
    "CarePlanStart",
    codes = list(code),
    activities = activities,
    goals = goals,
    reason = reason,
    label = label
  )
  if (is.null(resolves_after)) {
    return(start)
  }
  delay <- create_delay(
    resolves_after$low,
    resolves_after$high,
    resolves_after$unit,
    label = paste0(label, " Resolves Delay")
  )
  end <- create_step(
    "CarePlanEnd",
    careplan = label,
    label = paste0(label, " Resolves")
  )
  chain(start, delay, end)
}

#' A single vaccine dose, as a fragment
#'
#' A multi-dose series is `chain()`ing several of these with `create_delay()` between them -- no
#' separate "series" construct needed.
#'
#' @param label The state's name.
#' @param code A `create_component_settings("code", ...)` result.
#' @param series The dose number (an integer). Defaults to `1`.
#' @return A fragment.
#' @examples
#' bnt162b2 <- create_component_settings("code",
#'   system = "CVX", code = "208",
#'   display = "COVID-19 vaccine, mRNA"
#' )
#' create_vaccine("First Dose", bnt162b2, series = 1)
#' @export
create_vaccine <- function(label, code, series = 1) {
  create_step("Vaccine", codes = list(code), series = series, label = label)
}

#' Set a person attribute to an explicit value, as a fragment
#'
#' A bare `SetAttribute`. Distinct from `pathways()`/`classify()`'s own `attribute =` (which
#' auto-tags *which option a patient's pathway went through*): `create_tag()` is for recording an
#' arbitrary derived fact outright.
#'
#' @param label The state's name.
#' @param attribute The person attribute's name.
#' @param value The value to set.
#' @return A fragment.
#' @examples
#' create_tag("Treated", attribute = "diabetes_cohort", value = "treated")
#' @export
create_tag <- function(label, attribute, value) {
  create_step(
    "SetAttribute",
    attribute = attribute,
    value = value,
    label = label
  )
}

#' Bump a numeric attribute up or down, as a fragment
#'
#' Wraps `Counter`. Paired with `repeat_until()`, this is how "N occurrences" eligibility criteria
#' get built: count qualifying events as they're observed, then exit the loop once an `Attribute`
#' `Logic` condition on the count is satisfied.
#'
#' @param attribute The person attribute's name.
#' @param action `"increment"` or `"decrement"`.
#' @param amount How much to bump by. Defaults to `1`.
#' @param label The state's name.
#' @return A fragment.
#' @examples
#' create_counter("qualifying_readings", action = "increment", label = "Count Qualifying Reading")
#' @export
create_counter <- function(
  attribute,
  action = c("increment", "decrement"),
  amount = 1,
  label
) {
  action <- match.arg(action)
  create_step(
    "Counter",
    attribute = attribute,
    action = action,
    amount = amount,
    label = label
  )
}
