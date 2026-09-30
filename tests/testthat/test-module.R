# module.R: build_module(), validate_module(), write_module_json(), build_disease_module(),
# build_cohort_module().

# build_module() ----

test_that("build_module returns pretty JSON by default, round-trips to the same states", {
  json <- build_module("Minimal Example", minimal_states())
  expect_type(json, "character")
  parsed <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  expect_equal(parsed$name, "Minimal Example")
  expect_equal(parsed$gmf_version, 2)
  expect_true("Initial" %in% names(parsed$states))
  expect_true("Terminal" %in% names(parsed$states))
})

test_that("build_module(as_json = FALSE) returns the R list instead", {
  lst <- build_module("Minimal Example", minimal_states(), as_json = FALSE)
  expect_type(lst, "list")
  expect_equal(lst$name, "Minimal Example")
  expect_equal(lst$states$Initial$type, "Initial")
})

test_that("build_module records specialty/remarks/gmf_version when given", {
  lst <- build_module("X", minimal_states(),
    specialty = "Cardiology",
    remarks = c("note one", "note two"), gmf_version = 3, as_json = FALSE
  )
  expect_equal(lst$specialty, "Cardiology")
  expect_equal(lst$remarks, list("note one", "note two"))
  expect_equal(lst$gmf_version, 3)
})

test_that("build_module requires a non-empty name", {
  expect_error(build_module("", minimal_states()))
  expect_error(build_module(character(0), minimal_states()))
})

test_that("build_module requires a named states list", {
  expect_error(build_module("X", list(minimal_states()[[1]])), "named list")
  unnamed <- minimal_states()
  names(unnamed) <- c("", "Terminal")
  expect_error(build_module("X", unnamed), "named list")
})

test_that("build_module runs validate_module by default and propagates its errors", {
  bad_states <- list(Terminal = create_state_settings("Terminal"))
  expect_error(build_module("X", bad_states), "Initial")
})

test_that("build_module(validate = FALSE) skips validation", {
  bad_states <- list(Terminal = create_state_settings("Terminal"))
  lst <- build_module("X", bad_states, validate = FALSE, as_json = FALSE)
  expect_equal(lst$name, "X")
})

# validate_module() ----

test_that("validate_module accepts a minimal valid module", {
  lst <- build_module("X", minimal_states(), as_json = FALSE, validate = FALSE)
  expect_true(validate_module(lst))
})

test_that("validate_module rejects a module with no states", {
  expect_error(validate_module(list(states = list())), "no states")
  expect_error(validate_module(list()), "no states")
})

test_that("validate_module requires exactly one state named 'Initial'", {
  states <- minimal_states()
  states[["Initial"]] <- NULL
  expect_error(validate_module(list(states = states)), "exactly one state named 'Initial'")

  dup <- minimal_states()
  # A named list can carry duplicate names in R -- exercise that edge case directly.
  dup2 <- c(dup, list(Initial = dup$Initial))
  expect_error(validate_module(list(states = dup2)), "exactly one state named 'Initial'")
})

test_that("validate_module requires at least one Terminal-typed state", {
  states <- list(Initial = create_state_settings("Initial",
    transition = create_transition_settings("direct", to = "Initial")
  ))
  expect_error(validate_module(list(states = states)), "no state of type 'Terminal'")
})

test_that("validate_module requires every state to have a type", {
  states <- minimal_states()
  states$Initial$type <- NULL
  expect_error(validate_module(list(states = states)), "has no 'type'")
})

test_that("validate_module requires exactly one transition property per non-Terminal state", {
  states <- minimal_states()
  states$Initial$direct_transition <- NULL
  expect_error(validate_module(list(states = states)), "no transition property")

  states2 <- minimal_states()
  states2$Initial$distributed_transition <- list(list(transition = "Terminal", distribution = 1))
  expect_error(validate_module(list(states = states2)), "more than one transition property")
})

test_that("validate_module catches undefined state-name references across every reference kind", {
  base <- function() minimal_states()

  s <- base()
  s$Initial$direct_transition <- "Nowhere"
  expect_error(validate_module(list(states = s)), "Nowhere")

  s <- base()
  s$Initial$distributed_transition <- list(list(transition = "Nowhere", distribution = 1))
  s$Initial$direct_transition <- NULL
  expect_error(validate_module(list(states = s)), "Nowhere")

  s <- base()
  s$Initial$conditional_transition <- list(list(transition = "Nowhere"))
  s$Initial$direct_transition <- NULL
  expect_error(validate_module(list(states = s)), "Nowhere")

  s <- base()
  s$Initial$complex_transition <- list(list(transition = "Nowhere"))
  s$Initial$direct_transition <- NULL
  expect_error(validate_module(list(states = s)), "Nowhere")

  s <- base()
  s$Initial$complex_transition <- list(list(distributions = list(list(transition = "Nowhere", distribution = 1))))
  s$Initial$direct_transition <- NULL
  expect_error(validate_module(list(states = s)), "Nowhere")

  s <- base()
  s$Initial$target_encounter <- "Nowhere"
  expect_error(validate_module(list(states = s)), "Nowhere")

  s <- base()
  s$Initial$condition_onset <- "Nowhere"
  expect_error(validate_module(list(states = s)), "Nowhere")

  s <- base()
  s$Initial$medication_order <- "Nowhere"
  expect_error(validate_module(list(states = s)), "Nowhere")

  s <- base()
  s$Initial$careplan <- "Nowhere"
  expect_error(validate_module(list(states = s)), "Nowhere")

  s <- base()
  s$Initial$allergy_onset <- "Nowhere"
  expect_error(validate_module(list(states = s)), "Nowhere")

  s <- base()
  s$Initial$device <- "Nowhere"
  expect_error(validate_module(list(states = s)), "Nowhere")
})

test_that("validate_module passes a graph exercising every transition kind's references", {
  states <- list(
    Initial = create_state_settings("Initial",
      transition = create_transition_settings("direct", to = "Onset")
    ),
    Onset = create_state_settings("ConditionOnset",
      codes = list(a_code()), target_encounter = "Enc",
      transition = create_transition_settings("direct", to = "Enc")
    ),
    Enc = create_state_settings("Encounter",
      wellness = TRUE,
      transition = create_transition_settings("distributed", options = list(
        list(transition = "Med", distribution = 0.5),
        list(transition = "Device1", distribution = 0.5)
      ))
    ),
    Med = create_state_settings("MedicationOrder",
      codes = list(another_code()), reason = "Onset",
      transition = create_transition_settings("conditional", options = list(
        list(condition = create_logic_settings("True"), transition = "Care"),
        list(transition = "Device1")
      ))
    ),
    Care = create_state_settings("CarePlanStart",
      codes = list(a_code()),
      transition = create_transition_settings("complex", options = list(
        list(condition = create_logic_settings("True"), transition = "Device1"),
        list(distributions = list(
          list(transition = "Device1", distribution = 1)
        ))
      ))
    ),
    Device1 = create_state_settings("Device",
      code = a_code(),
      transition = create_transition_settings("direct", to = "DeviceEnd1")
    ),
    DeviceEnd1 = create_state_settings("DeviceEnd",
      device = "Device1",
      transition = create_transition_settings("direct", to = "AllergyOnset1")
    ),
    AllergyOnset1 = create_state_settings("AllergyOnset",
      codes = list(a_code()),
      transition = create_transition_settings("direct", to = "Terminal")
    ),
    Terminal = create_state_settings("Terminal")
  )
  expect_true(validate_module(list(states = states)))
})

# write_module_json() ----

test_that("write_module_json writes a JSON-string module and creates parent directories", {
  json <- build_module("X", minimal_states())
  path <- file.path(tempdir(), "sub", "dir", "module.json")
  on.exit(unlink(dirname(dirname(path)), recursive = TRUE), add = TRUE)
  result <- write_module_json(json, path)
  expect_equal(result, path)
  expect_true(file.exists(path))
  round_tripped <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  expect_equal(round_tripped$name, "X")
})

test_that("write_module_json accepts an R-list module and re-validates it", {
  lst <- build_module("X", minimal_states(), as_json = FALSE)
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  write_module_json(lst, path)
  round_tripped <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  expect_equal(round_tripped$name, "X")

  bad <- lst
  bad$states$Initial <- NULL
  expect_error(write_module_json(bad, tempfile(fileext = ".json")), "Initial")
})

test_that("write_module_json rejects anything else", {
  expect_error(write_module_json(42, tempfile()), "must be a JSON string or a module list")
})

# build_disease_module() ----

test_that("build_disease_module builds a minimal wellness-diagnosed chronic condition module", {
  code <- a_code()
  m <- build_disease_module("Hypertension", condition_code = code, diagnosis = "wellness")
  expect_true(validate_module(m))
  expect_true("Condition Onset" %in% names(m$states))
  expect_equal(m$states[["Condition Onset"]]$codes[[1]], code)
  expect_true(isTRUE(m$states[["Diagnosis Encounter"]]$wellness))
  expect_equal(m$states[["End Encounter"]]$direct_transition, "Terminal")
})

test_that("build_disease_module standalone diagnosis requires encounter_class", {
  expect_error(
    build_disease_module("X", condition_code = a_code(), diagnosis = "standalone"),
    "encounter_class is required"
  )
  m <- build_disease_module("X",
    condition_code = a_code(), diagnosis = "standalone",
    encounter_class = "ambulatory"
  )
  expect_equal(m$states[["Diagnosis Encounter"]]$encounter_class, "ambulatory")
})

test_that("build_disease_module wires an age_gate and onset_delay in front of onset", {
  m <- build_disease_module("X",
    condition_code = a_code(), diagnosis = "wellness",
    age_gate = list(operator = ">=", quantity = 40, unit = "years"),
    onset_delay = list(low = 1, high = 5, unit = "years")
  )
  expect_true(validate_module(m))
  expect_equal(m$states[["Initial"]]$direct_transition, "Age Gate")
  expect_equal(m$states[["Age Gate"]]$type, "Guard")
  expect_equal(m$states[["Age Gate"]]$direct_transition, "Onset Delay")
  expect_equal(m$states[["Onset Delay"]]$direct_transition, "Condition Onset")
})

test_that("build_disease_module wires resolves_after into a Delay -> ConditionEnd tail", {
  m <- build_disease_module("X",
    condition_code = a_code(), diagnosis = "wellness",
    resolves_after = list(low = 6, high = 12, unit = "months")
  )
  expect_true(validate_module(m))
  expect_equal(m$states[["End Encounter"]]$direct_transition, "Condition Resolves Delay")
  expect_equal(m$states[["Condition Resolves"]]$type, "ConditionEnd")
  expect_equal(m$states[["Condition Resolves"]]$condition_onset, "Condition Onset")
  expect_equal(m$states[["Condition Resolves"]]$direct_transition, "Terminal")
})

test_that("build_disease_module wires a death branch as a distributed transition off EncounterEnd", {
  m <- build_disease_module("X",
    condition_code = a_code(), diagnosis = "wellness",
    death = list(probability = 0.3)
  )
  expect_true(validate_module(m))
  opts <- m$states[["End Encounter"]]$distributed_transition
  expect_length(opts, 2)
  targets <- vapply(opts, function(o) o$transition, character(1))
  expect_true("Death" %in% targets)
  expect_equal(m$states[["Death"]]$condition_onset, "Condition Onset")
})

test_that("build_disease_module validates the death probability is in (0, 1]", {
  expect_error(
    build_disease_module("X",
      condition_code = a_code(), diagnosis = "wellness",
      death = list(probability = 0)
    )
  )
  expect_error(
    build_disease_module("X",
      condition_code = a_code(), diagnosis = "wellness",
      death = list(probability = 1.5)
    )
  )
  expect_true(validate_module(
    build_disease_module("X",
      condition_code = a_code(), diagnosis = "wellness",
      death = list(probability = 1)
    )
  ))
})

test_that("build_disease_module accepts an explicit death code instead of condition_onset", {
  death_code <- create_component_settings("code",
    system = "SNOMED-CT", code = "1",
    display = "Death by X"
  )
  m <- build_disease_module("X",
    condition_code = a_code(), diagnosis = "wellness",
    death = list(probability = 0.1, code = death_code)
  )
  expect_equal(m$states[["Death"]]$codes[[1]], death_code)
  expect_null(m$states[["Death"]]$condition_onset)
})

# build_cohort_module() ----

test_that("build_cohort_module wraps a fragment into a full Initial/Terminal module", {
  frag <- create_condition("Diabetes", another_code(), diagnosis = "wellness")
  json <- build_cohort_module("Diabetes Cohort", frag)
  parsed <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  expect_equal(parsed$name, "Diabetes Cohort")
  expect_equal(parsed$states$Initial$direct_transition, "Diabetes")
  expect_true("Terminal" %in% names(parsed$states))
})

test_that("build_cohort_module(as_json = FALSE) returns an R list and can skip validation", {
  frag <- create_tag("Tagged", attribute = "x", value = "y")
  lst <- build_cohort_module("X", frag, as_json = FALSE)
  expect_true(validate_module(lst))
  expect_equal(lst$states[["Tagged"]]$direct_transition, "Terminal")
})
