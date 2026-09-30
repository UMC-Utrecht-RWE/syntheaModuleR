# states.R: create_state_settings() -- all 31 State types, plus the transition
# required/optional/forbidden rules layered on top of the shared schema machinery.

# Minimal valid required-field values for every state type, keyed by `type`. Used to build one
# state of each type in a single sweep, so every type is touched at least once.
minimal_required_fields <- list(
  Initial = list(),
  Terminal = list(),
  Simple = list(),
  CallSubmodule = list(submodule = "Some Submodule"),
  Physiology = list(model = "model.xml", solver = "rk4", step_size = 0.1, sim_duration = 10,
                     alt_direct_transition = "Terminal"),
  Guard = list(allow = create_logic_settings("True")),
  Delay = list(exact = create_component_settings("exact", quantity = 1, unit = "days")),
  SetAttribute = list(attribute = "x"),
  Counter = list(attribute = "x", action = "increment"),
  Encounter = list(),
  EncounterEnd = list(),
  ConditionOnset = list(codes = list(a_code())),
  ConditionEnd = list(),
  AllergyOnset = list(codes = list(a_code())),
  AllergyEnd = list(),
  MedicationOrder = list(codes = list(a_code())),
  MedicationEnd = list(),
  CarePlanStart = list(codes = list(a_code())),
  CarePlanEnd = list(),
  Procedure = list(codes = list(a_code())),
  VitalSign = list(vital_sign = "Blood Pressure Systolic",
                    exact = create_component_settings("exact", quantity = 120)),
  Observation = list(codes = list(a_code())),
  MultiObservation = list(codes = list(a_code()),
                           observations = list(create_state_settings("Observation", codes = list(a_code())))),
  DiagnosticReport = list(codes = list(a_code()),
                           observations = list(create_state_settings("Observation", codes = list(a_code())))),
  ImagingStudy = list(procedure_code = a_code(),
                       series = list(list(modality = "US", body_site = "trunk", instances = 1))),
  Symptom = list(symptom = "Chest Pain"),
  Device = list(code = a_code()),
  DeviceEnd = list(),
  SupplyList = list(supplies = list(list(code = a_code(), quantity = 1))),
  Death = list(),
  Vaccine = list(series = 1, codes = list(a_code()))
)

test_that("every state type in .state_schema has a required-fields fixture", {
  schema_types <- names(syntheaModuleR:::.state_schema)
  expect_setequal(names(minimal_required_fields), schema_types)
})

test_that("every state type builds successfully with minimal required fields", {
  for (type in names(minimal_required_fields)) {
    args <- minimal_required_fields[[type]]
    if (!identical(type, "Terminal")) {
      args$transition <- create_transition_settings("direct", to = "Terminal")
    }
    st <- do.call(create_state_settings, c(list(type = type), args))
    expect_equal(st$type, type, info = type)
  }
})

test_that("missing a required field errors for every type that has one", {
  for (type in names(minimal_required_fields)) {
    args <- minimal_required_fields[[type]]
    if (length(args) == 0) next
    dropped <- args[-1]
    if (!identical(type, "Terminal")) {
      dropped$transition <- create_transition_settings("direct", to = "Terminal")
    }
    expect_error(
      do.call(create_state_settings, c(list(type = type), dropped)),
      "missing required field|exactly one of",
      info = type
    )
  }
})

test_that("unknown state type errors", {
  expect_error(create_state_settings("NotAType"), "unknown type")
})

test_that("a field valid elsewhere but not allowed for this type errors", {
  # `attribute` is a real create_state_settings() argument (used by e.g. SetAttribute/Counter),
  # but not part of Simple's schema -- this is what reaches .validate_settings()'s "unknown
  # field" check, unlike a name that isn't a function argument at all (R's own "unused argument").
  expect_error(
    create_state_settings("Simple", attribute = "x",
                           transition = create_transition_settings("direct", to = "Terminal")),
    "unknown field"
  )
  expect_error(
    create_state_settings("Simple", bogus_field = 1,
                           transition = create_transition_settings("direct", to = "Terminal")),
    "unused argument"
  )
})

# Transition required/optional/forbidden ----

test_that("Terminal must not have a transition", {
  expect_error(
    create_state_settings("Terminal", transition = create_transition_settings("direct", to = "X")),
    "must not have a transition"
  )
  x <- create_state_settings("Terminal")
  expect_null(x$direct_transition)
})

test_that("non-Observation, non-Terminal states require a transition", {
  expect_error(create_state_settings("Simple"), "a transition is required")
  expect_error(create_state_settings("ConditionOnset", codes = list(a_code())),
               "a transition is required")
})

test_that("Observation's transition is optional (nested-use case)", {
  x <- create_state_settings("Observation", codes = list(a_code()))
  expect_null(x$direct_transition)
  x2 <- create_state_settings("Observation", codes = list(a_code()),
                               transition = create_transition_settings("direct", to = "Terminal"))
  expect_equal(x2$direct_transition, "Terminal")
})

# one_of constraints ----

test_that("Delay requires exactly one of range/exact/distribution", {
  expect_error(
    create_state_settings("Delay", transition = create_transition_settings("direct", to = "Terminal")),
    "exactly one of"
  )
  expect_error(
    create_state_settings("Delay", range = a_range(), exact = create_component_settings("exact", quantity = 1),
                           transition = create_transition_settings("direct", to = "Terminal")),
    "exactly one of"
  )
  ok <- create_state_settings("Delay", range = a_range(),
                               transition = create_transition_settings("direct", to = "Terminal"))
  expect_equal(ok$range, a_range())

  ok2 <- create_state_settings("Delay",
    distribution = create_component_settings("distribution", kind = "UNIFORM", low = 1, high = 3),
    unit = "days", transition = create_transition_settings("direct", to = "Terminal"))
  expect_equal(ok2$distribution$kind, "UNIFORM")
})

test_that("Procedure allows at most one of duration/distribution (neither required)", {
  ok_neither <- create_state_settings("Procedure", codes = list(a_code()),
    transition = create_transition_settings("direct", to = "Terminal"))
  expect_null(ok_neither$duration)

  ok_one <- create_state_settings("Procedure", codes = list(a_code()), duration = a_range(),
    transition = create_transition_settings("direct", to = "Terminal"))
  expect_equal(ok_one$duration, a_range())

  expect_error(
    create_state_settings("Procedure", codes = list(a_code()), duration = a_range(),
      distribution = create_component_settings("distribution", kind = "EXACT", value = 1),
      transition = create_transition_settings("direct", to = "Terminal")),
    "at most one of"
  )
})

test_that("VitalSign requires exactly one of exact/range/expression/distribution", {
  expect_error(
    create_state_settings("VitalSign", vital_sign = "Blood Pressure Systolic",
                           transition = create_transition_settings("direct", to = "Terminal")),
    "exactly one of"
  )
  ok <- create_state_settings("VitalSign", vital_sign = "Blood Pressure Systolic",
    range = a_range(low = 110, high = 130, unit = "mmHg"),
    transition = create_transition_settings("direct", to = "Terminal"))
  expect_equal(ok$vital_sign, "Blood Pressure Systolic")
})

# Field-name reuse and the Device/DeviceEnd asymmetry ----

test_that("Device takes a singular `code`, DeviceEnd takes a plural `codes` list", {
  dev <- create_state_settings("Device", code = a_code(),
    transition = create_transition_settings("direct", to = "Terminal"))
  expect_equal(dev$code, a_code())
  expect_null(dev$codes)

  dev_end <- create_state_settings("DeviceEnd", codes = list(a_code()),
    transition = create_transition_settings("direct", to = "Terminal"))
  expect_length(dev_end$codes, 1)
})

test_that("`model` means different things for Physiology vs Device", {
  phys <- create_state_settings("Physiology", model = "cardiac.xml", solver = "rk4",
    step_size = 0.1, sim_duration = 10, alt_direct_transition = "Terminal",
    transition = create_transition_settings("direct", to = "Terminal"))
  expect_equal(phys$model, "cardiac.xml")

  dev <- create_state_settings("Device", code = a_code(), model = "Model-100",
    transition = create_transition_settings("direct", to = "Terminal"))
  expect_equal(dev$model, "Model-100")
})

test_that("`series` means different things for ImagingStudy (list) vs Vaccine (dose number)", {
  img <- create_state_settings("ImagingStudy", procedure_code = a_code(),
    series = list(list(modality = "US", body_site = "trunk", instances = 1)),
    transition = create_transition_settings("direct", to = "Terminal"))
  expect_length(img$series, 1)

  vax <- create_state_settings("Vaccine", series = 2, codes = list(a_code()),
    transition = create_transition_settings("direct", to = "Terminal"))
  expect_equal(vax$series, 2)
})

test_that("Physiology inputs/outputs accept io_mapper components as an array", {
  mapper <- create_component_settings("io_mapper", type = "ATTRIBUTE", from = "x", to = "y")
  phys <- create_state_settings("Physiology", model = "m", solver = "rk4", step_size = 0.1,
    sim_duration = 10, alt_direct_transition = "Terminal", inputs = list(mapper),
    transition = create_transition_settings("direct", to = "Terminal"))
  expect_length(phys$inputs, 1)
})
