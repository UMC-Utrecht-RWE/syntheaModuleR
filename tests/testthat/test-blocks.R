# blocks.R: the Frontend layer's leaf constructors. Every leaf returns a fragment --
# list(states, entry, exit) -- with its exit state's direct_transition left as "__PENDING__".

expect_fragment <- function(frag, entry = NULL, exit = NULL) {
  expect_type(frag, "list")
  expect_named(frag, c("states", "entry", "exit"))
  expect_true(is.list(frag$states))
  expect_true(all(nzchar(names(frag$states))))
  if (!is.null(entry)) {
    expect_equal(frag$entry, entry)
  }
  if (!is.null(exit)) {
    expect_equal(frag$exit, exit)
  }
  expect_equal(frag$states[[frag$exit]]$direct_transition, "__PENDING__")
}

test_that("create_step wraps any state type as a one-state fragment", {
  frag <- create_step(
    "Symptom",
    symptom = "Chest Pain",
    label = "Chest Pain Symptom"
  )
  expect_fragment(
    frag,
    entry = "Chest Pain Symptom",
    exit = "Chest Pain Symptom"
  )
  expect_equal(frag$states[["Chest Pain Symptom"]]$type, "Symptom")
})

test_that("create_delay builds a Delay fragment from low/high/unit", {
  frag <- create_delay(6, 18, "months")
  expect_fragment(frag, entry = "Delay", exit = "Delay")
  expect_equal(frag$states$Delay$type, "Delay")
  expect_equal(
    frag$states$Delay$range,
    create_component_settings("range", low = 6, high = 18, unit = "months")
  )
})

test_that("create_delay respects a custom label", {
  frag <- create_delay(1, 2, "days", label = "Wait a bit")
  expect_fragment(frag, entry = "Wait a bit", exit = "Wait a bit")
})

test_that("create_guard wraps a Logic condition in a Guard fragment", {
  frag <- create_guard(create_logic_settings(
    "Age",
    operator = ">=",
    quantity = 18,
    unit = "years"
  ))
  expect_fragment(frag, entry = "Guard", exit = "Guard")
  expect_equal(frag$states$Guard$allow$condition_type, "Age")
})

test_that("create_population combines given demographic filters with And", {
  frag <- create_population(
    "Adults",
    age = list(operator = ">=", quantity = 18, unit = "years"),
    gender = "F"
  )
  expect_fragment(frag, entry = "Adults", exit = "Adults")
  expect_equal(frag$states$Adults$allow$condition_type, "And")
  expect_length(frag$states$Adults$allow$conditions, 2)
})

test_that("create_population with a single filter skips the And wrapper", {
  frag <- create_population("Women", gender = "F")
  expect_equal(frag$states$Women$allow$condition_type, "Gender")
})

test_that("create_population returns NULL when every filter is omitted", {
  expect_null(create_population("Nobody"))
})

test_that("create_population validates socioeconomic category", {
  expect_error(create_population("X", socioeconomic = "Extreme"))
  frag <- create_population("X", socioeconomic = "Low")
  expect_equal(frag$states$X$allow$category, "Low")
})

test_that("create_population adds a Date filter", {
  frag <- create_population(
    "Older Adults From 2023",
    age = list(operator = ">=", quantity = 60, unit = "years"),
    date = list(operator = ">=", year = 2023)
  )
  conds <- frag$states$`Older Adults From 2023`$allow$conditions
  expect_length(conds, 2)
  expect_equal(conds[[2]]$condition_type, "Date")
  expect_equal(conds[[2]]$year, 2023)
})

test_that("create_condition builds onset -> encounter -> encounter-end (wellness)", {
  frag <- create_condition("Diabetes", another_code(), diagnosis = "wellness")
  expect_fragment(frag, entry = "Diabetes", exit = "Diabetes Encounter End")
  expect_equal(frag$states$Diabetes$type, "ConditionOnset")
  expect_equal(frag$states$Diabetes$target_encounter, "Diabetes Encounter")
  expect_true(isTRUE(frag$states[["Diabetes Encounter"]]$wellness))
})

test_that("create_condition standalone diagnosis requires encounter_class", {
  expect_error(
    create_condition("X", a_code(), diagnosis = "standalone"),
    "encounter_class"
  )
  frag <- create_condition(
    "X",
    a_code(),
    diagnosis = "standalone",
    encounter_class = "ambulatory"
  )
  expect_equal(frag$states[["X Encounter"]]$encounter_class, "ambulatory")
})

test_that("create_condition wires an onset_delay in front and resolves_after behind", {
  frag <- create_condition(
    "X",
    a_code(),
    diagnosis = "wellness",
    onset_delay = list(low = 1, high = 3, unit = "months"),
    resolves_after = list(low = 6, high = 12, unit = "months")
  )
  expect_equal(frag$entry, "X Onset Delay")
  expect_equal(frag$states[["X Onset Delay"]]$direct_transition, "X")
  expect_equal(frag$exit, "X Resolves")
  expect_equal(frag$states[["X Resolves"]]$type, "ConditionEnd")
  expect_equal(frag$states[["X Resolves"]]$condition_onset, "X")
})

test_that("create_medication defaults to chronic (long) with no end", {
  frag <- create_medication("Metformin", another_code(), condition = "Diabetes")
  expect_fragment(frag, entry = "Metformin", exit = "Metformin")
  expect_true(frag$states$Metformin$chronic)
  expect_equal(frag$states$Metformin$reason, "Diabetes")
})

test_that("create_medication short duration appends a course Delay -> MedicationEnd", {
  frag <- create_medication(
    "Metformin",
    another_code(),
    condition = "Diabetes",
    duration = "short",
    course = list(low = 30, high = 30, unit = "days")
  )
  expect_equal(frag$entry, "Metformin")
  expect_equal(frag$exit, "Metformin Course End")
  expect_false(frag$states$Metformin$chronic)
  expect_equal(frag$states[["Metformin Course End"]]$type, "MedicationEnd")
  expect_equal(
    frag$states[["Metformin Course End"]]$medication_order,
    "Metformin"
  )
})

test_that("create_medication short duration requires course", {
  expect_error(
    create_medication("X", a_code(), condition = "Y", duration = "short"),
    "course"
  )
})

test_that("create_discontinue is a bare MedicationEnd", {
  frag <- create_discontinue("Stop Metformin", medication = "Metformin")
  expect_fragment(frag)
  expect_equal(frag$states[["Stop Metformin"]]$type, "MedicationEnd")
  expect_equal(frag$states[["Stop Metformin"]]$medication_order, "Metformin")
})

test_that("create_observation sets both the exact component and the top-level unit", {
  frag <- create_observation("HbA1c", a_code(), value = 8.5, unit = "%")
  expect_fragment(frag)
  expect_equal(
    frag$states$HbA1c$exact,
    create_component_settings("exact", quantity = 8.5, unit = "%")
  )
  expect_equal(frag$states$HbA1c$unit, "%")
})

test_that("create_observation supports value_code instead of a numeric value", {
  frag <- create_observation("Result", a_code(), value_code = a_code())
  expect_equal(frag$states$Result$value_code, a_code())
  expect_null(frag$states$Result$exact)
})

test_that("create_vital_sign mirrors create_observation's value convenience", {
  frag <- create_vital_sign(
    "High Systolic",
    "Blood Pressure Systolic",
    value = 145,
    unit = "mmHg"
  )
  expect_fragment(frag)
  expect_equal(
    frag$states[["High Systolic"]]$vital_sign,
    "Blood Pressure Systolic"
  )
  expect_equal(frag$states[["High Systolic"]]$exact$quantity, 145)
})

test_that("create_procedure supports an optional condition reason and length", {
  frag <- create_procedure(
    "Appendectomy",
    a_code(),
    condition = "Appendicitis",
    length = list(low = 30, high = 60, unit = "minutes")
  )
  expect_fragment(frag)
  expect_equal(frag$states$Appendectomy$reason, "Appendicitis")
  expect_equal(
    frag$states$Appendectomy$duration,
    create_component_settings("range", low = 30, high = 60, unit = "minutes")
  )
})

test_that("create_procedure omits duration when length is not given", {
  frag <- create_procedure("Appendectomy", a_code())
  expect_null(frag$states$Appendectomy$duration)
})

test_that("create_death supports condition, codes, and a delay", {
  frag <- create_death(
    condition = "Diabetes",
    after = list(low = 1, high = 10, unit = "years")
  )
  expect_fragment(frag, entry = "Death", exit = "Death")
  expect_equal(frag$states$Death$condition_onset, "Diabetes")
  expect_equal(
    frag$states$Death$range,
    create_component_settings("range", low = 1, high = 10, unit = "years")
  )
})

test_that("create_encounter builds an Encounter/EncounterEnd pair", {
  frag <- create_encounter("Annual Checkup")
  expect_fragment(frag, entry = "Annual Checkup", exit = "Annual Checkup End")
  expect_true(isTRUE(frag$states[["Annual Checkup"]]$wellness))
})

test_that("create_encounter standalone requires encounter_class", {
  frag <- create_encounter(
    "Visit",
    wellness = FALSE,
    encounter_class = "emergency"
  )
  expect_null(frag$states$Visit$wellness)
  expect_equal(frag$states$Visit$encounter_class, "emergency")
})

test_that("create_allergy without resolves_after is a single-state fragment", {
  frag <- create_allergy("Penicillin Allergy", a_code())
  expect_fragment(
    frag,
    entry = "Penicillin Allergy",
    exit = "Penicillin Allergy"
  )
})

test_that("create_allergy with resolves_after appends Delay -> AllergyEnd", {
  frag <- create_allergy(
    "Penicillin Allergy",
    a_code(),
    resolves_after = list(low = 1, high = 2, unit = "years")
  )
  expect_equal(frag$entry, "Penicillin Allergy")
  expect_equal(frag$exit, "Penicillin Allergy Resolves")
  expect_equal(
    frag$states[["Penicillin Allergy Resolves"]]$allergy_onset,
    "Penicillin Allergy"
  )
})

test_that("create_careplan mirrors create_allergy's optional resolution", {
  frag <- create_careplan("Diabetes Care Plan", a_code())
  expect_fragment(
    frag,
    entry = "Diabetes Care Plan",
    exit = "Diabetes Care Plan"
  )

  frag2 <- create_careplan(
    "Diabetes Care Plan",
    a_code(),
    resolves_after = list(low = 6, high = 12, unit = "months")
  )
  expect_equal(frag2$exit, "Diabetes Care Plan Resolves")
  expect_equal(
    frag2$states[["Diabetes Care Plan Resolves"]]$type,
    "CarePlanEnd"
  )
})

test_that("create_vaccine defaults series to dose 1", {
  frag <- create_vaccine("First Dose", a_code())
  expect_fragment(frag)
  expect_equal(frag$states[["First Dose"]]$series, 1)

  frag2 <- create_vaccine("Second Dose", a_code(), series = 2)
  expect_equal(frag2$states[["Second Dose"]]$series, 2)
})

test_that("create_tag is a bare SetAttribute", {
  frag <- create_tag("Treated", attribute = "cohort", value = "treated")
  expect_fragment(frag)
  expect_equal(frag$states$Treated$attribute, "cohort")
  expect_equal(frag$states$Treated$value, "treated")
})

test_that("create_counter defaults amount to 1 and validates action", {
  frag <- create_counter("readings", action = "increment", label = "Count")
  expect_fragment(frag)
  expect_equal(frag$states$Count$amount, 1)
  expect_equal(frag$states$Count$action, "increment")

  frag2 <- create_counter(
    "readings",
    action = "decrement",
    amount = 5,
    label = "Uncount"
  )
  expect_equal(frag2$states$Uncount$amount, 5)

  expect_error(create_counter("x", action = "bogus", label = "Y"))
})
