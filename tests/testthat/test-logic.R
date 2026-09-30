# logic.R: create_logic_settings() -- the 21 Logic condition types.

test_that("Gender requires gender", {
  x <- create_logic_settings("Gender", gender = "F")
  expect_equal(x, list(condition_type = "Gender", gender = "F"))
  expect_error(create_logic_settings("Gender"), "missing required field")
})

test_that("Age requires quantity/unit/operator", {
  x <- create_logic_settings(
    "Age",
    quantity = 18,
    unit = "years",
    operator = ">="
  )
  expect_equal(x$condition_type, "Age")
  expect_error(
    create_logic_settings("Age", quantity = 18),
    "missing required field"
  )
})

test_that("Date requires operator and exactly one of year/month/date", {
  expect_error(create_logic_settings("Date", operator = "=="), "exactly one of")
  expect_error(
    create_logic_settings("Date", operator = "==", year = 2020, month = 1),
    "exactly one of"
  )
  x <- create_logic_settings("Date", operator = "==", year = 2020)
  expect_equal(x$year, 2020)
})

test_that("SocioeconomicStatus requires category", {
  x <- create_logic_settings("SocioeconomicStatus", category = "High")
  expect_equal(x$category, "High")
  expect_error(
    create_logic_settings("SocioeconomicStatus"),
    "missing required field"
  )
})

test_that("Race requires race", {
  x <- create_logic_settings("Race", race = "White")
  expect_equal(x$race, "White")
})

test_that("Symptom (logic) requires symptom/operator/value", {
  x <- create_logic_settings(
    "Symptom",
    symptom = "Chest Pain",
    operator = ">",
    value = 50
  )
  expect_equal(x$condition_type, "Symptom")
  expect_error(
    create_logic_settings("Symptom", symptom = "x"),
    "missing required field"
  )
})

test_that("Observation (logic) requires operator and exactly one of codes/referenced_by_attribute", {
  expect_error(
    create_logic_settings("Observation", operator = "=="),
    "exactly one of"
  )
  expect_error(
    create_logic_settings(
      "Observation",
      operator = "==",
      codes = list(a_code()),
      referenced_by_attribute = "x"
    ),
    "exactly one of"
  )
  x <- create_logic_settings(
    "Observation",
    operator = "is nil",
    referenced_by_attribute = "bp"
  )
  expect_equal(x$referenced_by_attribute, "bp")

  x2 <- create_logic_settings(
    "Observation",
    operator = ">",
    codes = list(a_code()),
    value = 5
  )
  expect_length(x2$codes, 1)
})

test_that("Attribute requires attribute/operator/value", {
  x <- create_logic_settings(
    "Attribute",
    attribute = "readings",
    operator = ">=",
    value = 2
  )
  expect_equal(x$condition_type, "Attribute")
  expect_error(
    create_logic_settings("Attribute", attribute = "x"),
    "missing required field"
  )
})

test_that("Attribute value is optional only for the is nil / is not nil operators", {
  x <- create_logic_settings("Attribute", attribute = "x", operator = "is not nil")
  expect_null(x$value)
  expect_silent(create_logic_settings("Attribute", attribute = "x", operator = "is nil"))
  expect_error(
    create_logic_settings("Attribute", attribute = "x", operator = "=="),
    "missing required field 'value'"
  )
})

test_that("And/Or wrap nested conditions as an array", {
  cond <- create_logic_settings(
    "And",
    conditions = list(
      create_logic_settings("Gender", gender = "F"),
      create_logic_settings(
        "Age",
        quantity = 40,
        unit = "years",
        operator = ">="
      )
    )
  )
  expect_equal(cond$condition_type, "And")
  expect_length(cond$conditions, 2)

  cond_or <- create_logic_settings(
    "Or",
    conditions = list(create_logic_settings("Gender", gender = "M"))
  )
  expect_equal(cond_or$condition_type, "Or")

  expect_error(
    create_logic_settings(
      "And",
      conditions = create_logic_settings("Gender", gender = "F")
    ),
    "looks like a single item"
  )
})

test_that("Not wraps exactly one condition, singular (not an array)", {
  cond <- create_logic_settings(
    "Not",
    condition = create_logic_settings("Gender", gender = "F")
  )
  expect_equal(cond$condition_type, "Not")
  expect_equal(cond$condition$condition_type, "Gender")
  expect_error(create_logic_settings("Not"), "missing required field")
})

test_that("AtLeast/AtMost require conditions + minimum/maximum", {
  al <- create_logic_settings(
    "AtLeast",
    minimum = 2,
    conditions = list(
      create_logic_settings("Gender", gender = "F"),
      create_logic_settings("Race", race = "White")
    )
  )
  expect_equal(al$minimum, 2)
  expect_error(
    create_logic_settings("AtLeast", conditions = list()),
    "missing required field"
  )

  am <- create_logic_settings(
    "AtMost",
    maximum = 1,
    conditions = list(
      create_logic_settings("Gender", gender = "F")
    )
  )
  expect_equal(am$maximum, 1)
})

test_that("True/False take no fields", {
  expect_equal(create_logic_settings("True"), list(condition_type = "True"))
  expect_equal(create_logic_settings("False"), list(condition_type = "False"))
  expect_error(create_logic_settings("True", gender = "F"), "unknown field")
})

test_that("PriorState requires name, optional since/within", {
  x <- create_logic_settings("PriorState", name = "Diagnosis")
  expect_equal(x$name, "Diagnosis")
  x2 <- create_logic_settings("PriorState", name = "Diagnosis", within = "PT1H")
  expect_equal(x2$within, "PT1H")
})

test_that("ActiveCondition/ActiveAllergy/ActiveMedication/ActiveCarePlan need exactly one of codes/referenced_by_attribute", {
  for (ct in c(
    "ActiveCondition",
    "ActiveAllergy",
    "ActiveMedication",
    "ActiveCarePlan"
  )) {
    expect_error(create_logic_settings(ct), "exactly one of", info = ct)
    ok_codes <- create_logic_settings(ct, codes = list(a_code()))
    expect_equal(ok_codes$condition_type, ct)
    ok_attr <- create_logic_settings(ct, referenced_by_attribute = "x")
    expect_equal(ok_attr$referenced_by_attribute, "x")
    expect_error(
      create_logic_settings(
        ct,
        codes = list(a_code()),
        referenced_by_attribute = "x"
      ),
      "exactly one of",
      info = ct
    )
  }
})

test_that("VitalSign (logic) requires vital_sign/operator/value", {
  x <- create_logic_settings(
    "VitalSign",
    vital_sign = "Blood Pressure Systolic",
    operator = ">",
    value = 140
  )
  expect_equal(x$condition_type, "VitalSign")
  expect_error(
    create_logic_settings("VitalSign", vital_sign = "x"),
    "missing required field"
  )
})

test_that("unknown condition_type errors", {
  expect_error(create_logic_settings("NotARealCondition"), "unknown type")
})

test_that("a field valid elsewhere but not allowed for this condition_type errors", {
  # `value` is a real create_logic_settings() argument (used by e.g. Attribute/VitalSign), but
  # not part of Gender's schema -- this is what reaches .validate_settings()'s "unknown field"
  # check, unlike a name that isn't a function argument at all (R's own "unused argument").
  expect_error(
    create_logic_settings("Gender", gender = "F", value = 1),
    "unknown field"
  )
  expect_error(
    create_logic_settings("Gender", gender = "F", bogus = 1),
    "unused argument"
  )
})
