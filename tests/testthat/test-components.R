# components.R: create_component_settings() + the shared internal helpers
# (.validate_settings / .apply_array_rule / .build_settings) every layer reuses.

test_that("code component requires system, code, display", {
  x <- create_component_settings("code",
    system = "SNOMED-CT", code = "38341003",
    display = "Hypertension"
  )
  expect_mapequal(x, list(system = "SNOMED-CT", code = "38341003", display = "Hypertension"))

  expect_error(
    create_component_settings("code", system = "SNOMED-CT"),
    "missing required field"
  )
})

test_that("code component rejects a field valid elsewhere but not allowed on 'code'", {
  # `low` is a real create_component_settings() argument (used by "range"), but not part of
  # the "code" schema -- this is what actually reaches .validate_settings()'s "unknown field"
  # check. A name that isn't a function argument at all fails earlier, with R's own error
  # (see the "unused argument" test below).
  expect_error(
    create_component_settings("code", system = "SNOMED-CT", code = "1", display = "x", low = 1),
    "unknown field"
  )
})

test_that("passing a name that isn't a create_component_settings() argument at all is R's own error", {
  expect_error(
    create_component_settings("code", system = "SNOMED-CT", code = "1", display = "x", bogus = "y"),
    "unused argument"
  )
})

test_that("range component: required low/high, optional unit/decimals", {
  x <- create_component_settings("range", low = 1, high = 10)
  expect_mapequal(x, list(low = 1, high = 10))

  x2 <- create_component_settings("range", low = 1, high = 10, unit = "days", decimals = 2)
  expect_equal(x2$unit, "days")
  expect_equal(x2$decimals, 2)

  expect_error(create_component_settings("range", low = 1), "missing required field")
})

test_that("exact component: required quantity, optional unit", {
  x <- create_component_settings("exact", quantity = 5)
  expect_mapequal(x, list(quantity = 5))
  x2 <- create_component_settings("exact", quantity = 5, unit = "mmHg")
  expect_equal(x2$unit, "mmHg")
})

test_that("date_input component: required year/month/day, optional time parts", {
  x <- create_component_settings("date_input", year = 2020, month = 1, day = 15)
  expect_equal(x$year, 2020)
  expect_error(
    create_component_settings("date_input", year = 2020, month = 1),
    "missing required field"
  )
})

test_that("sampled_data component: required origin_value/attributes (array)", {
  x <- create_component_settings("sampled_data",
    origin_value = 0,
    attributes = list("a", "b")
  )
  expect_equal(x$attributes, list("a", "b"))
  expect_error(
    create_component_settings("sampled_data", attributes = list("a")),
    "missing required field"
  )
})

test_that("attachment component: exactly one of chart/url/data", {
  expect_error(
    create_component_settings("attachment"),
    "exactly one of"
  )
  expect_error(
    create_component_settings("attachment", url = "http://x", data = "abc"),
    "exactly one of"
  )
  x <- create_component_settings("attachment", url = "http://example.com/img.png")
  expect_equal(x$url, "http://example.com/img.png")
})

test_that("io_mapper component: required type, optional from/to/etc", {
  x <- create_component_settings("io_mapper", type = "ATTRIBUTE", from = "a", to = "b")
  expect_mapequal(x, list(type = "ATTRIBUTE", from = "a", to = "b"))
  expect_error(create_component_settings("io_mapper"), "missing required field")
})

test_that("unknown component selector errors", {
  expect_error(create_component_settings("not_a_component"), "unknown type")
})

# Distribution -- special-cased: `kind` selects which `parameters` keys are required/optional,
# and those keys are NOT snake_cased (parameters is a HashMap<String, Double>).
test_that("distribution EXACT kind requires value", {
  x <- create_component_settings("distribution", kind = "EXACT", value = 42)
  expect_equal(x, list(kind = "EXACT", parameters = list(value = 42)))
})

test_that("distribution UNIFORM kind requires low/high", {
  x <- create_component_settings("distribution", kind = "UNIFORM", low = 1, high = 5)
  expect_equal(x$kind, "UNIFORM")
  expect_mapequal(x$parameters, list(low = 1, high = 5))
  expect_error(
    create_component_settings("distribution", kind = "UNIFORM", low = 1),
    "missing required field"
  )
})

test_that("distribution GAUSSIAN kind keeps standardDeviation camelCase and accepts min/max", {
  x <- create_component_settings("distribution",
    kind = "GAUSSIAN", mean = 10,
    standardDeviation = 2, min = 0, max = 20
  )
  expect_mapequal(x$parameters, list(mean = 10, standardDeviation = 2, min = 0, max = 20))
  expect_true("standardDeviation" %in% names(x$parameters))

  expect_error(
    create_component_settings("distribution", kind = "GAUSSIAN", mean = 10),
    "missing required field"
  )
})

test_that("distribution EXPONENTIAL kind requires mean only", {
  x <- create_component_settings("distribution", kind = "EXPONENTIAL", mean = 5)
  expect_mapequal(x$parameters, list(mean = 5))
})

test_that("distribution TRIANGULAR kind requires min/mode/max", {
  x <- create_component_settings("distribution", kind = "TRIANGULAR", min = 1, mode = 2, max = 3)
  expect_mapequal(x$parameters, list(min = 1, mode = 2, max = 3))
  expect_error(
    create_component_settings("distribution", kind = "TRIANGULAR", min = 1, mode = 2),
    "missing required field"
  )
})

test_that("distribution: round is optional and top-level, not in parameters", {
  x <- create_component_settings("distribution", kind = "EXACT", value = 1, round = TRUE)
  expect_true(x$round)
  expect_false("round" %in% names(x$parameters))
})

test_that("distribution: invalid kind errors", {
  expect_error(
    create_component_settings("distribution", kind = "BOGUS", value = 1),
    "kind must be one of"
  )
  expect_error(
    create_component_settings("distribution", value = 1),
    "kind must be one of"
  )
})

test_that("distribution: unknown parameter for a kind errors", {
  expect_error(
    create_component_settings("distribution", kind = "EXACT", value = 1, mode = 2),
    "unknown field"
  )
})

# Internal helpers, exercised directly (not exported, but the toolkit's shared foundation).
test_that(".validate_settings enforces required/optional/unknown/one_of", {
  entry <- list(
    required = "a", optional = "b",
    one_of = list(list(fields = c("c", "d"), required = TRUE))
  )
  expect_true(syntheaModuleR:::.validate_settings(entry, list(a = 1, c = 1), "x"))
  expect_error(syntheaModuleR:::.validate_settings(entry, list(b = 1), "x"), "missing required")
  expect_error(syntheaModuleR:::.validate_settings(entry, list(a = 1, z = 1), "x"), "unknown field")
  expect_error(syntheaModuleR:::.validate_settings(entry, list(a = 1), "x"), "exactly one of")
  expect_error(
    syntheaModuleR:::.validate_settings(entry, list(a = 1, c = 1, d = 1), "x"),
    "exactly one of"
  )
})

test_that(".validate_settings one_of required = FALSE allows zero or one, not more", {
  entry <- list(
    required = c(), optional = c("c", "d"),
    one_of = list(list(fields = c("c", "d"), required = FALSE))
  )
  expect_true(syntheaModuleR:::.validate_settings(entry, list(), "x"))
  expect_true(syntheaModuleR:::.validate_settings(entry, list(c = 1), "x"))
  expect_error(
    syntheaModuleR:::.validate_settings(entry, list(c = 1, d = 1), "x"),
    "at most one of"
  )
})

test_that(".apply_array_rule requires a list, rejects a bare named-list item, unwraps names", {
  entry <- list(array = "codes")
  ok <- syntheaModuleR:::.apply_array_rule(entry, list(codes = list(a_code())))
  expect_null(names(ok$codes))

  expect_error(
    syntheaModuleR:::.apply_array_rule(entry, list(codes = a_code())),
    "looks like a single item"
  )
  expect_error(
    syntheaModuleR:::.apply_array_rule(entry, list(codes = "not-a-list")),
    "must be a list"
  )
})

test_that("the array rule is enforced end-to-end through create_state_settings", {
  expect_error(
    create_state_settings("ConditionOnset",
      codes = a_code(),
      transition = create_transition_settings("direct", to = "Terminal")
    ),
    "looks like a single item"
  )
  ok <- create_state_settings("ConditionOnset",
    codes = list(a_code()),
    transition = create_transition_settings("direct", to = "Terminal")
  )
  expect_length(ok$codes, 1)
})
