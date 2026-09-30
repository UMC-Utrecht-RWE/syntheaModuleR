# transitions.R: create_transition_settings() -- the 6 Transition kinds. Unlike the other
# layers, the JSON key itself (direct_transition/distributed_transition/...) is the
# discriminator, so these tests check the returned key name as well as its contents.

test_that("direct transition returns direct_transition = to", {
  x <- create_transition_settings("direct", to = "Terminal")
  expect_equal(x, list(direct_transition = "Terminal"))
  expect_error(create_transition_settings("direct"), "missing required field")
})

test_that("distributed transition wraps options with transition/distribution", {
  x <- create_transition_settings(
    "distributed",
    options = list(
      list(transition = "A", distribution = 0.6),
      list(transition = "B", distribution = 0.4)
    )
  )
  expect_named(x, "distributed_transition")
  expect_length(x$distributed_transition, 2)
  expect_equal(x$distributed_transition[[1]]$transition, "A")
  expect_equal(x$distributed_transition[[1]]$distribution, 0.6)
})

test_that("distributed transition supports a NamedDistribution (attribute + default)", {
  x <- create_transition_settings(
    "distributed",
    options = list(
      list(
        transition = "A",
        distribution = list(attribute = "share_a", default = 0.5)
      ),
      list(transition = "B", distribution = 0.5)
    )
  )
  expect_equal(
    x$distributed_transition[[1]]$distribution,
    list(attribute = "share_a", default = 0.5)
  )
})

test_that("distributed transition options require transition and distribution", {
  expect_error(
    create_transition_settings(
      "distributed",
      options = list(list(distribution = 1))
    ),
    "each option needs 'transition'"
  )
  expect_error(
    create_transition_settings(
      "distributed",
      options = list(list(transition = "A"))
    ),
    "each option needs 'distribution'"
  )
})

test_that("conditional transition: condition present except on the fallback/else option", {
  cond <- create_logic_settings("Gender", gender = "F")
  x <- create_transition_settings(
    "conditional",
    options = list(
      list(condition = cond, transition = "A"),
      list(transition = "B")
    )
  )
  expect_named(x, "conditional_transition")
  expect_true(!is.null(x$conditional_transition[[1]]$condition))
  expect_null(x$conditional_transition[[2]]$condition)
})

test_that("conditional transition options require transition", {
  expect_error(
    create_transition_settings(
      "conditional",
      options = list(list(condition = create_logic_settings("True")))
    ),
    "each option needs 'transition'"
  )
})

test_that("complex transition: each option is either a direct transition or a nested distributed one", {
  cond <- create_logic_settings("Gender", gender = "F")
  x <- create_transition_settings(
    "complex",
    options = list(
      list(condition = cond, transition = "A"),
      list(
        distributions = list(
          list(transition = "B", distribution = 0.5),
          list(transition = "C", distribution = 0.5)
        )
      )
    )
  )
  expect_named(x, "complex_transition")
  expect_equal(x$complex_transition[[1]]$transition, "A")
  expect_length(x$complex_transition[[2]]$distributions, 2)
})

test_that("complex transition option needs exactly one of transition/distributions", {
  expect_error(
    create_transition_settings(
      "complex",
      options = list(list(condition = create_logic_settings("True")))
    ),
    "exactly one of"
  )
  expect_error(
    create_transition_settings(
      "complex",
      options = list(list(
        transition = "A",
        distributions = list(list(transition = "B", distribution = 1))
      ))
    ),
    "exactly one of"
  )
})

test_that("lookup_table transition requires lookup_table_name and default_probability per option", {
  x <- create_transition_settings(
    "lookup_table",
    lookup_table_name = "risk.csv",
    options = list(
      list(transition = "A", default_probability = 0.5),
      list(transition = "B", default_probability = 0.5)
    )
  )
  expect_named(x, "lookup_table_transition")
  expect_equal(x$lookup_table_transition[[1]]$lookup_table_name, "risk.csv")

  expect_error(
    create_transition_settings(
      "lookup_table",
      lookup_table_name = "risk.csv",
      options = list(list(transition = "A"))
    ),
    "each option needs 'transition' and 'default_probability'"
  )
})

test_that("type_of_care transition requires ambulatory/telemedicine/emergency", {
  x <- create_transition_settings(
    "type_of_care",
    ambulatory = "A",
    telemedicine = "T",
    emergency = "E"
  )
  expect_equal(
    x,
    list(
      type_of_care_transition = list(
        ambulatory = "A",
        telemedicine = "T",
        emergency = "E"
      )
    )
  )
  expect_error(
    create_transition_settings("type_of_care", ambulatory = "A"),
    "missing required field"
  )
})

test_that("unknown transition kind errors", {
  expect_error(create_transition_settings("bogus"), "unknown kind")
})

test_that("array-rule violation surfaces for transition options", {
  expect_error(
    create_transition_settings(
      "distributed",
      options = list(transition = "A", distribution = 1)
    ),
    "looks like a single item"
  )
})

test_that("option-list transitions reject an empty options list", {
  for (kind in c("distributed", "conditional", "complex")) {
    expect_error(
      create_transition_settings(kind, options = list()),
      "'options' must not be empty"
    )
  }
  expect_error(
    create_transition_settings(
      "lookup_table",
      lookup_table_name = "risk.csv",
      options = list()
    ),
    "'options' must not be empty"
  )
})

test_that("conditional/complex only allow a conditionless option in last position", {
  cond <- create_logic_settings("True")
  expect_error(
    create_transition_settings(
      "conditional",
      options = list(list(transition = "A"), list(condition = cond, transition = "B"))
    ),
    "only the last option may omit 'condition'"
  )
  expect_error(
    create_transition_settings(
      "complex",
      options = list(list(transition = "A"), list(condition = cond, transition = "B"))
    ),
    "only the last option may omit 'condition'"
  )
})

test_that("complex nested distributions are checked like distributed options", {
  expect_error(
    create_transition_settings(
      "complex",
      options = list(list(distributions = list(list(transition = "A"))))
    ),
    "each option needs 'distribution'"
  )
  expect_error(
    create_transition_settings(
      "complex",
      options = list(list(distributions = list()))
    ),
    "options must not be empty"
  )
  expect_error(
    create_transition_settings(
      "complex",
      options = list(list(distributions = list(transition = "A", distribution = 1)))
    ),
    "looks like a single item"
  )
})
