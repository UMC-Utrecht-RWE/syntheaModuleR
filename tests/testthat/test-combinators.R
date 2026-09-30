# combinators.R: chain()/add(), pathways(), classify(), repeat_until() -- consume/produce
# fragments (list(states, entry, exit)) and wire each other's "__PENDING__" placeholders.

test_that("chain wires each fragment's exit to the next fragment's entry, in order", {
  frag <- chain(create_delay(1, 2, "days", label = "D1"), create_tag("T", attribute = "a", value = "b"))
  expect_equal(frag$entry, "D1")
  expect_equal(frag$exit, "T")
  expect_equal(frag$states$D1$direct_transition, "T")
  expect_equal(frag$states$T$direct_transition, "__PENDING__")
})

test_that("chain of three fragments wires the middle links too", {
  frag <- chain(
    create_tag("A", attribute = "x", value = 1),
    create_tag("B", attribute = "x", value = 2),
    create_tag("C", attribute = "x", value = 3)
  )
  expect_equal(frag$states$A$direct_transition, "B")
  expect_equal(frag$states$B$direct_transition, "C")
  expect_equal(frag$entry, "A")
  expect_equal(frag$exit, "C")
})

test_that("chain skips NULL fragments (an omitted create_population(), etc.)", {
  frag <- chain(NULL, create_tag("A", attribute = "x", value = 1), NULL,
                create_tag("B", attribute = "x", value = 2))
  expect_equal(frag$entry, "A")
  expect_equal(frag$exit, "B")
  expect_equal(frag$states$A$direct_transition, "B")
})

test_that("chain with a single fragment returns it unchanged", {
  one <- create_tag("A", attribute = "x", value = 1)
  expect_equal(chain(one), one)
})

test_that("chain requires at least one non-NULL fragment", {
  expect_error(chain(NULL, NULL), "at least one non-NULL fragment")
})

test_that("chain errors loudly on a duplicate state name across fragments", {
  expect_error(
    chain(create_tag("A", attribute = "x", value = 1), create_tag("A", attribute = "y", value = 2)),
    "duplicate state name"
  )
})

test_that("add() is a pipe-friendly two-argument alias for chain()", {
  a <- create_tag("A", attribute = "x", value = 1)
  b <- create_tag("B", attribute = "x", value = 2)
  expect_equal(add(a, b), chain(a, b))
})

test_that("pathways builds a distributed choice state and a join state", {
  frag <- pathways("Treatment",
    options = list(
      metformin = create_medication("Metformin", another_code(), condition = "Diabetes"),
      none = NULL
    ),
    shares = c(metformin = 0.8, none = 0.2))

  expect_equal(frag$entry, "Treatment Choice")
  expect_equal(frag$exit, "Treatment Join")
  choice <- frag$states[["Treatment Choice"]]
  expect_equal(choice$type, "Simple")
  opts <- choice$distributed_transition
  expect_length(opts, 2)
  targets <- vapply(opts, function(o) o$transition, character(1))
  expect_true("Metformin" %in% targets)
  # the NULL "none" option routes straight to the join
  expect_true("Treatment Join" %in% targets)
  # the metformin fragment's own exit was patched to route to the join
  expect_equal(frag$states$Metformin$direct_transition, "Treatment Join")
})

test_that("pathways requires every option to have a matching share", {
  expect_error(
    pathways("X", options = list(a = NULL, b = NULL), shares = c(a = 1)),
    "matching entry in `shares`"
  )
})

test_that("pathways requires options to be a named list", {
  expect_error(pathways("X", options = list(NULL, NULL), shares = c(1, 1)), "named list")
})

test_that("pathways validates terminal_options against option names", {
  expect_error(
    pathways("X", options = list(a = NULL), shares = c(a = 1), terminal_options = "b"),
    "terminal_options not found"
  )
})

test_that("pathways routes a terminal_options branch straight to Terminal, not the join", {
  frag <- pathways("X",
    options = list(dies = create_death(), survives = NULL),
    shares = c(dies = 0.1, survives = 0.9),
    terminal_options = "dies")
  expect_equal(frag$states$Death$direct_transition, "Terminal")
})

test_that("pathways' attribute argument auto-tags each option before the join", {
  frag <- pathways("X",
    options = list(a = NULL, b = NULL),
    shares = c(a = 0.5, b = 0.5),
    attribute = "arm")
  tag_a <- frag$states[["X Tag a"]]
  expect_equal(tag_a$type, "SetAttribute")
  expect_equal(tag_a$attribute, "arm")
  expect_equal(tag_a$value, "a")
  expect_equal(tag_a$direct_transition, "X Join")

  choice_targets <- vapply(frag$states[["X Choice"]]$distributed_transition, function(o) o$transition, character(1))
  expect_true("X Tag a" %in% choice_targets)
  expect_true("X Tag b" %in% choice_targets)
})

test_that("classify requires exactly 2 named options", {
  cond <- create_logic_settings("True")
  expect_error(classify("X", cond, options = list(a = NULL)), "exactly 2 entries")
  expect_error(classify("X", cond, options = list(a = NULL, b = NULL, c = NULL)), "exactly 2 entries")
})

test_that("classify routes the first option when true, the second as the else fallback", {
  cond <- create_logic_settings("ActiveMedication", codes = list(another_code()))
  frag <- classify("Treatment Status", condition = cond,
    options = list(
      treated = create_tag("Treated", attribute = "cohort", value = "treated"),
      untreated = create_tag("Untreated", attribute = "cohort", value = "untreated")
    ))
  expect_equal(frag$entry, "Treatment Status Check")
  expect_equal(frag$exit, "Treatment Status Join")
  check <- frag$states[["Treatment Status Check"]]
  opts <- check$conditional_transition
  expect_length(opts, 2)
  expect_false(is.null(opts[[1]]$condition))
  expect_null(opts[[2]]$condition)
  expect_equal(opts[[1]]$transition, "Treated")
  expect_equal(opts[[2]]$transition, "Untreated")
  expect_equal(frag$states$Treated$direct_transition, "Treatment Status Join")
})

test_that("classify supports NULL options and terminal_options like pathways", {
  cond <- create_logic_settings("True")
  frag <- classify("X", cond, options = list(dies = create_death(), lives = NULL),
                    terminal_options = "dies")
  expect_equal(frag$states$Death$direct_transition, "Terminal")
  opts <- frag$states[["X Check"]]$conditional_transition
  expect_equal(opts[[2]]$transition, "X Join")
})

test_that("repeat_until wires the loop back to body$entry until the condition passes", {
  body <- chain(create_delay(90, 90, "days"),
                create_counter("readings", action = "increment", label = "Count"))
  until <- create_logic_settings("Attribute", attribute = "readings", operator = ">=", value = 2)
  frag <- repeat_until(body, until = until)

  expect_equal(frag$entry, "Delay")
  expect_equal(frag$exit, "Loop Continue")
  expect_equal(frag$states$Count$direct_transition, "Loop Until Check")

  check <- frag$states[["Loop Until Check"]]
  expect_equal(check$type, "Simple")
  opts <- check$conditional_transition
  expect_equal(opts[[1]]$condition, until)
  expect_equal(opts[[1]]$transition, "Loop Continue")
  expect_equal(opts[[2]]$transition, "Delay")
  expect_null(opts[[2]]$condition)

  expect_equal(frag$states[["Loop Continue"]]$direct_transition, "__PENDING__")
})

test_that("repeat_until respects a custom label", {
  body <- create_tag("A", attribute = "x", value = 1)
  frag <- repeat_until(body, until = create_logic_settings("True"), label = "Retry")
  expect_equal(frag$exit, "Retry Continue")
  expect_true("Retry Until Check" %in% names(frag$states))
})

# Integration: combinators + build_cohort_module produce a fully valid module ----

test_that("a pathways()-based cohort with a death branch validates end-to-end", {
  frag <- create_condition("Diabetes", another_code(), diagnosis = "wellness") |>
    add(pathways("First-Line",
      options = list(
        metformin = create_medication("Metformin", another_code(), condition = "Diabetes"),
        none = NULL
      ),
      shares = c(metformin = 0.8, none = 0.2))) |>
    add(pathways("Outcome",
      options = list(dies = create_death(condition = "Diabetes"), survives = NULL),
      shares = c(dies = 0.05, survives = 0.95),
      terminal_options = "dies"))

  m <- build_cohort_module("Diabetes Cohort", frag, as_json = FALSE)
  expect_true(validate_module(m))
  expect_true("Death" %in% names(m$states))
  expect_equal(m$states$Death$direct_transition, "Terminal")
})

test_that("a repeat_until() loop composed into a full module validates", {
  body <- chain(create_delay(90, 90, "days"),
                create_counter("readings", action = "increment", label = "Count Reading"))
  loop <- repeat_until(body, until = create_logic_settings("Attribute", attribute = "readings",
                                                             operator = ">=", value = 3))
  m <- build_cohort_module("Counting Loop", loop, as_json = FALSE)
  expect_true(validate_module(m))
  expect_equal(m$states[["Loop Continue"]]$direct_transition, "Terminal")
})

test_that("a classify()-based branch composed into a full module validates", {
  cond <- create_logic_settings("ActiveMedication", codes = list(another_code()))
  frag <- classify("Treatment Status", condition = cond,
    options = list(
      treated = create_tag("Treated", attribute = "cohort", value = "treated"),
      untreated = create_tag("Untreated", attribute = "cohort", value = "untreated")
    ))
  m <- build_cohort_module("Classified Cohort", frag, as_json = FALSE)
  expect_true(validate_module(m))
})
