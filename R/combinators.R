# combinators.R -- the Frontend layer's combinators: chain()/add() (sequence), pathways()
# (probabilistic split-and-rejoin), classify() (condition-driven split-and-rejoin), repeat_until()
# (the loop combinator). Each takes one or more fragments and returns a new fragment -- the
# fragment contract (list(states, entry, exit), exit's direct_transition left pending) never
# changes no matter which combinator produced it. See the plan at validated-floating-garden.md.

#' Merge several fragments' `states`, erroring on any state-name collision
#' @param fragments A list of fragments (already `NULL`-filtered).
#' @return A single merged, named list of states.
.merge_fragment_states <- function(fragments) {
  states <- list()
  for (frag in fragments) {
    dup <- intersect(names(states), names(frag$states))
    if (length(dup) > 0) {
      stop(sprintf("duplicate state name across fragments: %s", paste(dup, collapse = ", ")))
    }
    states <- c(states, frag$states)
  }
  states
}

#' Sequentially compose fragments
#'
#' Wires each fragment's `exit` state's `direct_transition` to the next fragment's `entry`. `NULL`
#' fragments are skipped, so an omitted `create_population()` just disappears.
#'
#' @param ... Two or more fragments (or `NULL`s, skipped).
#' @return A fragment: `entry` = the first fragment's `entry`, `exit` = the last fragment's `exit`.
#' @examples
#' chain(create_delay(6, 18, "months"), create_tag("Done", attribute = "x", value = "y"))
#' @export
chain <- function(...) {
  fragments <- Filter(Negate(is.null), list(...))
  if (length(fragments) == 0) stop("chain(): at least one non-NULL fragment is required")
  if (length(fragments) == 1) return(fragments[[1]])

  states <- .merge_fragment_states(fragments)
  for (i in seq_len(length(fragments) - 1)) {
    exit_name <- fragments[[i]]$exit
    states[[exit_name]]$direct_transition <- fragments[[i + 1]]$entry
  }
  list(states = states, entry = fragments[[1]]$entry, exit = fragments[[length(fragments)]]$exit)
}

#' Extend a fragment by one more fragment (pipe-friendly alias for `chain()`)
#'
#' Same function as `chain()`'s two-argument case; exists purely for reading direction --
#' `chain(a, b, c)` reads as "assemble these fresh pieces together," while
#' `cohort <- cohort |> add(create_medication(...))` reads as "extend the thing I already have."
#'
#' @param fragment The fragment to extend.
#' @param nxt The fragment to append.
#' @return A fragment, per `chain()`.
#' @examples
#' create_condition("Diabetes", create_component_settings("code", system = "SNOMED-CT",
#'   code = "44054006", display = "Diabetes")) |>
#'   add(create_tag("Diagnosed", attribute = "seen", value = TRUE))
#' @export
add <- function(fragment, nxt) {
  chain(fragment, nxt)
}

#' Build one option's routing target, patching its fragment's exit if it has one
#'
#' Shared by `pathways()`/`classify()`: handles the optional `attribute` auto-tag, the
#' `terminal_options` early-exit-to-`"Terminal"` routing, and merging/patching a non-`NULL`
#' option's fragment. Returns the states to add and the transition target this option should
#' route to from the choice/check state.
#' @param option_name The option's name (used as the auto-tag value and looked up in
#'   `terminal_options`).
#' @param fragment The option's fragment, or `NULL` for "nothing happens."
#' @param combinator_name Used to derive the tag state's name (`"<combinator_name> Tag
#'   <option_name>"`).
#' @param attribute,terminal_options,join_label As in `pathways()`/`classify()`.
#' @param states_so_far States merged so far, for the collision check.
#' @return `list(states, target)` -- `states` are the new states this option contributes
#'   (tag state, if any, plus the option's own fragment states), `target` is where the choice/
#'   check state should route this option to.
.route_option <- function(option_name, fragment, combinator_name, attribute, terminal_options,
                           join_label, states_so_far) {
  is_terminal <- option_name %in% terminal_options
  after_tag <- if (is_terminal) "Terminal" else join_label

  new_states <- list()
  if (!is.null(attribute)) {
    tag_label <- paste0(combinator_name, " Tag ", option_name)
    new_states[[tag_label]] <- create_state_settings("SetAttribute", attribute = attribute, value = option_name,
      transition = create_transition_settings("direct", to = after_tag))
    entry_target <- tag_label
  } else {
    entry_target <- after_tag
  }

  if (is.null(fragment)) {
    return(list(states = new_states, target = entry_target))
  }

  dup <- intersect(names(states_so_far), names(fragment$states))
  if (length(dup) > 0) {
    stop(sprintf("duplicate state name: %s", paste(dup, collapse = ", ")))
  }
  fragment$states[[fragment$exit]]$direct_transition <- entry_target
  new_states <- c(new_states, fragment$states)
  list(states = new_states, target = fragment$entry)
}

#' Split into probability-weighted pathways, then rejoin
#'
#' "Some get A, some get B, some get nothing." Builds one new `distributed_transition` "choice"
#' state routing into each option, and one new "join" state every non-terminal option rejoins
#' into.
#'
#' @param name Used to derive the choice/join/tag state names.
#' @param options A named list of fragments; an option can be `NULL` for "nothing happens on this
#'   pathway."
#' @param shares A named numeric vector matching `options`' names.
#' @param attribute Optional -- auto-tags the chosen option's name (as this `attribute`) before
#'   the join.
#' @param terminal_options Optional character vector naming options (e.g. a `create_death()`
#'   pathway) that route straight to `"Terminal"` instead of the join.
#' @return A fragment: `entry` = the choice state, `exit` = the join state.
#' @examples
#' pathways("First-Line Treatment",
#'   options = list(metformin = create_medication("Metformin",
#'                    create_component_settings("code", system = "RxNorm", code = "860975",
#'                                               display = "Metformin"),
#'                    condition = "Diabetes", duration = "long"),
#'                  none = NULL),
#'   shares = c(metformin = 0.8, none = 0.2))
#' @export
pathways <- function(name, options, shares, attribute = NULL, terminal_options = NULL) {
  opt_names <- names(options)
  if (is.null(opt_names) || any(!nzchar(opt_names))) {
    stop("pathways(): `options` must be a named list")
  }
  if (!all(opt_names %in% names(shares))) {
    stop("pathways(): every name in `options` must have a matching entry in `shares`")
  }
  if (!is.null(terminal_options) && !all(terminal_options %in% opt_names)) {
    stop(sprintf("pathways(): terminal_options not found in options: %s",
                 paste(setdiff(terminal_options, opt_names), collapse = ", ")))
  }

  choice_label <- paste0(name, " Choice")
  join_label <- paste0(name, " Join")

  states <- list()
  choice_options <- list()
  for (opt_name in opt_names) {
    routed <- .route_option(opt_name, options[[opt_name]], name, attribute, terminal_options,
                             join_label, states)
    states <- c(states, routed$states)
    choice_options[[length(choice_options) + 1]] <- list(transition = routed$target,
                                                           distribution = unname(shares[[opt_name]]))
  }

  states[[choice_label]] <- create_state_settings("Simple",
    transition = create_transition_settings("distributed", options = choice_options))
  states[[join_label]] <- create_state_settings("Simple",
    transition = create_transition_settings("direct", to = "__PENDING__"))

  list(states = states, entry = choice_label, exit = join_label)
}

#' Split by a `Logic` condition, then rejoin
#'
#' Same split-and-rejoin shape as `pathways()`, but `condition`-driven via
#' `conditional_transition` instead of probability-driven -- sort a patient into exactly one of
#' `options` based on a fact that's true about them *right now*, rather than trusting a tag set
#' earlier in the pathway (which can go stale).
#'
#' @param name Used to derive the check/join/tag state names.
#' @param condition A `create_logic_settings()` result.
#' @param options A named list of exactly 2 fragments (an option can be `NULL`). The **first**
#'   entry is taken when `condition` is true; the **second** is the `else` fallback.
#' @param attribute Optional -- auto-tags the chosen option's name before the join.
#' @param terminal_options Optional character vector naming options that route straight to
#'   `"Terminal"` instead of the join.
#' @return A fragment: `entry` = the check state, `exit` = the join state.
#' @examples
#' on_med <- create_logic_settings("ActiveMedication",
#'   codes = list(create_component_settings("code", system = "RxNorm", code = "860975",
#'                                           display = "Metformin")))
#' classify("Treatment Status", condition = on_med,
#'   options = list(treated = create_tag("Treated", attribute = "cohort", value = "treated"),
#'                  untreated = create_tag("Untreated", attribute = "cohort", value = "untreated")))
#' @export
classify <- function(name, condition, options, attribute = NULL, terminal_options = NULL) {
  opt_names <- names(options)
  if (length(options) != 2 || is.null(opt_names) || any(!nzchar(opt_names))) {
    stop("classify(): `options` must be a named list of exactly 2 entries -- the first is taken ",
         "when `condition` is true, the second is the else fallback")
  }
  if (!is.null(terminal_options) && !all(terminal_options %in% opt_names)) {
    stop(sprintf("classify(): terminal_options not found in options: %s",
                 paste(setdiff(terminal_options, opt_names), collapse = ", ")))
  }

  check_label <- paste0(name, " Check")
  join_label <- paste0(name, " Join")

  states <- list()
  cond_options <- vector("list", 2)
  for (i in seq_along(options)) {
    opt_name <- opt_names[i]
    routed <- .route_option(opt_name, options[[i]], name, attribute, terminal_options, join_label, states)
    states <- c(states, routed$states)
    cond_options[[i]] <- if (i == 1) list(condition = condition, transition = routed$target)
                         else list(transition = routed$target)
  }

  states[[check_label]] <- create_state_settings("Simple",
    transition = create_transition_settings("conditional", options = cond_options))
  states[[join_label]] <- create_state_settings("Simple",
    transition = create_transition_settings("direct", to = "__PENDING__"))

  list(states = states, entry = check_label, exit = join_label)
}

#' Repeat a fragment until a condition is satisfied (do-while), then continue
#'
#' `chain()`/`pathways()`/`classify()` only ever build forward (a DAG); this is the one combinator
#' that wires a state's transition back to an earlier one. Runs `body` once, checks `until`, and
#' only stops once it's satisfied -- e.g. occurrence-counting eligibility ("N qualifying readings")
#' built with `create_counter()` + an `Attribute` `Logic` condition.
#'
#' @param body A fragment, run once per iteration.
#' @param until A `create_logic_settings()` result; the loop exits once this is true.
#' @param label Used to derive the check/continue state names. Defaults to `"Loop"`.
#' @return A fragment: `entry` = `body$entry`, `exit` = the new "Continue" state. The fragment
#'   contract never changes -- `chain()`/`pathways()`/`classify()` accept this like any other
#'   fragment.
#' @examples
#' body <- chain(create_delay(90, 90, "days"),
#'                create_counter("readings", action = "increment", label = "Count"))
#' repeat_until(body, until = create_logic_settings("Attribute", attribute = "readings",
#'                                                   operator = ">=", value = 2))
#' @export
repeat_until <- function(body, until, label = "Loop") {
  check_label <- paste0(label, " Until Check")
  continue_label <- paste0(label, " Continue")

  states <- body$states
  states[[body$exit]]$direct_transition <- check_label
  states[[check_label]] <- create_state_settings("Simple",
    transition = create_transition_settings("conditional", options = list(
      list(condition = until, transition = continue_label),
      list(transition = body$entry)
    )))
  states[[continue_label]] <- create_state_settings("Simple",
    transition = create_transition_settings("direct", to = "__PENDING__"))

  list(states = states, entry = body$entry, exit = continue_label)
}
