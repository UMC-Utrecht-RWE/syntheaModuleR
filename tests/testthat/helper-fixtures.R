# Shared fixtures reused across test files. testthat auto-sources helper-*.R before running tests.

a_code <- function(code = "38341003", display = "Hypertension", system = "SNOMED-CT") {
  create_component_settings("code", system = system, code = code, display = display)
}

another_code <- function() {
  create_component_settings("code",
    system = "RxNorm", code = "860975",
    display = "Metformin hydrochloride 500 MG Oral Tablet"
  )
}

a_range <- function(low = 1, high = 5, unit = "days") {
  create_component_settings("range", low = low, high = high, unit = unit)
}

# A minimal, valid Initial -> Terminal states list, usable directly with build_module().
minimal_states <- function() {
  list(
    Initial = create_state_settings("Initial",
      transition = create_transition_settings("direct", to = "Terminal")
    ),
    Terminal = create_state_settings("Terminal")
  )
}

# Fetch a field from within a module's states list regardless of as_json TRUE/FALSE.
module_states <- function(module_json_or_list) {
  if (is.character(module_json_or_list)) {
    parsed <- jsonlite::fromJSON(module_json_or_list, simplifyVector = FALSE)
    parsed$states
  } else {
    module_json_or_list$states
  }
}
