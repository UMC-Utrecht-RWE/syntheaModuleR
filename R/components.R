# components.R -- shared value-shape "Component" settings (Components.java / Distribution.java /
# IoMapper.java) plus the internal helpers reused by every layer's create_*_settings().
#
# create_component_settings(component, ...) is this layer's single entry point. Unlike
# states/logic, component objects carry no self-describing discriminator field in the JSON
# itself (a Code is just {system, code, display}) -- `component` is purely an R-side selector
# and never appears in the returned list.

# Shared internal helpers, reused by every layer's create_*_settings() ----

#' Validate collected fields against a schema entry
#'
#' Checks a named list of fields (typically collected from a `create_*_settings()` call)
#' against one schema entry's required/optional/one_of rules, erroring on unknown fields,
#' missing required fields, or a violated one_of constraint.
#'
#' @param entry A schema entry: `list(required = c(...), optional = c(...), one_of = list(...))`.
#'   `one_of` is an optional list of `list(fields = c(...), required = TRUE/FALSE)`; `required =
#'   TRUE` means exactly one of `fields` must be present, `required = FALSE` means at most one
#'   may be present.
#' @param fields A named list collected from the caller's `...`/arguments, with `NULL` entries
#'   already dropped.
#' @param label A short description of the calling context, used to prefix error messages (e.g.
#'   `"component 'code'"`).
#' @return `TRUE`, invisibly, if `fields` satisfies `entry`; otherwise stops with a descriptive
#'   error.
#' @examples
#' syntheaModuleR:::.validate_settings(
#'   list(required = "system", optional = c("code", "display")),
#'   list(system = "SNOMED-CT"),
#'   "component 'code'"
#' )
.validate_settings <- function(entry, fields, label) {
  allowed <- unique(c(entry$required, entry$optional, unlist(lapply(entry$one_of, `[[`, "fields"))))
  given <- names(fields)
  if (is.null(given)) given <- character(0)

  unknown <- setdiff(given, allowed)
  if (length(unknown) > 0) {
    stop(sprintf("%s: unknown field(s): %s (allowed: %s)",
                 label, paste(unknown, collapse = ", "), paste(allowed, collapse = ", ")))
  }
  missing_req <- setdiff(entry$required, given)
  if (length(missing_req) > 0) {
    stop(sprintf("%s: missing required field(s): %s", label, paste(missing_req, collapse = ", ")))
  }
  for (grp in entry$one_of) {
    present <- intersect(grp$fields, given)
    if (isTRUE(grp$required) && length(present) != 1) {
      stop(sprintf("%s: exactly one of {%s} must be given (got: %s)", label,
                   paste(grp$fields, collapse = ", "),
                   if (length(present) == 0) "none" else paste(present, collapse = ", ")))
    }
    if (!isTRUE(grp$required) && length(present) > 1) {
      stop(sprintf("%s: at most one of {%s} may be given (got: %s)", label,
                   paste(grp$fields, collapse = ", "), paste(present, collapse = ", ")))
    }
  }
  invisible(TRUE)
}

#' Enforce the toolkit's array rule on a set of fields
#'
#' Every field named in `entry$array` must always be a plain (unnamed) list of items, even for
#' one item -- never a bare single item. This is enforced with an error rather than silently
#' auto-wrapped, because a single item is itself usually a named list (e.g. a Code has names
#' system/code/display) and so is indistinguishable from a length-1 list of items except by
#' checking whether the outer list itself has names.
#'
#' @param entry A schema entry; only its `array` character vector (field names that must be
#'   lists) is used here.
#' @param fields A named list collected from the caller's `...`/arguments.
#' @return `fields`, with every field named in `entry$array` coerced to an unnamed list.
#' @examples
#' syntheaModuleR:::.apply_array_rule(
#'   list(array = "codes"),
#'   list(codes = list(list(system = "SNOMED-CT", code = "38341003", display = "Hypertension")))
#' )
.apply_array_rule <- function(entry, fields) {
  for (nm in entry$array) {
    val <- fields[[nm]]
    if (is.null(val)) next
    if (!is.list(val)) {
      stop(sprintf("field '%s' must be a list (array rule) -- got %s", nm, class(val)[1]))
    }
    val_names <- names(val)
    if (!is.null(val_names) && any(nzchar(val_names))) {
      stop(sprintf(paste0("field '%s' looks like a single item (a named list), not a list of ",
                           "items -- per the array rule, wrap it: %s = list(<that item>)"), nm, nm))
    }
    fields[[nm]] <- unname(val)
  }
  fields
}

#' Assemble one validated settings object
#'
#' Shared body for `create_state_settings()`/`create_logic_settings()`/
#' `create_component_settings()`: validates `fields` against `schema[[key]]`, applies the array
#' rule, optionally prepends a discriminator field (e.g. `type = key`), and returns the
#' assembled list.
#'
#' @param schema One of the layer schema tables (e.g. `.state_schema`, `.logic_schema`,
#'   `.component_schema`), keyed by type/condition_type/component name.
#' @param key The schema key to validate `fields` against (e.g. `"ConditionOnset"`).
#' @param fields A named list collected from the caller's `...`/arguments, with `NULL` entries
#'   already dropped.
#' @param label A short description of the calling layer, used to prefix error messages (e.g.
#'   `"state"`, `"logic"`, `"component"`).
#' @param discriminator_field If not `NULL`, the name of a field to prepend with value `key`
#'   (e.g. `"type"` for states, `"condition_type"` for logic conditions).
#' @return A plain named list, ready to nest inside a state/logic/transition settings call.
#' @examples
#' syntheaModuleR:::.build_settings(
#'   list(code = list(required = c("system", "code", "display"), optional = c())),
#'   "code",
#'   list(system = "SNOMED-CT", code = "38341003", display = "Hypertension"),
#'   label = "component"
#' )
.build_settings <- function(schema, key, fields, label, discriminator_field = NULL) {
  entry <- schema[[key]]
  if (is.null(entry)) {
    stop(sprintf("%s: unknown type '%s'. Valid types: %s",
                 label, key, paste(names(schema), collapse = ", ")))
  }
  .validate_settings(entry, fields, sprintf("%s '%s'", label, key))
  fields <- .apply_array_rule(entry, fields)

  if (!is.null(discriminator_field)) {
    result <- list(key)
    names(result) <- discriminator_field
    result <- c(result, fields)
  } else {
    result <- fields
  }
  result
}

# Component schema ----

.component_schema <- list(
  code = list(required = c("system", "code", "display"), optional = c()),

  range = list(required = c("low", "high"), optional = c("unit", "decimals")),

  exact = list(required = c("quantity"), optional = c("unit")),

  date_input = list(required = c("year", "month", "day"),
                     optional = c("hour", "minute", "second", "millisecond")),

  sampled_data = list(required = c("origin_value", "attributes"),
                       optional = c("factor", "lower_limit", "upper_limit", "decimal_format"),
                       array = c("attributes")),

  attachment = list(required = c(),
                     optional = c("content_type", "language", "title", "creation",
                                  "height", "width", "frames", "duration", "pages",
                                  "chart", "url", "data"),
                     one_of = list(list(fields = c("chart", "url", "data"), required = TRUE))),

  # Distribution (Distribution.java) is handled specially in create_component_settings(): `kind`
  # selects which parameter keys (inside the `parameters` map) are required/optional, per
  # Distribution.validate(). This entry only covers the two flat fields.
  distribution = list(required = c("kind"), optional = c("round")),

  io_mapper = list(required = c("type"),
                    optional = c("from", "to", "from_list", "from_exp", "variance", "vital_sign"))
)

# Distribution.Kind-specific parameter requirements (Distribution.java validate()/generate()).
# IMPORTANT: these parameter names live inside a `HashMap<String, Double> parameters` field,
# which Gson does NOT subject to the LOWER_CASE_WITH_UNDERSCORES field-naming policy (that
# policy only rewrites declared Java field names, not arbitrary map keys) -- so
# "standardDeviation" must stay camelCase here, unlike every other field name in this toolkit.
.distribution_param_schema <- list(
  EXACT       = list(required = c("value"),                      optional = c()),
  UNIFORM     = list(required = c("low", "high"),                 optional = c()),
  GAUSSIAN    = list(required = c("mean", "standardDeviation"),    optional = c("min", "max")),
  EXPONENTIAL = list(required = c("mean"),                         optional = c()),
  TRIANGULAR  = list(required = c("min", "mode", "max"),           optional = c())
)

#' Build a single "Component" value shape: Code, Range, Exact, DateInput, SampledData,
#' Attachment, a Distribution, or an IoMapper's core fields.
#'
#' Every field used by any component shape -- plus every Distribution.Kind-specific `parameters`
#' key (mean, standardDeviation, min, max, mode, value; low/high are shared with Range) -- is a
#' named argument here (all default NULL).
#'
#' @param component One of "code", "range", "exact", "date_input", "sampled_data",
#'   "attachment", "distribution", "io_mapper".
#' @param kind For component = "distribution" only: selects a Distribution.Kind
#'   ("EXACT"/"UNIFORM"/"GAUSSIAN"/"EXPONENTIAL"/"TRIANGULAR" -- must stay exact-case, Gson
#'   serializes enums by name()). `round` is optional; every other argument relevant to that
#'   Kind (`value` / `low,high` / `mean,standardDeviation` with optional `min,max` / `mean` /
#'   `min,mode,max`) becomes a `parameters` entry, validated against that Kind's
#'   required/optional keys.
#' @param attributes,chart,code,content_type,creation,data,day,decimal_format,decimals,display,duration,factor,frames,from,from_exp,from_list,height,high,hour,language,low,lower_limit,max,mean,millisecond,min,minute,mode,month,origin_value,pages,quantity,round,second,standardDeviation,system,title,to,type,unit,upper_limit,url,value,variance,vital_sign,width,year
#'   Field values for whichever `component` (and, for `component = "distribution"`, `kind`)
#'   was selected -- see `.component_schema`/`.distribution_param_schema` for which fields apply
#'   to which selector, or `vignette("component-reference")` for the per-shape table.
#' @return A plain named list, ready to nest inside a state/logic/transition settings call.
#' @examples
#' create_component_settings("code", system = "SNOMED-CT", code = "38341003",
#'                            display = "Hypertension")
#'
#' create_component_settings("distribution", kind = "UNIFORM", low = 1, high = 5)
#' @export
create_component_settings <- function(component,
                                       attributes = NULL,
                                       chart = NULL,
                                       code = NULL,
                                       content_type = NULL,
                                       creation = NULL,
                                       data = NULL,
                                       day = NULL,
                                       decimal_format = NULL,
                                       decimals = NULL,
                                       display = NULL,
                                       duration = NULL,
                                       factor = NULL,
                                       frames = NULL,
                                       from = NULL,
                                       from_exp = NULL,
                                       from_list = NULL,
                                       height = NULL,
                                       high = NULL,
                                       hour = NULL,
                                       kind = NULL,
                                       language = NULL,
                                       low = NULL,
                                       lower_limit = NULL,
                                       max = NULL,
                                       mean = NULL,
                                       millisecond = NULL,
                                       min = NULL,
                                       minute = NULL,
                                       mode = NULL,
                                       month = NULL,
                                       origin_value = NULL,
                                       pages = NULL,
                                       quantity = NULL,
                                       round = NULL,
                                       second = NULL,
                                       standardDeviation = NULL,
                                       system = NULL,
                                       title = NULL,
                                       to = NULL,
                                       type = NULL,
                                       unit = NULL,
                                       upper_limit = NULL,
                                       url = NULL,
                                       value = NULL,
                                       variance = NULL,
                                       vital_sign = NULL,
                                       width = NULL,
                                       year = NULL) {
  fields <- list(
    attributes = attributes,
    chart = chart,
    code = code,
    content_type = content_type,
    creation = creation,
    data = data,
    day = day,
    decimal_format = decimal_format,
    decimals = decimals,
    display = display,
    duration = duration,
    factor = factor,
    frames = frames,
    from = from,
    from_exp = from_exp,
    from_list = from_list,
    height = height,
    high = high,
    hour = hour,
    kind = kind,
    language = language,
    low = low,
    lower_limit = lower_limit,
    max = max,
    mean = mean,
    millisecond = millisecond,
    min = min,
    minute = minute,
    mode = mode,
    month = month,
    origin_value = origin_value,
    pages = pages,
    quantity = quantity,
    round = round,
    second = second,
    standardDeviation = standardDeviation,
    system = system,
    title = title,
    to = to,
    type = type,
    unit = unit,
    upper_limit = upper_limit,
    url = url,
    value = value,
    variance = variance,
    vital_sign = vital_sign,
    width = width,
    year = year
  )
  fields <- fields[!vapply(fields, is.null, logical(1))]

  if (identical(component, "distribution")) {
    if (is.null(fields$kind) || !fields$kind %in% names(.distribution_param_schema)) {
      stop(sprintf("component 'distribution': kind must be one of: %s",
                   paste(names(.distribution_param_schema), collapse = ", ")))
    }
    kind <- fields$kind
    round <- fields$round
    params <- fields[setdiff(names(fields), c("kind", "round"))]
    param_entry <- .distribution_param_schema[[kind]]
    .validate_settings(param_entry, params, sprintf("component 'distribution' (kind = %s)", kind))

    out <- list(kind = kind)
    if (!is.null(round)) out$round <- round
    out$parameters <- params
    return(out)
  }

  .build_settings(.component_schema, component, fields, label = "component", discriminator_field = NULL)
}
