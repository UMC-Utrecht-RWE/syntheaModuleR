# logic.R -- the optional predicate sublanguage (Logic.java). Only meaningful nested inside a
# Guard's `allow`, or a conditional/complex transition option's `condition` -- never attached to
# a state directly. create_logic_settings(condition_type, ...) is this layer's single entry point.

.logic_schema <- list(
  Gender = list(required = c("gender"), optional = c()),
  Age = list(required = c("quantity", "unit", "operator"), optional = c()),
  Date = list(
    required = c("operator"), optional = c("year", "month", "date"),
    one_of = list(list(fields = c("year", "month", "date"), required = TRUE))
  ),
  SocioeconomicStatus = list(required = c("category"), optional = c()),
  Race = list(required = c("race"), optional = c()),
  Symptom = list(required = c("symptom", "operator", "value"), optional = c()),
  Observation = list(
    required = c("operator"),
    optional = c("codes", "referenced_by_attribute", "value", "value_code"),
    array = c("codes"),
    one_of = list(list(fields = c("codes", "referenced_by_attribute"), required = TRUE))
  ),
  Attribute = list(required = c("attribute", "operator", "value"), optional = c()),
  And = list(required = c("conditions"), optional = c(), array = c("conditions")),
  Or = list(required = c("conditions"), optional = c(), array = c("conditions")),
  Not = list(required = c("condition"), optional = c()),
  AtLeast = list(required = c("conditions", "minimum"), optional = c(), array = c("conditions")),
  AtMost = list(required = c("conditions", "maximum"), optional = c(), array = c("conditions")),
  True = list(required = c(), optional = c()),
  False = list(required = c(), optional = c()),
  PriorState = list(required = c("name"), optional = c("since", "within")),
  ActiveCondition = list(
    required = c(), optional = c("codes", "referenced_by_attribute"),
    array = c("codes"),
    one_of = list(list(fields = c("codes", "referenced_by_attribute"), required = TRUE))
  ),
  ActiveAllergy = list(
    required = c(), optional = c("codes", "referenced_by_attribute"),
    array = c("codes"),
    one_of = list(list(fields = c("codes", "referenced_by_attribute"), required = TRUE))
  ),
  ActiveMedication = list(
    required = c(), optional = c("codes", "referenced_by_attribute"),
    array = c("codes"),
    one_of = list(list(fields = c("codes", "referenced_by_attribute"), required = TRUE))
  ),
  ActiveCarePlan = list(
    required = c(), optional = c("codes", "referenced_by_attribute"),
    array = c("codes"),
    one_of = list(list(fields = c("codes", "referenced_by_attribute"), required = TRUE))
  ),
  VitalSign = list(required = c("vital_sign", "operator", "value"), optional = c())
)

#' Build a Logic condition settings object (Logic.java).
#'
#' Every field used by any of the Logic subclasses is a named argument here (all default NULL);
#' `condition_type` plus `.logic_schema[[condition_type]]` determine which ones are actually
#' required/optional/unknown for a given call.
#'
#' @param condition_type One of the names in `.logic_schema`, matching the Java Logic subclass
#'   name exactly (e.g. "Age", "SocioeconomicStatus", "ActiveCondition"). Combinators (And/Or/
#'   AtLeast/AtMost) take other create_logic_settings() results via `conditions = list(...)`;
#'   Not takes one via `condition = <result>`. No `...` here deliberately -- every field is a
#'   named argument below, so a typo'd field name gets R's own clear "unused argument" error
#'   instead of being silently swallowed.
#' @param attribute,category,codes,condition,conditions,date,gender,maximum,minimum,month,name,operator,quantity,race,referenced_by_attribute,since,symptom,unit,value,value_code,vital_sign,within,year
#'   Field values for whichever `condition_type` was selected -- see `.logic_schema` for which
#'   fields apply to which `condition_type`, or `vignette("logic-reference")` for the per-type
#'   table.
#' @return A `condition_type`-tagged named list.
#' @examples
#' create_logic_settings("Age", operator = ">=", quantity = 18, unit = "years")
#'
#' create_logic_settings("And", conditions = list(
#'   create_logic_settings("Gender", gender = "F"),
#'   create_logic_settings("Age", operator = ">=", quantity = 40, unit = "years")
#' ))
#' @export
create_logic_settings <- function(condition_type,
                                  attribute = NULL,
                                  category = NULL,
                                  codes = NULL,
                                  condition = NULL,
                                  conditions = NULL,
                                  date = NULL,
                                  gender = NULL,
                                  maximum = NULL,
                                  minimum = NULL,
                                  month = NULL,
                                  name = NULL,
                                  operator = NULL,
                                  quantity = NULL,
                                  race = NULL,
                                  referenced_by_attribute = NULL,
                                  since = NULL,
                                  symptom = NULL,
                                  unit = NULL,
                                  value = NULL,
                                  value_code = NULL,
                                  vital_sign = NULL,
                                  within = NULL,
                                  year = NULL) {
  fields <- list(
    attribute = attribute,
    category = category,
    codes = codes,
    condition = condition,
    conditions = conditions,
    date = date,
    gender = gender,
    maximum = maximum,
    minimum = minimum,
    month = month,
    name = name,
    operator = operator,
    quantity = quantity,
    race = race,
    referenced_by_attribute = referenced_by_attribute,
    since = since,
    symptom = symptom,
    unit = unit,
    value = value,
    value_code = value_code,
    vital_sign = vital_sign,
    within = within,
    year = year
  )
  fields <- fields[!vapply(fields, is.null, logical(1))]

  .build_settings(.logic_schema, condition_type, fields,
    label = "logic", discriminator_field = "condition_type"
  )
}
