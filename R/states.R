# states.R -- one schema entry per State.java subclass (31 total), plus create_state_settings(),
# the single entry point for this layer, and the primary building block for a `states` list
# passed to build_module().

.state_schema <- list(
  Initial = list(required = c(), optional = c(), needs_transition = "required"),
  Terminal = list(
    required = c(),
    optional = c(),
    needs_transition = "forbidden"
  ),
  Simple = list(required = c(), optional = c(), needs_transition = "required"),
  CallSubmodule = list(
    required = c("submodule"),
    optional = c(),
    needs_transition = "required"
  ),
  Physiology = list(
    required = c(
      "model",
      "solver",
      "step_size",
      "sim_duration",
      "alt_direct_transition"
    ),
    optional = c("lead_time", "inputs", "outputs"),
    array = c("inputs", "outputs"),
    needs_transition = "required"
  ),
  Guard = list(
    required = c("allow"),
    optional = c(),
    needs_transition = "required"
  ),
  Delay = list(
    required = c(),
    optional = c("range", "exact", "distribution", "unit"),
    one_of = list(list(
      fields = c("range", "exact", "distribution"),
      required = TRUE
    )),
    needs_transition = "required"
  ),
  SetAttribute = list(
    required = c("attribute"),
    optional = c(
      "value",
      "value_code",
      "value_attribute",
      "range",
      "expression",
      "series_data",
      "period",
      "distribution"
    ),
    needs_transition = "required"
  ),
  Counter = list(
    required = c("attribute", "action"),
    optional = c("amount"),
    needs_transition = "required"
  ),
  Encounter = list(
    required = c(),
    optional = c(
      "wellness",
      "encounter_class",
      "codes",
      "reason",
      "telemedicine_possibility"
    ),
    array = c("codes"),
    needs_transition = "required"
  ),
  EncounterEnd = list(
    required = c(),
    optional = c("discharge_disposition"),
    needs_transition = "required"
  ),
  ConditionOnset = list(
    required = c("codes"),
    optional = c("target_encounter", "assign_to_attribute"),
    array = c("codes"),
    needs_transition = "required"
  ),
  ConditionEnd = list(
    required = c(),
    optional = c("codes", "condition_onset", "referenced_by_attribute"),
    array = c("codes"),
    needs_transition = "required"
  ),
  AllergyOnset = list(
    required = c("codes"),
    optional = c(
      "target_encounter",
      "assign_to_attribute",
      "allergy_type",
      "category",
      "reactions"
    ),
    array = c("codes", "reactions"),
    needs_transition = "required"
  ),
  AllergyEnd = list(
    required = c(),
    optional = c("codes", "allergy_onset", "referenced_by_attribute"),
    array = c("codes"),
    needs_transition = "required"
  ),
  MedicationOrder = list(
    required = c("codes"),
    optional = c(
      "reason",
      "prescription",
      "administration",
      "chronic",
      "assign_to_attribute"
    ),
    array = c("codes"),
    needs_transition = "required"
  ),
  MedicationEnd = list(
    required = c(),
    optional = c("codes", "medication_order", "referenced_by_attribute"),
    array = c("codes"),
    needs_transition = "required"
  ),
  CarePlanStart = list(
    required = c("codes"),
    optional = c("activities", "goals", "reason", "assign_to_attribute"),
    array = c("codes", "activities", "goals"),
    needs_transition = "required"
  ),
  CarePlanEnd = list(
    required = c(),
    optional = c("codes", "careplan", "referenced_by_attribute"),
    array = c("codes"),
    needs_transition = "required"
  ),
  Procedure = list(
    required = c("codes"),
    optional = c(
      "reason",
      "duration",
      "assign_to_attribute",
      "distribution",
      "unit"
    ),
    array = c("codes"),
    one_of = list(list(
      fields = c("duration", "distribution"),
      required = FALSE
    )),
    needs_transition = "required"
  ),
  VitalSign = list(
    required = c("vital_sign"),
    optional = c("unit", "exact", "range", "expression", "distribution"),
    one_of = list(list(
      fields = c("exact", "range", "expression", "distribution"),
      required = TRUE
    )),
    needs_transition = "required"
  ),

  # NOTE on needs_transition = "optional": an Observation nested inside a MultiObservation's or
  # DiagnosticReport's `observations` list is not an independent graph state and carries no
  # transition at all in real module JSON. If you use Observation as a genuine top-level state
  # and forget its transition, validate_module()'s graph-level check (every non-Terminal
  # top-level state needs exactly one transition property) still catches it.
  Observation = list(
    required = c("codes"),
    optional = c(
      "value_code",
      "attribute",
      "vital_sign",
      "sampled_data",
      "attachment",
      "category",
      "unit",
      "exact",
      "range",
      "expression",
      "distribution"
    ),
    array = c("codes"),
    needs_transition = "optional"
  ),
  MultiObservation = list(
    required = c("codes", "observations"),
    optional = c("category"),
    array = c("codes", "observations"),
    needs_transition = "required"
  ),
  DiagnosticReport = list(
    required = c("codes", "observations"),
    optional = c(),
    array = c("codes", "observations"),
    needs_transition = "required"
  ),

  # `series` items follow HealthRecord.ImagingStudy.Series's JSON shape (modality, body_site,
  # instances) -- passed through as raw lists, not independently schema-validated here.
  ImagingStudy = list(
    required = c("procedure_code", "series"),
    optional = c("min_number_series", "max_number_series"),
    array = c("series"),
    needs_transition = "required"
  ),
  Symptom = list(
    required = c("symptom"),
    optional = c("cause", "probability", "exact", "range", "distribution"),
    needs_transition = "required"
  ),

  # NOTE: singular `code` (one create_component_settings("code", ...) result), unlike every
  # *End state's plural `codes` list below -- easy to mix up, called out deliberately.
  Device = list(
    required = c("code"),
    optional = c("manufacturer", "model", "assign_to_attribute"),
    needs_transition = "required"
  ),
  DeviceEnd = list(
    required = c(),
    optional = c("codes", "device", "referenced_by_attribute"),
    array = c("codes"),
    needs_transition = "required"
  ),

  # each `supplies` item is a raw list(code = create_component_settings("code", ...),
  # quantity = <n>) -- not independently schema-validated.
  SupplyList = list(
    required = c("supplies"),
    optional = c(),
    array = c("supplies"),
    needs_transition = "required"
  ),
  Death = list(
    required = c(),
    optional = c(
      "codes",
      "condition_onset",
      "referenced_by_attribute",
      "range",
      "exact"
    ),
    array = c("codes"),
    needs_transition = "required"
  ),
  Vaccine = list(
    required = c("series", "codes"),
    optional = c(),
    array = c("codes"),
    needs_transition = "required"
  )
)

#' Build a State settings object (State.java). This is what goes into a `states` list passed to
#' build_module() -- the primary building block of this toolkit.
#'
#' Every field used by any of the 31 state types is a named argument here (all default NULL);
#' `type` plus `.state_schema[[type]]` determine which ones are actually required/optional/
#' unknown for a given call -- see create_state_settings.R's schema table above, or just let the
#' error message on a bad call name what's missing/unknown/not applicable.
#'
#' Two field names are intentionally reused with a different meaning depending on `type` (rather
#' than inventing type-prefixed names for every field, which would defeat the point of a single
#' shared signature): `model` is the Physiology model name for `type = "Physiology"`, but the
#' device model number for `type = "Device"`; `series` is a list of Series settings for
#' `type = "ImagingStudy"`, but a plain dose number (integer) for `type = "Vaccine"`.
#'
#' @param type One of the names in `.state_schema`, matching the Java State subclass name
#'   exactly (e.g. "ConditionOnset", "EncounterEnd", "MedicationOrder").
#' @param transition A create_transition_settings() result. Required for every type except
#'   `Terminal` (which must not have one) and `Observation` (optional -- see the schema note
#'   above).
#' @param action,activities,administration,allergy_onset,allergy_type,allow,alt_direct_transition,amount,assign_to_attribute,attachment,attribute,careplan,category,cause,chronic,code,codes,condition_onset,device,discharge_disposition,distribution,duration,encounter_class,exact,expression,goals,inputs,lead_time,manufacturer,max_number_series,medication_order,min_number_series,model,observations,outputs,period,prescription,probability,procedure_code,range,reactions,reason,referenced_by_attribute,sampled_data,series,series_data,sim_duration,solver,step_size,submodule,supplies,symptom,target_encounter,telemedicine_possibility,unit,value,value_attribute,value_code,vital_sign,wellness
#'   Field values for whichever `type` was selected -- see `.state_schema` for which fields apply
#'   to which `type`, or `vignette("state-reference")` for the per-type table.
#' @return A plain named list, `type`-tagged, ready to insert into a `states` list.
#' @examples
#' create_state_settings("Initial",
#'   transition = create_transition_settings("direct", to = "Terminal")
#' )
#'
#' create_state_settings("ConditionOnset",
#'   codes = list(create_component_settings("code",
#'     system = "SNOMED-CT",
#'     code = "38341003", display = "Hypertension"
#'   )),
#'   transition = create_transition_settings("direct", to = "Terminal")
#' )
#' @export
create_state_settings <- function(
  type,
  action = NULL,
  activities = NULL,
  administration = NULL,
  allergy_onset = NULL,
  allergy_type = NULL,
  allow = NULL,
  alt_direct_transition = NULL,
  amount = NULL,
  assign_to_attribute = NULL,
  attachment = NULL,
  attribute = NULL,
  careplan = NULL,
  category = NULL,
  cause = NULL,
  chronic = NULL,
  code = NULL,
  codes = NULL,
  condition_onset = NULL,
  device = NULL,
  discharge_disposition = NULL,
  distribution = NULL,
  duration = NULL,
  encounter_class = NULL,
  exact = NULL,
  expression = NULL,
  goals = NULL,
  inputs = NULL,
  lead_time = NULL,
  manufacturer = NULL,
  max_number_series = NULL,
  medication_order = NULL,
  min_number_series = NULL,
  model = NULL,
  observations = NULL,
  outputs = NULL,
  period = NULL,
  prescription = NULL,
  probability = NULL,
  procedure_code = NULL,
  range = NULL,
  reactions = NULL,
  reason = NULL,
  referenced_by_attribute = NULL,
  sampled_data = NULL,
  series = NULL,
  series_data = NULL,
  sim_duration = NULL,
  solver = NULL,
  step_size = NULL,
  submodule = NULL,
  supplies = NULL,
  symptom = NULL,
  target_encounter = NULL,
  telemedicine_possibility = NULL,
  unit = NULL,
  value = NULL,
  value_attribute = NULL,
  value_code = NULL,
  vital_sign = NULL,
  wellness = NULL,
  transition = NULL
) {
  fields <- list(
    action = action,
    activities = activities,
    administration = administration,
    allergy_onset = allergy_onset,
    allergy_type = allergy_type,
    allow = allow,
    alt_direct_transition = alt_direct_transition,
    amount = amount,
    assign_to_attribute = assign_to_attribute,
    attachment = attachment,
    attribute = attribute,
    careplan = careplan,
    category = category,
    cause = cause,
    chronic = chronic,
    code = code,
    codes = codes,
    condition_onset = condition_onset,
    device = device,
    discharge_disposition = discharge_disposition,
    distribution = distribution,
    duration = duration,
    encounter_class = encounter_class,
    exact = exact,
    expression = expression,
    goals = goals,
    inputs = inputs,
    lead_time = lead_time,
    manufacturer = manufacturer,
    max_number_series = max_number_series,
    medication_order = medication_order,
    min_number_series = min_number_series,
    model = model,
    observations = observations,
    outputs = outputs,
    period = period,
    prescription = prescription,
    probability = probability,
    procedure_code = procedure_code,
    range = range,
    reactions = reactions,
    reason = reason,
    referenced_by_attribute = referenced_by_attribute,
    sampled_data = sampled_data,
    series = series,
    series_data = series_data,
    sim_duration = sim_duration,
    solver = solver,
    step_size = step_size,
    submodule = submodule,
    supplies = supplies,
    symptom = symptom,
    target_encounter = target_encounter,
    telemedicine_possibility = telemedicine_possibility,
    unit = unit,
    value = value,
    value_attribute = value_attribute,
    value_code = value_code,
    vital_sign = vital_sign,
    wellness = wellness
  )
  fields <- fields[!vapply(fields, is.null, logical(1))]

  entry <- .state_schema[[type]]
  if (is.null(entry)) {
    stop(sprintf(
      "state: unknown type '%s'. Valid types: %s",
      type,
      paste(names(.state_schema), collapse = ", ")
    ))
  }

  tr_mode <- entry$needs_transition
  if (is.null(tr_mode)) {
    tr_mode <- "required"
  }
  if (identical(tr_mode, "required") && is.null(transition)) {
    stop(sprintf(
      "state '%s': a transition is required (see create_transition_settings())",
      type
    ))
  }
  if (identical(tr_mode, "forbidden") && !is.null(transition)) {
    stop(sprintf("state '%s': must not have a transition", type))
  }

  result <- .build_settings(
    .state_schema,
    type,
    fields,
    label = "state",
    discriminator_field = "type"
  )
  if (!is.null(transition)) {
    result <- c(result, transition)
  }
  result
}
