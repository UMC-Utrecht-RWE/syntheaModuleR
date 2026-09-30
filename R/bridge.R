# bridge.R -- reads RWE-BRIDGE-style reference codelists (the "event_definition / coding_system /
# code / concept_name / tags / type / event_abbreviation" schema family used across several
# UMC-Utrecht-RWE study repos, e.g. RSV-OA-1038, RSV-1026, case-study-01-covid19,
# case-study-07-valproate) into a clean, canonically-named code table. This is stage 1 of the
# bridge -> spec -> module pipeline (see spec.R for stages 2-4): "specify where the necessary
# information is."
#
# Deliberately does NOT attempt to read a study's own study_variables.csv/composite_study_
# variables.csv-style pipeline-computed-logic files, and deliberately does NOT attempt to support
# the structurally different OMOP/CDM-concept codelist family (no tags/type vocabulary at all,
# seen in e.g. case-study-02-lungcancer, case-study-10-coloncancer, case-study-08-rsv) -- a source
# file that doesn't resolve the columns actually asked for errors clearly rather than silently
# returning nonsense.

#' The canonical-name -> actual-column-name alias map `read_bridge_codelist()` uses
#'
#' Confirmed real-world header variation across RWE-BRIDGE-style codelists (missing `origin`,
#' `concept_name` renamed `label`, a drug-proxy file using `product_identifier`/`product_name`
#' instead of `coding_system`/`code_name`, ...) means a fixed set of column names can't be
#' hardcoded. Every argument defaults to the "full events/AESI/COV codelist" header shape (the
#' schema `read_bridge_codelist()`'s own reference case, RSV-OA-1038, uses); override individual
#' entries for a source that deviates, or set an entry to `NULL` to mean "this field doesn't exist
#' in this source" (fine as long as nothing requires it -- see `read_bridge_codelist()`).
#'
#' @param event_abbreviation Actual column name for the concept-grouping key.
#' @param coding_system Actual column name for the coding system a row's `code` is in. For a
#'   drug-proxy-style file this is usually `"product_identifier"` instead.
#' @param code Actual column name for the code value itself.
#' @param display Actual column name for the human-readable label. For a drug-proxy-style file
#'   this is usually `"product_name"` instead of the default `"code_name"`.
#' @param tags Actual column name for the row's match-quality tag (`narrow`/`possible`/`exclude`/
#'   `ignore`, possibly compound like `"multiple:narrow+possible"`).
#' @param type Actual column name for the concept category (e.g. `"AESI"`/`"COV"`).
#' @return A named list, suitable as `read_bridge_codelist()`'s `columns` argument.
#' @examples
#' bridge_columns()
#' bridge_columns(coding_system = "product_identifier", display = "product_name")
#' @export
bridge_columns <- function(
  event_abbreviation = "event_abbreviation",
  coding_system = "coding_system",
  code = "code",
  display = "code_name",
  tags = "tags",
  type = "type"
) {
  list(
    event_abbreviation = event_abbreviation,
    coding_system = coding_system,
    code = code,
    display = display,
    tags = tags,
    type = type
  )
}

#' Read a RWE-BRIDGE-style codelist into a clean, canonically-named code table
#'
#' Always applies `columns` aliasing and the `type`/`coding_system` filters, returning a
#' `data.frame` with canonical column names (`code`, `display`, plus `coding_system`/`type` when
#' resolvable). With `group_by` set, additionally collapses to one row per distinct value of that
#' column, preferring rows whose `tags` value matches the best-ranked entry in `tag_preference`
#' (case-insensitively, substring-matched so a compound tag like `"multiple:narrow+possible"`
#' still counts as `"narrow"`) and dropping `exclude`/`ignore`/unmapped rows entirely -- this is
#' the AESI-style extraction shape (many candidate codes per concept, pick the best one), the
#' generalization of the tag-filtering fix applied by hand while building the RSV-OA-1038 module
#' (an unfiltered pick had landed on an `exclude`-tagged row for `ANAPHYLAXIS`). Leave `group_by =
#' NULL` (the default) for the exposure-style extraction shape instead -- a handful of
#' already-distinct rows (e.g. specific vaccine products) that the caller filters directly
#' afterward (`subset()`/`grepl()` on `display`), where tag-based ranking doesn't apply.
#'
#' Errors clearly, naming the missing column and the file's actual header, whenever a column
#' `type`/`coding_system`/`group_by`/`tags` needs to resolve for an active filter but can't --
#' this is what keeps a structurally different (e.g. OMOP/CDM-concept-shaped) codelist from being
#' fed in and silently producing nonsense instead of a clear failure.
#'
#' @param x A file path (read with every column as character), or an already-loaded `data.frame`.
#' @param type Keep rows whose aliased `type` column equals this value (case-insensitive,
#'   trimmed). `NULL` (default): no filter, and the `type` column isn't required to exist.
#' @param coding_system Keep rows whose aliased `coding_system` column equals this value
#'   (case-insensitive, trimmed). `NULL` (default): no filter.
#' @param event_abbreviation Keep rows whose aliased `event_abbreviation` column equals this value
#'   (case-insensitive, trimmed) -- the exposure-style filter (e.g. `"RSV"`, matching several
#'   distinct product rows you then pick apart yourself by `display`). `NULL` (default): no
#'   filter. Independent of `group_by`: set this alone to get every matching row back unco
#'   llapsed, or combine with `group_by` to additionally collapse within the filtered set.
#' @param group_by A canonical column name (resolved via `columns`, typically
#'   `"event_abbreviation"`) to collapse to one best-tagged row per distinct value -- the
#'   AESI-style shape. `NULL` (default): no collapsing, every filtered row is returned as-is, and
#'   `tags` isn't required to exist.
#' @param tag_preference Character vector, best first. Only used when `group_by` is set.
#' @param columns A `bridge_columns()` alias map (or a plain list with the same names).
#' @return A `data.frame`: `code`, `display`, plus `coding_system`/`type`/`event_abbreviation`
#'   when resolvable/requested, and (when `group_by` is set) the `group_by` column, one row per
#'   distinct value.
#' @examples
#' \dontrun{
#' # AESI-style: many candidate codes per concept, collapse to the best-tagged one.
#' read_bridge_codelist(
#'   "20260504_V2_ALL_full_codelist.csv",
#'   type = "AESI", coding_system = "MEDCODEID",
#'   group_by = "event_abbreviation"
#' )
#' # Exposure-style: a handful of already-distinct product rows, filtered by event_abbreviation,
#' # picked apart afterward by display text.
#' read_bridge_codelist(
#'   "20260611_All_drug_proxies_codelist.csv",
#'   event_abbreviation = "RSV", coding_system = "PRODCODEID",
#'   columns = bridge_columns(coding_system = "product_identifier", display = "product_name")
#' )
#' }
#' @export
read_bridge_codelist <- function(
  x,
  type = NULL,
  coding_system = NULL,
  event_abbreviation = NULL,
  group_by = NULL,
  tag_preference = c("narrow", "possible"),
  columns = bridge_columns()
) {
  df <- .read_bridge_source(x)

  needs_event_abbrev_col <- !is.null(event_abbreviation) ||
    identical(group_by, "event_abbreviation")

  code_col <- .resolve_bridge_column("code", required = TRUE, columns, df)
  display_col <- .resolve_bridge_column("display", required = TRUE, columns, df)
  type_col <- .resolve_bridge_column("type", required = !is.null(type), columns, df)
  coding_system_col <- .resolve_bridge_column(
    "coding_system", required = !is.null(coding_system), columns, df
  )
  event_abbrev_col <- if (needs_event_abbrev_col) {
    .resolve_bridge_column("event_abbreviation", required = TRUE, columns, df)
  } else {
    NULL
  }
  group_col <- if (!is.null(group_by)) {
    .resolve_bridge_column(group_by, required = TRUE, columns, df)
  } else {
    NULL
  }
  tags_col <- if (!is.null(group_by)) {
    .resolve_bridge_column("tags", required = TRUE, columns, df)
  } else {
    NULL
  }

  keep <- rep(TRUE, nrow(df))
  if (!is.null(type)) {
    keep <- keep & toupper(trimws(df[[type_col]])) == toupper(trimws(type))
  }
  if (!is.null(coding_system)) {
    keep <- keep & toupper(trimws(df[[coding_system_col]])) == toupper(trimws(coding_system))
  }
  if (!is.null(event_abbreviation)) {
    keep <- keep & toupper(trimws(df[[event_abbrev_col]])) == toupper(trimws(event_abbreviation))
  }
  out <- df[keep, , drop = FALSE]
  if (nrow(out) == 0) {
    stop("read_bridge_codelist(): no rows remain after filtering by `type`/`coding_system`/`event_abbreviation`.")
  }

  result <- data.frame(
    code = out[[code_col]],
    display = out[[display_col]],
    stringsAsFactors = FALSE
  )
  if (!is.null(coding_system_col)) result$coding_system <- out[[coding_system_col]]
  if (!is.null(type_col)) result$type <- out[[type_col]]
  if (!is.null(event_abbrev_col)) result$event_abbreviation <- out[[event_abbrev_col]]

  if (is.null(group_by)) {
    rownames(result) <- NULL
    return(result)
  }

  if (!identical(group_by, "event_abbreviation")) {
    result[[group_by]] <- out[[group_col]]
  }
  tags_raw <- out[[tags_col]]

  ranks <- vapply(tags_raw, .tag_rank, integer(1), tag_preference = tag_preference)

  all_groups <- unique(result[[group_by]])
  eligible <- !is.na(ranks)
  result <- result[eligible, , drop = FALSE]
  ranks <- ranks[eligible]

  ord <- order(result[[group_by]], ranks)
  result <- result[ord, , drop = FALSE]
  ranks <- ranks[ord]
  result <- result[!duplicated(result[[group_by]]), , drop = FALSE]
  rownames(result) <- NULL

  missing_groups <- setdiff(all_groups, result[[group_by]])
  if (length(missing_groups) > 0) {
    stop(sprintf(
      "read_bridge_codelist(): %d value(s) of '%s' have no row matching tag_preference = [%s]: %s",
      length(missing_groups), group_by, paste(tag_preference, collapse = ", "),
      paste(missing_groups, collapse = ", ")
    ))
  }

  result
}

#' @param x A file path or `data.frame`.
#' @return `x` itself if already a `data.frame`; otherwise the CSV at `x` read with every column
#'   as character (so codes like `"014238641000033110"`-shaped values never get numeric-coerced).
#' @noRd
.read_bridge_source <- function(x) {
  if (is.data.frame(x)) {
    return(x)
  }
  if (is.character(x) && length(x) == 1) {
    if (!file.exists(x)) {
      stop(sprintf("read_bridge_codelist(): file not found: %s", x))
    }
    return(utils::read.csv(x, colClasses = "character", stringsAsFactors = FALSE, check.names = FALSE))
  }
  stop("read_bridge_codelist(): `x` must be a file path (character) or a data.frame")
}

#' Map a canonical codelist field to the actual column name in a BRIDGE table
#'
#' @param canonical Canonical field name, e.g. `"code"` or `"event_abbreviation"` -- a name in
#'   `columns`.
#' @param required If TRUE, an unresolvable field is an error; if FALSE, it resolves to `NULL`
#'   (the caller didn't ask to filter or group on it, so its absence is fine).
#' @param columns Canonical-to-actual column-name mapping, as from `bridge_columns()`.
#' @param df The codelist `data.frame` the columns are looked up in.
#' @return The actual column name (a string), or `NULL` if unresolvable and not `required`.
#' @noRd
.resolve_bridge_column <- function(canonical, required, columns, df) {
  actual <- columns[[canonical]]
  if (is.null(actual) || !actual %in% names(df)) {
    if (required) {
      stop(sprintf(
        paste0(
          "read_bridge_codelist(): '%s' could not be resolved (columns$%s = %s). ",
          "Available columns: %s."
        ),
        canonical, canonical,
        if (is.null(actual)) "NULL" else sprintf("\"%s\"", actual),
        paste(names(df), collapse = ", ")
      ))
    }
    return(NULL)
  }
  actual
}

#' Rank a row's tags cell against `tag_preference`
#'
#' @param tag_value One raw `tags` cell.
#' @param tag_preference Tag substrings in preference order; matching is case-insensitive and
#'   by substring, so e.g. `"narrow"` matches `"Narrow, RSV"`.
#' @return The index of the first `tag_preference` entry found in `tag_value`, or `NA_integer_`
#'   if none match or the cell is blank (the row is then ineligible).
#' @noRd
.tag_rank <- function(tag_value, tag_preference) {
  tl <- tolower(trimws(tag_value))
  if (!nzchar(tl)) {
    return(NA_integer_)
  }
  for (i in seq_along(tag_preference)) {
    if (grepl(tag_preference[i], tl, fixed = TRUE)) {
      return(i)
    }
  }
  NA_integer_
}
