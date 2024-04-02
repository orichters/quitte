#' Calculate new variables
#'
#' Calculate new variables from existing ones, using generic formulas.
#'
#' `...` is a list of name-value pairs with the general format
#' ```
#' "lhs" = "rhs + calculations - formula", "`lhs 2`" = "lhs / `rhs 2`"
#' ```
#' where `lhs` are the names of new variables to be calculated and
#' `rhs` are the variables to calculate from. If `lhs` and `rhs`
#' are no proper *identifiers*, they need to be quoted (see
#' [Quotes][base::Quotes] for details). When in doubt, just quote.
#'
#' If the new variables should have units, set `units` appropriately.
#'
#' `.dots` is a named list of strings denoting formulas and optionally
#' units. The general format is
#' ```
#' list("`lhs 1`" = "`rhs` / `calculation`",
#'      "`lhs 2`" = "sin(`rhs 2`)")
#' ```
#'
#' Units are optionally included with the formulas in a vector like
#' ```
#' list("`lhs w/ unit`" = c("`rhs 1` + `rhs 2`", "rhs unit")
#' ```
#' Units do not require quoting.
#'
#' `...` and `.dots` are processed in order, and variables already
#' calculated in the same call can be used for further calculations. Other
#' existing columns, including `period`, can be referenced, but this is
#' not supported and the results are considered *undefined*.
#'
#' @param data A data frame.
#' @param ... Name-value pairs of calculation formulas. See details.
#' @param units Character vector of units corresponding to new variables.  Must
#'   be of length equal to `...` or of length one (in which case all new
#'   variables receive the same unit).
#' @param na.rm If `TRUE` (the default), remove items calculated as `NA`.  This
#'   is generally the case for all calculations involving `NA` values, and all
#'   calculations involving missing variables.  See `completeMissing` parameter.
#' @param completeMissing If `TRUE`, implicitly missing data, i.e. missing
#'   combinations of input data, are filled up with 0 before the calculation,
#'   and they are therefore not computed as `NA` (and potentially removed from
#'   the output).  Make sure `0` is a sensible value for your calculations, else
#'   complete missing values manually.  Defaults to `FALSE`.
#' @param only.new If `FALSE` (the default), add new variables to existing
#'   ones.  If `TRUE`, return only new variables.
#' @param variable Column name of variables.  Defaults to `"variable"`.
#' @param unit Column name of units.  Defaults to `"unit"`.  Ignored if no
#'   column with the same name is in `data` (e.g. data frames without unit
#'   column).
#' @param value Column name of values.  Defaults to `"value"`.
#' @param overwrite If `TRUE` (the default), values are overwritten if they
#'   already exist. If `FALSE` values are discarded and not overwritten if they
#'   already exist
#' @param skip.missing.rhs If `FALSE` (the default), fail if any right-hand-side
#'   variable is missing.  If `TRUE`, warn, and skip that calculation.
#' @param .dots Used to work around non-standard evaluation.  See details.
#'
#' @return A data frame.
#'
#' @examples
#' data <- inline.data.frame(c(
#'     "model;    scenario;   region;   variable;     unit;                 period;   value",
#'     "REMIND;   Baseline;   USA;      GDP|MER;      billion US$2005/yr;   2010;     12990",
#'     "REMIND;   Baseline;   USA;      Population;   million;              2010;       310.4",
#'     "REMIND;   Baseline;   USA;      PE;           EJ/yr;                2010;        91.62",
#'     "REMIND;   Baseline;   CHN;      GDP|MER;      billion US$2005/yr;   2020;      8882",
#'     "REMIND;   Baseline;   CHN;      GDP|MER;      billion US$2005/yr;   2010;      4119",
#'     "REMIND;   Baseline;   CHN;      Population;   million;              2020;      1387",
#'     "REMIND;   Baseline;   CHN;      Population;   million;              2010;      1349"))
#'
#' calc_addVariable(data, "GDPpC" = "`GDP|MER` / Population * 1e3",
#'                        "`ln GDPpC`" = "log(GDPpC)",
#'                        units = c("US$2005/cap", NA))
#' calc_addVariable_(
#'     data,
#'     list("`GDPpC`"    = c("`GDP|MER` / `Population` * 1e3", "US$/cap"),
#'          "`ln GDPpC`" = "log(`GDPpC`)")
#' )
#'
#' @author Michaja Pehl
#'
#' @importFrom dplyr anti_join bind_rows filter mutate n select
#' @importFrom glue glue
#' @importFrom lazyeval f_eval interp
#' @importFrom magrittr %>%
#' @importFrom rlang := is_false sym syms
#' @importFrom stats formula setNames
#' @importFrom tidyr pivot_wider replace_na
#' @importFrom tidyselect all_of any_of
#'
#' @export
calc_addVariable <- function(data, ..., units = NA, na.rm = TRUE,
                             completeMissing = FALSE, only.new = FALSE,
                             variable = variable,  unit = unit,
                             value = value, overwrite = TRUE,
                             skip.missing.rhs = FALSE) {

  .dots    <- list(...)

  if (!all(is.na(units))) {
    if (length(units) == length(.dots)) {
      for (i in seq_along(.dots))
        .dots[i][[1]] <- c(.dots[i][[1]], units[i])
    } else if (1 == length(units)) {
      for (i in seq_along(.dots))
        .dots[i][[1]] <- c(.dots[i][[1]], units)
    } else
      stop('`units` must be of the same length as `...` or of length one.')
  }

  variable <- deparse(substitute(variable))
  unit     <- deparse(substitute(unit))
  value    <- deparse(substitute(value))

  calc_addVariable_(data, .dots, na.rm, completeMissing, only.new, variable,
                    unit, value, overwrite, skip.missing.rhs)
}

#' @export
#' @rdname calc_addVariable
calc_addVariable_ <- function(data, .dots, na.rm = TRUE,
                              completeMissing = FALSE, only.new = FALSE,
                              variable = 'variable', unit = 'unit',
                              value = 'value', overwrite = TRUE,
                              skip.missing.rhs = FALSE) {
  . <- NULL

  # guardians ----
  if (!is.data.frame(data))
    stop('Only works with data frames')

  if (!is.list(.dots))
    stop('`.dots` must be a list of formula strings')

  .colnames <- colnames(data)
  for (column in c(variable, value)) {
    if (!column %in% .colnames) {
      stop(glue('No column \'{column}\' found'))
    }
  }

  # prepare `.dots` ----
  for (i in seq_along(.dots)) {
    .dots[[i]] <- list(
      name = gsub('`', '', names(.dots[i])),

      formula = paste0("~", .dots[[i]][1]) %>%
        gsub('\\n *', ' ', .) %>%
        formula() %>%
        interp(),

      unit = .dots[[i]][2]
    )

    .dots[[i]]$variables <- .dots[[i]]$formula %>%
      all.vars() %>%
      unique()
  }

  # filter for required data ----
  rhs_variables <- .dots %>%
    lapply(getElement, name = 'variables') %>%
    unlist(use.names = FALSE)

  data_work <- data %>%
    filter(!!sym(variable) %in% rhs_variables) %>%
    droplevels()

  # check for duplicates ----
  duplicates <- data_work %>%
    group_by(!!!syms(setdiff(colnames(.), value))) %>%
    filter(1 < n()) %>%
    ungroup()

  if (nrow(duplicates)) {
    stop(paste(c('Duplicate rows in data.frame', format(duplicates)),
               collapse = '\n'))
  }

  # calculate new variables ----
  for (i in seq_along(.dots)) {
    missing_rhs_variables <- setdiff(.dots[[i]]$variables,
                                     data_work[[variable]])
    if (0 < length(missing_rhs_variables)) {
      msg <- paste0(length(missing_rhs_variables), ' variable',
                    ifelse(1 < length(missing_rhs_variables), 's are', ' is'),
                    ' missing for the calculation of `', .dots[[i]]$name,
                    '`:\n',
                    paste(paste0('   `', missing_rhs_variables, '`'),
                          collapse = '\n'))
      if (isTRUE(skip.missing.rhs)) {
        warning(msg)
        next
      }
      else if ('silent' == skip.missing.rhs) {
        next
      }
      else {
        stop(msg)
      }
    }

    data_work <- bind_rows(
      data_work %>%
        filter(.dots[[i]]$name != !!sym(variable)),

      data_work %>%
        filter(!!sym(variable) %in% .dots[[i]]$variables) %>%
        select(!any_of(replace_na(unit, ''))) %>%
        pivot_wider(names_from = variable, values_from = value,
                    values_fill = ifelse(!is_false(completeMissing), 0, NA)) %>%
        mutate(!!sym(value) := f_eval(f = .dots[[i]]$formula, data = .),
               '{variable}' := .dots[[i]]$name,
               '{unit}' := .dots[[i]]$unit) %>%
        select(all_of(.colnames))
    )
  }

  # clean up ----
  new_variables <- sapply(.dots, getElement, name = 'name')

  data_work <- data_work %>%
    filter(!!sym(variable) %in% new_variables)

  if (na.rm) {
    data_work <- data_work %>%
      filter(!is.na(!!sym(value)))
  }

  if (only.new) {
    data <- data_work
  } else {

    if (overwrite) {
      data <- bind_rows(
        anti_join(
          data,

          data_work,

          setdiff(.colnames, c(unit, value))
        ),

        data_work
      )
    } else {
      data <- bind_rows(
        anti_join(
          data_work,

          data,

          setdiff(.colnames, c(unit, value))
        ),

        data
      )
    }
  }

  return(data)
}
