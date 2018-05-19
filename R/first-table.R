#' First Table options
#'
#' @param digits digits used for formatting variables by default
#' @param include_p digits used for formatting p values by default
#' @param p_digits whether to include p values in table
#' @param small_p_format format for small p values
#' @param small_p_cutoff cutoff for small p values
#' @param include_n whether to include number of non-missing values for each row
#' @param include_n_per_col whether to include the number of individuals in each column
#' @param workspace default workspace passed onto \code{\link[stats]{fisher.test}}
#' @param default_non_parametric whether to default to non parametric tests for
#'   continuous variables

#' @export
first_table_options <- function(
  digits = 1,
  include_p = TRUE,
  p_digits = 3,
  small_p_format = c("<", "E", "x10", "html"),
  small_p_cutoff = NULL,
  include_n = FALSE,
  include_n_per_col = FALSE,
  workspace = 2e5,
  default_non_parametric = TRUE
) {
  list(
    digits = digits,
    include_p = include_p,
    p_digits = p_digits,
    small_p_format = small_p_format,
    small_p_cutoff = small_p_cutoff,
    include_n = include_n,
    include_n_per_col = include_n_per_col,
    workspace = workspace,
    default_non_parametric = default_non_parametric
  )
}

#' First Table
#'
#' @param .data `data.frame` or `tibble` to use as data source
#' @param .column_variable variable used for columns (if any)
#' @param .options options to use for formatting (see details)
#' @param ... row details
#' @return character matrix with the requested rows and columns
#'
#' @details This function takes a \code{\link[base]{data.frame}} or \code{\link[tibble]{tibble}}
#' and a row and column specification and generates a table along the lines of the first table
#' used in many medical journal articles. The row specification can either use one of the \code{_row}
#' functions, or if the defaults are appropriate can just be a bare column name or calculation
#' using a column. These calculations are implemented using \code{\link[rlang]{eval_tidy}} and
#' support the \code{\link[rlang]{quasiquotation}} operators such as \code{\link[rlang]{!!}} and
#' \code{\link[rlang]{!!!}}.
#'
#' Options can be specified as a list or by using \code{\link{first_table_options}}.

#'
#' The output is a character matrix which is formatted so as to work well with \code{\link[pander]{pander}}
#' and \code{\link[knitr]{knit}}.
#'
#' @export
#' @import rlang
#'
#' @examples
#' first_table(mtcars,
#'   .column_variable = cyl,
#'   "Miles per gallon" = mpg,
#'   "Transmission" = factor(am))
#'
#' # Example demonstrating use of quasiquotation from rlang
#' library(rlang)
#' my_rows <- quos(
#'   kruskal_row(Sepal.Length, row_digits = 0),
#'   "Sepal width" = first_table_row(Sepal.Width, row_digits = 2),
#'   "Wide petals" = Petal.Width > median(Petal.Width)
#' )
#' first_table(iris,
#'   .column_variable = Species,
#'   !!!my_rows
#' )
#'
#' # Example demonstrating use of survival column variable
#' library(survival)
#' first_table(lung,
#'   .column_variable = Surv(time, status),
#'   .options = list(include_n = TRUE, include_n_per_col = TRUE),
#'    ECOG = factor(ph.ecog),
#'    `Meal calories` = first_table_row(meal.cal, row_digits = 2)
#' )

first_table <- function(.data,
                      ...,
                      .column_variable = NULL,
                      .options = first_table_options()
                      ) {
  row_details <- quos(...)

  ft_options <- first_table_options()
  if (!missing(.options)) {
    stopifnot(is.list(.options))
    ft_options[names(.options)] <- .options
  }

  poss_legacy_options <-
    which(
      names(row_details) %in% c(
        "digits",
        "include_p",
        "p_digits",
        "small_p_format",
        "small_p_cutoff",
        "include_n",
        "include_n_per_col",
        "workspace"
      )
    )

  if (length(poss_legacy_options) > 0L) {
    warning("Options should now be specified using the .options argument. See ?first_table")
    legacy_options <- lapply(row_details[poss_legacy_options], get_expr)
    names(legacy_options) <- names(row_details)[poss_legacy_options]
    legacy_options <- legacy_options[!vapply(legacy_options, is.null, logical(1))]
    if (length(legacy_options) > 0L) {
      ft_options[names(legacy_options)] <- legacy_options
    }
    row_details[poss_legacy_options] <- NULL
  }

  ft_options$small_p_format <- match.arg(ft_options$small_p_format, first_table_options()$small_p_format)

  if (is.null(ft_options$small_p_cutoff)) {
    ft_options$small_p_cutoff <- 10 ^ -ft_options$p_digits
  }

  if ("column_variable" %in% names(row_details)) {
    warning("Column variable should now be specified using the .column_variable argument. See ?first_table")
    column_variable <- row_details$column_variable
    row_details$column_variable <- NULL
  } else {
    column_variable <- enquo(.column_variable)
  }

  col_item <- get_column_item(column_variable, .data)
  if (is.null(get_expr(column_variable))) {
    ft_options$include_p <- FALSE
  }
  n_row <- length(row_details)
  n_col <- length(levels(col_item))

  output <- list_len(n_row)

  row_names <- names(row_details)
  row_names[row_names == ""] <- NA_character_

  for (i in seq_along(row_details)) {
    details_item <- row_details[[i]]
    data_item <- eval_tidy(details_item, .data)
    # Check if the item for this row is a call to a row function or not
    if (is.call(details_item[[2L]]) &&
        is.list(data_item) &&
        all(c("data_item", "data_function") %in% names(data_item))) {
        row_names[i] <- row_names[i] %|%
          paste(trimws(deparse(get_expr(details_item)[[2L]], width.cutoff = 500)), collapse = " ")
        if (!is.null(data_item$data) ||
            !is.null(get_expr(data_item$data_filter))) {
          row_data <- data_item$data %||% .data
          if (!is.null(get_expr(data_item$data_filter))) {
            stopifnot(requireNamespace("dplyr"))
            row_data <- dplyr::filter(row_data, !!data_item$data_filter)
          }
          row_item <- eval_tidy(data_item$data_item, row_data)
          current_col_item <- get_column_item(column_variable, row_data)
        } else {
          row_item <- eval_tidy(data_item$data_item, .data)
          current_col_item <- col_item
        }
    } else {
      row_item <- data_item
      current_col_item <- col_item
      row_names[i] <- row_names[i] %|%
        paste(trimws(deparse(get_expr(details_item), width.cutoff = 500)), collapse = " ")
      data_item <- first_table_row(!!details_item, workspace = ft_options$workspace,
                                   non_parametric = ft_options$default_non_parametric)
    }
    row_data_function <- data_item$data_function

    output_data <- row_data_function(row_item, current_col_item, ft_options)

    row_output <- output_data$row_output
    if (!is.array(row_output)) {
      row_output <- matrix(c("", row_output), nrow = 1L)
    }
    pad_item <- function(item) {
      mat <- matrix("", ncol = 1, nrow = nrow(row_output))
      mat[1:length(item)] <- item
      mat
    }
    if (ft_options$include_n) {
      row_output <- cbind(pad_item(sum(!is.na(row_item) & !is.na(current_col_item))), row_output)
    }
    row_output <- cbind(pad_item(row_names[i]), row_output)
    if (ft_options$include_p) {
      row_output <- cbind(
        row_output,
        pad_item(pretty_p(output_data$p, ft_options$p_digits, ft_options$small_p_format, ft_options$small_p_cutoff))
      )
    }
    output[[i]] <- row_output
  }

  output <- invoke(rbind, output)

  col_names <- c("Variable")
  if (ft_options$include_n) {
    col_names <- c(col_names, "n")
  }
  col_names <- c(col_names, "Level")
  if (!is.null(get_expr(column_variable))) {
    if (!inherits(col_item, "Surv")) {
      col_names <- c(col_names, levels(col_item))
    } else {
      col_names <- c(col_names, "Hazard ratio (95% CI)")
    }
  } else {
    col_names <- c(col_names, "Value")
  }
  if (ft_options$include_p) {
    col_names <- c(col_names, "p")
  }
  colnames(output) <- col_names

  if (ft_options$include_n_per_col && n_col >= 1) {
    row_with_n <- matrix("", ncol = ncol(output), nrow = 1)
    colnames(row_with_n) <- col_names
    row_with_n[1, "Variable"] <- "n"
    row_with_n[1, levels(col_item)] <- table(col_item)[levels(col_item)]
    output <- rbind(row_with_n, output)
  }

  output
}

get_column_item <- function(.column_variable, .data) {
  if (!is.null(get_expr(.column_variable))) {
    col_item <- eval_tidy(.column_variable, .data)
    if (inherits(col_item, "Surv")) {
      col_item
    } else {
      as.factor(col_item)
    }
  } else {
    col_item <- factor(rep(1, nrow(.data)))
  }
}
