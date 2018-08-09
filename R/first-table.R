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
#' @param na_text text to use for NA values
#' @param pretty_p whether to format p values for display
#' @param escape_name whether to escape the row name when displayed as HTML
#' @param hide_single_level whether to hide levels for factors when only one
#'
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
  default_non_parametric = TRUE,
  na_text = "NA",
  pretty_p = TRUE,
  escape_name = TRUE,
  hide_single_level = FALSE
) {
  list(
    digits = digits,
    include_p = include_p,
    p_digits = p_digits,
    small_p_format = match.arg(small_p_format),
    small_p_cutoff = small_p_cutoff,
    include_n = include_n,
    include_n_per_col = include_n_per_col,
    workspace = workspace,
    default_non_parametric = default_non_parametric,
    na_text = na_text,
    pretty_p = pretty_p,
    escape_name = escape_name,
    hide_single_level = hide_single_level
  )
}

#' First Table
#'
#' @param .data `data.frame` or `tibble` to use as data source
#' @param .column_variable variable used for columns (if any)
#' @param .options options to use for formatting (see details)
#' @param ... row details
#' @return character matrix (\code{first_table}) or \code{data.frame}
#' (\code{first_table}) with the requested rows and columns;
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
#' The output for \code{first_table} is a character matrix which is formatted so as to work well with \code{\link[pander]{pander}}
#' and \code{\link[knitr]{knit}}. \code{first_table_df} returns a data.frame.
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
  ft_options <- first_table_options()
  if (!missing(.options)) {
    stopifnot(is.list(.options))
    ft_options[names(.options)] <- .options
  }

  df_out <- eval_tidy(first_table_df(.data, !!!enquos(...), .column_variable = !!enquo(.column_variable), .options = ft_options))
  df_out$Variable[duplicated(df_out$Variable)] <- ""
  if (ft_options$include_n) {
    df_out$n[df_out$Variable == ""] <- ""
  }
  if (ft_options$include_p &&
      "p" %in% colnames(df_out) &&
      !("Hazard ratio (95% CI)" %in% colnames(df_out))) {
    df_out$p[df_out$Variable == ""] <- ""
  }
  as.matrix(df_out)
}

#' @export
#'
#' @rdname first_table
first_table_df <- function(.data,
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

  ft_options$small_p_format <- match.arg(ft_options$small_p_format, c("<", "E", "x10", "html"))

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

  if (!is.null(get_expr(column_variable))) {
    if (!inherits(col_item, "Surv")) {
      col_names <- levels(col_item)
    } else {
      col_names <- "Hazard ratio (95% CI)"
    }
  } else {
    col_names <- "Value"
  }

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
      row_output <- matrix(c("", row_output), nrow = 1)
    }
    colnames(row_output) <- c("Level", col_names)

    row_output <- cbind.data.frame(
      Variable = row_names[i],
      n = NA_integer_,
      row_output,
      p = NA_character_,
      stringsAsFactors = FALSE
    )
    if (nrow(row_output) == 1L && ft_options$hide_single_level) {
      row_output$Level <- ""
    }
    if (ft_options$include_n) {
      row_output$n <- sum(!is.na(row_item) & !is.na(current_col_item))
    } else {
      row_output$n <- NULL
    }
    if (ft_options$include_p) {
      if (ft_options$pretty_p) {
        row_output$p <- pretty_p(output_data$p, ft_options$p_digits, ft_options$small_p_format, ft_options$small_p_cutoff)
      } else {
        row_output$p <- output_data$p
      }
    } else {
      row_output$p <- NULL
    }
    output[[i]] <- row_output
  }

  if (ft_options$include_n_per_col && n_col >= 1) {
    row_with_n <- cbind.data.frame(
      Variable = "n",
      Level = "",
      n = as.character(nrow(.data)),
      `colnames<-`(matrix(NA_character_, ncol = length(col_names), nrow = 1), col_names),
      p = "",
      stringsAsFactors = FALSE
    )
    if (!ft_options$include_n) {
      row_with_n$n <- NULL
    }
    if (!ft_options$include_p) {
      row_with_n$p <- NULL
    }
    row_with_n[1, levels(col_item)] <- table(col_item)[levels(col_item)]
    output <- c(list(row_with_n), output)
  }
  invoke(rbind, output)
}

#' @export
#'
#' @rdname first_table
first_table_flextable <- function(.data,
                                  ...,
                                  .column_variable = NULL,
                                  .options = first_table_options()) {
  if (!requireNamespace("flextable", quietly = TRUE) ||
      !requireNamespace("officer", quietly = TRUE)) {
    stop("`first_table_flextable` requires the flextable and officer packages")
  }
  ft_options <- first_table_options()
  if (!missing(.options)) {
    stopifnot(is.list(.options))
    ft_options[names(.options)] <- .options
  }

  df_out <-
    eval_tidy(
      first_table_df(.data, !!!enquos(...),.column_variable = !!enquo(.column_variable),
      .options = ft_options
    ))
  rows_to_merge <- split(seq_len(nrow(df_out)), df_out$Variable)
  cols_to_merge <- "Variable"
  if (ft_options$include_n) {
    cols_to_merge <- c(cols_to_merge, "n")
  }
  if (ft_options$include_p &&
      "p" %in% colnames(df_out) &&
      !("Hazard ratio (95% CI)" %in% colnames(df_out))) {
    cols_to_merge <- c(cols_to_merge, "p")
  }

  ft_out <- flextable::regulartable(df_out)
  ft_out <- flextable::style(ft_out, j = cols_to_merge, pr_c = officer::fp_cell(vertical.align = "top"))
  for (rtm in rows_to_merge) {
    for (ctm in cols_to_merge) {
      ft_out <- flextable::merge_at(ft_out, i = rtm, j = ctm)
    }
  }
  flextable::autofit(ft_out)
}

#' @export
#'
#' @rdname first_table
first_table_hux <- function(.data,
                                  ...,
                                  .column_variable = NULL,
                                  .options = first_table_options()) {
  if (!requireNamespace("huxtable", quietly = TRUE)) {
    stop("`first_table_huxtable` requires the huxtable package")
  }
  ft_options <- first_table_options()
  if (!missing(.options)) {
    stopifnot(is.list(.options))
    ft_options[names(.options)] <- .options
  }

  df_out <-
    eval_tidy(
      first_table_df(.data, !!!enquos(...),.column_variable = !!enquo(.column_variable),
                     .options = ft_options
      ))
  rows_to_merge <- split(seq_len(nrow(df_out)), df_out$Variable)
  cols_to_merge <- "Variable"

  ht_out <- huxtable::hux(df_out)

  if (ft_options$include_n) {
    cols_to_merge <- c(cols_to_merge, "n")
  }
  if (ft_options$include_p &&
      "p" %in% colnames(df_out) &&
      !("Hazard ratio (95% CI)" %in% colnames(df_out))) {
    cols_to_merge <- c(cols_to_merge, "p")
    if (ft_options$small_p_format == "html") {
      huxtable::escape_contents(ht_out)[, "p"] <- FALSE
    }
  }

  if (!ft_options$include_n) {
    huxtable::colspan(ht_out)[which(df_out$Level == ""), 1] <- 2
  }

  huxtable::escape_contents(ht_out)[, "Variable"] <- ft_options$escape_name

  for (rtm in rows_to_merge) {
    for (ctm in cols_to_merge) {
      huxtable::rowspan(ht_out)[rtm[1], ctm] <- length(rtm)
    }
  }
  ht_out <- huxtable::add_colnames(ht_out)
  ht_out <- huxtable::set_all_borders(ht_out, 1)
  ht_out <- huxtable::set_bold(ht_out, 1, huxtable::everywhere, TRUE)
  ht_out
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
