#' First Table options
#'
#' @param template existing first_table_options to populate defaults
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
#' @param p_sig_fig whether to use significant figures for p value (rather than
#'   decimal digits)
#' @param p_n_sig_fig number of significant figures
#' @param escape_name whether to escape the row name when displayed as HTML
#' @param hide_single_level whether to hide levels for factors when only one
#' @param cor_method default correlation method for \code{\link{cor_row}}
#' @param digits_percent digits used by default for percentages
#'   (overrides \code{digits})
#' @param include_denom whether to include the denominator for categorical
#'   variables
#' @param percent_first whether to put the percent before the n for categorical
#'   variables
#' @param hybrid_fisher whether to use a hybrid approach for
#'   \code{\link[stats]{fisher.test}} and >2x2 tables
#' @param simulate_p_value_fisher whether to simulate p-values for
#'   \code{\link[stats]{fisher.test}} and >2x2 tables
#' @param include_estimate_diff whether to include an estimate of the difference
#'   for continuous data (with appropriate estimates for parametric and
#'   non-parametric data)
#' @param factor_name_own_row whether to have the name of a factor in a row
#'   on its own; only affects huxtable output
#' @param cat_out_of_row whether percentages in categories should be calculated
#'   out of row rather than column
#' @param include_overall_column whether to include an overall column in
#'   addition to separate columns by column variable
#' @param hide_level_logical hide the display of the level TRUE for logical rows
#' @param use_interpuncts replaces decimal points with interpuncts;
#'   most commonly used for Lancet journals
#'
#' @export
first_table_options <- function(
  template = NULL,
  digits = 1,
  include_p = TRUE,
  p_digits = 3,
  small_p_format = c("<", "E", "x10", "html"),
  small_p_cutoff = NULL,
  include_n = FALSE,
  include_n_per_col = c("no", "row", "embed"),
  workspace = 2e5,
  default_non_parametric = TRUE,
  na_text = "NA",
  pretty_p = TRUE,
  p_sig_fig = FALSE,
  p_n_sig_fig = 2,
  escape_name = TRUE,
  hide_single_level = FALSE,
  cor_method = c("pearson", "kendall", "spearman"),
  digits_percent = digits,
  include_denom = FALSE,
  percent_first = FALSE,
  hybrid_fisher = FALSE,
  simulate_p_value_fisher = FALSE,
  include_estimate_diff = FALSE,
  factor_name_own_row = FALSE,
  cat_out_of_row = FALSE,
  include_overall_column = FALSE,
  hide_level_logical = FALSE,
  use_interpuncts = FALSE
) {
  if (!is.null(template)) {
    out <- template
  } else {
    out <- as.list(formals())
    out$template <- NULL
  }
  specified <- as.list(match.call())[-1L]
  specified$template <- NULL
  if (is.logical(specified$include_n_per_col)) {
    if (specified$include_n_per_col) {
      specified$include_n_per_col <- "row"
    } else{
      specified$include_n_per_col <- "no"
    }
  }
  for (i in seq_len(length(specified))) {
    if (length(out[[names(specified)[[i]]]]) > 1L) {
      out[[names(specified)[[i]]]] <- match.arg(specified[[i]], eval(out[[names(specified)[[i]]]]))
    } else {
      out[[names(specified)[[i]]]] <- specified[[i]]
    }
  }
  # Evaluate remaining choices from the formals that were language items
  remaining_choices <- which(vapply(out, is.language, logical(1)))
  for (i in remaining_choices) {
    out[[i]] <- eval(out[[i]], out)[[1L]]
  }
  out
}

#' First Table
#'
#' @param .data `data.frame` or `tibble` to use as data source
#' @param .column_variable variable used for columns (if any)
#' @param .column_type type of column (default or numeric)
#' @param .options options to use for formatting (see details)
#' @param ... row details
#' @return object of class \code{first_table}  with the requested rows and columns;
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
                        .column_type = c("default", "numeric"),
                        .options = first_table_options()
) {
  row_details <- quos(...)

  if (length(row_details) == 0L) {
    stop("No row items provided")
  }

  .column_type <- match.arg(.column_type)

  ft_options <- first_table_options()
  if (!missing(.options)) {
    stopifnot(is.list(.options))
    ft_options[names(.options)] <- .options
  }

  ft_options$small_p_format <- match.arg(ft_options$small_p_format, c("<", "E", "x10", "html"))

  if (is.null(ft_options$small_p_cutoff)) {
    ft_options$small_p_cutoff <- 10 ^ -ft_options$p_digits
  }

  .column_variable <- enquo(.column_variable)

  col_item <- get_column_item(.column_variable, .data, .column_type)

  if (!inherits_any(col_item, c("numeric", "factor", "Surv"))) {
    stop(sprintf(
      "Column variable %s is of wrong type '%s'",
      expr_label(get_expr(.column_variable)),
      class(col_item)
    ))
  }

  if (is.null(get_expr(.column_variable))) {
    ft_options$include_p <- FALSE
  }
  n_row <- length(row_details)
  n_col <- length(levels(col_item))

  output <- new_list(n_row)

  row_names <- names(row_details)
  row_names[row_names == ""] <- NA_character_

  if (.column_type == "default" && !is.null(get_expr(.column_variable))) {
    if (!inherits(col_item, "Surv")) {
      col_names <- levels(col_item)
      if (ft_options$include_overall_column) {
        col_names <- c(col_names, "Overall")
      }
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
          filter_mask <- eval_tidy(data_item$data_filter, row_data)
        } else {
          filter_mask <- TRUE
        }
        row_item <- eval_tidy(data_item$data_item, row_data)[filter_mask]
        current_col_item <- get_column_item(.column_variable, row_data, .column_type)[filter_mask]
      } else {
        row_item <- eval_tidy(data_item$data_item, .data)
        current_col_item <- col_item
      }
    } else if (is.null(data_item)) {
      stop(sprintf("Row item '%s' is NULL", ifelse(
        names(row_details)[i] == "", i, names(row_details)[i]
      )))
    } else {
      row_item <- data_item
      current_col_item <- col_item
      row_names[i] <- row_names[i] %|%
        paste(trimws(deparse(get_expr(details_item), width.cutoff = 500)), collapse = " ")
      data_item <- first_table_row(!!details_item, workspace = ft_options$workspace,
                                   non_parametric = ft_options$default_non_parametric,
                                   cat_out_of_row = ft_options$cat_out_of_row)
    }
    row_data_function <- data_item$data_function

    if (.column_type == "numeric") {
      # Filter when value missing
      current_col_item <- current_col_item[!is.na(row_item)]
      row_item <- row_item[!is.na(row_item)]
      # Swap row and columns for numeric column data to allow e.g. wilcox_test to work
      output_data <- row_data_function(current_col_item, row_item, ft_options)
    } else {
      # Filter when value missing
      row_item <- row_item[!is.na(current_col_item)]
      current_col_item <- current_col_item[!is.na(current_col_item)]
      output_data <- row_data_function(row_item, current_col_item, ft_options)
    }

    row_output <- output_data$row_output
    if (!is.array(row_output)) {
      if (.column_type == "default" || length(row_output) == 1L) {
        row_output <- matrix(c("", row_output), nrow = 1)
      } else {
        row_output <- cbind(levels(factor(row_item)), row_output)
      }
    }
    colnames(row_output) <- c("Level", col_names)

    row_output <- cbind.data.frame(
      Variable = row_names[i],
      n = NA_integer_,
      row_output,
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
    if (ft_options$include_estimate_diff) {
      row_output$`Estimate of difference (95% CI)` <- output_data$estimate_diff %||% ""
    }
    if (ft_options$include_p) {
      if (ft_options$pretty_p) {
        row_output$p <- pretty_p(
          p = output_data$p,
          p_digits = ft_options$p_digits,
          small_p_format = ft_options$small_p_format,
          small_p_cutoff = ft_options$small_p_cutoff,
          sig_fig = ft_options$p_sig_fig,
          n_sig_fig = ft_options$p_n_sig_fig
        )
      } else {
        row_output$p <- output_data$p
      }
    }
    output[[i]] <- row_output
  }

  if (ft_options$include_n_per_col == "row" && n_col >= 1) {
    row_with_n <- cbind.data.frame(
      Variable = "n",
      Level = "",
      n = as.character(nrow(.data)),
      `colnames<-`(matrix(NA_character_, ncol = length(col_names), nrow = 1), col_names),
      `Estimate of difference (95% CI)` = NA,
      p = if (ft_options$pretty_p) "" else NA_real_,
      stringsAsFactors = FALSE
    )
    if (!ft_options$include_n) {
      row_with_n$n <- NULL
    }
    if (!ft_options$include_p) {
      row_with_n$p <- NULL
    }
    if (!ft_options$include_estimate_diff) {
      row_with_n$`Estimate of difference (95% CI)` <- NULL
    }
    row_with_n[1, col_names] <- table(col_item)[col_names]
    output <- c(list(row_with_n), output)
  }
  df_out <- do.call(rbind, output)
  if (ft_options$include_n_per_col == "embed" && n_col >= 1) {
    colnames(df_out)[match(col_names, colnames(df_out))] <-
      sprintf("%s\nn = %d", col_names, table(col_item)[col_names])
  }
  if (ft_options$use_interpuncts) {
    df_out[] <- lapply(df_out, function(x) {
      if (is.character(x)) {
        x <- gsub("(\\d)\\.(\\d)", "\\1\U00B7\\2", x)
      }
      x
    })
  }
  attr(df_out, "ft_options") <- ft_options
  class(df_out) <- c("first_table", "data.frame")
  df_out
}

#' @exportS3Method huxtable::as_huxtable first_table
as_huxtable.first_table <- function(x) {
  if (!requireNamespace("huxtable", quietly = TRUE)) {
    stop("`first_table_huxtable` requires the huxtable package")
  }
  ft_options <- attr(x, "ft_options")

  if (ft_options$factor_name_own_row) {
    x_split <- split(seq_len(nrow(x)), x$Variable)
    x_split <- x_split[order(vapply(x_split, `[`, integer(1), 1))]
    x_split <- lapply(x_split, function(i) x[i, ])
    x_name_own_row <- lapply(
      x_split,
      function(df) {
        df$Split <- df$Variable
        if (nrow(df) > 1) {
          name_row <- df[1, ]
          name_row[, -1] <- NA # Blank out all cells except variable
          df$Variable <- df$Level
          df <- rbind(name_row, df)
        }
        df$Level <- NULL
        df
      }
    )
    x <- do.call("rbind", x_name_own_row)
    rows_to_merge <- split(seq_len(nrow(x)), x$Split)
    cols_to_merge <- character()
  } else {
    rows_to_merge <- split(seq_len(nrow(x)), x$Variable)
    cols_to_merge <- "Variable"
  }

  ht_out <- huxtable::hux(as.data.frame(x), add_colnames = FALSE, add_rownames = FALSE)

  if (ft_options$include_n) {
    cols_to_merge <- c(cols_to_merge, "n")
  }
  if (ft_options$include_p && "p" %in% colnames(x)) {
    if (!any(grepl("(?<!difference )\\(95% CI\\)", colnames(x), perl = TRUE))) {
      cols_to_merge <- c(cols_to_merge, "p")
    }
    if (ft_options$small_p_format == "html") {
      huxtable::escape_contents(ht_out)[, "p"] <- FALSE
    }
  }

  if (!ft_options$include_n && !ft_options$factor_name_own_row) {
    huxtable::colspan(ht_out)[x$Level == "", 1] <- 2
  }

  if (ft_options$factor_name_own_row) {
    # huxtable::colspan(ht_out)[is.na(ht_out$Split), 1] <- ncol(ht_out) - 1
    huxtable::bold(ht_out)[is.na(ht_out$Split), 1] <- TRUE
    for (rows in rows_to_merge) {
      if (length(rows) == 1) {
        huxtable::bold(ht_out)[rows[1], 1] <- TRUE
      }
    }

    ht_out$Split <- NULL
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


get_column_item <- function(.column_variable, .data, .column_type) {
  if (!is.null(get_expr(.column_variable))) {
    col_item <- eval_tidy(.column_variable, .data)
    if (inherits(col_item, "Surv") || .column_type == "numeric") {
      col_item
    } else if (is.logical(col_item)) {
      # prevent collapsing to single column where only one value seen in particular data item
      col_item <- factor(col_item, levels = c(FALSE, TRUE))
    } else {
      as.factor(col_item)
    }
  } else {
    col_item <- factor(rep(1, nrow(.data)))
  }
}

#' @export
as.matrix.first_table <- function(x) {
  ft_options <- attr(x, "ft_options")
  x$Variable[duplicated(x$Variable)] <- ""
  if (ft_options$include_n) {
    x$n[x$Variable == ""] <- ""
  }
  if (ft_options$include_p &&
      "p" %in% colnames(x) &&
      !("Hazard ratio (95% CI)" %in% colnames(x))) {
    x$p[x$Variable == ""] <- ""
  }
  as.matrix.data.frame(x)
}

#' @export
as.data.frame.first_table <- function(x, ...) {
  class(x) <- "data.frame"
  x
}

#' @export
print.first_table <- function(x) {
  if (requireNamespace("huxtable", quietly = TRUE)) {
    print(as_huxtable.first_table(x))
  } else {
    print(as.matrix(x), quote = FALSE)
  }
}

#' @exportS3Method knitr::knit_print first_table
knit_print.first_table <- function(x, ...) {
  if (requireNamespace("huxtable", quietly = TRUE)) {
    knitr::knit_print(as_huxtable.first_table(x))
  } else {
    knitr::knit_print(as.matrix(x))
  }
}
