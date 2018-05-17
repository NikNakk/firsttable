
#' First Table
#'
#' @param data `data.frame` or `tibble` to use as data source
#' @param column_variable variable used for columns (if any)
#' @param digits digits used for formatting variables by default
#' @param p_digits digits used for formatting p values by default
#' @param ... row details
#' @param include_p whether to include p values in table
#' @param small_p_format format for small p values
#' @param small_p_cutoff cutoff for small p values
#' @param include_n whether to include number of non-missing values for each row
#' @param include_n_per_col whether to include the number of individuals in each column
#'
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
#' The output is a character matrix which is formatted so as to work well with \code{\link[pander]{pander}}
#' and \code{\link[knitr]{knit}}.
#'
#' @export
#' @import rlang
#'
#' @examples
#' first_table(mtcars,
#'   column_variable = cyl,
#'   "Miles per gallon" = mpg,
#'   "Transmission" = factor(am))
#'
first_table <- function(data,
                      ...,
                      column_variable = NULL,
                      digits = 1,
                      include_p = TRUE,
                      p_digits = 3,
                      small_p_format = c("<", "E", "x10", "html"),
                      small_p_cutoff = 10 ^ -p_digits,
                      include_n = FALSE,
                      include_n_per_col = FALSE) {
  row_details <- quos(...)
  small_p_format <- match.arg(small_p_format)

  column_variable <- enquo(column_variable)
  col_item <- get_column_item(column_variable, data)
  if (is.null(UQE(column_variable))) {
    include_p <- FALSE
  }
  n_row <- length(row_details)
  n_col <- length(levels(col_item))

  output <- list_len(n_row)

  row_names <- names(row_details)
  row_names[row_names == ""] <- NA_character_

  for (i in seq_along(row_details)) {
    details_item <- row_details[[i]]
    data_item <- eval_tidy(details_item, data)
    if (is.call(details_item[[2L]]) &&
        is.list(data_item) &&
        all(c("data_item", "data_function") %in% names(data_item))) {
        row_names[i] <- row_names[i] %|%
          paste(trimws(deparse(UQE(details_item)[[2L]], width.cutoff = 500)), collapse = " ")
        if (!is.null(data_item$data) ||
            !is.null(UQE(data_item$data_filter))) {
          row_data <- data_item$data %||% data
          if (!is.null(UQE(data_item$data_filter))) {
            stopifnot(requireNamespace("dplyr"))
            row_data <- dplyr::filter(row_data, !!data_item$data_filter)
          }
          row_item <- eval_tidy(data_item$data_item, row_data)
          current_col_item <- get_column_item(column_variable, row_data)
        } else {
          row_item <- eval_tidy(data_item$data_item, data)
          current_col_item <- col_item
        }
    } else {
      row_item <- data_item
      current_col_item <- col_item
      row_names[i] <- row_names[i] %|%
        paste(trimws(deparse(UQE(details_item), width.cutoff = 500)), collapse = " ")
      if (inherits(col_item, "Surv")) {
        data_item <- coxph_row(!!details_item)
      } else if (is.numeric(data_item)) {
        if (length(unique(current_col_item)) <= 2) {
          data_item <- wilcox_row(!!details_item)
        } else {
          data_item <- kruskal_row(!!details_item)
        }
      } else if (is.logical(data_item)) {
        data_item <- fisher_row(!!details_item, reference_level = "FALSE")
      } else {
        data_item <- fisher_row(!!details_item)
      }
    }
    row_function <- data_item$data_function

    output_data <- row_function(row_item, current_col_item, digits, include_p)

    row_output <- output_data$row_output
    if (!is.array(row_output)) {
      row_output <- matrix(c("", row_output), nrow = 1L)
    }
    pad_item <- function(item) {
      mat <- matrix("", ncol = 1, nrow = nrow(row_output))
      mat[1:length(item)] <- item
      mat
    }
    if (include_n) {
      row_output <- cbind(pad_item(sum(!is.na(row_item))), row_output)
    }
    row_output <- cbind(pad_item(row_names[i]), row_output)
    if (include_p) {
      row_output <- cbind(
        row_output,
        pad_item(pretty_p(output_data$p, p_digits, small_p_format, small_p_cutoff))
      )
    }
    output[[i]] <- row_output
  }

  output <- invoke(rbind, output)

  col_names <- c("Variable")
  if (include_n) {
    col_names <- c(col_names, "n")
  }
  col_names <- c(col_names, "Level")
  if (!is.null(UQE(column_variable))) {
    if (!inherits(col_item, "Surv")) {
      col_names <- c(col_names, levels(col_item))
    } else {
      col_names <- c(col_names, "Hazard ratio (95% CI)")
    }
  } else {
    col_names <- c(col_names, "Value")
  }
  if (include_p) {
    col_names <- c(col_names, "p")
  }
  colnames(output) <- col_names

  if (include_n_per_col && n_col >= 1) {
    row_with_n <- matrix("", ncol = ncol(output), nrow = 1)
    colnames(row_with_n) <- col_names
    row_with_n[1, "Variable"] <- "n"
    row_with_n[1, levels(col_item)] <- table(col_item)[levels(col_item)]
    output <- rbind(row_with_n, output)
  }

  output
}

get_column_item <- function(column_variable, data) {
  if (!is.null(UQE(column_variable))) {
    col_item <- eval_tidy(column_variable, data)
    if (inherits(col_item, "Surv")) {
      col_item
    } else {
      as.factor(col_item)
    }
  } else {
    col_item <- factor(rep(1, nrow(data)))
  }
}
