
#' Table One
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
#'
#' @return character matrix with table
#' @export
#' @import rlang
#'
table_one <- function(data,
                      ...,
                      column_variable = NULL,
                      digits = 1,
                      include_p = TRUE,
                      p_digits = 3,
                      small_p_format = c("<", "E", "x10"),
                      small_p_cutoff = 10 ^ -p_digits,
                      include_n = FALSE) {
  row_details <- quos(...)
  small_p_format <- match.arg(small_p_format)

  column_variable <- enquo(column_variable)
  if (!is.null(UQE(column_variable))) {
    col_item <- as.factor(eval_tidy(column_variable, data))
  } else {
    col_item <- factor(rep(1, nrow(data)))
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
        row_names[i] <- row_names[i] %|% deparse(UQE(details_item)[[2L]])
        if (!is.null(data_item$data) ||
            !is.null(UQE(data_item$data_filter))) {
          row_data <- data_item$data %||% data
          if (!is.null(UQE(data_item$data_filter))) {
            stopifnot(requireNamespace("dplyr"))
            row_data <- dplyr::filter(row_data, !!data_item$data_filter)
          }
          row_item <- eval_tidy(data_item$data_item, row_data)
          current_col_item <- as.factor(eval_tidy(column_variable, row_data))
        } else {
          row_item <- eval_tidy(data_item$data_item, data)
          current_col_item <- col_item
        }
    } else {
      row_item <- data_item
      current_col_item <- col_item
      row_names[i] <- row_names[i] %|% deparse(UQE(details_item))
      if (is.numeric(data_item)) {
        data_item <- wilcox_row(!!details_item)
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
      mat[1] <- item
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
    col_names <- c(col_names, levels(col_item))
  } else {
    col_names <- c(col_names, "Value")
  }
  if (include_p) {
    col_names <- c(col_names, "p")
  }
  colnames(output) <- col_names
  output
}
