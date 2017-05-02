
#' Table One
#'
#' @param data `data.frame` or `tibble` to use as data source
#' @param column_variable variable used for columns (if any)
#' @param digits digits used for formatting variables by default
#' @param p_digits digits used for formatting p values by default
#' @param small_p format used for small p values
#' @param ... row details
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
                      small_p = c("<", "E", "x10"),
                      include_n = FALSE) {
  row_details <- quos(...)
  small_p <- match.arg(small_p)

  row_structure <- lapply(
    row_details,
    function(row_item) {
      data_item <- eval_tidy(row_item, data)
      if (is.call(row_item[[2L]])) {
        # Already calls a function
        if (is.list(data_item) && names(data_item) == c("data_function", "p_function")) {
          return(data_item)
        }
      }
      if (is.numeric(data_item)) {
        wilcox_row(!!row_item)
      } else if (is.logical(data_item)) {
        fisher_row(!!row_item, reference_level = "FALSE")
      } else {
        fisher_row(!!row_item)
      }
    })

  if (is.null(names(row_details))) {
    names(row_structure) <- rep("", length(row_structure))
  } else {
    names(row_structure) <- names(row_details)
  }

  for (i in which(names(row_structure) == "")) {
    expr <- UQE(row_details[[i]])
    if (is_symbol(expr)) {
      names(row_structure)[[i]] <- deparse(expr)
    } else if (is.call(expr)) {
      names(row_structure)[[i]] <- deparse(expr[[2L]])
    }
  }

  column_variable <- enquo(column_variable)
  if (!is.null(UQE(column_variable))) {
    column_split <- as.factor(eval_tidy(column_variable, data))
  } else {
    column_split <- factor(rep(1, nrow(data)))
    include_p <- FALSE
  }
  n_row <- length(row_structure)
  n_col <- length(levels(column_split))

  output <- list_len(n_row)

  for (i in seq_along(row_structure)) {
    output_data <- row_structure[[i]]$data_function(data, column_split, digits, include_n)
    output[[i]] <- cbind(
      matrix(
        c(
          names(row_structure)[i],
          rep("", nrow(output_data) - 1L)
        ),
        nrow = nrow(output_data)),
      output_data
      )
    if (include_p) {
      output[[i]] <- cbind(output[[i]],
        matrix(
          c(
            row_structure[[i]]$p_function(data, column_split, p_digits, small_p),
            rep("", nrow(output_data) - 1L)
          ),
          nrow = nrow(output_data)
        )
      )
    }
  }

  output <- invoke(rbind, output)

  col_names <- c("Variable", "Level")
  if (include_n) {
    col_names <- c(col_names, "n")
  }
  if (!is.null(UQE(column_variable))) {
    col_names <- c(col_names, levels(column_split))
  } else {
    col_names <- c(col_names, "Value")
  }
  if (include_p) {
    col_names <- c(col_names, "p")
  }
  colnames(output) <- col_names
  output
}
