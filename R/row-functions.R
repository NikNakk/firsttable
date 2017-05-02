# Row functions

# wilcox_row --------------------------------------------------------------

#' Wilcox test row
#'
#' @param data_item item to be taken from data for row
#' @param row_digits digits for data item (overrides table as a whole)
#' @param row_p_digits digits for p value (overrides table as a whole)
#' @param na.rm whether to remove NA before reporting median and quartiles
#'
#' @export
#'
wilcox_row <- function(data_item,
                       row_digits = NULL,
                       row_p_digits = NULL,
                       na.rm = TRUE) {
  data_item <- enquo(data_item)
  list(
    data_function = med_iqr(data_item, row_digits, na.rm),
    p_function = function(data, column_split, p_digits, small_p) {
      p_digits <- row_p_digits %||% p_digits
      stopifnot(length(unique(column_split)) == 2L)
      current_item <- eval_tidy(data_item, data)
      p <- wilcox.test(current_item ~ column_split)$p.value
      pretty_p(p, p_digits, small_p)
    }
  )
}


# med_iqr -----------------------------------------------------------------

med_iqr <- function(data_item, row_digits, na.rm) {
  function(data, column_split, digits) {
  digits <- row_digits %||% digits
  quartiles <- tapply(
    eval_tidy(data_item, data),
    column_split,
    quantile,
    probs = seq(0.25, 0.75, 0.25),
    na.rm = na.rm,
    simplify = FALSE
  )
  quartiles <- simplify2array(quartiles)
  output <- sprintf(
    "%2$.*1$f (%3$.*1$f - %4$.*1$f)",
    digits,
    quartiles[2, ],
    quartiles[1, ],
    quartiles[3, ]
  )
  output <- matrix(c("", output), nrow = 1L)
  }
}


# fisher_row --------------------------------------------------------------

#' Title
#'
#' @inheritParams wilcox_row
#' @param na.rm whether to include NA in the denominator for percentages
#'
#' @export
#'
fisher_row <- function(data_item,
                       row_digits = NULL,
                       row_p_digits = NULL,
                       na.rm = TRUE,
                       reference_level = NULL) {
  data_item <- enquo(data_item)
  list(
    data_function = function(data, column_split, digits) {
      digits <- row_digits %||% digits
      item <- eval_tidy(data_item, data)
      tab <- table(item, column_split)
      totals <- colSums(tab, na.rm = na.rm)
      output <- sprintf(
        "%2$d (%3$.*1$f%%)",
        digits,
        tab,
        tab / rep(totals, each = nrow(tab)) * 100
      )
      dim(output) <- dim(tab)
      output <- cbind(rownames(tab), output)
      if (!is.null(reference_level)) {
        output <- output[rownames(tab) != reference_level, , drop = FALSE]
      }
      output
    },
    p_function = function(data, column_split, p_digits, small_p) {
      p_digits <- row_p_digits %||% p_digits
      tab <- table(eval_tidy(data_item, data), column_split)
      p <- fisher.test(tab)$p.value
      pretty_p(p, p_digits, small_p)
    }
  )
}

pretty_p <- function(p, p_digits, small_p = c("<", "E", "x10")) {
  small_p <- match.arg(small_p)
  small_p_func <- switch(
    small_p,
    `<` = function(p, p_digits) {
      sprintf("<%.*f", p_digits, 10 ^ -p_digits)
    },
    `E` = function(p, p_digits) {
      sprintf("%.1E", p)
    },
    `x10` = function(p, p_digits) {
      sub("E(-?)\\+?0?(\\d+)", "x10^\\1\\2", sprintf("%.1E", p))
    }
  )
  ifelse(
    p >= 10 ^ -p_digits,
    sprintf("%.*f", p_digits, p),
    small_p_func(p, p_digits)
  )
}
