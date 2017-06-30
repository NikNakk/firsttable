# Row functions

# wilcox_row --------------------------------------------------------------

#' Wilcox test row
#'
#' @param data_item item to be taken from data for row
#' @param row_digits digits for data item (overrides table as a whole)
#' @param na.rm whether to remove NA before reporting median and quartiles
#' @param data separate dataset to use
#' @param data_filter filter to apply to dataset
#'
#' @export
#'
wilcox_row <- function(data_item,
                       data = NULL,
                       data_filter = NULL,
                       row_digits = NULL,
                       na.rm = TRUE) {
  list(
    data_item = enquo(data_item),
    data = data,
    data_filter = enquo(data_filter),
    data_function = function(row_item, col_item, digits, include_p) {
      digits <- row_digits %||% digits
      list(
        row_output = med_iqr(row_item, col_item, digits, na.rm),
         p = if (include_p) {
           stats::wilcox.test(row_item ~ col_item)$p.value
           } else {
             NULL
           }
        )
    }
  )
}


# med_iqr -----------------------------------------------------------------

med_iqr <- function(row_item, col_item, digits, na.rm) {
  quartiles <- tapply(
    row_item,
    col_item,
    stats::quantile,
    probs = seq(0.25, 0.75, 0.25),
    na.rm = na.rm,
    simplify = FALSE
  )
  quartiles <- simplify2array(quartiles)
  sprintf(
    "%2$.*1$f (%3$.*1$f - %4$.*1$f)",
    digits,
    quartiles[2, ],
    quartiles[1, ],
    quartiles[3, ]
  )
}


# fisher_row --------------------------------------------------------------

#' Row using Fisher's exact test
#'
#' @inheritParams wilcox_row
#' @param na.rm whether to include NA in the denominator for percentages
#' @param reference_level a level of the variable to drop from display
#'
#' @export
#'
fisher_row <- function(data_item,
                       data = NULL,
                       data_filter = NULL,
                       row_digits = NULL,
                       na.rm = TRUE,
                       reference_level = NULL,
                       include_reference = FALSE) {
  list(
    data_item = enquo(data_item),
    data = data,
    data_filter = enquo(data_filter),
    data_function = function(row_item, col_item, digits, include_p) {
      digits <- row_digits %||% digits
      tab <- table(row_item, col_item)
      totals <- colSums(tab, na.rm = na.rm)
      output <- sprintf(
        "%2$d (%3$.*1$f%%)",
        digits,
        tab,
        tab / rep(totals, each = nrow(tab)) * 100
      )
      dim(output) <- dim(tab)
      output <- cbind(rownames(tab), output)
      if (include_reference && is.null(reference_level)) {
        reference_level <- rownames(tab)[1]
      }
      if (!is.null(reference_level)) {
        output <- output[rownames(tab) != reference_level, , drop = FALSE]
      }
      list(
        row_output = output,
        p = if (include_p) {
          stats::fisher.test(tab)$p.value
        } else {
          NULL
        }
      )
    }
  )
}

#' Cox Proportional Hazards Row
#'
#' @inheritParams wilcox_row
#' @param row_digits Number of digits to include in the OR
#' @param include_reference whether to include a row for the reference level of
#'   a factor
#'
#' @return row for inclusion in `first_table`
#' @export
#'
coxph_row <- function(data_item,
                      data = NULL,
                      data_filter = NULL,
                      row_digits = NULL,
                      include_reference = TRUE) {
  stopifnot(requireNamespace("survival"))
  list(
    data_item = enquo(data_item),
    data = data,
    data_filter = enquo(data_filter),
    data_function = function(row_item, col_item, digits, include_p) {
      digits <- row_digits %||% digits
      model <- survival::coxph(col_item ~ row_item)
      hrs <- exp(coef(model))
      cis <- exp(confint(model))
      ps <- summary(model)$coefficients[, "Pr(>|z|)", drop = TRUE]
      if (names(hrs)[1L] == "row_item") {
        levs <- ""
        cis <- matrix(cis, ncol = 2)
      } else {
        levs <- sub("row_item", "", names(hrs))
      }
      output <- sprintf(
        "%2$.*1$f (%3$.*1$f - %4$.*1$f)",
        digits,
        hrs,
        cis[, 1, drop = TRUE],
        cis[, 2, drop = TRUE]
      )
      if (include_reference & !identical(levs, "") & !is.logical(row_item)) {
        output <- c("Reference", output)
        levs <- c(levels(as.factor(row_item))[1L], levs)
        ps <- c(NA, ps)
      }
      list(row_output = cbind(levs, output),
           p = ps)
    }
  )

}

#' Format p values for display
#'
#' @param p p value to format
#' @param p_digits number of digits to display
#' @param small_p_format format used to display p values below a threshold
#' @param small_p_cutoff cutoff for displaying alternative formatting
#'
#' @return formatted p value
#' @export
#'
pretty_p <- function(p,
                     p_digits,
                     small_p_format = c("<", "E", "x10", "plotmath", "html"),
                     small_p_cutoff = 10^-p_digits
                     ) {
  small_p_format <- match.arg(small_p_format)
  if (small_p_format == "<") {
    small_p_func <- function(p, small_p_cutoff) {
      sprintf("<%.*f", p_digits, small_p_cutoff)
    }
  } else if (small_p_format == "E") {
    small_p_func <- function(p, small_p_cutoff) {
      sprintf("%.1E", p)
    }
  } else if (small_p_format == "x10") {
    small_p_func <- function(p, small_p_cutoff) {
      sub("E(-?)\\+?0?(\\d+)", "x10^\\1\\2", sprintf("%.1E", p))
    }
  } else if (small_p_format == "plotmath") {
    small_p_func <- function(p, small_p_cutoff) {
      sub("E(-?)\\+?0?(\\d+)", " %*% 10^\\1\\2", sprintf("%.1E", p))
    }
  } else if (small_p_format == "html") {
    small_p_func <- function(p, small_p_cutoff) {
      sub("E(-?)\\+?0?(\\d+)", "×10<sup>\\1\\2</sup>", sprintf("%.1E", p))
    }
  }

  ifelse(
    is.na(p) | p == "",
    "",
    ifelse(
      p >= small_p_cutoff,
      sprintf("%.*f", p_digits, p),
      small_p_func(p, small_p_cutoff)
    )
  )
}
