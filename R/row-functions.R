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
#' @examples
#' first_table(
#'   mtcars,
#'   .column_variable = am,
#'   wilcox_row(disp, row_digits = 2)
#' )
wilcox_row <- function(data_item,
                       data = NULL,
                       data_filter = NULL,
                       row_digits = NULL,
                       na.rm = TRUE) {
  list(
    data_item = enquo(data_item),
    data = data,
    data_filter = enquo(data_filter),
    data_function = function(row_item, col_item, ft_options) {
      digits <- row_digits %||% ft_options$digits
      if ((ft_options$include_p || ft_options$include_estimate_diff) &&
          length(unique(col_item[!is.na(row_item)])) == 2L) {
        test <- stats::wilcox.test(row_item ~ col_item, conf.int = ft_options$include_estimate_diff)
      }
      list(row_output = med_iqr(row_item, col_item, digits, na.rm, ft_options),
           estimate_diff = if (ft_options$include_estimate_diff) {
             if (length(unique(col_item[!is.na(row_item)])) == 2L) {
               sprintf(
               "%2$.*1$f (%3$.*1$f - %4$.*1$f)",
               digits,
               -test$estimate,
               -test$conf.int[2],
               -test$conf.int[1]
             )
             } else {
               NA_character_
             }
           } else {
             NULL
           },
           p = if (ft_options$include_p) {
             if (length(unique(col_item[!is.na(row_item)])) == 2L) {
               test$p.value
             } else {
               NA_real_
             }
           } else {
             NULL
           }
           )
    }
  )
}

# med_iqr -----------------------------------------------------------------

med_iqr <- function(row_item, col_item, digits, na.rm, ft_options) {
  num_data <- split(row_item, col_item)
  if (ft_options$include_overall_column) {
    num_data <- c(num_data, list(row_item))
  }
    quartiles <- lapply(
    num_data,
    stats::quantile,
    probs = seq(0.25, 0.75, 0.25),
    na.rm = na.rm
  )
  quartiles <- simplify2array(quartiles)
  out <- sprintf(
    "%2$.*1$f (%3$.*1$f - %4$.*1$f)",
    digits,
    quartiles[2, ],
    quartiles[1, ],
    quartiles[3, ]
  )
  out[out == "NA (NA - NA)"] <- ft_options$na_text
  out
}

# parametric_row --------------------------------------------------------------

#' Row for parametric data
#'
#' @param data_item item to be taken from data for row
#' @param row_digits digits for data item (overrides table as a whole)
#' @param na.rm whether to remove NA before reporting median and quartiles
#' @param data separate dataset to use
#' @param data_filter filter to apply to dataset
#'
#' @export
#' @examples
#' first_table(
#'   mtcars,
#'   .column_variable = am,
#'   parametric_row(disp, row_digits = 2)
#' )

parametric_row <- function(data_item,
                           data = NULL,
                           data_filter = NULL,
                           row_digits = NULL,
                           na.rm = TRUE) {
  list(
    data_item = enquo(data_item),
    data = data,
    data_filter = enquo(data_filter),
    data_function = function(row_item, col_item, ft_options) {
      digits <- row_digits %||% ft_options$digits
      if ((ft_options$include_p || ft_options$include_estimate_diff) &&
          length(unique(col_item[!is.na(row_item)])) == 2L) {
        test <- stats::t.test(row_item ~ col_item)
      }

      list(row_output = mean_sd(row_item, col_item, digits, na.rm, ft_options),
           estimate_diff = if (ft_options$include_estimate_diff) {
             if (length(unique(col_item[!is.na(row_item)])) == 2L) {
               sprintf(
                 "%2$.*1$f (%3$.*1$f - %4$.*1$f)",
                 digits,
                 diff(test$estimate),
                 -test$conf.int[2],
                 -test$conf.int[1]
               )
             } else {
               NA_character_
             }
           } else {
             NULL
           },
           p = if (ft_options$include_p) {
             if (length(unique(col_item[!is.na(row_item)])) == 2L) {
               stats::t.test(row_item ~ col_item)$p.value
             } else {
               NA_real_
             }
           } else {
             NULL
           })
    }
  )
}

# mean_sd -----------------------------------------------------------------

mean_sd <- function(row_item, col_item, digits, na.rm, ft_options) {
  values <- lapply(
    split(row_item, col_item),
    function(x) {c(mean(x, na.rm = na.rm), stats::sd(x, na.rm = na.rm))}
  )
  values <- simplify2array(values)
  out <- sprintf(
    "%2$.*1$f (%3$.*1$f)",
    digits,
    values[1, ],
    values[2, ]
  )
  out[out == "NA (NA)"] <- ft_options$na_text
  out
}

# kruskal_row --------------------------------------------------------------

#' Kruskal Wallis test row
#'
#' @param data_item item to be taken from data for row
#' @param row_digits digits for data item (overrides table as a whole)
#' @param na.rm whether to remove NA before reporting median and quartiles
#' @param data separate dataset to use
#' @param data_filter filter to apply to dataset
#'
#' @export
#' @examples
#' first_table(
#'   mtcars,
#'   .column_variable = cyl,
#'   kruskal_row(disp, row_digits = 2)
#' )
kruskal_row <- function(data_item,
                        data = NULL,
                        data_filter = NULL,
                        row_digits = NULL,
                        na.rm = TRUE) {
  list(
    data_item = enquo(data_item),
    data = data,
    data_filter = enquo(data_filter),
    data_function = function(row_item, col_item, ft_options) {
      digits <- row_digits %||% ft_options$digits
      list(
        row_output = med_iqr(row_item, col_item, digits, na.rm, ft_options),
        p = if (ft_options$include_p) {
          if (length(unique(col_item[!is.na(row_item)])) >= 2L) {
            stats::kruskal.test(row_item ~ factor(col_item))$p.value
          } else {
            NA_real_
          }
        } else {
          NULL
        }
      )
    }
  )
}


# fisher_row --------------------------------------------------------------

#' Row using Fisher's exact test
#'
#' @inheritParams wilcox_row
#' @param na.rm whether to include NA in the denominator for percentages
#' @param reference_level a level of the variable to drop from display
#' @param include_reference whether to include the first level of the factor
#'        in the report
#' @param workspace passed onto \code{\link[stats]{fisher.test}}
#' @param include_denom whether to include the denominator for categorical
#'   variables
#' @param percent_first whether to put the percent before the n for categorical
#'   variables
#'
#' @export
#'
#' @examples
#' first_table(
#'   mtcars,
#'   .column_variable = cyl,
#'   fisher_row(gear, row_digits = 2, include_reference = TRUE)
#' )

fisher_row <- function(data_item,
                       data = NULL,
                       data_filter = NULL,
                       row_digits = NULL,
                       na.rm = TRUE,
                       reference_level = NULL,
                       include_reference = TRUE,
                       workspace = NULL,
                       include_denom = NULL,
                       percent_first = NULL,
                       cat_out_of_row = NULL
) {
  list(
    data_item = enquo(data_item),
    data = data,
    data_filter = enquo(data_filter),
    data_function = function(row_item, col_item, ft_options) {
      digits <- row_digits %||% ft_options$digits_percent
      include_denom <- include_denom %||% ft_options$include_denom
      percent_first <- percent_first %||% ft_options$percent_first
      cat_out_of_row <- cat_out_of_row %||% ft_options$cat_out_of_row
      if (is.logical(row_item)) {
        row_item <- factor(row_item, levels = c(FALSE, TRUE))
      }
      tab <- table(row_item, col_item)
      if (nrow(tab) == 0) {
        tab <- table(row_item, col_item, useNA = "ifany")
      }
      output <-
        n_percent(
          tab,
          na.rm = na.rm,
          digits = digits,
          include_denom = include_denom,
          percent_first = percent_first,
          include_reference = include_reference,
          reference_level = reference_level,
          cat_out_of_row = cat_out_of_row,
          include_overall_column = ft_options$include_overall_column,
          hide_level_logical = ft_options$hide_level_logical
        )
      list(
        row_output = output,
        p = if (ft_options$include_p) {
          if (all(dim(tab) > 1L) &&
              sum(rowSums(tab) > 0, na.rm = TRUE) > 1 &&
              sum(colSums(tab) > 0, na.rm = TRUE) > 1) {
            workspace <- workspace %||% ft_options$workspace
            hybrid <- any(dim(tab) > 2L) && ft_options$hybrid_fisher
            simulate.p.value <- any(dim(tab) > 2L) && ft_options$simulate_p_value_fisher
            stats::fisher.test(
              tab,
              workspace = workspace,
              hybrid = hybrid,
              simulate.p.value = simulate.p.value
            )$p.value
          } else {
            NA_real_
          }
        } else {
          NULL
        }
      )
    }
  )
}

# chisq_row --------------------------------------------------------------

#' Row using chi squared test
#'
#' @inheritParams wilcox_row
#' @param na.rm whether to include NA in the denominator for percentages
#' @param reference_level a level of the variable to drop from display
#' @param include_reference whether to include the first level of the factor
#'        in the report
#'
#' @export
#'
#' @examples
#' first_table(
#'   mtcars,
#'   .column_variable = cyl,
#'   chisq_row(gear, row_digits = 2, include_reference = TRUE)
#' )

chisq_row <- function(data_item,
                      data = NULL,
                      data_filter = NULL,
                      row_digits = NULL,
                      na.rm = TRUE,
                      reference_level = NULL,
                      include_reference = TRUE,
                      include_denom = NULL,
                      percent_first = NULL,
                      cat_out_of_row = NULL) {
  list(
    data_item = enquo(data_item),
    data = data,
    data_filter = enquo(data_filter),
    data_function = function(row_item, col_item, ft_options) {
      digits <- row_digits %||% ft_options$digits_percent
      include_denom <- include_denom %||% ft_options$include_denom
      percent_first <- percent_first %||% ft_options$percent_first
      cat_out_of_row <- cat_out_of_row %||% ft_options$cat_out_of_row
      if (is.logical(row_item)) {
        row_item <- factor(row_item, levels = c(FALSE, TRUE))
      }
      tab <- table(row_item, col_item)
      if (nrow(tab) == 0) {
        tab <- table(row_item, col_item, useNA = "ifany")
      }
      output <-
        n_percent(
          tab,
          na.rm = na.rm,
          digits = digits,
          include_denom = include_denom,
          percent_first = percent_first,
          include_reference = include_reference,
          reference_level = reference_level,
          include_overall_column = ft_options$include_overall_column,
          hide_level_logical = ft_options$hide_level_logical
        )
      list(
        row_output = output,
        p = if (ft_options$include_p) {
          if (all(dim(tab) > 1L)) {
            stats::chisq.test(tab)$p.value
          } else {
            NA_real_
          }
        } else {
          NULL
        }
      )
    }
  )
}

n_percent <- function(tab,
           na.rm,
           digits,
           include_denom,
           percent_first,
           include_reference,
           reference_level,
           cat_out_of_row,
           include_overall_column,
           hide_level_logical) {
  if (include_overall_column) {
    tab_display <- cbind(tab, rowSums(tab))
  } else {
    tab_display <- tab
  }
  if (cat_out_of_row) {
    totals <- rep(rowSums(tab, na.rm = na.rm), ncol(tab_display))
  } else {
    totals <- rep(colSums(tab_display, na.rm = na.rm), each = nrow(tab))
  }
  pattern <- "%2$d"
  if (include_denom) {
    pattern <- paste0(pattern, "/%4$d")
  }
  if (percent_first) {
    pattern <- paste0("%3$.*1$f%% (", pattern, ")")
  } else {
    pattern <- paste0(pattern, " (%3$.*1$f%%)")
  }
  output <- sprintf(
    pattern,
    digits,
    tab_display,
    tab_display / totals * 100,
    totals
  )

  dim(output) <- dim(tab_display)
  output <- cbind(rownames(tab_display), output)
  if (!include_reference && is.null(reference_level)) {
    reference_level <- rownames(tab)[1]
  }
  if (!include_reference && !is.null(reference_level) && nrow(tab) > 1) {
    output <- output[rownames(tab) != reference_level, , drop = FALSE]
    if (hide_level_logical && identical(output[, 1], "TRUE")) {
      output[, 1] <- ""
    }
  }
  output
}


#' Cox Proportional Hazards Row
#'
#' @inheritParams wilcox_row
#' @param row_digits Number of digits to include in the HR
#' @param include_reference whether to include a row for the reference level of
#'   a factor
#'
#' @return row for inclusion in `first_table`
#' @export
#'
#' @examples
#' library(survival)
#' first_table(lung,
#'   .column_variable = Surv(time, status),
#'    ECOG = coxph_row(factor(ph.ecog), row_digits = 2)
#'  )

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
    data_function = function(row_item, col_item, ft_options) {
      digits <- row_digits %||% ft_options$digits
      if (!all(is.na(row_item))) {
        model <- survival::coxph(col_item ~ row_item)
        hrs <- exp(stats::coef(model))
        cis <- exp(stats::confint(model))
        ps <- stats::pchisq(
          (summary(model)$coefficients[, "z", drop = TRUE]) ^ 2,
          df = 1,
          lower.tail = FALSE
        )
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
             p = if (ft_options$include_p) ps else NULL
        )
      } else {
        list(row_output = matrix(c("", ft_options$na_text), nrow = 1), p = NA_real_)
      }
    }
  )

}

#' Row with type selected by firsttable

#' @inheritParams wilcox_row
#' @param include_reference whether to include a row for the reference level of
#'   a factor (only relevant for logical/factor/character variables)
#' @param reference_level a level of the variable to drop from display (only
#'   relevant for logical/factor/character variables)
#' @param workspace passed onto \code{\link[stats]{fisher.test}}
#' @param non_parametric whether to use non-parametric tests
#' @param rows_digits_default digits where \code{.column_type = "default"}
#' @param rows_digits_surv digits where \code{.column_type = "default"} and
#'   \code{.column_variable} inherits \code{Surv}
#' @param rows_digits_numeric digits where \code{.column_type = "numeric"}

#' @return row for inclusion in `first_table`
#'
#' @details This provides a generic row for \code{\link{first_table}} with
#' the type of row determined from the \code{class} of the data. This allows a
#' \code{list} of \code{\link[rlang]{quos}} to be created and then used for
#' both a standard \code{\link{first_table}} and one that uses a
#' \code{\link[survival]{Surv}} column.
#'
#' @import rlang
#' @export
#'
#' @examples
#' library(survival)
#' first_table(lung,
#'   .column_variable = Surv(time, status),
#'   .options = list(include_n = TRUE, include_n_per_col = TRUE),
#'    `Meal calories` = first_table_row(meal.cal, row_digits = 2)
#'  )

first_table_row <- function(data_item,
                            data = NULL,
                            data_filter = NULL,
                            row_digits = NULL,
                            na.rm = TRUE,
                            reference_level = NULL,
                            include_reference = NULL,
                            workspace = NULL,
                            non_parametric = NULL,
                            row_digits_default = NULL,
                            row_digits_surv = NULL,
                            row_digits_numeric = NULL,
                            cat_out_of_row = NULL) {
  data_item <- enquo(data_item)
  data_filter <- enquo(data_filter)
  list(
    data_item = data_item,
    data = data,
    data_filter = data_filter,
    data_function = function(row_item, col_item, ft_options) {
      digits <- row_digits %||% ft_options$digits
      workspace <- workspace %||% ft_options$workspace
      non_parametric <- non_parametric %||% ft_options$default_non_parametric
      if (inherits(col_item, "Surv")) {
        row_function <- coxph_row(!!data_item, data = data, data_filter = !!data_filter,
                                  row_digits = row_digits_surv %||% row_digits,
                                  include_reference = if (is.null(include_reference)) TRUE else include_reference)
      } else if (is.numeric(row_item) && !is.numeric(col_item)) {
        if (non_parametric) {
          if (length(unique(na.omit(col_item))) <= 2) {
            row_function <- wilcox_row(!!data_item, data = data, data_filter = !!data_filter,
                                       row_digits = row_digits_default %||% row_digits, na.rm = na.rm)
          } else {
            row_function <- kruskal_row(!!data_item, data = data, data_filter = !!data_filter,
                                        row_digits = row_digits_default %||% row_digits, na.rm = na.rm)
          }
        } else {
          row_function <- parametric_row(!!data_item, data = data, data_filter = !!data_filter,
                                         row_digits = row_digits_default %||% row_digits, na.rm = na.rm)
        }
      } else if (is.numeric(row_item) && is.numeric(col_item)) {
        row_function <- cor_row(!!data_item, data = data, data_filter = !!data_filter,
                                row_digits = row_digits_numeric %||% row_digits)
      } else if (is.logical(row_item)) {
        row_function <- fisher_row(!!data_item, data = data, data_filter = !!data_filter,
                                   row_digits = row_digits_default %||% row_digits,
                                   na.rm = na.rm, reference_level = reference_level %||% "FALSE",
                                   include_reference = if (is.null(include_reference)) cat_out_of_row %||% FALSE else include_reference,
                                   workspace = workspace, cat_out_of_row = cat_out_of_row)
      } else {
        row_function <- fisher_row(!!data_item, data = data, data_filter = !!data_filter,
                                   row_digits = row_digits_default %||% row_digits,
                                   na.rm = na.rm, reference_level = reference_level,
                                   include_reference = if (is.null(include_reference)) TRUE else include_reference,
                                   workspace = workspace, cat_out_of_row = cat_out_of_row)
      }
      row_function$data_function(row_item, col_item, ft_options)
    }
  )
}

# cor_row --------------------------------------------------------------

#' Correlation row
#'
#' @inheritParams wilcox_row
#' @param method method parameter passed onto \code{\link[stats]{cor}}:
#'   \code{"pearson"}, \code{"kendall"} or \code{"spearman"}.
#'
#' @export
#'
#' @examples
#' first_table(
#'   mtcars,
#'   .column_variable = gear,
#'   .column_type = "numeric",
#'   cor_row(disp, method = "spearman")
#' )
cor_row <- function(data_item,
                    data = NULL,
                    data_filter = NULL,
                    row_digits = NULL,
                    method = c("pearson", "kendall", "spearman")) {
  if (missing(method)) {
    method <- NULL
  } else {
    method <- match.arg(method)
  }
  list(
    data_item = enquo(data_item),
    data = data,
    data_filter = enquo(data_filter),
    data_function = function(row_item, col_item, ft_options) {
      digits <- row_digits %||% ft_options$digits
      method <- method %||% ft_options$cor_method
      if (sum(!is.na(col_item)) <= 3) {
        list(row_output = "", p = if (ft_options$include_p) NA_real_ else NULL)
      } else {
        test_output <- cor.test(row_item, col_item, method = method)
        list(
          row_output = sprintf(
            "%4$s = %2$.*1$f%3$s",
            digits,
            test_output$estimate,
            if (!is.null(test_output$conf.int)) {
              sprintf(" (%2$.*1$f - %3$.*1$f)", digits, test_output$conf.int[1], test_output$conf.int[2])
            } else {
              ""
            },
            c("r", "tau", "rho")[match(method, c("pearson", "kendall", "spearman"))]
          ),
          p = if (ft_options$include_p) {
            test_output$p.value
          } else {
            NULL
          })
      }
    }
  )
}
