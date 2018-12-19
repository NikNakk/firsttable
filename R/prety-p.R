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
#' @examples
#' p_values <- 10 ^ seq(-1, -6, -1)
#' # Format p values as for example 1.0x10^-4
#' pretty_p(p_values, p_digits = 3, small_p_format = "x10")
#' # Format p values for use on a graph
#' x <- rlnorm(100, 3, 1)
#' y <- rlnorm(100, 4, 1)
#' p <- wilcox.test(x, y)$p.value
#' p_formatted <- pretty_p(p, p_digits = 3, small_p_format = "plotmath")
#' boxplot(x, y, ylim = c(0, max(c(x, y)) + 100))
#' text(1.5, max(c(x, y) + 50), parse(text = paste0("p == ", p_formatted)))
pretty_p <- function(p,
                     p_digits,
                     small_p_format = c("<", "E", "x10", "plotmath", "html"),
                     small_p_cutoff = 10^-p_digits,
                     sig_fig = FALSE,
                     n_sig_fig = 2
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
      sub("E(-?)\\+?0?(\\d+)", " &times; 10<sup>\\1\\2</sup>", sprintf("%.1E", p))
    }
  }

  ifelse(
    is.na(p) | p == "",
    "",
    ifelse(
      p >= small_p_cutoff,
      sprintf(
        "%.*f",
        ifelse(
          sig_fig & !is.na(p) & p != "",
          n_sig_fig - 1 - floor(log10(signif(p, n_sig_fig))),
          p_digits
        ),
        p
      ),
      small_p_func(p, small_p_cutoff)
    )
  )
}

