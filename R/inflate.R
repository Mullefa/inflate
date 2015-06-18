#' Inflate a data frame
#'
#' @param data(data.frame) A data frame to inflate
#' @param var(string) Column name specifying column to inflate over.
#'   The column should be of class \code{Date}.
#' @param domain(Date) A vector of two dates (or two strings that can be
#'   coerced to dates). Used to specifiy the range to inflate over.
#'   The first date should specify the lower bound of the date range; the second
#'   date should specify the upper bound of the date range. If \code{NULL} the
#'   range is inferred by taking the minumum and maximum dates in the column.
#'
#' @section S3 generic:
#' This function has methods for:
#' \itemize{
#'   \item \code{data.frame}
#'   \item \code{grouped_df}
#' }
#' @export
#' @examples
#' library(dplyr)
#'
#' sales_data <- data_frame(
#'   date = as.Date(c("2014-01-01", "2014-01-03")),
#'   sales = c(10, 13)
#' )
#'
#' inflate_(sales_data, "date")
#' inflate_(sales_data, "date", domain = c("2013-12-31", "2014-01-04"))
#'
#' grp_sales_data <- data_frame(
#'   group = c("a", "a", "b", "b"),
#'   date = as.Date(c("2014-01-01", "2014-01-03", "2014-01-01", "2014-01-03")),
#'   sales = c(10, 13, 20, 30)
#' )
#'
#' grp_sales_data %>%
#'   group_by(group) %>%
#'   inflate_("date")
inflate_ <- function(data, var, domain = NULL) {
  UseMethod("inflate_")
}


#' @export
inflate_.data.frame <- function(data, var, domain = NULL) {
  inflate_impl(data, var, domain)
}


#' @export
inflate_.grouped_df <- function(data, var, domain = NULL) {
  group_vars <- as.character(groups(data))
  dplyr::do(data, inflate_impl(., var, domain, group_vars))
}


inflate_impl <- function(data, var, domain = NULL, group_vars = NULL) {
  vals <- data[[var]] %||% error("no column named ", var, " in the data set")

  if (!is.null(group_vars)) {
    group_vals <- as.list(data[1, group_vars, drop = FALSE])
  } else {
    group_vals <- NULL
  }

  if (!inherits(vals, "Date")) {
    error("the column ", var, " is not of class Date")
  }

  if (!all_distinct(vals)) {
    error("all values in the column ", var, " must be distinct (by group)")
  }

  min_val <- min(vals)
  max_val <- max(vals)

  domain <- as.Date(domain %||% c(min_val, max_val))

  lb <- domain[1]
  ub <- domain[2]

  if (lb >= ub) {
    error(
      "upper bound of domain ", bracket(ub), " ",
      "must be greater than lower bound of domain ", bracket(lb)
    )
  }

  if (lb > min_val) {
    error(
      "lower bound of domain ", bracket(lb), " ",
      "can not be greater than min value ", bracket(min_val), " ",
      "in column ", var
    )
  }

  if (ub < max_val) {
    error(
      "upper bound of domain ", bracket(ub), " ",
      "can not be smaller than max value ", bracket(max_val), " ",
      "in column ", var
    )
  }

  all_vals <- dplyr::data_frame(seq(domain[1], domain[2], by = 1))
  out <- dplyr::left_join(setNames(all_vals, var), data, by = var)

  out <- replace_numeric_nas(out)

  if (!is.null(group_vals)) {
    for (group_var in names(group_vals)) {
      out[[group_var]] <- group_vals[[group_var]]
    }
  }

  out
}
