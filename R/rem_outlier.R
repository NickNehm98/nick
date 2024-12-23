#' Remove Outliers from Data
#'
#' This function removes outliers based on the Interquartile Range (IQR).
#' Optionally, grouping variables can be specified to calculate outliers within each group.
#'
#' @param data A dataframe from which outliers should be removed.
#' @param variable The name of the variable to clean (as a string).
#' @param group1 An optional first grouping variable (as a string).
#' @param group2 An optional second grouping variable (as a string).
#' @param remove_outliers Logical; if TRUE, outliers are removed (Default: TRUE).
#' @return A cleaned dataframe without outliers.
#' @examples
#' df <- data.frame(values = c(1, 2, 3, 100, 5), group1 = c("A", "A", "B", "B", "B"), group2 = c("X", "X", "Y", "Y", "Y"))
#' remove_outliers(df, variable = "values")
#' remove_outliers(df, variable = "values", group1 = "group1")
#' remove_outliers(df, variable = "values", group1 = "group1", group2 = "group2")
#' @export
nick_remOutliers <- function(data, variable, group1 = NULL, group2 = NULL, remove_outliers = TRUE) {
  if (!variable %in% names(data)) {
    stop("Die angegebene Variable ist nicht im Dataframe enthalten.")
  }

  # Funktion zur Berechnung der IQR-Grenzen
  calculate_iqr_limits <- function(x) {
    q1 <- quantile(x, 0.25, na.rm = TRUE)
    q3 <- quantile(x, 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    lower <- q1 - 1.5 * iqr
    upper <- q3 + 1.5 * iqr
    return(c(lower, upper))
  }

  # Mit oder ohne Gruppen
  if (is.null(group1) && is.null(group2)) {
    limits <- calculate_iqr_limits(data[[variable]])
    if (remove_outliers) {
      data <- data[data[[variable]] >= limits[1] & data[[variable]] <= limits[2], ]
    } else {
      data$outlier <- !(data[[variable]] >= limits[1] & data[[variable]] <= limits[2])
    }
  } else {
    # Gruppierungen bestimmen
    group_vars <- rlang::syms(c(group1, group2) %>% purrr::compact())

    # Ausreißer innerhalb der Gruppen berechnen
    data <- data %>%
      group_by(!!!group_vars) %>%
      mutate(
        lower_limit = quantile(!!sym(variable), 0.25, na.rm = TRUE) - 1.5 * IQR(!!sym(variable), na.rm = TRUE),
        upper_limit = quantile(!!sym(variable), 0.75, na.rm = TRUE) + 1.5 * IQR(!!sym(variable), na.rm = TRUE),
        outlier = !(!!sym(variable) >= lower_limit & !!sym(variable) <= upper_limit)
      ) %>%
      ungroup()

    if (remove_outliers) {
      data <- data %>% filter(!outlier)
    }

    # Entferne Hilfsspalten
    data <- data %>% select(-c(lower_limit, upper_limit, outlier))
  }

  return(invisible(data))
}
