#' Entferne Ausreißer aus Daten
#'
#' Diese Funktion entfernt Ausreißer basierend auf dem Interquartilsabstand (IQR).
#' Optional können Gruppierungsvariablen angegeben werden, um die Ausreißer innerhalb jeder Gruppe zu berechnen.
#'
#' @param data Ein Dataframe, aus dem Ausreißer entfernt werden sollen.
#' @param variable Der Name der zu bereinigenden Variable (als String).
#' @param group1 Eine optionale erste Gruppierungsvariable (als String).
#' @param group2 Eine optionale zweite Gruppierungsvariable (als String).
#' @param remove_outliers Logical; wenn TRUE, werden Ausreißer entfernt (Default: TRUE).
#' @return Ein bereinigter Dataframe ohne Ausreißer.
#' @examples
#' df <- data.frame(values = c(1, 2, 3, 100, 5), group1 = c("A", "A", "B", "B", "B"), group2 = c("X", "X", "Y", "Y", "Y"))
#' remove_outliers(df, variable = "values")
#' remove_outliers(df, variable = "values", group1 = "group1")
#' remove_outliers(df, variable = "values", group1 = "group1", group2 = "group2")
#' @export
remove_outliers <- function(data, variable, group1 = NULL, group2 = NULL, remove_outliers = TRUE) {
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
