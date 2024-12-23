#' APA-Style T-Test Reporting Function
#'
#' This function performs independent t-tests for each numeric variable in the dataset grouped by a categorical variable, producing descriptive statistics, inferential statistics (t-values, degrees of freedom, p-values), and effect sizes (Cohen's d) in APA style. The results are presented as a formatted table and APA-style text, with an option to export them to a Word document.
#'
#' @param data A data frame containing the dataset.
#' @param group A character string specifying the grouping variable. This variable must have exactly two levels.
#' @param variables A character vector specifying the names of numeric variables to test.
#' @param language A character string specifying the language for the output. Options are "en" (English, default) or "de" (German).
#' @param output_file A character string specifying the file name for the exported Word document. Defaults to "APA_Table.docx".
#' @param export A logical value. If `TRUE`, the table and text reports are exported to a Word document. If `FALSE`, the table is displayed in the RStudio Viewer, and the text is printed to the console. Defaults to `FALSE`.
#'
#' @return A `flextable` object containing the formatted APA-style table. When `export = FALSE`, the table is shown in the Viewer, and text reports are printed to the console. When `export = TRUE`, both are saved to a Word document specified by `output_file`.
#'
#' @details This function is designed for two-group comparisons using independent (Welch) t-tests. It dynamically generates APA-style reports in English or German based on the `language` parameter. The output can be tailored for console/Viewer display or exported to a Word document for use in reports and publications.
#'
#' @examples
#' # Example usage
#' library(dplyr)
#' data(mtcars)
#' mtcars$am <- as.factor(mtcars$am) # Convert grouping variable to a factor
#'
#' # Generate APA-style table and text without exporting
#' nick_ttest_apa(mtcars, group = "am", variables = c("mpg", "hp"), language = "en", export = FALSE)
#'
#' # Generate APA-style table and text and export to a Word file
#' nick_ttest_apa(mtcars, group = "am", variables = c("mpg", "hp"), language = "de", output_file = "APA_Output.docx", export = TRUE)
#'
#' @export
nick_ttest_apa <- function(data, group, variables, language = "en", output_file = "APA_Table.docx", export = FALSE) {

  # Eingabe validieren
  if (!(group %in% colnames(data))) {
    stop(sprintf(ifelse(language == "de", "Die Gruppenvariable '%s' wurde im Datensatz nicht gefunden.",
                        "Grouping variable '%s' not found in the dataset."), group))
  }

  # Variablen überprüfen
  missing_vars <- setdiff(variables, colnames(data))
  if (length(missing_vars) > 0) {
    stop(sprintf(ifelse(language == "de", "Die folgenden Variablen fehlen im Datensatz: %s",
                        "The following variables are missing in the dataset: %s"),
                 paste(missing_vars, collapse = ", ")))
  }

  # Sicherstellen, dass die Gruppenvariable genau zwei Stufen hat
  unique_groups <- unique(data[[group]])
  if (length(unique_groups) != 2) {
    stop(sprintf(ifelse(language == "de", "Die Gruppenvariable '%s' muss genau zwei Werte haben.",
                        "Grouping variable '%s' must have exactly two unique values."), group))
  }

  # Ergebnis-Datenrahmen initialisieren
  results <- data.frame(
    Variable = character(),
    Group1_M = numeric(),
    Group1_SD = numeric(),
    Group2_M = numeric(),
    Group2_SD = numeric(),
    t_value = numeric(),
    df = numeric(),
    p_value = numeric(),
    Cohens_d = numeric(),
    stringsAsFactors = FALSE
  )

  # Beschreibungstexte sammeln
  description_texts <- ""

  # Für jede Testvariable Statistik berechnen
  for (var in variables) {
    # Gruppendaten extrahieren
    group1 <- data %>% filter(.data[[group]] == unique_groups[1]) %>% pull(.data[[var]])
    group2 <- data %>% filter(.data[[group]] == unique_groups[2]) %>% pull(.data[[var]])

    # Statistiken berechnen
    group1_M <- mean(group1, na.rm = TRUE)
    group1_SD <- sd(group1, na.rm = TRUE)
    group2_M <- mean(group2, na.rm = TRUE)
    group2_SD <- sd(group2, na.rm = TRUE)

    # t-Test durchführen
    t_test <- t.test(group1, group2)
    t_value <- t_test$statistic
    df <- round(t_test$parameter, 2)
    p_value <- t_test$p.value

    # Cohen's d berechnen
    d_value <- cohen.d(group1, group2)$estimate

    # Generate APA-style description
    if (p_value < 0.05) {
      description_text <- sprintf(
        ifelse(language == "en",
               "APA-style result for %s:\nIndependent (Welch) T-Test\nThe %d participants in the %s group (M = %.2f, SD = %.2f) compared to the %d participants in the %s group (M = %.2f, SD = %.2f) demonstrated significantly different scores, t(%.2f) = %.2f, p = %.3f., d = %.2f\n",
               "APA-Stil Ergebnis für %s:\nUnabhängiger (Welch) T-Test\nDie %d Teilnehmer in der %s-Gruppe (M = %.2f, SD = %.2f) im Vergleich zu den %d Teilnehmern in der %s-Gruppe (M = %.2f, SD = %.2f) zeigten signifikant unterschiedliche Werte, t(%.2f) = %.2f, p = %.3f., d = %.2f\n"),
        var, length(group1), unique_groups[1], group1_M, group1_SD,
        length(group2), unique_groups[2], group2_M, group2_SD, df, t_value, p_value, d_value)
    } else {
      description_text <- sprintf(
        ifelse(language == "en",
               "APA-style result for %s:\nNo significant effect, t(%.2f) = %.2f, p = %.3f, d = %.2f, despite %s (M = %.2f, SD = %.2f) scoring differently than %s (M = %.2f, SD = %.2f).\n",
               "APA-Stil Ergebnis für %s:\nEs gab keinen signifikanten Effekt, t(%.2f) = %.2f, p = %.3f, d = %.2f, obwohl %s (M = %.2f, SD = %.2f) unterschiedlich abschnitten als %s (M = %.2f, SD = %.2f).\n"),
        var, df, t_value, p_value, d_value,
        unique_groups[1], group1_M, group1_SD,
        unique_groups[2], group2_M, group2_SD)
    }

    # Print to console
    cat(description_text)

    # Beschreibung sammeln
    description_texts <- paste(description_texts, description_text, sep = "\n")

    # Ergebnisse hinzufügen
    results <- rbind(
      results,
      data.frame(
        Variable = var,
        Group1_M = group1_M,
        Group1_SD = group1_SD,
        Group2_M = group2_M,
        Group2_SD = group2_SD,
        t_value = t_value,
        df = df,
        p_value = p_value,
        Cohens_d = d_value
      )
    )
  }

  # Ergebnisse formatieren
  results <- results %>%
    mutate(
      Group1_M = sprintf("%.2f", Group1_M),
      Group1_SD = sprintf("%.2f", Group1_SD),
      Group2_M = sprintf("%.2f", Group2_M),
      Group2_SD = sprintf("%.2f", Group2_SD),
      t_value = sprintf("%.2f", t_value),
      t_display = sprintf("%.2f", df),
      p_value = sprintf("%.3f", p_value),
      Cohens_d = sprintf("%.2f", Cohens_d)
    ) %>%
    select(Variable, Group1_M, Group1_SD, Group2_M, Group2_SD, t_value, t_display, p_value, Cohens_d)

  # Tabelle erstellen
  group_labels <- as.character(unique_groups)
  ft <- flextable(results)
  ft <- add_header_row(
    ft,
    values = c("", group_labels[1], group_labels[2], "", "", "", ""),
    colwidths = c(1, 2, 2, 1, 1, 1, 1)
  )
  ft <- set_header_labels(
    ft,
    Variable = ifelse(language == "de", "Variable", "Variable"),
    Group1_M = "M",
    Group1_SD = "SD",
    Group2_M = "M",
    Group2_SD = "SD",
    t_value = ifelse(language == "de", "t-Wert", "t"),
    t_display = ifelse(language == "de", "df", "df"),
    p_value = "p",
    Cohens_d = ifelse(language == "de", "Cohen's d", "Cohen's d")
  )

  # Linien im Header anpassen
  ft <- hline(ft, part = "header", border = officer::fp_border(width = 0))  # Alle Linien entfernen
  ft <- hline(ft, i = 1, j = 2:5, part = "header", border = officer::fp_border(width = 1.5)) # Linie unter den Gruppennamen
  ft <- hline_top(ft, part = "header", border = officer::fp_border(width = 1.5))            # Obere Linie
  ft <- hline_bottom(ft, part = "header", border = officer::fp_border(width = 1.5))         # Untere Linie

  # Titel dynamisch anpassen
  title <- if (length(variables) == 2) {
    if (language == "de") {
      paste("Deskr. Statistiken und T-Test Ergebnisse für", variables[1], "und", variables[2], "gruppiert nach", group)
    } else {
      paste("Descr. Statistics and T-Test results for", variables[1], "and", variables[2], "grouped by", group)
    }
  } else {
    if (language == "de") {
      paste("Deskr. Statistiken und T-Test Ergebnisse für verschiedene Variablen gruppiert nach", group)
    } else {
      paste("Descr. statistics and T-Test results for various variables grouped by", group)
    }
  }

  ft <- set_caption(ft, caption = title)

  # Ausrichtung und Autofit
  ft <- align(ft, align = "center", part = "all")
  ft <- align(ft, j = 1, align = "left", part = "all")  # Linksbündig für die Variable-Spalte
  ft <- autofit(ft)

  # Exportoption
  if (export) {
    doc <- read_docx()
    doc <- body_add_flextable(doc, ft)
    doc <- body_add_par(doc, value = "\n", style = "Normal") # Add spacing between table and descriptions
    doc <- body_add_par(doc, value = description_texts, style = "Normal")
    print(doc, target = output_file)
    cat(ifelse(language == "de", "\nDas Word-Dokument wurde erfolgreich erstellt: ", "\nThe Word document was successfully created: "), output_file, "\n")
  }

  # Tabelle zurückgeben
  return(ft)
}


