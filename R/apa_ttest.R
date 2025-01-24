#' APA-Style T-Test Reporting Function
#'
#' This function performs independent t-tests for each numeric variable in the dataset grouped by a categorical variable, producing descriptive statistics, inferential statistics (t-values, degrees of freedom, p-values), and effect sizes (Cohen's d) in APA style. The results are presented as a formatted table and APA-style text, with an option to export them to a Word document.
#'
#' @param data A data frame containing the dataset.
#' @param group A character string specifying the grouping variable. This variable must have exactly two levels.
#' @param variables A character vector specifying the names of numeric variables to test.
#' @param language A character string specifying the language for the output. Options are "en" (English, default) or "ger" (German).
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
#' nick_ttest_apa(mtcars, group = "am", variables = c("mpg", "hp"), language = "ger", output_file = "APA_Output.docx", export = TRUE)
#'
#' @export
nick_ttest_apa <- function(data, group, variables, language = "en", output_file = "APA_Table.docx", export = FALSE) {

  # Eingabe validieren
  if (!(group %in% colnames(data))) {
    stop(sprintf(ifelse(language == "ger", "Die Gruppenvariable '%s' wurde im Datensatz nicht gefunden.",
                        "Grouping variable '%s' not found in the dataset."), group))
  }

  # Variablen überprüfen
  missing_vars <- setdiff(variables, colnames(data))
  if (length(missing_vars) > 0) {
    stop(sprintf(ifelse(language == "ger", "Die folgenden Variablen fehlen im Datensatz: %s",
                        "The following variables are missing in the dataset: %s"),
                 paste(missing_vars, collapse = ", ")))
  }

  # Sicherstellen, dass die Gruppenvariable genau zwei Stufen hat
  unique_groups <- unique(data[[group]])
  if (length(unique_groups) != 2) {
    stop(sprintf(ifelse(language == "ger", "Die Gruppenvariable '%s' muss genau zwei Werte haben.",
                        "Grouping variable '%s' must have exactly two unique values."), group))
  }

  # Ergebnis-Datenrahmen initialisieren
  results <- data.frame(
    Variable = character(),
    Group1_M = numeric(), Group1_SD = numeric(),
    Group2_M = numeric(), Group2_SD = numeric(),
    t_value = numeric(), df = numeric(),
    p_value = numeric(), Cohens_d = numeric(),
    Hedges_g = numeric(), CI_Lower = numeric(), CI_Upper = numeric(),
    Normality = character(), Heteroscedasticity = character(),
    stringsAsFactors = FALSE
  )

  # Beschreibungstexte sammeln
  description_texts <- ""

  # Für jede Testvariable Statistik berechnen
  for (var in variables) {

    cat(strrep("#", 60), "\n")
    cat(strrep("#", 60), "\n")
    cat("\n",sprintf("            Analysis for Variable: %s", var), "\n", "\n")
    cat(strrep("#", 60),"\n")
    cat(strrep("#", 60), "\n\n")

    # Remove outliers from the dataset before performing the t-test
    data <- nick_remOutliers(data, variable = var, group1 = group, remove_outliers = TRUE)

    # Count the number of outliers removed
    original_count <- nrow(data)
    data_cleaned <- nick_remOutliers(data, variable = var, group1 = group, remove_outliers = TRUE)
    remaining_count <- nrow(data_cleaned)
    outliers_removed <- original_count - remaining_count

    cat(strrep("=", 20), "\n")
    cat("Outlier Removal\n")
    cat(strrep("=", 20), "\n\n")

    cat(sprintf("A total of %d outliers were removed prior to visualization and analysis to ensure robust results.\n\n", outliers_removed))
    # Generate and display the descriptive statistics and density plots
    plot <- nick_descrPlot(data, variables = c(var), group1 = group, language = language, show_jitter = TRUE)

    # Print the visualization to the console
    print(plot)

    # Gruppendaten extrahieren
    group1 <- data %>% filter(.data[[group]] == unique_groups[1]) %>% pull(.data[[var]])
    group2 <- data %>% filter(.data[[group]] == unique_groups[2]) %>% pull(.data[[var]])

    # Statistiken berechnen
    group1_M <- mean(group1, na.rm = TRUE)
    group1_SD <- sd(group1, na.rm = TRUE)
    group2_M <- mean(group2, na.rm = TRUE)
    group2_SD <- sd(group2, na.rm = TRUE)

    # Assumption Checks
    cat(strrep("=", 20), "\n")
    cat("Assumption Checks\n")
    cat(strrep("=", 20), "\n\n")

    # Shapiro-Wilk Normality Test
    shapiro_group1 <- shapiro.test(group1)
    shapiro_group2 <- shapiro.test(group2)

    normality_status <- ifelse(shapiro_group1$p.value > 0.05 & shapiro_group2$p.value > 0.05, "met", "violated")
    normality_text <- sprintf(
      "Normality assumption: %s\n  - %s: W = %.2f, p = %.3f\n  - %s: W = %.2f, p = %.3f\n",
      normality_status, unique_groups[1], shapiro_group1$statistic, shapiro_group1$p.value,
      unique_groups[2], shapiro_group2$statistic, shapiro_group2$p.value
    )
    cat(normality_text, "\n")

    # Levene's Test for Homogeneity of Variance
    levene_test <- leveneTest(data[[var]] ~ data[[group]], data = data)
    equal_var <- levene_test$`Pr(>F)`[1] > 0.05
    heteroscedasticity_text <- sprintf(
      "Homogeneity of variance: %s\n  - F(%d, %d) = %.2f, p = %.3f\n",
      ifelse(equal_var, "met", "violated"),
      levene_test$Df[1], levene_test$Df[2], levene_test$"F value"[1], levene_test$`Pr(>F)`[1]
    )
    cat(heteroscedasticity_text, "\n")

    # Sample Size Considerations
    sample_size <- length(group1) + length(group2)
    sample_size_text <- ifelse(
      sample_size > 30,
      "Sample size is sufficiently large (n > 30). The t-test is robust to normality violations.",
      "Sample size is small (n ≤ 30). Interpret results with caution regarding normality violations."
    )
    cat("Sample size check:", sample_size_text, "\n\n")

    # Run t-Test
    cat(strrep("=", 20), "\n")
    cat("T-Test Results\n")
    cat(strrep("=", 20), "\n\n")

    t_test <- t.test(group1, group2)
    t_value <- t_test$statistic
    df <- round(t_test$parameter, 2)
    p_value <- t_test$p.value
    conf_int <- t_test$conf.int

    # Cohen's d Calculation
    d_value <- cohen.d(group1, group2)$estimate

    # APA-style result formatting
    if (p_value < 0.05) {
      description_text <- sprintf(
        "Significant difference found:\n  - %s (n = %d, M = %.2f, SD = %.2f)\n  - %s (n = %d, M = %.2f, SD = %.2f)\n  - t(%.2f) = %.2f, p = %.3f, d = %.2f\n",
        unique_groups[1], length(group1), group1_M, group1_SD,
        unique_groups[2], length(group2), group2_M, group2_SD,
        df, t_value, p_value, d_value
      )
    } else {
      description_text <- sprintf(
        "No significant difference:\n  - %s (n = %d, M = %.2f, SD = %.2f)\n  - %s (n = %d, M = %.2f, SD = %.2f)\n  - t(%.2f) = %.2f, p = %.3f, d = %.2f\n",
        unique_groups[1], length(group1), group1_M, group1_SD,
        unique_groups[2], length(group2), group2_M, group2_SD,
        df, t_value, p_value, d_value
      )
    }
    cat(description_text, "\n")

    # Collect summary text for the final report
    # Final Summary Text with updated references and Welch test clarification

    cat(strrep("=", 20), "\n")
    cat(green$bold("Summary of Findings"), "\n")
    cat(strrep("=", 20), "\n\n")

    # Format statistical values to 2 decimal places, p-value to 3 decimal places
    formatted_shapiro_1 <- sprintf("%.2f", shapiro_group1$statistic[[1]])
    formatted_shapiro_2 <- sprintf("%.2f", shapiro_group2$statistic[[1]])
    formatted_p1 <- sprintf("%.3f", shapiro_group1$p.value)
    formatted_p2 <- sprintf("%.3f", shapiro_group2$p.value)
    formatted_F <- sprintf("%.2f", levene_test$`F value`[[1]])
    formatted_p_levene <- sprintf("%.3f", levene_test$`Pr(>F)`[1])
    formatted_t_value <- sprintf("%.2f", t_value)
    formatted_p_value <- sprintf("%.3f", p_value)
    formatted_d_value <- sprintf("%.2f", d_value)
    formatted_conf_int_1 <- sprintf("%.2f", conf_int[1])
    formatted_conf_int_2 <- sprintf("%.2f", conf_int[2])
    formatted_group1_M <- sprintf("%.2f", group1_M)
    formatted_group1_SD <- sprintf("%.2f", group1_SD)
    formatted_group2_M <- sprintf("%.2f", group2_M)
    formatted_group2_SD <- sprintf("%.2f", group2_SD)

    # Normality and Homogeneity of Variance Section
    normality_section <- paste0(
      bold("Normality and Homogeneity of Variance\n\n"),
      "A Shapiro-Wilk test was conducted to assess normality for the variable ", bold(var),
      ". Results indicated that normality was ",
      ifelse(normality_status == "met", green$bold("met"), red$bold("violated")),
      ", with the ", unique_groups[1], " group having W = ", formatted_shapiro_1,
      " and p = ", formatted_p1, ", while the ", unique_groups[2],
      " group had W = ", formatted_shapiro_2, " and p = ", formatted_p2,
      ". When the sample size exceeds thirty observations, the Welch test remains robust against violations of normality, ",
      "making it appropriate to use in such cases (Herzog, Francis, & Clarke, 2019; Stone, 2010). ",
      "Levene’s test indicated that the assumption of equal variances was ",
      ifelse(equal_var, green("met"), red("violated")),
      ", with F(", levene_test$Df[1], ", ", levene_test$Df[2],
      ") = ", formatted_F, " and p = ", formatted_p_levene, ".\n\n"
    )

    # Welch t-test Results Section
    welch_section <- paste0(
      bold("Welch t-test Results\n\n"),
      "An independent Welch t-test was conducted to compare the means of the two groups for ", bold(var),
      ". The ", unique_groups[1], " group had a mean of ", formatted_group1_M,
      " with a standard deviation of ", formatted_group1_SD,
      ", while the ", unique_groups[2], " group had a mean of ", formatted_group2_M,
      " with a standard deviation of ", formatted_group2_SD,
      ". The analysis revealed ", ifelse(p_value < 0.05, red$bold("a significant difference"), green$bold("no significant difference")),
      " between the groups. The test statistic was t(", df, ") = ", formatted_t_value,
      ", with p = ", formatted_p_value, ". Cohen’s d was ", formatted_d_value,
      " with a 95% confidence interval ranging from ", formatted_conf_int_1,
      " to ", formatted_conf_int_2, ", suggesting ",
      ifelse(p_value < 0.05, red("a meaningful difference"), green("no meaningful difference")),
      " between the groups.\n\n"
    )

    # Why Welch Test Section
    why_welch_section <- paste0(
      bold("Why the Welch Test?"), "\n\n",
      "The Welch test was used due to its robustness against normality violations and its ability to handle unequal variances.\n",
      "It is widely recommended over the traditional Student's t-test (Herzog, Francis, & Clarke, 2019; Stone, 2010).\n",
      "Research by Kubinger, Rasch, and Moder (2009) further supports the use of the Welch test without prior normality checks, ",
      "stating that it maintains the nominal Type I error rate and provides higher power under unequal variances.\n\n"
    )

    # References Section
    references_section <- paste0(
      bold("Recommended Literature"), "\n\n",
      "- Herzog, M. H., Francis, G., & Clarke, A. (2019). Understanding statistics and experimental design: How to not lie with statistics. Springer.\n",
      "- Stone, E. R. (2010). t Test, Independent Samples. In Salkind, N. J. (Ed.), Encyclopedia of research design (pp. 1551–1556). SAGE.\n",
      "- Kubinger, K. D., Rasch, D., & Moder, K. (2009). Zur Legende der Voraussetzungen des t-Tests für unabhängige Stichproben. Psychologische Rundschau, 60(1), 26–27.\n",
      "- Rasch, D., Kubinger, K. D., & Moder, K. (2011). The two-sample t test: Pre-testing its assumptions does not pay off. Statistical Papers, 52(1), 219–231.\n",
      "- Ruxton, G. D. (2006). The unequal variance t-test is an underused alternative to Student’s t-test and the Mann-Whitney U test. Behavioral Ecology, 17(4), 688–690.\n"
    )


    # Print the final summar

    cat(normality_section, "\n\n")
    cat(welch_section, "\n\n")
    cat(why_welch_section, "\n\n")
    cat(references_section, "\n\n")


    # Collect description texts
    # description_texts <- paste(description_texts, description_text, sep = "\n")

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
    Variable = ifelse(language == "ger", "Variable", "Variable"),
    Group1_M = "M",
    Group1_SD = "SD",
    Group2_M = "M",
    Group2_SD = "SD",
    t_value = ifelse(language == "ger", "t-Wert", "t"),
    t_display = ifelse(language == "ger", "df", "df"),
    p_value = "p",
    Cohens_d = ifelse(language == "ger", "Cohen's d", "Cohen's d")
  )

  # Linien im Header anpassen
  ft <- hline(ft, part = "header", border = officer::fp_border(width = 0))  # Alle Linien entfernen
  ft <- hline(ft, i = 1, j = 2:5, part = "header", border = officer::fp_border(width = 1.5)) # Linie unter den Gruppennamen
  ft <- hline_top(ft, part = "header", border = officer::fp_border(width = 1.5))            # Obere Linie
  ft <- hline_bottom(ft, part = "header", border = officer::fp_border(width = 1.5))         # Untere Linie

  # Titel dynamisch anpassen
  title <- if (length(variables) == 2) {
    if (language == "ger") {
      paste("Deskr. Statistiken und T-Test Ergebnisse für", variables[1], "und", variables[2], "gruppiert nach", group)
    } else {
      paste("Descr. Statistics and T-Test results for", variables[1], "and", variables[2], "grouped by", group)
    }
  } else {
    if (language == "ger") {
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
    #doc <- body_add_par(doc, value = "\n", style = "Normal") # Add spacing between table and descriptions
    #doc <- body_add_par(doc, value = description_texts, style = "Normal")
    print(doc, target = output_file)
    cat(ifelse(language == "ger", "\nDas Word-Dokument wurde erfolgreich erstellt: ", "\nThe Word document was successfully created: "), output_file, "\n")
  }

  # Tabelle zurückgeben
  return(ft)
}


