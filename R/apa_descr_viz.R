#' APA-Style Combined Boxplot and Density Plot
#'
#' Generates a combined APA-style boxplot and density plot for a single variable, optionally grouped by one or two categorical variables.
#'
#'#' @param data A data frame containing the variable to be plotted.
#' @param variable The name of the variable to be plotted (as a string).
#' @param group1 An optional grouping variable (as a string).
#' @param group2 An optional second grouping variable (as a string).
#' @param subtitle_boxplot An optional subtitle for the boxplot.
#' @param subtitle_density An optional subtitle for the density plot.
#' @param y_label Custom label for the Y-axis (default is based on `variable`).
#' @param x_label Custom label for the X-axis (default is based on `variable`).
#' @param language Language for labels and subtitles, either "english" or "german".
#' @param show_jitter Logical; whether to show jittered points on the boxplot (default: TRUE).
#'
#' @return A combined ggplot object with the boxplot and density plot side by side.
#'
#' @details This function uses APA-style formatting to create a boxplot and density plot.
#' Default APA colors are used, and the function allows customization of subtitles, axis labels, and grouping.
#' The boxplot and density plot are aligned side-by-side using the `cowplot` package.
#'
#' @examples
#' data <- data.frame(values = rnorm(100), group = sample(c("A", "B"), 100, replace = TRUE))
#' nick_descrPlot(data, variable = "values", group1 = "group")
#'
#' @export
nick_descrPlot <- function(data, variable, group1 = NULL, group2 = NULL, subtitle_boxplot = NULL, subtitle_density = NULL, y_label = NULL, x_label = NULL, language = "english", show_jitter = TRUE) {
  if (!variable %in% names(data)) {
    warning("The specified variable is not a valid column in the dataset.")
    return(invisible(NULL))
  }

  # Language-specific labels
  ... # The full function code remains unchanged
}

# Combined APA-style boxplot and density plot for a single variable with optional second grouping
nick_descrPlot <- function(data, variable, group1 = NULL, group2 = NULL, subtitle_boxplot = NULL, subtitle_density = NULL, y_label = NULL, x_label = NULL, language = "english", show_jitter = TRUE) {
  if (!variable %in% names(data)) {
    warning("The specified variable is not a valid column in the dataset.")
    return(invisible(NULL))
  }

  # Language-specific labels
  if (language == "german") {
    default_y_label <- paste("Wert von", variable)
    density_label <- "Dichte"
    default_subtitle_density <- paste("Dichteplot von", variable)
    default_subtitle_boxplot <- paste("Boxplot von", variable)
    legend_title1 <- paste("Gruppen", group2)
    legend_title2 <- paste("Gruppen", group2)
  } else {
    default_y_label <- paste("Value of", variable)
    density_label <- "Density"
    default_subtitle_density <- paste("Density plot of", variable)
    default_subtitle_boxplot <- paste("Boxplot of", variable)
    legend_title1 <- paste("Groups", group2)
    legend_title2 <- paste("Groups", group2)
  }

  # Generate default labels if not provided
  if (is.null(y_label)) {
    y_label <- default_y_label
  }

  if (is.null(x_label)) {
    x_label <- variable
  }

  # Generate subtitles based on groups
  if (is.null(group1) && is.null(group2)) {
    subtitle_boxplot <- ifelse(language == "german",
                               paste("Boxplot von", variable),
                               paste("Boxplot of", variable))
    subtitle_density <- ifelse(language == "german",
                               paste("Dichteplot von", variable),
                               paste("Density plot of", variable))
  } else if (!is.null(group1) && is.null(group2)) {
    subtitle_boxplot <- ifelse(language == "german",
                               paste("Boxplot gruppiert nach", group1),
                               paste("Boxplot grouped by", group1))
    subtitle_density <- ifelse(language == "german",
                               paste("Dichteplot gruppiert nach", group1),
                               paste("Density plot grouped by", group1))
  } else {
    subtitle_boxplot <- ifelse(language == "german",
                               paste("Boxplot gruppiert nach", group1, "und", group2),
                               paste("Boxplot grouped by", group1, "and", group2))
    subtitle_density <- ifelse(language == "german",
                               paste("Dichteplot gruppiert nach", group1, "und", group2),
                               paste("Density plot grouped by", group1, "und", group2))
  }


  # Automatically determine optimal binwidth for density plot
  optimal_binwidth <- function(data, variable) {
    range_var <- diff(range(data[[variable]], na.rm = TRUE))
    n <- sum(!is.na(data[[variable]]))
    return(range_var / (1 + 9 * log10(n))) # More conservative approach
  }
  binwidth <- optimal_binwidth(data, variable)

  # Define color palette for groups
  apa_colors <- c("#004C97", "#00B18F", "#E7231E", "#FCCB00", "#3BB24A", "#15BEF0", "#DC3545", "#8DC440")

  # Boxplot
  if (is.null(group1) && is.null(group2)) {
    boxplot <- ggplot(data, aes(x = factor(1), y = !!sym(variable))) +
      geom_boxplot(alpha = 0.7, outlier.color = "red", outlier.size = 3, fill = "#004C97", color = "#212529")
    if (show_jitter) {
      boxplot <- boxplot + geom_jitter(width = 0.1, alpha = 0.2, color = "black", size = 3)
    }
    boxplot <- boxplot +
      labs(
        subtitle = subtitle_boxplot,
        y = y_label,
        x = NULL
      ) +
      theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
      theme_apa() +
      theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
            panel.grid.major.y = element_line(color = "grey85", linewidth = 0.2),
            plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 10)))
  } else if (!is.null(group1) && is.null(group2)) {
    boxplot <- ggplot(data, aes(x = !!sym(group1), y = !!sym(variable), fill = !!sym(group1))) +
      geom_boxplot(alpha = 0.7, outlier.color = "red", outlier.size = 3)
    if (show_jitter) {
      boxplot <- boxplot + geom_jitter(position = position_jitterdodge(dodge.width = 0.75, jitter.width = 0.2), alpha = 0.2, color = "black", size = 3)
    }
    boxplot <- boxplot +
      scale_fill_manual(values = apa_colors) +
      labs(
        subtitle = subtitle_boxplot,
        y = y_label,
        x = x_label
      ) +
      theme(legend.position = "none") +
      theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
            panel.grid.major.y = element_line(color = "grey85", linewidth = 0.2),
            panel.background = element_blank(),
            plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 10)))
  } else {
    boxplot <- ggplot(data, aes(x = factor(!!sym(group1)), y = !!sym(variable), fill = factor(!!sym(group2)))) +
      geom_boxplot(alpha = 0.7, outlier.color = "red", outlier.size = 3)
    if (show_jitter) {
      boxplot <- boxplot + geom_jitter(position = position_jitterdodge(dodge.width = 0.75, jitter.width = 0.2), alpha = 0.2, color = "black", size = 3)
    }
    boxplot <- boxplot +
      scale_fill_manual(values = apa_colors) +
      labs(
        subtitle = subtitle_boxplot,
        y = y_label,
        x = x_label
      ) +
      guides(fill = guide_legend(title = legend_title2)) +
      theme() +
      theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
            panel.grid.major.y = element_line(color = "grey85", linewidth = 0.2),
            panel.background = element_blank(),
            plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 10)))

  }

  # Density Plot
  if (is.null(group1) && is.null(group2)) {
    density_plot <- ggplot(data, aes(x = !!sym(variable))) +
      geom_histogram(aes(y = after_stat(density)), fill = "#004C97", color = "#212529", alpha = 0.6, binwidth = binwidth) +
      geom_density(linewidth = 1.2, colour = "#DC3545", fill = "#DC3545", alpha = 0.4) +
      labs(
        subtitle = subtitle_density,
        y = density_label,
        x = x_label
      ) +
      theme_apa() +
      theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
            panel.grid.major.y = element_line(color = "grey85", linewidth = 0.2),
            plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 10)))
  } else if (!is.null(group1) && is.null(group2)) {
    density_plot <- ggplot(data, aes(x = !!sym(variable), fill = !!sym(group1))) +
      geom_density(alpha = 0.4) +
      scale_fill_manual(values = apa_colors) +
      labs(
        subtitle = subtitle_density,
        y = density_label,
        x = x_label
      ) +
      guides(fill = guide_legend(title = legend_title1, title.position = "top")) +
      theme(legend.position.inside = c(0.8, 0.8),
            legend.background = element_rect(fill = "white", color = "black"),
            panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
            panel.grid.major.y = element_line(color = "grey85", linewidth = 0.2),
            panel.background = element_blank(),
            plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 10)))
  } else {
    density_plot <- ggplot(data, aes(x = !!sym(variable), fill = factor(!!sym(group2)))) +
      geom_density(alpha = 0.4) +
      facet_grid(rows = vars(factor(!!sym(group1)))) +
      scale_fill_manual(values = apa_colors) +
      labs(
        subtitle = subtitle_density,
        y = density_label,
        x = x_label
      ) +
      theme(legend.position.inside = c(0.8, 0.5), # Place legend within the plot
            legend.background = element_rect(fill = "white", color = "black"),
            panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
            panel.grid.major.y = element_line(color = "grey85", linewidth = 0.2),
            panel.background = element_blank(),
            plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 10))) +
      guides(fill = guide_legend(title = legend_title1))
  }

  # Combine plots side by side
  combined_plot <- plot_grid(
    boxplot + theme(plot.margin = margin(t = 15, r = 10, b = 15, l = 0)),
    density_plot + theme(plot.margin = margin(t = 15, r = 15, b = 15, l = 0)),
    align = "hv", axis = "tblr", ncol = 2 # Side by side with two columns
  )

  return(combined_plot)
}

