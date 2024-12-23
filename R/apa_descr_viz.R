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
nick_descrPlot <- function(data, variables, group1 = NULL, group2 = NULL, language = "english", show_jitter = TRUE) {
  if (!all(variables %in% names(data))) {
    warning("One or more specified variables are not valid columns in the dataset.")
    return(invisible(NULL))
  }

  # Helper function to create plots for a single variable
  create_plots <- function(variable) {
    # Language-specific labels
    if (language == "german") {
      y_label <- paste("Wert von", variable)
      density_label <- "Dichte"
      subtitle_density <- paste("Dichteplot von", variable)
      subtitle_boxplot <- paste("Boxplot von", variable)
      legend_title1 <- paste("Gruppen", group2)
      legend_title2 <- paste("Gruppen", group2)
    } else {
      y_label <- paste("Value of", variable)
      density_label <- "Density"
      subtitle_density <- paste("Density plot of", variable)
      subtitle_boxplot <- paste("Boxplot of", variable)
      legend_title1 <- paste("Groups", group2)
      legend_title2 <- paste("Groups", group2)
    }

    # Generate default subtitles based on groups
    if (is.null(group1) && is.null(group2)) {
      subtitle_boxplot <- paste("Boxplot of", variable)
      subtitle_density <- paste("Density plot of", variable)
    } else if (!is.null(group1) && is.null(group2)) {
      subtitle_boxplot <- paste("Boxplot grouped by", group1)
      subtitle_density <- paste("Density plot grouped by", group1)
    } else {
      subtitle_boxplot <- paste("Boxplot grouped by", group1, "and", group2)
      subtitle_density <- paste("Density plot grouped by", group1, "and", group2)
    }

    # Automatically determine optimal binwidth for density plot
    optimal_binwidth <- function(data, variable) {
      range_var <- diff(range(data[[variable]], na.rm = TRUE))
      n <- sum(!is.na(data[[variable]]))
      return(range_var / (1 + 9 * log10(n))) # Conservative approach
    }
    binwidth <- optimal_binwidth(data, variable)

    # Define color palette for groups
    apa_colors <- c("#004C97", "#00B18F", "#E7231E", "#FCCB00", "#3BB24A", "#15BEF0", "#DC3545", "#8DC440")

    # Create Boxplot
    boxplot <- ggplot(data, aes(x = factor(!!sym(group1)), y = !!sym(variable), fill = factor(!!sym(group2)))) +
      geom_boxplot(alpha = 0.7, outlier.color = "red", outlier.size = 3) +
      labs(
        subtitle = subtitle_boxplot,
        y = y_label,
        x = NULL
      ) +
      theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1))

    # Add jitter if required
    if (show_jitter) {
      boxplot <- boxplot +
        geom_jitter(position = position_jitterdodge(dodge.width = 0.75, jitter.width = 0.2), alpha = 0.2, color = "black", size = 3)
    }

    # Create Density Plot
    density_plot <- ggplot(data, aes(x = !!sym(variable), fill = factor(!!sym(group2)))) +
      geom_density(alpha = 0.4) +
      labs(
        subtitle = subtitle_density,
        y = density_label,
        x = variable
      ) +
      theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1))

    # Combine plots side by side
    plot_combined <- plot_grid(
      boxplot + theme(plot.margin = margin(t = 15, r = 10, b = 15, l = 0)),
      density_plot + theme(plot.margin = margin(t = 15, r = 15, b = 15, l = 0)),
      align = "hv", axis = "tblr", ncol = 2
    )

    return(plot_combined)
  }

  # Iterate over variables and create combined plots
  all_plots <- lapply(variables, create_plots)

  # Stack all plots vertically
  final_plot <- plot_grid(plotlist = all_plots, ncol = 1, align = "v", axis = "tblr")

  return(final_plot)
}


