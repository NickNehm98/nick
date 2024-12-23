#' Descriptive Statistics and Density Plots
#'
#' Generates boxplots and density plots for one or more numeric variables, optionally grouped by one or two categorical variables. The function supports APA-style visualizations with language options for English and German.
#'
#' @param data A data frame containing the dataset.
#' @param variables A character vector specifying the names of numeric variables to visualize.
#' @param group1 An optional character string specifying the primary grouping variable for the boxplot (default: `NULL`).
#' @param group2 An optional character string specifying the secondary grouping variable for the boxplot and density plot (default: `NULL`).
#' @param language A character string specifying the language for plot labels. Options are "english" (default) or "german".
#' @param show_jitter A logical value. If `TRUE` (default), adds jittered points to the boxplot for better visualization of individual data points.
#'
#' @return A combined ggplot object containing the boxplots and density plots for the specified variables.
#'
#' @details This function allows users to generate visualizations for numeric variables, optionally grouped by one or two categorical variables. The boxplot shows the distribution of values within groups, while the density plot visualizes the overall distribution of the variable. The function automatically adjusts bin width for the density plot and uses an APA-compliant color palette for group differentiation.
#'
#' @examples
#' # Load ggplot2 for visualization
#' library(ggplot2)
#' library(cowplot)
#'
#' # Example dataset
#' data(mtcars)
#' mtcars$cyl <- as.factor(mtcars$cyl)  # Convert grouping variable to factor
#' mtcars$am <- as.factor(mtcars$am)   # Convert another grouping variable to factor
#'
#' # Single variable visualization
#' nick_descrPlot(mtcars, variables = c("mpg"), group1 = "cyl", group2 = "am", language = "english")
#'
#' # Multiple variables visualization
#' nick_descrPlot(mtcars, variables = c("mpg", "hp"), group1 = "cyl", group2 = "am", language = "german", show_jitter = FALSE)
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
    } else {
      y_label <- paste("Value of", variable)
      density_label <- "Density"
      subtitle_density <- paste("Density plot of", variable)
      subtitle_boxplot <- paste("Boxplot of", variable)
    }

    # Generate subtitles based on groups
    if (is.null(group1) && is.null(group2)) {
      subtitle_boxplot <- paste("Boxplot of", variable)
      subtitle_density <- paste("Density plot of", variable)
    } else if (!is.null(group1) && is.null(group2)) {
      subtitle_boxplot <- paste("Boxplot grouped by", group1)
      subtitle_density <- paste("Density plot grouped by", group1)
    } else if (!is.null(group1) && !is.null(group2)) {
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

    # Ensure grouping variables are factors
    if (!is.null(group1)) {
      data[[group1]] <- as.factor(data[[group1]])
    }
    if (!is.null(group2)) {
      data[[group2]] <- as.factor(data[[group2]])
    }

    # Create Boxplot
    if (is.null(group1) && is.null(group2)) {
      boxplot <- ggplot(data, aes(x = factor(1), y = !!sym(variable))) +
        geom_boxplot(alpha = 0.7, outlier.color = "red", outlier.size = 3, fill = "#004C97", color = "#212529")
      if (show_jitter) {
        boxplot <- boxplot + geom_jitter(width = 0.1, alpha = 0.2, color = "black", size = 3)
      }
    } else if (!is.null(group1) && is.null(group2)) {
      boxplot <- ggplot(data, aes(x = !!sym(group1), y = !!sym(variable), fill = !!sym(group1))) +
        geom_boxplot(alpha = 0.7, outlier.color = "red", outlier.size = 3)
      if (show_jitter) {
        boxplot <- boxplot + geom_jitter(position = position_jitter(width = 0.2), alpha = 0.2, color = "black", size = 3)
      }
      boxplot <- boxplot + scale_fill_manual(values = apa_colors)
    } else {
      boxplot <- ggplot(data, aes(x = !!sym(group1), y = !!sym(variable), fill = !!sym(group2))) +
        geom_boxplot(alpha = 0.7, outlier.color = "red", outlier.size = 3)
      if (show_jitter) {
        boxplot <- boxplot + geom_jitter(position = position_jitterdodge(dodge.width = 0.75, jitter.width = 0.2), alpha = 0.2, color = "black", size = 3)
      }
      boxplot <- boxplot + scale_fill_manual(values = apa_colors)
    }

    boxplot <- boxplot +
      labs(
        subtitle = subtitle_boxplot,
        y = y_label,
        x = NULL
      ) +
      theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1))

    # Create Density Plot
    if (is.null(group1) && is.null(group2)) {
      density_plot <- ggplot(data, aes(x = !!sym(variable))) +
        geom_histogram(aes(y = after_stat(density)), fill = "#004C97", color = "#212529", alpha = 0.6, binwidth = binwidth) +
        geom_density(linewidth = 1.2, colour = "#DC3545", fill = "#DC3545", alpha = 0.4)
    } else if (!is.null(group1) && is.null(group2)) {
      density_plot <- ggplot(data, aes(x = !!sym(variable), fill = !!sym(group1))) +
        geom_density(alpha = 0.4) +
        scale_fill_manual(values = apa_colors)
    } else {
      density_plot <- ggplot(data, aes(x = !!sym(variable), fill = !!sym(group2))) +
        geom_density(alpha = 0.4) +
        facet_grid(rows = vars(!!sym(group1))) +
        scale_fill_manual(values = apa_colors)
    }

    density_plot <- density_plot +
      labs(
        subtitle = subtitle_density,
        y = "Density",
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
