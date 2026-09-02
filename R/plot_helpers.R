library(ggplot2)

# -----------------------
# Theme
# -----------------------
plot_fill <- "#6C8EBF"
mean_color <- "#D33F3F"
median_color <- "#000000"

project_theme <- theme_minimal(base_size=13) +
theme(
    plot.title = element_text(size=16, face="bold"),
    plot.subtitle = element_text(size=11),
    axis.title = element_text(size=12, face="bold"),
    axis.text = element_text(size=11),
    panel.grid.inor = element_blank(),
    plot.margin = margin(12,15,12,12)
)

# -----------------------
# Variable labels
# -----------------------
get_display_label <- function(variable, dictionary) {
    row <- dictionary[dictionary$variable == variable,]
    if (nrow(row) == 0) {return(variable)}
    label <- row$label[1]
    unit = row$unit[1]
    if (!is.na(unit) && unit != "") {
        paste0(label, " (", unit, ")")
    } else{
        label
    }
}

# -----------------------
# Plots
# -----------------------

# Histogram settings (bin width)
get_binwidth <- function(x) {
    binwidth <- 2 * IQR(x, na.rm = TRUE) / sum(!is.na(x))^(1/3) # fridman-diaconis rule
    if (!is.finite(binwidth) || binwidth <= 0) {
        binwidth <- diff(range(x, na.rm=TRUE)) / 30
    }
    binwidth
}

# Histogram plot
plot_numeric_histogram <- function(data, variables, dictionary, figure_dir) {
    for (variable in variables) {
        label <- get_display_label(variable, dictionary)

        values <- data[[variable]]
        values <- values[!is.na(values)]

        mean_value <- mean(values)
        median_value <- median(values)
        
        line_data <- data.frame(
            statistic = c("Mean", "Median"),
            value = c(mean_value, median_value)
        )

        plot_data <- data.frame(value = values)

        p <- ggplot(plot_data, aes(x=value)) +
            geom_histogram(
                binwidth = get_binwidth(values),
                fill = plot_fill,
                color = "white",
                linewidth = 0.4
            ) +
            geom_vline(
                data = line_data,
                aes(xintercept=value, color=statistic, linetype=statistic),
                linewidth = 1.5
            ) + 
            scale_color_manual(
                values = c(Mean = mean_color, Median = median_color)
            ) +
            scale_linetype_manual(
                values = c(Mean = "dashed", Median = "solid")
            ) +
            labs(
                title = label,
                subtitle = paste(
                    "Mean:", round(mean_value, 1),
                    "| Median:", round(median_value, 1)
                ),
                x = NULL,
                y = "Number of patients",
                color = NULL,
                linetype = NULL
            ) +
            project_theme +
            theme(legend.position = "top")

            ggsave(file.path(
                figure_dir, paste0(tolower(variable), "_histogram.png")
            ),
            p,
            width = 8,
            height = 5,
            dpi = 300,
            bg = "white"
            )
    }
}

# Boxplot
plot_numeric_boxplot <- function(data, variables, dictionary, figure_dir) {
    for (variable in variables) {
        label <- get_display_label(variable, dictionary)

        values <- data[[variable]]
        values <- values[!is.na(values)]

        plot_data <- data.frame(value=values)

        p <- ggplot(plot_data, aes(y=value)) +
            geom_boxplot(
                fill = plot_fill,
                width = 0.35,
                alpha = 0.8,
                outlier.alpha = 0.4
            ) +
            labs(
                title = paste(label, "_boxplot"),
                x = NULL,
                y = label
            ) +
            project_theme + theme(
                axis.text.x = element_blank(),
                axis.ticks.x = element_blank()
            )

            ggsave(
                file.path(
                    figure_dir, paste(tolower(variable), "_boxplot.png"
                )
            ),
            p,
            width = 6,
            height = 5,
            dpi = 300,
            bg = "white"
        )
    }
}
