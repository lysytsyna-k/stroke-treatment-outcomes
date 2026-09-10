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
    panel.grid.minor = element_blank(),
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

        p <- ggplot(plot_data, aes(x=value, y="")) +
            geom_boxplot(
                fill = plot_fill,
                width = 0.35,
                alpha = 0.8,
                outlier.alpha = 0.4
            ) +
            labs(
                title = label,
                y = NULL,
                x = label
            ) +
            project_theme +
            theme(
                axis.text.y = element_blank(),
                axis.ticks.y = element_blank(),
                panel.grid.major.y = element_blank(),
                panel.grid.minor.y = element_blank()
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

# Categorical plots
plot_categorical_bar <- function(summary_data, figure_dir) {
    variables <- unique(summary_data$variable)

    for (variable in variables) {
        plot_data <- summary_data |>
            filter(.data$variable == .env$variable) |>
            arrange(percent) |>
            mutate(
                category = factor(category, levels = category)
            )
        label <- plot_data$label[1]
        p <- ggplot(plot_data, aes(x = percent, y = category)) +
            geom_col(fill = plot_fill, width = 0.7) +
            geom_text(aes(label = paste0(round(percent, 1), "%")), 
                            hjust = -0.15, size = 4) +
            scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
            labs(title = label, x = "Percent of patients", y = NULL) +
            project_theme +
            theme(panel.grid.major.y = element_blank())

        ggsave(file.path(figure_dir, paste0(tolower(variable),"_bar.png")),
            p,
            width = 8,
            height = 5,
            dpi = 300,
            bg = "white"
        )
    }
}

# Balance plot
plot_balance <- function(balance_data, treatment, dictionary, figure_dir) {
    treatment_label <- get_display_label(treatment, dictionary)

    plot_data <- balance_data |>
        arrange(absolute_smd) |>
        mutate(label = factor(label, levels = label))

    p <- ggplot(plot_data, aes(x = absolute_smd, y = label)) +
        geom_point(size = 3, color = plot_fill) +
        geom_vline(xintercept = 0.10, linetype = "dashed", color = mean_color) +
        scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
        labs(
            title = paste(treatment_label, "baseline balance"),
            subtitle = "Maximum absolute standardized mean difference",
            x = "Absolute standardized mean difference",
            y = NULL
        ) +
        project_theme +
        theme(panel.grid.major.y = element_blank())

    ggsave(file.path(figure_dir, paste0(tolower(treatment), "_balance.png")),
        p, 
        width = 8, 
        height = 7, 
        dpi = 300, 
        bg = "white"
    )
}

# Outcomes plot
plot_binary_outcome_rates <- function(summary_data, figure_dir) {
    plot_data <- summary_data |>
        arrange(event_rate) |>
        mutate(label = factor(label, levels = label))

    p <- ggplot(plot_data, aes(x = event_rate, y = label)) +
        geom_col(fill = plot_fill, width = 0.7) +
        geom_text(aes(label = paste0(round(event_rate, 1), "%")), hjust = -0.15, size = 4) +
        scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
        labs(
            title = "Clinical outcome rates",
            x = "Percent of patients",
            y = NULL
        ) +
        project_theme +
        theme(panel.grid.major.y = element_blank())

    ggsave(file.path(figure_dir, "binary_outcome_rates.png"),
           p, width = 9, height = 7, dpi = 300, bg = "white")
}

# Treatment effects
plot_risk_differences <- function(data, title, file_path) {
    plot <- ggplot(data, aes(x = risk_difference * 100, y = reorder(label, risk_difference))) +
        geom_vline(xintercept = 0, linetype = "dashed") +
        geom_errorbarh(aes(xmin = ci_lower * 100, xmax = ci_upper * 100), height = 0.2) +
        geom_point(size = 2.5) +
        labs(
            title = title,
            x = "Risk difference (percentage points)",
            y = NULL
        ) +
        project_theme

    ggsave(file_path, plot, width = 9, height = 6)
}