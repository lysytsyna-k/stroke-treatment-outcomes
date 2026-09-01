library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

# Setup

data_path <- "data/processed/ist_analysis.csv"

table_dir <- "output/tables/eda"
figure_dir <- "output/figures/eda"

numeric_summary_path <- "output/tables/validation/numeric_summary.csv"

# -----------------------
# Helper functions
# -----------------------

# Extracts continuous vars
get_continuous_baseline_vars <- function(continuous_vars) {
    continuous_vars[grepl("^R", continuous_vars) | continuous_vars == "AGE"]
}


# Summarizes numeric baseline
summarize_numeric_baseline <- function(data, continuous_vars) {
    baseline_vars <- get_continuous_baseline_vars(continuous_vars)

    data |>
    select(all_of(baseline_vars)) |>
    pivot_longer(
        cols = everything(),
        names_to = "variable",
        values_to = "value"
    ) |>
    group_by(variable) |>
    summarise(
        `Number of observations` = sum(!is.na(value)),
        `Missing values` = sum(is.na(value)),
        Mean = mean(value, na.rm = TRUE),
        `Standard deviation` = sd(value, na.rm = TRUE),
        Median = median(value, na.rm = TRUE),
        `First quartile` = quantile(value, 0.25, na.rm = TRUE),
        `Third quartile` = quantile(value, 0.75, na.rm = TRUE),
        `Interquartile range` = IQR(value, na.rm =TRUE),
        .groups = "drop"
    )
}

# Plots numeric baseline 
plot_numeric_baseline <- function(data, variables) {
    for (variable in variables) {
        plot_data <- data |>
            select(all_of(variable)) |>
            filter(!is.na(.data[[variable]]))

        p <- ggplot(plot_data, aes(x = .data[[variable]])) +
            geom_histogram(bins = 30) +
            labs(
                title = paste("Distribution of", variable),
                x = variable,
                y = "Number of patients"
            ) +
            theme_minimal()

        ggsave(
            file.path(figure_dir, paste0(tolower(variable), "_distribution.png")),
            plot = p,
            width = 8,
            height = 5
        )
    }
}


# -----------------------
# Main
# -----------------------
main <- function() {
    data <- read_csv(data_path, show_col_types = FALSE)
    numeric_validation <- read_csv(numeric_summary_path, 
        show_col_types = FALSE
    )

    # Dataset overview
    cat("Dataset overview", "\n\n")
    cat("Rows:", nrow(data), "\n")
    cat("Coulumns:", ncol(data), "\n")

    # Continuous vars
    continuous_vars = numeric_validation$variable[
        numeric_validation$variable %in% names(data)
    ]
    
    numeric_baseline <- summarize_numeric_baseline(
        data, continuous_vars
    )

    cat("\nNumeric baseline summary\n")
    print(numeric_baseline, width = Inf)
    write_csv(numeric_baseline, 
        file.path(table_dir, "baseline_numeric_summary.csv")
    )

    continuous_baseline_vars <- get_continuous_baseline_vars(continuous_vars)
    plot_numeric_baseline(data, continuous_baseline_vars)
}

main()