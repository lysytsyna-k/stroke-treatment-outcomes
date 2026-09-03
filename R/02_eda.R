library(readr)
library(dplyr)
library(tidyr)

source("R/plot_helpers.R")

# -----------------------
# Paths
# -----------------------
data_path <- "data/processed/ist_analysis.csv"
dictionary_path <- "data/reference/analysis_variable_dictionary.csv"
table_dir <- "output/tables/eda"
figure_dir <- "output/figures/eda"
numeric_figure_dir <- file.path(figure_dir, "numeric")
categorical_figure_dir <- file.path(figure_dir, "categorical")
treatment_figure_dir <- file.path(figure_dir, "treatment")
balance_table_dir <- file.path(table_dir, "balance")
balance_figure_dir <- file.path(figure_dir, "balance")

# Extracts variables for EDA
get_analysis_vars <- function(dictionary, data, target_role, target_type = NULL) {
    selected <- dictionary |>
        filter(role == target_role, variable %in% names(data))
    if (!is.null(target_type)) {
        selected <- selected |>
            filter(statistical_type %in% target_type)
    }
    selected |> pull(variable)
}

# Adds labels
add_labels <- function(data, dictionary) {
    data |>
        left_join(dictionary |> select(variable, label), by = "variable") |>
        relocate(variable, label)
}

# -----------------------
# Numeric vars
# -----------------------

# Gets numeric stats
get_numeric_stats <- function(value) {
    tibble(
        observations = sum(!is.na(value)),
        missing = sum(is.na(value)),
        mean = mean(value, na.rm = TRUE),
        standard_deviation = sd(value, na.rm = TRUE),
        median = median(value, na.rm = TRUE),
        first_quartile = as.numeric(quantile(value, 0.25, na.rm = TRUE)),
        third_quartile = as.numeric(quantile(value, 0.75, na.rm = TRUE)),
        interquartile_range = IQR(value, na.rm = TRUE),
        minimum = min(value, na.rm = TRUE),
        maximum = max(value, na.rm = TRUE)
    )
}

# Summarizes numeric stats
summarize_numeric <- function(data, variables, dictionary, group = NULL) {
    data |>
        select(all_of(c(group, variables))) |>
        pivot_longer(all_of(variables), names_to = "variable", values_to = "value") |>
        group_by(across(all_of(c(group, "variable")))) |>
        reframe(get_numeric_stats(value)) |>
        add_labels(dictionary)
}

# Formats treatment groups
format_group <- function(x) {
    x <- as.character(x)
    case_when(x == "0" ~ "No", x == "1" ~ "Yes", TRUE ~ x)
}

# Calculates numeric SMD
calculate_numeric_smd <- function(summary_data, group) {
    balance <- summary_data |>
        rename(group = all_of(group)) |>
        mutate(group = format_group(group)) |>
        select(variable, label, group, mean, standard_deviation)

    balance |>
        inner_join(
            balance,
            by = c("variable", "label"),
            suffix = c("_1", "_2"),
            relationship = "many-to-many"
        ) |>
        filter(group_1 < group_2) |>
        mutate(
            comparison = paste(group_1, "vs", group_2),
            pooled_sd = sqrt((standard_deviation_1^2 + standard_deviation_2^2) / 2),
            smd = if_else(pooled_sd == 0, 0, (mean_2 - mean_1) / pooled_sd),
            absolute_smd = abs(smd)
        ) |>
        select(variable, label, comparison, smd, absolute_smd)
}

# -----------------------
# Categorical vars
# -----------------------

# Summarizes categorical stats
summarize_categorical <- function(data, variables, dictionary, group = NULL) {
    data |>
        select(all_of(c(group, variables))) |>
        mutate(across(all_of(variables), as.character)) |>
        pivot_longer(all_of(variables), names_to = "variable", values_to = "category") |>
        mutate(
            category = replace_na(category, "Missing"),
            category = case_when(category == "0" ~ "No", category == "1" ~ "Yes", TRUE ~ category)
        ) |>
        group_by(across(all_of(c(group, "variable", "category")))) |>
        summarise(count = n(), .groups = "drop") |>
        group_by(across(all_of(c(group, "variable")))) |>
        mutate(percent = 100 * count / sum(count)) |>
        ungroup() |>
        add_labels(dictionary)
}

# Calculates categorical SMD
calculate_categorical_smd <- function(summary_data, group) {
    balance <- summary_data |>
        rename(group = all_of(group)) |>
        mutate(group = format_group(group)) |>
        filter(category != "Missing") |>
        group_by(variable, label, group) |>
        mutate(proportion = count / sum(count)) |>
        ungroup() |>
        select(variable, label, group, category, proportion)

    balance |>
        inner_join(
            balance,
            by = c("variable", "label", "category"),
            suffix = c("_1", "_2"),
            relationship = "many-to-many"
        ) |>
        filter(group_1 < group_2) |>
        mutate(
            comparison = paste(group_1, "vs", group_2),
            pooled_sd = sqrt((proportion_1 * (1 - proportion_1) + proportion_2 * (1 - proportion_2)) / 2),
            smd = if_else(pooled_sd == 0, 0, (proportion_2 - proportion_1) / pooled_sd),
            absolute_smd = abs(smd)
        ) |>
        select(variable, label, category, comparison, smd, absolute_smd)
}

# Prepares SMD summary
prepare_balance_summary <- function(numeric_smd, categorical_smd) {
    numeric <- numeric_smd |>
        mutate(category = NA_character_, type = "Continuous")
    categorical <- categorical_smd |>
        mutate(type = "Categorical")

    bind_rows(numeric, categorical) |>
        group_by(variable, label) |>
        slice_max(absolute_smd, n = 1, with_ties = FALSE) |>
        ungroup()
}

# -----------------------
# Main
# -----------------------

main <- function() {
    data <- read_csv(data_path, show_col_types = FALSE)
    dictionary <- read_csv(dictionary_path, show_col_types = FALSE)

    # Directories
    dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
    dir.create(numeric_figure_dir, recursive = TRUE, showWarnings = FALSE)
    dir.create(categorical_figure_dir, recursive = TRUE, showWarnings = FALSE)
    dir.create(treatment_figure_dir, recursive = TRUE, showWarnings = FALSE)
    dir.create(balance_table_dir, recursive = TRUE, showWarnings = FALSE)
    dir.create(balance_figure_dir, recursive = TRUE, showWarnings = FALSE)

    # Variable types
    continuous_baseline_vars <- get_analysis_vars(dictionary, data, "baseline", "continuous")
    categorical_baseline_vars <- get_analysis_vars(dictionary, data, "baseline", c("binary", "categorical"))
    treatment_vars <- get_analysis_vars(dictionary, data, "treatment")
    binary_outcome_vars <- get_analysis_vars(dictionary, data, "outcome", "binary")
    categorical_outcome_vars <- get_analysis_vars(dictionary, data, "outcome", "categorical")

    # -------------------
    # Numeric
    # -------------------
    numeric_baseline <- summarize_numeric(data, continuous_baseline_vars, dictionary)
    print(numeric_baseline, width = Inf)
    write_csv(numeric_baseline, file.path(table_dir, "baseline_numeric_summary.csv"))
    plot_numeric_histogram(data, continuous_baseline_vars, dictionary, numeric_figure_dir)
    plot_numeric_boxplot(data, continuous_baseline_vars, dictionary, numeric_figure_dir)

    # -------------------
    # Categorical
    # -------------------
    categorical_baseline <- summarize_categorical(data, categorical_baseline_vars, dictionary)
    print(categorical_baseline, n = Inf)
    write_csv(categorical_baseline, file.path(table_dir, "baseline_categorical_summary.csv"))
    plot_categorical_bar(categorical_baseline, categorical_figure_dir)

    # -------------------
    # Treatment allocation
    # -------------------
    treatment_summary <- summarize_categorical(data, treatment_vars, dictionary)
    print(treatment_summary, n = Inf)
    write_csv(treatment_summary, file.path(table_dir, "treatment_allocation_summary.csv"))
    plot_categorical_bar(treatment_summary, treatment_figure_dir)

    # -------------------
    # Baseline balance
    # -------------------
    for (treatment in treatment_vars) {
        numeric_balance <- summarize_numeric(data, continuous_baseline_vars, dictionary, group = treatment)
        categorical_balance <- summarize_categorical(data, categorical_baseline_vars, dictionary, group = treatment)
        numeric_smd <- calculate_numeric_smd(numeric_balance, treatment)
        categorical_smd <- calculate_categorical_smd(categorical_balance, treatment)
        balance_summary <- prepare_balance_summary(numeric_smd, categorical_smd)

        write_csv(numeric_balance, file.path(balance_table_dir, paste0(treatment, "_numeric_balance.csv")))
        write_csv(categorical_balance, file.path(balance_table_dir, paste0(treatment, "_categorical_balance.csv")))
        write_csv(numeric_smd, file.path(balance_table_dir, paste0(treatment, "_numeric_smd.csv")))
        write_csv(categorical_smd, file.path(balance_table_dir, paste0(treatment, "_categorical_smd.csv")))
        write_csv(balance_summary, file.path(balance_table_dir, paste0(treatment, "_smd_summary.csv")))
        plot_balance(balance_summary, treatment, dictionary, balance_figure_dir)
    }
}

main()