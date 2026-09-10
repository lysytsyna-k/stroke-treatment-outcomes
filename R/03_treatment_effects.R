library(readr)
library(dplyr)

source("R/plot_helpers.R")

# -----------------------
# Paths
# -----------------------
data_path <- "data/processed/ist_analysis.csv"
dictionary_path <- "data/reference/analysis_variable_dictionary.csv"
table_dir <- "output/tables/treatment_effects"
figure_dir <- "output/figures/treatment_effects"

# Risk difference
calculate_risk_difference <- function(data, outcome, treatment) {
    summary <- data |>
        filter(!is.na(.data[[outcome]]), !is.na(.data[[treatment]])) |>
        group_by(.data[[treatment]]) |>
        summarise(
            patients = n(),
            events = sum(.data[[outcome]] == 1),
            risk = events / patients,
            .groups = "drop"
        ) |>
        mutate(
            standard_error = sqrt(risk * (1 - risk) / patients),
            ci_lower = risk - 1.96 * standard_error,
            ci_upper = risk + 1.96 * standard_error
        )

    risk_control <- summary$risk[summary[[treatment]] == 0]
    risk_treated <- summary$risk[summary[[treatment]] == 1]
    n_control <- summary$patients[summary[[treatment]] == 0]
    n_treated <- summary$patients[summary[[treatment]] == 1]
    events_control <- summary$events[summary[[treatment]] == 0]
    events_treated <- summary$events[summary[[treatment]] == 1]

    risk_difference <- risk_treated - risk_control
    se_difference <- sqrt(
        risk_treated * (1 - risk_treated) / n_treated +
        risk_control * (1 - risk_control) / n_control
    )
    tibble(
        variable = outcome,
        n_control = n_control,
        events_control = events_control,
        risk_control = risk_control,
        n_treated = n_treated,
        events_treated = events_treated,
        risk_treated = risk_treated,
        risk_difference = risk_difference,
        ci_lower = risk_difference - 1.96 * se_difference,
        ci_upper = risk_difference + 1.96 * se_difference
    )
}

# -----------------------
# Main
# -----------------------
main <- function() {
    data <- read_csv(data_path, show_col_types= FALSE)
    dictionary <- read_csv(dictionary_path, show_col_types = FALSE)

    dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
    dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

    binary_outcome_vars <- dictionary |>
        filter(
            role == "outcome",
            statistical_type == "binary",
            !variable %in% c("dependent_6m", "recovered_6m"),
            variable %in% names(data)
        ) |>
        pull(variable)

    aspirin_effects <- bind_rows(
        lapply(
            binary_outcome_vars,
            function(outcome) calculate_risk_difference(data, outcome, "aspirin_alloc")
        )
    ) |>
        left_join(dictionary |> select(variable, label), by = "variable") |>
        relocate(variable, label)

    print(aspirin_effects, n = Inf, width = Inf)
    write_csv(aspirin_effects, file.path(table_dir, "aspirin_risk_differences.csv"))
    plot_risk_differences(aspirin_effects,
    "Effect of Aspirin Allocation on Clinical Outcomes",
    file.path(figure_dir, "aspirin_risk_differences.png"))
}

main()