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
calculate_risk_difference <- function(data, outcome, treatment, control_level, treated_level) {
    summary <- data |>
        filter(!is.na(.data[[outcome]]), !is.na(.data[[treatment]])) |>
        group_by(.data[[treatment]]) |>
        summarise(
            patients=n(),
            events=sum(.data[[outcome]] == 1),
            risk=events / patients,
            .groups="drop"
        )

    risk_control <- summary$risk[summary[[treatment]] == control_level]
    risk_treated <- summary$risk[summary[[treatment]] == treated_level]
    n_control <- summary$patients[summary[[treatment]] == control_level]
    n_treated <- summary$patients[summary[[treatment]] == treated_level]
    events_control <- summary$events[summary[[treatment]] == control_level]
    events_treated <- summary$events[summary[[treatment]] == treated_level]

    risk_difference <- risk_treated - risk_control
    se_difference <- sqrt(risk_treated*(1-risk_treated)/n_treated + risk_control*(1-risk_control)/n_control)
    ci_lower <- risk_difference - 1.96*se_difference
    ci_upper <- risk_difference + 1.96*se_difference
    effect_type <- case_when(risk_difference < 0 ~ "NNT", risk_difference > 0 ~ "NNH", TRUE ~ NA_character_)
    patients_per_event <- if_else(risk_difference == 0, NA_real_, ceiling(1/abs(risk_difference)))

    tibble(
        variable=outcome,
        n_control=n_control,
        events_control=events_control,
        risk_control=risk_control,
        n_treated=n_treated,
        events_treated=events_treated,
        risk_treated=risk_treated,
        risk_difference=risk_difference,
        ci_lower=ci_lower,
        ci_upper=ci_upper,
        ci_crosses_zero=ci_lower <= 0 & ci_upper >= 0,
        effect_type=effect_type,
        patients_per_event=patients_per_event
    )
}

# Treatment effects
calculate_treatment_effects <- function(data, outcomes, treatment, control_level, treated_level, dictionary) {
    bind_rows(lapply(outcomes, function(outcome) {
        calculate_risk_difference(data, outcome, treatment, control_level, treated_level)
    })) |>
        left_join(dictionary |> select(variable, label), by="variable") |>
        relocate(variable, label)
}

# Factorial treatment effects
calculate_factorial_effects <- function(data, outcomes, dictionary) {
    heparin_levels <- c("None", "Low", "Medium")

    bind_rows(lapply(heparin_levels, function(level) {
        calculate_treatment_effects(
            data |> filter(heparin_alloc == level),
            outcomes,
            "aspirin_alloc",
            0,
            1,
            dictionary
        ) |>
            mutate(heparin_alloc=level, .after=label)
    }))
}

# -----------------------
# Main
# -----------------------
main <- function() {
    data <- read_csv(data_path, show_col_types=FALSE)
    dictionary <- read_csv(dictionary_path, show_col_types=FALSE)

    dir.create(table_dir, recursive=TRUE, showWarnings=FALSE)
    dir.create(figure_dir, recursive=TRUE, showWarnings=FALSE)

    binary_outcome_vars <- dictionary |>
        filter(
            role == "outcome",
            statistical_type == "binary",
            !variable %in% c("dependent_6m", "recovered_6m"),
            variable %in% names(data)
        ) |>
        pull(variable)

    # Aspirin
    aspirin_effects <- calculate_treatment_effects(data, binary_outcome_vars, "aspirin_alloc", 0, 1, dictionary)

    print(aspirin_effects, n=Inf, width=Inf)
    write_csv(aspirin_effects, file.path(table_dir, "aspirin_risk_differences.csv"))
    plot_risk_differences(aspirin_effects, "Effect of Aspirin Allocation on Clinical Outcomes",
        file.path(figure_dir, "aspirin_risk_differences.png"))

    # Heparin
    heparin_low_effects <- calculate_treatment_effects(data, binary_outcome_vars, "heparin_alloc", "None", "Low", dictionary) |>
        mutate(comparison="Low vs None")

    heparin_medium_effects <- calculate_treatment_effects(data, binary_outcome_vars, "heparin_alloc", "None", "Medium", dictionary) |>
        mutate(comparison="Medium vs None")

    heparin_effects <- bind_rows(heparin_low_effects, heparin_medium_effects) |>
        relocate(variable, label, comparison)

    print(heparin_effects, n=Inf, width=Inf)
    write_csv(heparin_effects, file.path(table_dir, "heparin_risk_differences.csv"))
    plot_risk_differences(heparin_effects, "Effect of Heparin Allocation on Clinical Outcomes",
        file.path(figure_dir, "heparin_risk_differences.png"))

    # Factorial treatment effects
    factorial_effects <- calculate_factorial_effects(data, binary_outcome_vars, dictionary)

    print(factorial_effects, n=Inf, width=Inf)
    write_csv(factorial_effects, file.path(table_dir, "factorial_treatment_effects.csv"))

    # Treatment combinations
    treatment_combinations <- data |>
        mutate(
            treatment_combo=case_when(
                aspirin_alloc == 0 & heparin_alloc == "None" ~ "No aspirin + No heparin",
                aspirin_alloc == 1 & heparin_alloc == "None" ~ "Aspirin + No heparin",
                aspirin_alloc == 0 & heparin_alloc == "Low" ~ "No aspirin + Low heparin",
                aspirin_alloc == 1 & heparin_alloc == "Low" ~ "Aspirin + Low heparin",
                aspirin_alloc == 0 & heparin_alloc == "Medium" ~ "No aspirin + Medium heparin",
                aspirin_alloc == 1 & heparin_alloc == "Medium" ~ "Aspirin + Medium heparin"
            )
        )

    combo_outcomes <- bind_rows(lapply(binary_outcome_vars, function(outcome) {
        treatment_combinations |>
            filter(!is.na(.data[[outcome]]), !is.na(treatment_combo)) |>
            group_by(aspirin_alloc, heparin_alloc, treatment_combo) |>
            summarise(
                patients=n(),
                events=sum(.data[[outcome]] == 1),
                risk=events/patients,
                standard_error=sqrt(risk*(1-risk)/patients),
                ci_lower=risk - 1.96*standard_error,
                ci_upper=risk + 1.96*standard_error,
                .groups="drop"
            ) |>
            mutate(variable=outcome, .before=1)
    })) |>
        left_join(dictionary |> select(variable, label), by="variable") |>
        relocate(variable, label)

    print(combo_outcomes, n=Inf, width=Inf)
    write_csv(combo_outcomes, file.path(table_dir, "treatment_combination_outcomes.csv"))

    # Interaction plots
    interaction_outcomes <- c("dead_6m", "NCB14")

    for (outcome in interaction_outcomes) {
        plot_treatment_interaction(combo_outcomes, outcome,
            file.path(figure_dir, paste0("interaction_", tolower(outcome), ".png")))
    }
}

main()