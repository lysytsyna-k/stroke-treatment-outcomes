library(readr)
library(dplyr)
library(stringr)

# -----------------------
# Paths
# -----------------------

variable_info_path <- "data/reference/IST_variables.csv"
dictionary_path <- "data/reference/analysis_variable_dictionary.csv"
analysis_data_path <- "data/processed/ist_analysis.csv"
schema_summary_path <- "output/tables/validation/schema_summary.csv"

# -----------------------
# Build dictionary 
# -----------------------

get_unit <- function(description) {
    parenthetical <- str_match(description, "\\(([^)]+)\\)")[, 2]

    case_when(
        !is.na(parenthetical) &
            str_length(parenthetical) <= 10 &
            !str_detect(parenthetical, "[=/,]") &
            !str_detect(parenthetical, "\\s") ~ parenthetical,

        str_detect(description, regex("\\bin years$", ignore_case = TRUE)) ~ "years",
        str_detect(description, regex("\\bin hours$", ignore_case = TRUE)) ~ "hours",
        str_detect(description,regex("\\bin days$", ignore_case = TRUE)) ~ "days",

        TRUE ~ NA_character_
    )
}

# Makes readable labels
make_readable_label <- function(description) {
    description |>
    str_remove("\\s*\\([^)]*\\).*") |>
    str_remove("\\s*;.*$") |>
    str_remove("\\s+in (years|hours|days)$") |>
    str_remove("\\s+at randomisation$") |>
    str_trim()
}

# Generates labels from  the column names from cleaned dataset
make_label_from_name <- function(variable) {
    variable |>
    str_replace_all("_", " ") |>
    str_replace_all("\\b6m\\b", "6 months") |>
    str_replace_all("\\b14d\\b", "14 days") |>
    str_squish() |>
    str_to_sentence()
}

# Removes headings and builds the dictionary itself
build_original_dictionary <- function(path) {
    read_delim(
        path,
        delim = ";",
        col_names = c("variable", "description"),
        locale = locale(encoding = "windows-1252"),
        show_col_types = FALSE,
        trim_ws = TRUE
    ) |>
    filter(
        !is.na(description),
        variable != ""
    ) |>
    mutate(
        unit = get_unit(description),
        label = make_readable_label(description)
    )
}

# Assigns automatic analytical role
classify_role <- function(variable, description = NA_character_) {
    case_when(
        str_detect(variable, "_alloc$") ~ "treatment",

        str_detect(variable, "_received$|_compliant$") ~ "post_treatment",
        variable == "ONDRUG" ~ "post_treatment",

        str_detect(variable, "_6m$") ~ "outcome",
        str_detect(variable, "14$") ~ "outcome",

        str_detect(variable, "^DRS|^DPE$|^DDEAD|^DIED$|^DEAD[1-8]$"
        ) ~ "outcome_detail",

        str_detect(variable, "^R") ~ "baseline",
        variable %in% c("AGE", "SEX") ~ "baseline",
        str_detect(
            variable,
            "^sex$|^atrial_|^prior_|^wakeup_|^ct_|^infarct_|^consciousness$|^stroke_subtype$|^deficit_"
        ) ~ "baseline",

        variable == "TD" ~ "time_to_event",
        variable %in% c("HOSPNUM", "NCCODE") ~ "identifier",

        TRUE ~ "other"
    )
}

# Gets statistical type for the vars from clean data, 
# where stat type is not available
get_statistical_type <- function(x) {
    n_unique <- n_distinct(x, na.rm = TRUE)

    case_when(
        n_unique <= 1 ~ "constant",
        n_unique == 2 ~ "binary",
        is.numeric(x) & n_unique > 10 ~ "continuous",
        n_unique <= 10 ~ "categorical",
        is.character(x) ~ "high-cardinality character",
        TRUE ~ "other"
    )
}

# Builds final dictionary
build_analysis_dictionary <- function(data, original_dictionary, schema_summary) {
    inferred_types <- tibble(
        variable = names(data),
        inferred_type = vapply(data, get_statistical_type, character(1))
    )
    tibble(variable = names(data)) |>
        left_join(
            original_dictionary,
            by = "variable"
        ) |>
        left_join(
            schema_summary |>
            select(
                variable=column,
                statistical_type
            ),
            by = "variable"
        ) |>
        left_join(
            inferred_types,
            by = "variable"
        ) |>
        mutate(
            statistical_type = coalesce(
                statistical_type,
                inferred_type
            ),
            label = if_else(
                is.na(label),
                make_label_from_name(variable),
                label
            ),
            role = mapply(
                classify_role,
                variable,
                description,
                USE.NAMES = FALSE
            )
    ) |>
    select(-inferred_type)
    
}

# Overrides labels where automatic cleaning does not work
# e.g, SEX = "M=male"
apply_label_overrides <- function(dictionary) {
    overrides <- c (
        SEX = "Sex",
        RDELAY = "Treatment delay",
        RCONSC = "Conscious state",
        RSBP = "Systolic blood pressure"
    )
    dictionary |>
    mutate(
        label = if_else(
            variable %in% names(overrides), 
            overrides[variable], 
            label
        )
    )
}

# -----------------------
# Main
# -----------------------
main <- function() {
    data <- read_csv(analysis_data_path, show_col_types = FALSE)
    schema_summary <- read_csv(schema_summary_path, show_col_types = FALSE)
    original_dictionary <- build_original_dictionary(variable_info_path)
    dictionary <- build_analysis_dictionary(data, original_dictionary, schema_summary)
    dictionary <- apply_label_overrides(dictionary)
    write_csv(dictionary, dictionary_path)
    print(head(dictionary))

    # Verification
    cat("\nDictionary summary\n")
    print(dictionary |> count(role, statistical_type))

    cat("\nMissing metadata\n")
    print(dictionary |> filter( is.na(role) | is.na(statistical_type)))

    cat("\nAnalysis variables\n")
    dictionary |> filter(is.na(role) | is.na(statistical_type))

    dictionary |> 
    select(variable, label, statistical_type, role) |> 
    arrange(role, statistical_type) |>
    print(n = Inf)
    }

main()