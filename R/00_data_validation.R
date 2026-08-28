library(readr)
library(dplyr)

# Paths
raw_data_path <- "data/raw/IST_corrected.csv"
processed_data_path <- "data/processed/ist_analysis.csv"

# Data loading
data <- read_csv(raw_data_path)

# Directories for validation figures and tables
figure_dir <- "output/figures/validation"
table_dir <- "output/tables/validation"

if (!dir.exists(figure_dir)) {
    dir.create(figure_dir, recursive = TRUE)
}

if (!dir.exists(table_dir)) {
    dir.create(table_dir, recursive = TRUE)
}


# --------------------
# Helper functions
# --------------------

# Summarizes dataset schema: column type, missingness, and unique values
summarize_schema <- function(df) {
    data.frame(
        column = names(df),
        type = sapply(df, function(x) class(x)[1]),
        non_missing = sapply(df, function(x) sum(!is.na(x))),
        missing = sapply(df, function(x) sum(is.na(x))),
        unique_values = sapply(df, function(x) length(unique(x)))
    )
}

# Shows frequency counts for a categorical variable, including missing values
check_values <- function(var) {
    table(var, useNA = "ifany")
}

# Compares two related categorical variables using a cross-tabulation
compare_vars <- function(var1, var2) {
    table(var1, var2, useNA = "ifany")
}

# Summarizes a numeric variable to check range, center, and missing values
check_numeric <- function(var) {
    summary(var)
}


# --------------------
# Plot functions
# --------------------

# Saves a stacked bar chart comparing a form-level variable
# with its standardized/derived outcome
plot_comparison <- function(form_var, derived_var, title, filename) {
    comparison <- table(form_var, derived_var, useNA = "ifany")

    png(
        filename = file.path(figure_dir, filename),
        width = 1000,
        height = 700
    )

    barplot(
        comparison,
        beside = FALSE,
        legend.text = TRUE,
        main = title,
        xlab = "Standardized outcome",
        ylab = "Number of patients"
    )

    dev.off()
}

# Saves a histogram for a numeric baseline variable
plot_numeric <- function(var, name) {
    png(
        filename = file.path(
            figure_dir,
            paste0(tolower(name), "_distribution.png")
        ),
        width = 1000,
        height = 700
    )

    hist(
        var,
        main = paste(name, "distribution"),
        xlab = name
    )

    dev.off()
}


# --------------------
# Main
# --------------------

main <- function() {

    # Inspect basic dataset structure
    print(dim(data))
    print(names(data))

    schema_summary <- summarize_schema(data)
    print(schema_summary)

    write_csv(
        schema_summary,
        file.path(table_dir, "schema_summary.csv")
    )

    # Treatment assignment distribution
    print(check_values(data$RXASP))
    print(check_values(data$RXHEP))

    # Main outcome coding
    outcomes <- c(
        "ID14",
        "ISC14",
        "H14",
        "NK14",
        "STRK14",
        "HTI14",
        "PE14",
        "DVT14",
        "TRAN14",
        "NCB14",
        "FDEAD",
        "FRECOVER",
        "FDENNIS",
        "OCCODE"
    )

    print(lapply(data[outcomes], check_values))

    # Form-level vs standardized outcomes
    comparisons <- list(
        DPE = c("PE14", "dpe_vs_pe14.png"),
        DRSISC = c("ISC14", "drsisc_vs_isc14.png"),
        DRSH = c("H14", "drsh_vs_h14.png"),
        DRSUNK = c("NK14", "drsunk_vs_nk14.png"),
        DDEAD = c("ID14", "ddead_vs_id14.png")
    )

    for (form_var in names(comparisons)) {
        derived_var <- comparisons[[form_var]][1]
        filename <- comparisons[[form_var]][2]

        print(
            compare_vars(
                data[[form_var]],
                data[[derived_var]]
            )
        )

        plot_comparison(
            data[[form_var]],
            data[[derived_var]],
            paste(form_var, "vs", derived_var),
            filename
        )
    }

    # Treatment assignment vs treatment received
    treatment_checks <- list(
        RXASP = "DASP14",
        RXHEP = c("DLH14", "DMH14", "DHH14")
    )

    for (assigned in names(treatment_checks)) {
        for (received in treatment_checks[[assigned]]) {
            print(
                compare_vars(
                    data[[assigned]],
                    data[[received]]
                )
            )
        }
    }

    # Baseline categorical coding
    baseline_categorical <- c(
        "SEX",
        "RSLEEP",
        "RATRIAL",
        "RCT",
        "RVISINF",
        "RCONSC",
        "STYPE",
        paste0("RDEF", 1:8)
    )

    print(lapply(data[baseline_categorical], check_values))

    # Baseline numeric validation
    baseline_numeric <- c(
        "AGE",
        "RSBP",
        "RDELAY"
    )

    print(lapply(data[baseline_numeric], check_numeric))

    for (var in baseline_numeric) {
        plot_numeric(data[[var]], var)
    }

    # Missing values
    missing_counts <- colSums(is.na(data))

    print(
        sort(
            missing_counts[missing_counts > 0],
            decreasing = TRUE
        )
    )
}

main()