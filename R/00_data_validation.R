library(readr)
library(dplyr)

# Paths
raw_data_path <- "data/raw/IST_corrected.csv"

# Data loading
data <- read_csv(raw_data_path)

# Directory for validation tables
table_dir <- "output/tables/validation"

if (!dir.exists(table_dir)) {
    dir.create(table_dir, recursive = TRUE)
}

# Directories for validation figures
figure_dir <- "output/figures/validation"
numeric_figure_dir <- file.path(figure_dir, "numeric")
missingness_figure_dir <- file.path(figure_dir, "missingness")
comparison_figure_dir <- file.path(figure_dir, "comparisons")

figure_dirs <- c(numeric_figure_dir, missingness_figure_dir, comparison_figure_dir)

for (dir in figure_dirs) {
    if (!dir.exists(dir)) {
        dir.create(dir, recursive = TRUE)
    }
}

if (!dir.exists(figure_dir)) {
    dir.create(figure_dir, recursive = TRUE)
}

# --------------------
# Helper functions
# --------------------

# Classifies variables based on R type and number of unique non-missing values
classify_variable <- function(x, max_cat = 10) {
    n_unique <- length(unique(na.omit(x)))

    if (n_unique <= 1) {return("constant")}
    if (n_unique == 2) {return("binary")}
    if (is.character(x) && n_unique > max_cat) {return("high-cardinality character")}
    if (n_unique <= max_cat) {return("categorical")}
    if (is.numeric(x)) {return("continuous")}

    return("other")
}

# Summarizes dataset schema, missingness, and automatically classified type
summarize_schema <- function(df) {
    data.frame(
        column = names(df),
        r_type = sapply(df, function(x) class(x)[1]),
        statistical_type = sapply(df, classify_variable),
        non_missing = sapply(df, function(x) sum(!is.na(x))),
        missing = sapply(df, function(x) sum(is.na(x))),
        missing_percent = round(
            sapply(df, function(x) mean(is.na(x)) * 100),
            2
        ),
        unique_values = sapply(
            df,
            function(x) length(unique(na.omit(x)))
        )
    )
}

# Summarizes observed values and counts for categorical variables
summarize_categories <- function(df, vars) {
    do.call(
        rbind,
        lapply(vars, function(var) {
            counts <- table(df[[var]], useNA = "ifany")

            data.frame(
                variable = var,
                value = names(counts),
                count = as.integer(counts)
            )
        })
    )
}

# Shows frequency counts for one categorical variable
check_values <- function(var) {table(var, useNA = "ifany")}

# Compares two related categorical variables using a cross-tabulation
compare_vars <- function(var1, var2) {table(var1, var2, useNA = "ifany")}

# Summarizes a numeric variable to check range, center, and missingness
check_numeric <- function(var) {summary(var)}

# --------------------
# Plot functions
# --------------------

# Saves a stacked bar chart comparing a form-level variable
# with its standardized/derived outcome
plot_comparison <- function(form_var, derived_var, title, filename) {
    comparison <- table(
        form_var,
        derived_var,
        useNA = "ifany"
    )

    png(
        filename = file.path(
        comparison_figure_dir,
        filename
        ),
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

# Saves a histogram for a continuous variable
plot_numeric <- function(var, name) {
    png(
    filename = file.path(
        numeric_figure_dir,
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

# Saves a bar chart of missing-value percentages
plot_missingness <- function(missing_summary, min_percent = 1) {

    plot_data <- missing_summary |>
        filter(missing_percent >= min_percent) |>
        arrange(missing_percent)

    png(
        filename = file.path(
            missingness_figure_dir,
            "missingness.png"
        ),
        width = 1400,
        height = 1000
    )

    par(mar = c(5, 10, 4, 2))

    barplot(
        plot_data$missing_percent,
        names.arg = plot_data$variable,
        horiz = TRUE,
        las = 1,
        main = "Variables with missing values",
        xlab = "Missing values (%)",
        cex.names = 0.8
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

    # Create and save schema summary
    schema_summary <- summarize_schema(data)

    print(schema_summary)

    write_csv(schema_summary, file.path(table_dir, "schema_summary.csv"))

    # --------------------
    # Variable groups
    # --------------------

    # Binary and categorical variables
    categorical_vars <- schema_summary |>
        filter(
            statistical_type %in% c(
                "binary",
                "categorical"
            )
        ) |>
        pull(column)

    # Continuous numeric variables
    continuous_vars <- schema_summary |>
        filter(statistical_type == "continuous") |>
        pull(column)

    # High-cardinality character variables
    high_cardinality_vars <- schema_summary |>
        filter(statistical_type == "high-cardinality character") |>
        pull(column)

    # Constant variables
    constant_vars <- schema_summary |>
        filter(statistical_type == "constant") |>
        pull(column)

    # --------------------
    # Categorical validation
    # --------------------

    category_summary <- summarize_categories(data, categorical_vars)

    print(category_summary)

    write_csv(category_summary, file.path(table_dir, "categorical_value_counts.csv"))

    # --------------------
    # Continuous variable validation
    # --------------------

    numeric_summary <- data.frame(
        variable = continuous_vars,
        min = sapply(data[continuous_vars], function(x) min(x, na.rm = TRUE)),
        q1 = sapply(data[continuous_vars], function(x) quantile(x, 0.25, na.rm = TRUE)),
        median = sapply(data[continuous_vars], function(x) median(x, na.rm = TRUE)),
        mean = sapply(data[continuous_vars], function(x) mean(x, na.rm = TRUE)),
        q3 = sapply(data[continuous_vars], function(x) quantile(x, 0.75, na.rm = TRUE)),
        max = sapply(data[continuous_vars],function(x) max(x, na.rm = TRUE))
    )

    print(numeric_summary)
    write_csv(numeric_summary, file.path(table_dir, "numeric_summary.csv"))

    for (var in continuous_vars) {
        plot_numeric(data[[var]], var)
    }

    # --------------------
    # Form-level vs standardized outcomes
    # --------------------

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

        print(compare_vars(data[[form_var]], data[[derived_var]]))

        plot_comparison(
            data[[form_var]],
            data[[derived_var]],
            paste(form_var, "vs", derived_var),
            filename
        )
    }

    # --------------------
    # Treatment assignment vs treatment received
    # --------------------

    treatment_checks <- list(
        RXASP = "DASP14",
        RXHEP = c("DLH14", "DMH14", "DHH14")
        )

    for (assigned in names(treatment_checks)) {
        for (received in treatment_checks[[assigned]]) {
            print(compare_vars(data[[assigned]], data[[received]]))
        }
    }

    # --------------------
    # Missingness
    # --------------------

    missing_summary <- data.frame(
        variable = names(data),
        missing = colSums(is.na(data)),
        missing_percent = round(
            colMeans(is.na(data)) * 100,
            2
        )
    ) |>
        arrange(desc(missing_percent))

    print(missing_summary)

    write_csv(missing_summary, file.path(table_dir, "missingness_summary.csv"))
    plot_missingness(missing_summary)

    # --------------------
    # Additional validation groups
    # --------------------
    high_cardinality_summary <- data.frame(variable = high_cardinality_vars)
    write_csv(high_cardinality_summary, file.path(table_dir, "high_cardinality_variables.csv"))

    constant_summary <- data.frame(variable = constant_vars)
    write_csv(constant_summary,file.path(table_dir, "constant_variables.csv"))

    # -------------------
    # 6-month outcome consistency
    # -------------------
    occode_zero_check <- data |>
    filter(OCCODE==0) |>
    count(FDEAD, FRECOVER, FDENNIS, sort=TRUE)
    
    print(occode_zero_check)

    write_csv(occode_zero_check, file.path(table_dir, "occode_zero_validation.csv"))


}


main()