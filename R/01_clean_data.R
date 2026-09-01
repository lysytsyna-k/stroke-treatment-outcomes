library(readr)
library(dplyr)

# Paths
raw_data_path <- "data/raw/IST_corrected.csv"
processed_data_path <- "data/processed/ist_analysis.csv"

# Data Loading
data <- read_csv(raw_data_path, show_col_types = FALSE)

# Variables not needed for the planned analysis
drop_vars <- c(
    # Randomization date/time metadata
    "RDATE",
    "HOURLOCAL",
    "MINLOCAL",
    "DAYLOCAL",
    "CNTRYNUM",

    # Event dates
    "DMAJNCHD",
    "DSIDED",
    "DRSISCD",
    "DRSHD",
    "DRSUNKD",
    "DPED",
    "DALIVED",
    "DDEADD",
    "FLASTD",
    "FDEADD",
    "FU1_RECD",
    "FU2_DONE",
    "FU1_COMP",

    # Free-text fields
    "DMAJNCHX",
    "DSIDEX",
    "DNOSTRKX",
    "DDEADX",
    "FDEADX",

    # Precomputed prediction variables
    "EXPDD",
    "EXPD6",
    "EXPD14"
)

# Source variables to drop after cleaning
source_vars <- c(
    # Randomized treatment
    "RXASP",
    "RXHEP",

    # Six-month outcomes
    "FDEAD",
    "FRECOVER",
    "FDENNIS",
    "OCCODE",

    # Baseline categorical variables
    "SEX",
    "RATRIAL",
    "RASP3",
    "RHEP24",
    "RSLEEP",
    "RCT",
    "RVISINF",
    "RCONSC",
    "STYPE",

    # Baseline deficits
    "RDEF1",
    "RDEF2",
    "RDEF3",
    "RDEF4",
    "RDEF5",
    "RDEF6",
    "RDEF7",
    "RDEF8",

    # Treatment received / compliance
    "DASP14",
    "DLH14",
    "DMH14",
    "DHH14",
    "CMPLASP",
    "CMPLHEP"
)

# -----------------------
# Checks of cleaned columns
# -----------------------

check_cleaned <- function(name, var) {
    cat("\n", name, "\n")
    print(table(var, useNA = "ifany"))
}

# -----------------------
# Unncessary variables
# -----------------------

# Removes variables that are not needed for the planned analysis
drop_unneeded_vars <- function(df, vars) {
    df |> select(-any_of(vars))
}

#------------------------
# Treatment allocation recoding
#------------------------

# Converts Yes/No values to 1/0 and converts missing values to NA
recode_yes_no <- function(x) {
    case_when(
        toupper(x) =="Y" ~ 1,
        toupper(x)=="N" ~ 0,
        TRUE ~ NA_real_
    )
}

# Recodes randomized heparin assignment into three treatment groups
recode_heparin <- function(x) {
    case_when(
        toupper(x) =="N" ~ "None",
        toupper(x)=="L" ~ "Low",
        toupper(x) %in% c("M", "H") ~ "Medium",
        TRUE ~ NA_character_
    )
}

#------------------------
# Outcomes recoding
#------------------------

# Converts Y/N/U outcome coding to 1/0/NA
# U is treated as unknown and therefore missing for analysis
recode_outcome <- function(x) {
    case_when(
        toupper(x) == "Y" ~ 1,
        toupper(x) == "N" ~ 0,
        toupper(x) == "U" ~ NA_real_,
        TRUE ~ NA_real_
    )
}


# Recodes six-month outcome categories

# OCCODE 8/9 are documented as missing status.
# OCCODE 0 is also treated as missing because all 97 cases lack a known
# six-month outcome in FDEAD/FRECOVER/FDENNIS
recode_occode <- function(x) {
    case_when(
        x == 1 ~ "Dead",
        x == 2 ~ "Dependent",
        x == 3 ~ "Not recovered",
        x == 4 ~ "Recovered",
        x %in% c(0, 8, 9) ~ NA_character_,
        TRUE ~ NA_character_
    )
}

# -----------------------
# Baseline variables cleaning
# -----------------------

# Recodes F/M sex variables to Female/Male
recode_sex <- function(x){
    case_when(
        toupper(x) == "F" ~ "Female",
        toupper(x) == "M" ~ "Male",
        TRUE ~ NA_character_
    )
}

# Recodes consciousness from F,D,U to Fully alert, Drowsy, Unconscious
recode_consciousness <- function(x) {
    case_when(
        toupper(x) == "F" ~ "Fully alert",
        toupper(x) == "D" ~ "Drowsy",
        toupper(x) == "U" ~ "Unconscious",
        TRUE ~ NA_character_
    )
}

# Recodes deficit from Y,N,C to 1, 0, NA
recode_deficit <- function(x) {
    case_when(
        toupper(x) == "Y" ~ 1,
        toupper(x) == "N" ~ 0,
        toupper(x) == "C" ~ NA_real_,
        TRUE ~ NA_real_
    )
}

# Recodes stroke subtype to their full names
recode_stype <- function(x) {
    case_when(
        x == "LACS" ~ "Lacunar syndrome",
        x == "PACS" ~ "Partial anterior circulation syndrome",
        x == "POCS" ~ "Posterior circulation syndrome",
        x == "TACS" ~ "Total anterior circulation syndrome",
        x == "OTH" ~ "Other",
        TRUE ~ NA_character_
    )
}


#------------------------
# Main
#------------------------

main <- function() {
    # Unneded vars drop
    analysis_data <- drop_unneeded_vars (data, drop_vars)
    
    # -------------------
    # Randomization treatment recoding
    # -------------------
    analysis_data <- analysis_data |>
    mutate(
        aspirin_alloc = recode_yes_no(RXASP),
        heparin_alloc = recode_heparin(RXHEP)
    )

    # -------------------
    # Six-month outcome recoding
    # -------------------
    analysis_data <- analysis_data |>
    mutate(
        dead_6m = recode_outcome(FDEAD),
        recovered_6m = recode_outcome(FRECOVER),
        dependent_6m = recode_outcome(FDENNIS),
        outcome_6m = recode_occode(OCCODE)
    )

    # -------------------
    # Baseline recoding
    # -------------------
    analysis_data <- analysis_data |>
    mutate(
        sex = recode_sex(SEX),
        atrial_fibrillation = recode_yes_no(RATRIAL),
        prior_aspirin = recode_yes_no(RASP3),
        prior_heparin = recode_yes_no(RHEP24),
        consciousness = recode_consciousness(RCONSC),
        stroke_subtype = recode_stype(STYPE),
        deficit_face = recode_deficit(RDEF1),
        deficit_arm = recode_deficit(RDEF2),
        deficit_leg = recode_deficit(RDEF3),
        deficit_dysphasia = recode_deficit(RDEF4),
        deficit_hemianopia = recode_deficit(RDEF5),
        deficit_visuospatial = recode_deficit(RDEF6),
        deficit_brainstem = recode_deficit(RDEF7),
        deficit_other = recode_deficit(RDEF8),
        wakeup_stroke = recode_yes_no(RSLEEP),
        ct_before_randomization = recode_yes_no(RCT),
        infarct_visible_ct = recode_yes_no(RVISINF),
    )

    # -------------------
    # Treatment received ()
    # -------------------
    analysis_data <- analysis_data |>
    mutate(
        aspirin_received = recode_yes_no(DASP14),
        heparin_low_received = recode_yes_no(DLH14),
        # DHH14 is the pilot-study version of medium-dose heparin.
        # Combined it with DMH14 into one medium-dose received variable.
        heparin_medium_received = case_when(
            recode_yes_no(DMH14) == 1 | recode_yes_no(DHH14) == 1 ~ 1,
            recode_yes_no(DMH14) == 0 | recode_yes_no(DHH14) == 0 ~ 0,
            TRUE ~ NA_real_
        )
    )

    # Medication Compliance
    analysis_data <- analysis_data |>
    mutate(
        aspirin_compliant = recode_yes_no(CMPLASP),
        heparin_compliant = recode_yes_no(CMPLHEP)
    )

    # -------------------
    # Verification of changes
    # -------------------
    check_cleaned("Aspirin allocation", analysis_data$aspirin_alloc)
    check_cleaned("Heparin allocation", analysis_data$heparin_alloc)
    check_cleaned("Six-month outcome", analysis_data$outcome_6m)
    check_cleaned("Sex", analysis_data$sex)
    check_cleaned("Conscious state", analysis_data$consciousness)
    check_cleaned("Stroke subtype", analysis_data$stroke_subtype)
    check_cleaned("Face deficit", analysis_data$deficit_face)
    check_cleaned("Medium-dose heparin received", analysis_data$heparin_medium_received)

    analysis_data <- analysis_data |>
    select(-any_of(source_vars))

    # -------------------
    # CSV
    # -------------------
    write_csv(analysis_data, processed_data_path)
    cat("\nCleaned dataset saved to:", processed_data_path, "\n")

}

main()