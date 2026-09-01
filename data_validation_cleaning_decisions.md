# Data Validation and Cleaning Notes

This document explains the main decisions I made while preparing the International Stroke Trial (IST) dataset for analysis.

I kept the raw dataset unchanged and did all validation and cleaning in separate R scripts. The goal was to make the analysis dataset easier to work with without hiding or overcorrecting unusual values.

## Project files

The main files used so far are:

- `data/raw/IST_corrected.csv`
- `data/raw/IST_variables.csv`
- `R/00_data_validation.R`
- `R/01_clean_data.R`
- `data/processed/ist_analysis.csv`

The raw IST file has about 19,435 patients and 112 original variables.

`00_data_validation.R` is used to inspect the raw data and document issues.  
`01_clean_data.R` contains the actual cleaning rules and creates the processed dataset.

I kept these two steps separate so that validation is not mixed with transformation.

---

## Validation approach

Instead of checking all 112 columns manually, I created a small validation framework that summarizes:

- data types
- missing values
- unique values
- categorical value counts
- numeric ranges and distributions
- high-cardinality character fields
- constant variables
- selected related-variable comparisons

The script creates outputs such as:

- `schema_summary.csv`
- `categorical_value_counts.csv`
- `numeric_summary.csv`
- `missingness_summary.csv`
- `high_cardinality_variables.csv`
- `constant_variables.csv`
- `occode_zero_validation.csv`

It also saves validation plots for numeric variables, missingness, and selected variable comparisons.

The automatic variable classification is only used as a first-pass inspection tool. A numeric variable, for example, is not automatically treated as a meaningful continuous feature just because it is stored as a number.

---

## Randomized treatment vs treatment received

One of the most important decisions was keeping randomized treatment assignment separate from treatment actually received.

### Randomized treatment

The main randomized variables are:

- `RXASP` — aspirin allocation
- `RXHEP` — heparin allocation

These will be used for the main treatment-effect analysis because they represent the original randomization.

### Treatment received

Variables such as:

- `DASP14`
- `DLH14`
- `DMH14`
- `DHH14`

describe what patients actually received after randomization.

These are useful for adherence and descriptive analysis, but I do not plan to use them instead of randomized assignment for the primary treatment-effect analysis.

---

## Aspirin allocation

`RXASP` contains:

- `N`: 9,715
- `Y`: 9,720

I recoded it as:

- `Y` -> 1
- `N` -> 0

The cleaned variable is:

`aspirin_alloc`

The allocation is almost perfectly balanced, which is what I would expect from randomization.

---

## Heparin allocation

`RXHEP` contains:

- `H`: 245
- `L`: 4,861
- `M`: 4,611
- `N`: 9,718

The documentation notes that `H` was used in the pilot study for the dose later coded as `M`.

Because of that, I did not treat `H` as a separate fourth treatment group.

I combined the values as:

- `N` -> `"None"`
- `L` -> `"Low"`
- `M` and `H` -> `"Medium"`

The cleaned variable is:

`heparin_alloc`

Final counts:

- None: 9,718
- Low: 4,861
- Medium: 4,856

I kept this as a categorical variable instead of coding it as 0, 1, and 2 because the treatment groups should not be treated as a continuous scale.

---

## Yes/no recoding

Several IST variables use `Y` and `N`.

I created a reusable `recode_yes_no()` function.

During validation, I found that `DASP14` had two lowercase values:

- one `y`
- one `n`

To make the cleaning more robust, the function first converts values to uppercase and then maps:

- `Y` -> 1
- `N` -> 0
- anything else -> missing

This way I do not need separate logic for isolated capitalization errors.

---

## Six-month outcomes

The main six-month fields are:

- `FDEAD`
- `FRECOVER`
- `FDENNIS`
- `OCCODE`

### FDEAD, FRECOVER, and FDENNIS

These variables contain:

- `Y`
- `N`
- `U`
- missing values

The codebook does not clearly define `U`, but based on the coding pattern I treated it as an unknown status.

The cleaning rule is:

- `Y` -> 1
- `N` -> 0
- `U` -> missing

The cleaned variables are:

- `dead_6m`
- `recovered_6m`
- `dependent_6m`

I documented this as an inference because the meaning of `U` was not stated clearly in the documentation.

---

## Six-month overall outcome: OCCODE

The documentation defines:

- 1 = dead
- 2 = dependent
- 3 = not recovered
- 4 = recovered
- 8 or 9 = missing status

The actual dataset also contains `0`, even though `0` is not explained in the codebook.

Before deciding what to do with it, I checked the 97 rows where `OCCODE == 0` against:

- `FDEAD`
- `FRECOVER`
- `FDENNIS`

The result was:

- 93 rows had all three follow-up fields missing
- 4 rows had `FRECOVER = U` and the other fields missing

So none of the 97 rows had a known six-month status.

Based on that check, I treated:

- `0`
- `8`
- `9`

as missing.

The cleaned variable is:

`outcome_6m`

with:

- 1 -> `"Dead"`
- 2 -> `"Dependent"`
- 3 -> `"Not recovered"`
- 4 -> `"Recovered"`

Final counts:

- Dead: 4,241
- Dependent: 7,884
- Not recovered: 3,864
- Recovered: 3,296
- Missing: 150

The `OCCODE == 0` check is saved in the validation script so this decision can be traced back to the data.

---

## Standardized 14-day outcomes

The corrected dataset includes standardized 14-day outcome variables such as:

- `ID14` — death at 14 days
- `ISC14` — ischemic stroke within 14 days
- `H14` — cerebral bleed / hemorrhagic stroke
- `NK14` — indeterminate or unknown stroke type
- `STRK14` — any stroke
- `HTI14` — hemorrhagic transformation
- `PE14`
- `DVT14`
- `TRAN14`
- `NCB14`

These variables are already coded as 0/1, so I left them unchanged.

There was no reason to create duplicate cleaned versions when the existing coding was already analysis-ready.

---

## Related form-level and standardized variables

Some events appear in more than one form in the dataset.

Examples include:

- `DPE` and `PE14`
- `DRSISC` and `ISC14`
- `DRSH` and `H14`
- `DRSUNK` and `NK14`
- `DDEAD` and `ID14`

I compared these during validation and found that they are related but not identical.

For example, `PE14` captures more events than `DPE`.

Similarly, `DDEAD` and `ID14` differ because they represent different timing or status concepts.

Because of that, I plan to use the standardized 14-day variables for the main analysis instead of assuming the form-level fields are interchangeable with them.

---

## Baseline variables

Baseline variables are fields known before randomized treatment assignment.

The main baseline variables cleaned so far include:

- `SEX`
- `RATRIAL`
- `RASP3`
- `RHEP24`
- `RSLEEP`
- `RCT`
- `RVISINF`
- `RCONSC`
- `STYPE`
- `RDEF1` through `RDEF8`

Numeric baseline fields such as:

- `AGE`
- `RSBP`
- `RDELAY`

were already numeric and did not need recoding.

Their observed ranges were:

- AGE: 16–99
- RSBP: 70–295
- RDELAY: 1–48

I did not see an obvious impossible-value code in these ranges, so I kept the original numeric values.

---

## Sex

`SEX` was recoded as:

- `F` -> `"Female"`
- `M` -> `"Male"`

The cleaned variable is:

`sex`

Counts:

- Female: 9,028
- Male: 10,407

---

## Atrial fibrillation and other baseline yes/no fields

The following variables use the same yes/no recoding logic:

- `RATRIAL` -> `atrial_fibrillation`
- `RASP3` -> `prior_aspirin`
- `RHEP24` -> `prior_heparin`
- `RSLEEP` -> `wakeup_stroke`
- `RCT` -> `ct_before_randomization`
- `RVISINF` -> `infarct_visible_ct`

For these:

- `Y` -> 1
- `N` -> 0
- anything else -> missing

For example, `atrial_fibrillation` has:

- 0: 15,282
- 1: 3,169
- missing: 984

---

## Consciousness at baseline

`RCONSC` is documented as:

- `F` = fully alert
- `D` = drowsy
- `U` = unconscious

I recoded it into readable labels.

The cleaned variable is:

`consciousness`

Counts:

- Fully alert: 14,921
- Drowsy: 4,254
- Unconscious: 260

I kept these as categories rather than converting them into arbitrary numbers.

---

## Stroke subtype

The codebook describes `STYPE` as stroke subtype.

I recoded:

- `LACS` -> `"Lacunar syndrome"`
- `PACS` -> `"Partial anterior circulation syndrome"`
- `POCS` -> `"Posterior circulation syndrome"`
- `TACS` -> `"Total anterior circulation syndrome"`
- `OTH` -> `"Other"`

The cleaned variable is:

`stroke_subtype`

Counts:

- Lacunar syndrome: 4,657
- Partial anterior circulation syndrome: 7,855
- Posterior circulation syndrome: 2,228
- Total anterior circulation syndrome: 4,638
- Other: 57

I used the name `stroke_subtype` because that matches the wording in the codebook.

---

## Neurological deficits

The variables `RDEF1` through `RDEF8` describe different baseline neurological deficits.

The codebook uses:

- `Y` = deficit present
- `N` = deficit absent
- `C` = cannot assess

I treated `C` as missing, not as 0.

A patient who cannot be assessed is not the same as a patient who does not have the deficit.

The cleaned variables are:

- `deficit_face`
- `deficit_arm`
- `deficit_leg`
- `deficit_dysphasia`
- `deficit_hemianopia`
- `deficit_visuospatial`
- `deficit_brainstem`
- `deficit_other`

Example for face deficit:

- 0: 5,089
- 1: 14,099
- missing: 247

---

## Treatment actually received

### Aspirin received

`DASP14` was recoded using the yes/no helper.

The cleaned variable is:

`aspirin_received`

The uppercase conversion handles the two lowercase values found during validation.

### Low-dose heparin received

`DLH14` was also recoded with the yes/no helper.

The cleaned variable is:

`heparin_low_received`

### Medium-dose heparin received

The documentation states that `DHH14` is the pilot-study version of the medium-dose heparin field and should be combined with `DMH14`.

I therefore created one cleaned variable:

`heparin_medium_received`

The final logic is:

- if either `DMH14` or `DHH14` says yes -> 1
- if either says no -> 0
- otherwise -> missing

This required a correction during cleaning.

My first version required both fields to be 0 before assigning 0. That produced too many missing values because one of the two fields is usually structurally missing depending on whether the record comes from the pilot or main study.

After fixing the logic, the counts were:

- 0: 15,112
- 1: 4,285
- missing: 38

---

## Compliance

The dataset also includes:

- `CMPLASP`
- `CMPLHEP`

I cleaned these as:

- `aspirin_compliant`
- `heparin_compliant`

These are different from treatment received.

They describe whether the randomized treatment protocol was followed.

I may use them later for adherence or sensitivity analysis, but not as a replacement for randomized treatment assignment in the main analysis.

---

## Variables I did not clean yet

There are still several post-randomization treatment and follow-up fields in the processed dataset, such as:

- `DSCH`
- `DIVH`
- `DAP`
- `DOAC`

I decided not to recode every remaining field just because it exists.

For now, I only cleaned variables that are clearly relevant to the planned analysis or needed for interpretation.

The remaining variables can be cleaned later if a specific question requires them.

---

## Country

The dataset includes both:

- `COUNTRY`
- `CNTRYNUM`

`COUNTRY` already contains readable country information, so I kept it.

`CNTRYNUM` was dropped because it is redundant for this project.

I did not drop `COUNTRY` simply because it has many unique values.

---

## High-cardinality character variables

Validation identified fields such as:

- `RDATE`
- `DMAJNCHX`
- `DSIDEX`
- `DNOSTRKX`
- `DDEADX`
- `FDEADX`
- `COUNTRY`
- `NCCODE`

I did not use high cardinality alone as a reason to remove a variable.

For example:

- free-text `*X` variables were dropped from the quantitative analysis dataset
- `COUNTRY` was kept because it may still be useful
- borderline administrative variables were handled conservatively rather than removed automatically

---

## Removing duplicate coded fields

Once I created readable or analysis-ready versions of coded fields, I removed the original source-coded versions from the processed dataset.

Examples include:

- `RXASP` after creating `aspirin_alloc`
- `RXHEP` after creating `heparin_alloc`
- `SEX` after creating `sex`
- `RCONSC` after creating `consciousness`
- `STYPE` after creating `stroke_subtype`
- `RDEF1` through `RDEF8` after creating the deficit variables
- `FDEAD`, `FRECOVER`, `FDENNIS`, and `OCCODE` after creating cleaned six-month outcomes
- treatment-received and compliance source fields after creating cleaned versions

I did this because keeping both the raw code and the cleaned version in the analysis dataset makes it easier to accidentally use the wrong column.

The original values are still preserved in the raw dataset.

---

## Processed dataset

After cleaning and removing redundant source-coded columns, the processed dataset contains about:

- 19,435 rows
- 88 columns

I intentionally did not reduce it to a tiny modeling dataset yet.

`01_clean_data.R` creates a reusable analysis dataset.

Later scripts can select smaller sets of variables depending on the question being analyzed.

---

## Checks kept in the cleaning script

The cleaning script only keeps a small number of labeled sanity checks.

These include examples such as:

- aspirin allocation
- heparin allocation
- six-month outcome
- sex
- consciousness
- stroke subtype
- face deficit
- medium-dose heparin received

More detailed checking belongs in `00_data_validation.R`.

This keeps the cleaning script easier to read while still making it obvious when an important transformation fails.

---

## Main analysis decisions established so far

For the main treatment-effect analysis, I plan to use:

### Treatment assignment

- `aspirin_alloc`
- `heparin_alloc`

### Short-term outcomes

Prefer standardized variables such as:

- `ID14`
- `ISC14`
- `H14`
- `STRK14`
- `PE14`

### Six-month outcomes

Use the cleaned fields:

- `dead_6m`
- `dependent_6m`
- `recovered_6m`
- `outcome_6m`

### Treatment received and compliance

Use these mainly for descriptive, adherence, or sensitivity analysis unless a later question specifically requires them.

### Unknown values

I do not convert unknown or unassessable values into the negative category.

Examples:

- `U` is treated as missing when it represents unknown status
- `C` in neurological deficit fields is treated as missing
- `OCCODE == 0` was treated as missing only after checking the related follow-up fields

---

## Cleaning principles used

The main rules I followed were:

1. Keep the raw data unchanged.
2. Do not recode a value until its meaning is understood.
3. Check undocumented or unusual codes before making assumptions.
4. Do not treat unknown values as negative outcomes.
5. Prefer standardized outcome variables when they are available.
6. Keep potentially useful fields unless there is a clear reason to remove them.
7. Remove raw coded duplicates after creating cleaner analysis variables.
8. Keep randomized assignment separate from treatment actually received.
9. Document decisions that required judgment.

These decisions make the analysis easier to reproduce and make it possible to explain exactly how the final dataset was created.

---

## Current status

Completed so far:

- raw-data validation
- schema and variable summaries
- categorical value checks
- numeric summaries
- missingness checks
- high-cardinality checks
- randomized treatment cleaning
- six-month outcome cleaning
- validation of `OCCODE == 0`
- baseline demographic and clinical recoding
- neurological deficit recoding
- treatment-received cleaning
- pilot/main medium-dose heparin combination
- compliance cleaning
- removal of duplicate coded source variables
- creation of the processed dataset

Current cleaned output:

`data/processed/ist_analysis.csv`

The next stage of the project is exploratory data analysis in:

`R/02_eda.R`

That stage will focus on describing the trial population, checking treatment-group balance, summarizing outcomes, and looking at analysis-relevant missingness before moving into formal statistical testing.
