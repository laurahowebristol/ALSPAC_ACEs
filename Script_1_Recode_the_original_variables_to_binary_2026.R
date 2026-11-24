################################################################################

### Script 1 : DATA CLEANING & RECODING ORIGINAL VARIABLES TO BINARY  ###
# (Script_1_Recode_the_original_variables_to_binary_2026.R)

# Last edited by Bushra Farooq July 2026 #

################################################################################


# REQUIRED INPUT FILES:

# 1. Excel file with description variables 
#   - 'SI_data1_ACE_definitions_overtime_2026.xlsx' 
#
# 2. File with ALSPAC data ('alspac.table_ACE.RData'), 
# The columns in this dataframe needs to include at least five groups of variables:
#
#   i. all variables in the column 'variable' of the excel sheet 
#      (SI_data1_ACE_definitions_overtime_2026.xlsx)
#
#  ii. all variables in the column 'followup_inc_other_question' 
#      of the excel sheet (SI_data1_ACE_definitions_overtime_2026.xlsx) 
#
# iii. variables needed to derive household class: 
#       "pa_sc_p","pb_sc_p", "b_sc_m","c_sc_ptnr_pp", "c_sc_m", "pd_sc_p",
#       "f_sc_ptnr_pp", "f_sc_m","pe_sc_p", "g_sc_ptnr_pp", "g_sc_m", 
#       "h_sc_ptnr_pp", "h_sc_m", "j_sc_ptnr_pp", "j_sc_m","pl_sc_p"
#
#  iv. Auxiliary imputation variables:
# "c804","kz030","dw002","dw042","a006","mz028b",
# "b032","a525","c645a","c666a","pb325a","pb342a","kz021",
# "b608","b593","c472","c520","c522","pb183","b370","c600",
# "pb260","b106","b107","b122","b123","b597","c093","c101",
# "d152","d169","d170","pa172","pa189","pa190","pb187","a600",
# "b598","pb188a","b578","b587","pb168","pb177",
# "d790","pb130","b701","b702","b714","d167","d168","pa187",
# "pa188","pb098","t3336","t3337","fpa3333","fpa3334","t3322",
# "t1360","t1362","fpa1360","fpa1362","t5404","t3327",
# "t3255","t5300","t5305","t5320","t5340","t5345","t5360",
# "t5365","t5380","t5385","fpa5404","t2035","fpa2035",
# "t3328","fpa3328","t3308","t3316","fpa3308","fpa3316","t3325",
# t3326","fpa3325","fpa3326","t5402","t5406","t5410","t5412",
# "t5510","fpa5402","fpa5406","fpa5410","fpa5411","fpa5510","YPA5005_dup",
# "YPA5007_dup","YPA5009_dup","YPA5011_dup","YPA5013_dup","YPA5015_dup",
# "YPA5017_dup","YPA5050","t3321","fpa3321","c482","e178"
#
#   v. ALSPAC do file variables (used for merging with other ALSPAC data)
# "aln","qlet","mz010","mz010a","mz013","mz014","mz028b",
# "a006","a525","b032","b650","b663","b665","b667",
# "c645a","c755","c765", paste0("c80",0:4),"bestgest",
# "kz011b","kz021","kz030",
# "in_core","in_alsp","in_phase2","in_phase3","tripquad","preg_in_core"

################################################################################


rm(list=ls()) # Clear workspace to avoid contamination

setwd("path/to/working/directory")

## -----------------------------------------------------------------------------
## BLOCK 1 - SETUP: Read in data, check all necessary variables are present
## -----------------------------------------------------------------------------

# Overview:
  # A:Load packages
  # B:Load data
  # C:Load metadata
  # D:Extract list of ACE variables

# A: Load packages 
library(dplyr)
library(readxl)
library(matrixStats)
library(haven)
library(openxlsx)

# B: Load data
alspac.table_ACE=read_dta("alspac.table_ACE.dta") %>%
  mutate_if(is.labelled, as_factor)

ls() #Should now have a dataframe called 'alspac.table_ACE' in environment
class(alspac.table_ACE)
dim(alspac.table_ACE)
#check aln is numeric, this is necessary to be able to merge ALSPAC stata files
class(alspac.table_ACE$aln)

# C: Load metadata describing ACE variables
adv_description<-data.frame(readxl::read_excel(
  'SI_data1_ACE_definitions_overtime_2026.xlsx', sheet = 'ACE variables',
  col_names=TRUE))

# D: Extract list of ACE variable
ACE_var <- adv_description$variable_unique

## -----------------------------------------------------------------------------
## BLOCK 2 - PREP: DERIVE CORE VARIABLE GROUPS + VALIDATE
## -----------------------------------------------------------------------------

# Overview:
  # A:Identify follow-up questions where response depends on earlier question
  # B:Extract parent variables that determine skip logic
  # C:Define administrative missing values
  # D:Clean admin missing values in parent questions
  # E:Define household social class
  # F:Define auxiliary imputation variables
  # G:Define variables from ALSPAC do files
  # H:Check all required variables exist


# A: Identify follow-up questions where response depends on earlier question
# ACE variables that are part of skips in questionnaire
# need to include previous question which determined 
# whether someone answered our question of interest 
# e.g. Were your personal belongings stolen? 
# followed up by our question of interest 
# How often were your personal belongings stolen? 

quest_follow_up <- unique(unlist(strsplit(
  gsub("OR", "_",
       adv_description$followup_inc_other_question[
         !is.na(adv_description$followup_inc_other_question) &
           adv_description$followup_inc_other_question != "no"
       ]),
  split = "_"
)))

# B: Extract parent variables that determine skip logic
parent_vars <- adv_description$followup_inc_other_question
parent_vars <- parent_vars[!is.na(parent_vars) & parent_vars != "no"]

# Replace OR with _ and split into individual variable names
parent_vars <- unique(unlist(strsplit(gsub("OR", "_", parent_vars), "_")))

# C: Define administrative missing values 
admin_missing <- c(
  "", " ", NA,
  "missing", "Missing", "Missing page",
  "Did not start task", "Did not start session", "Did not happen",
  "Did not attend", "Did not attend TF3",
  "Did not do F&S", "Did not do F&Y session",
  "Consent withdrawn", "Child refused", 
  "Not Applicable", "Not applicable",
  "Not stated", "No response", "No/Missing",
  "No SA record", "No SE record",
  "Unknown", "Dont know", "Don't know", "Don't Know", "DK", "NK",
  "NS/NA", "NS/NK", "Questionnaire not sent", "NA / NS",
  "Error check", "Skipped", "Not completed", 
  "Questionnaire not completed", "Questionnaire not Completed",
  "Questionnaire not Returned",
  "Other", "other", "Other text", "Other text answer",
  "Other Text Answer", "Text response", "Text written, no box ticked",
  "Text answer but no box ticked", "Text answer but no frequency",
  "Relevant comment", "Relevant text but no box ticked",
  "See text", "Unresolvable", "Refused to answer", "NA", "-1"
)

# D: Clean admin missing values in parent questions
cat("\n=== CLEANING ADMIN MISSING CATEGORIES IN PARENT QUESTIONS ===\n")

admin_clean_log <- list()
for (v in parent_vars) {
  if (!v %in% names(alspac.table_ACE)) {
    cat("\nSkipping", v, "(not in dataset)\n")
    next
  }
  
  cat("\n\n--- Cleaning variable:", v, "---\n")
  
  x <- as.character(alspac.table_ACE[[v]])
  
  # Print levels before cleaning
  cat("Levels BEFORE cleaning:\n")
  print(sort(unique(x)))
  
  # Identify rows to convert to NA
  to_na <- which(x %in% admin_missing)
  
  if (length(to_na) > 0) {
    admin_clean_log[[v]] <- data.frame(
      row = to_na,
      before = x[to_na],
      stringsAsFactors = FALSE
    )
    
    x[to_na] <- NA
  }
  
  alspac.table_ACE[[v]] <- x
  
  # Print levels after cleaning
  cat("Levels AFTER cleaning:\n")
  print(sort(unique(alspac.table_ACE[[v]])))
}

cat("\n=== ADMIN MISSING CLEANING COMPLETE ===\n")

# E: variables needed to derive household social class
# Define social class variables
org_sc_vars <- c(
  "pa_sc_p","pb_sc_p","b_sc_m","c_sc_ptnr_pp","c_sc_m",
  "pd_sc_p","f_sc_ptnr_pp","f_sc_m","pe_sc_p","g_sc_ptnr_pp","g_sc_m",
  "h_sc_ptnr_pp","h_sc_m","j_sc_ptnr_pp","j_sc_m","pl_sc_p"
)

# F: Auxiliary imputation variables (add substantive analysis variables here)
impute_ACE <- c(
  "c804","kz030","dw002","dw042","a006","mz028b",
  "b032","a525","c645a","c666a","pb325a","pb342a","kz021",
  "b608","b593","c472","c520","c522","pb183","b370","c600",
  "pb260","b106","b107","b122","b123","b597","c093","c101",
  "d152","d169","d170","pa172","pa189","pa190","pb187","a600",
  "b598","pb188a","b578","b587","pb168","pb177",
  "d790","pb130","b701","b702","b714","d167","d168","pa187",
  "pa188","pb098","t3336","t3337","fpa3333","fpa3334","t3322",
  "t1360","t1362","fpa1360","fpa1362","t5404","t3327",
  "t3255","t5300","t5305","t5320","t5340","t5345","t5360",
  "t5365","t5380","t5385","fpa5404","t2035","fpa2035",
  "t3328","fpa3328","t3308","t3316","fpa3308","fpa3316","t3325",
  "t3326","fpa3325","fpa3326","t5402","t5406","t5410","t5412",
  "t5510","fpa5402","fpa5406","fpa5410","fpa5411","fpa5510","YPA5005_dup",
  "YPA5007_dup","YPA5009_dup","YPA5011_dup","YPA5013_dup","YPA5015_dup",
  "YPA5017_dup","YPA5050","t3321","fpa3321","c482","e178"
)

# G: Variables from ALSPAC do files
# Variables always required from ALSPAC do-files
alspac_do_file <- c(
  "aln","qlet","mz010","mz010a","mz013","mz014","mz028b",
  "a006","a525","b032","b650","b663","b665","b667",
  "c645a","c755","c765", paste0("c80",0:4),"bestgest",
  "kz011b","kz021","kz030",
  "in_core","in_alsp","in_phase2","in_phase3","tripquad","preg_in_core"
)

# H: Check all required variables exist
check_in_alspac_data <- c(
  alspac_do_file,
  ACE_var[!grepl("sc_household|dup", ACE_var)],
  quest_follow_up,
  org_sc_vars,
  impute_ACE[!grepl("sc_household|dup", impute_ACE)]
)

# Run check variables in file
all(check_in_alspac_data%in%names(alspac.table_ACE))
# Prints missing vars if FALSE
check_in_alspac_data [!check_in_alspac_data %in%names(alspac.table_ACE)]

## -----------------------------------------------------------------------------
## BLOCK 3 - DERIVE HIGHEST HOUSEHOLD SOCIAL CLASS
## -----------------------------------------------------------------------------


# Overview:
  # This block derives household-level social class variables at multiple time points
  # using parental social class indicators - highest of mother and partner's social class

# Clean admin missing values in org_sc_vars
cat("\n=== CLEANING ADMIN MISSING CATEGORIES IN ORG_SC_VARS ===\n")

org_sc_clean_log <- list()

for (v in org_sc_vars) {
  
  if (!v %in% names(alspac.table_ACE)) {
    cat("\nSkipping", v, "(not in dataset)\n")
    next
  }
  
  cat("\n\n--- Cleaning variable:", v, "---\n")
  
  x <- as.character(alspac.table_ACE[[v]])
  
  # Print levels before cleaning
  cat("Levels BEFORE cleaning:\n")
  print(sort(unique(x)))
  
  # Identify rows to convert to NA
  to_na <- which(x %in% admin_missing)
  
  if (length(to_na) > 0) {
    
    org_sc_clean_log[[v]] <- data.frame(
      row = to_na,
      before = x[to_na],
      stringsAsFactors = FALSE
    )
    
    x[to_na] <- NA
  }
  
  alspac.table_ACE[[v]] <- factor(x)
  
  # Print levels after cleaning
  cat("Levels AFTER cleaning:\n")
  print(sort(unique(alspac.table_ACE[[v]])))
}

cat("\n=== ORG_SC_VARS ADMIN MISSING CLEANING COMPLETE ===\n")
 
# Derive highest of mother and partner for each time point
 
sc_household_numeric <- sapply(alspac.table_ACE[, org_sc_vars], as.numeric)

labs <- c(
  "I - Professional","II - Managerial and technical",
  "IIIM - Skilled manual","IIINM - Skilled non-manual",
  "IV - Partly skilled","V - Unskilled"
)

alspac.table_ACE$sc_household_12wgest <-
  factor(sc_household_numeric[,"pa_sc_p"], labels = labs)

alspac.table_ACE$sc_household_18wgest <-
  rowMins(sc_household_numeric[, c("pb_sc_p","b_sc_m")], na.rm = TRUE)
alspac.table_ACE$sc_household_18wgest[
  is.infinite(alspac.table_ACE$sc_household_18wgest)] <- NA
alspac.table_ACE$sc_household_18wgest <-
  factor(alspac.table_ACE$sc_household_18wgest, labels = labs)

alspac.table_ACE$sc_household_32wgest <-
  rowMins(sc_household_numeric[, c("c_sc_ptnr_pp","c_sc_m")], na.rm = TRUE)
alspac.table_ACE$sc_household_32wgest[
  is.infinite(alspac.table_ACE$sc_household_32wgest)] <- NA
alspac.table_ACE$sc_household_32wgest <-
  factor(alspac.table_ACE$sc_household_32wgest, labels = labs)

alspac.table_ACE$sc_household_8m <-
  rowMins(sc_household_numeric[, c("pd_sc_p","f_sc_ptnr_pp","f_sc_m")], na.rm = TRUE)
alspac.table_ACE$sc_household_8m[
  is.infinite(alspac.table_ACE$sc_household_8m)] <- NA
alspac.table_ACE$sc_household_8m <-
  factor(alspac.table_ACE$sc_household_8m, labels = labs)

alspac.table_ACE$sc_household_2yr <-
  rowMins(sc_household_numeric[, c("pe_sc_p","g_sc_ptnr_pp","g_sc_m")], na.rm = TRUE)
alspac.table_ACE$sc_household_2yr[
  is.infinite(alspac.table_ACE$sc_household_2yr)] <- NA
alspac.table_ACE$sc_household_2yr <-
  factor(alspac.table_ACE$sc_household_2yr, labels = labs)

alspac.table_ACE$sc_household_3yr <-
  rowMins(sc_household_numeric[, c("h_sc_ptnr_pp","h_sc_m")], na.rm = TRUE)
alspac.table_ACE$sc_household_3yr[
  is.infinite(alspac.table_ACE$sc_household_3yr)] <- NA
alspac.table_ACE$sc_household_3yr <-
  factor(alspac.table_ACE$sc_household_3yr, labels = labs)

alspac.table_ACE$sc_household_4yr <-
  rowMins(sc_household_numeric[, c("j_sc_ptnr_pp","j_sc_m")], na.rm = TRUE)
alspac.table_ACE$sc_household_4yr[
  is.infinite(alspac.table_ACE$sc_household_4yr)] <- NA
alspac.table_ACE$sc_household_4yr <-
  factor(alspac.table_ACE$sc_household_4yr, labels = labs)

alspac.table_ACE$sc_household_8yr <-
  factor(sc_household_numeric[, "pl_sc_p"], labels = labs)

# Checks 
table(
  alspac.table_ACE$pa_sc_p,
  alspac.table_ACE$sc_household_12wgest,
  useNA = "ifany"
)

rm(sc_household_numeric, org_sc_vars)

## -----------------------------------------------------------------------------
## BLOCK 4 - DERIVE BINARY MATERNAL SMOKING VARIABLES
## -----------------------------------------------------------------------------

# Overview:
  # Clean smoking variables and derive binary indicators

# c482: custom numeric conversion
char_vals <- as.character(alspac.table_ACE$c482)
alspac.table_ACE$c482_numeric <- suppressWarnings(as.numeric(char_vals))
alspac.table_ACE$c482_numeric[char_vals == "None"] <- 0
alspac.table_ACE$c482 <- alspac.table_ACE$c482_numeric
alspac.table_ACE$c482_numeric <- NULL

# e178: clean "Missing" to NA
alspac.table_ACE$e178 <- as.character(alspac.table_ACE$e178)
alspac.table_ACE$e178[alspac.table_ACE$e178 == "Missing"] <- NA
alspac.table_ACE$e178 <- as.factor(alspac.table_ACE$e178)

# binary smoking variables
alspac.table_ACE$matsmok_tri1  <- ifelse(alspac.table_ACE$b665 == "N", 0, 1)
alspac.table_ACE$matsmok_tri2  <- ifelse(alspac.table_ACE$b667 == "N", 0, 1)
alspac.table_ACE$matsmok_tri3_c<- ifelse(alspac.table_ACE$c482 == 0, 0, 1)
alspac.table_ACE$matsmok_tri3_e<- ifelse(alspac.table_ACE$e178 == "Not at all", 0, 1)

# update impute_ACE
impute_ACE <- impute_ACE[!impute_ACE %in% c("b665","b667","c482","e178")]
impute_ACE <- c(impute_ACE, grep("matsmok", names(alspac.table_ACE), value = TRUE))

## -----------------------------------------------------------------------------
## BLOCK 5 - NORMALISE TEXT (WHITESPACE CLEANING)
## -----------------------------------------------------------------------------

# Overview: 
  # This block standardises text across selected variables to prevent subtle 
  # formatting issues from breaking comparisons, joins, or recoding later in the workflow.

# What it does:
# - Detects and cleans problematic whitespace characters, including:
#   * Non-breaking spaces (often introduced by copy/paste or imports)
#   * Other hidden Unicode space characters
#   * Tabs, newlines, and carriage returns
#   * Repeated spaces

# Logs any detected issues and modifications:
  #   * Flags variables containing suspicious whitespace before cleaning
  #   * Records how many values were changed
  #   * Stores a small sample of before/after examples for inspection
  #   * Tracks changes to factor levels where applicable
  #  Outputs a summary listing which variables were modified
  

# Additional step:
  # Applies targeted, variable-specific cleaning to `h061` and `kd058`
  #   to handle known issues (e.g. empty strings, text labels, numeric conversion).


# Fix hidden whitespace inconsistencies that break matching
normalize_ws <- function(x) {
  x <- gsub("[\u00A0\u2000-\u200B]", " ", x)  # Unicode spaces
  x <- gsub("[\t\r\n]", " ", x)              # tabs/newlines
  x <- gsub(" +", " ", x)                    # collapse spaces
  trimws(x)
}

detect_suspicious <- function(x) {
  patterns <- list(
    "non-breaking space" = "\u00A0",
    "unicode spaces"     = "[\u2000-\u200B]",
    "tabs"               = "\t",
    "newlines"           = "\n",
    "carriage returns"   = "\r",
    "multiple spaces"    = "  +"
  )
  
  found <- names(Filter(function(p) any(grepl(p, x, perl = TRUE)), patterns))
  found
}

log_changes <- list()

for (v in ACE_var) {
  if (v %in% names(alspac.table_ACE)) {
    x <- alspac.table_ACE[[v]]
    original_x <- x
    entry_log <- list()
    
    # Detect suspicious whitespace BEFORE cleaning
    suspicious <- detect_suspicious(if (is.factor(x)) levels(x) else x)
    if (length(suspicious) > 0) {
      entry_log$warnings <- paste("Suspicious whitespace detected:", 
                                  paste(suspicious, collapse = ", "))
    }
    
    if (is.factor(x)) {
      old_levels <- levels(x)
      new_levels <- normalize_ws(old_levels)
      
      level_changed <- !identical(old_levels, new_levels)
      if (level_changed) {
        entry_log$level_changes <- data.frame(
          before = old_levels,
          after  = new_levels,
          changed = old_levels != new_levels
        )
      }
      
      levels(x) <- new_levels
      
      old_values <- as.character(x)
      new_values <- normalize_ws(old_values)
      
      value_changed <- !identical(old_values, new_values)
      if (value_changed) {
        changed_idx <- which(old_values != new_values)
        entry_log$value_change_count <- length(changed_idx)
        
        # sample up to 5 examples
        sample_idx <- head(changed_idx, 5)
        entry_log$value_examples <- data.frame(
          before = old_values[sample_idx],
          after  = new_values[sample_idx]
        )
      }
      
      x <- factor(new_values, levels = new_levels)
      
    } else {
      old_values <- x
      new_values <- normalize_ws(x)
      
      value_changed <- !identical(old_values, new_values)
      if (value_changed) {
        changed_idx <- which(old_values != new_values)
        entry_log$value_change_count <- length(changed_idx)
        
        sample_idx <- head(changed_idx, 5)
        entry_log$value_examples <- data.frame(
          before = old_values[sample_idx],
          after  = new_values[sample_idx]
        )
      }
      
      x <- new_values
    }
    
    alspac.table_ACE[[v]] <- x
    
    if (length(entry_log) > 0) {
      log_changes[[v]] <- entry_log
    }
  }
}

# Print summary
if (length(log_changes) == 0) {
  message("No variables were modified.")
} else {
  message("Variables modified:")
  for (nm in names(log_changes)) {
    message("  - ", nm)
  }
}


# Additional manual fixes (variable-specific cleaning)
alspac.table_ACE$h061 <- as_factor(alspac.table_ACE$h061)
alspac.table_ACE$h061 <- as.character(alspac.table_ACE$h061)
alspac.table_ACE$h061[alspac.table_ACE$h061 == ""] <- "0"
alspac.table_ACE$h061 <- suppressWarnings(as.numeric(alspac.table_ACE$h061))

alspac.table_ACE$kd058 <- as_factor(alspac.table_ACE$kd058)
alspac.table_ACE$kd058 <- as.character(alspac.table_ACE$kd058)
alspac.table_ACE$kd058[alspac.table_ACE$kd058 %in% c("Not admitted")] <- NA
alspac.table_ACE$kd058 <- suppressWarnings(as.numeric(alspac.table_ACE$kd058))

## -----------------------------------------------------------------------------
## BLOCK 6 - BUILD RECODING MAP FROM METADATA
## -----------------------------------------------------------------------------
# Parse recode_ACE column into structured lookup lists

all_levels_by_var <- lapply(ACE_var, function(v) {
  if (v %in% names(alspac.table_ACE) && is.factor(alspac.table_ACE[[v]])) {
    levels(alspac.table_ACE[[v]])   # already cleaned by your earlier block
  } else {
    NULL
  }
})
names(all_levels_by_var) <- ACE_var

## -----------------------------------------------------------------------------
## BLOCK 7 -  Clean & parse recode_ACE: IDENTIFY & REMOVE ADMIN MISSING CATEGORIES
## ----------------------------------------------------------------------------

# Overview:
  # This block aligns the dataset values with the expected category definitions 
  # stored in metadata (`recode_ACE`). It ensures that only valid, meaningful 
  # response categories are retained for analysis.


# What it does:
# 1) Cleans and standardises recode definitions
#    - Parses the `recode_ACE` column into individual categories
#    - Applies the SAME whitespace cleaning rules used on the data (see Block 5)
#    - Removes formatting artefacts (e.g., stray trailing "]")
#    - Drops empty or malformed entries
#    - Builds a clean lookup (`recode_map`) of valid categories per variable
#    - Logs any issues (e.g., suspicious whitespace, formatting changes)

# 2) Compares data values vs expected categories
#    - Checks each variable’s actual data levels against its cleaned recode list
#    - Identifies values present in the data but NOT defined in recode_ACE
#    - These typically represent administrative or missing categories 
#      (e.g., "Not completed", "Missing", "-1")
#    - Prints a diagnostic report showing these discrepancies


# 3) Removes invalid / administrative categories
#    - For factor variables, values not present in recode_ACE are set to NA
#    - Ensures only analytically valid categories remain
#    - Logs:
#        * Which categories were removed
#        * Number of affected rows
#        * Example rows (for traceability)


# 4) Detects numeric category mismatches
#    - Identifies cases where numeric values in the data are not listed in recode_ACE
#    - Reports mismatches separately for review
#    - Optional auto-fix (disabled by default):
#        * Can convert unexpected numeric values to NA if enabled


# 5) Outputs summaries
#    - Diagnostic printout of mismatched categories
#    - Summary of values removed per variable
#    - Report of numeric mismatches (if any)

# NOTE:
  # - The original `recode_ACE` column is NOT modified
  # - Cleaned recode definitions are stored separately in `recode_map`
  # - Data in `alspac.table_ACE` IS modified (invalid categories set to NA)
  # - All changes are logged (see `recode_log` and `removed_log`)
  
recode_log <- list()

parse_recode_levels <- function(x, varname) {
  original <- x
  
  # Detect suspicious patterns BEFORE cleaning
  susp <- detect_suspicious(original)
  
  # Split on semicolons
  parts <- unlist(strsplit(original, ";"))
  
  # Clean whitespace using the SAME rules as the data
  cleaned <- normalize_ws(parts)
  
  # Remove ONLY trailing ']' (artifact), preserve intentional ones
  cleaned2 <- sub("\\]$", "", cleaned)
  
  # Drop empty entries
  cleaned2 <- cleaned2[nzchar(cleaned2)]
  
  # Logging
  entry <- list()
  
  if (length(susp) > 0) {
    entry$warnings <- paste("Suspicious whitespace:", paste(susp, collapse = ", "))
  }
  
  if (!identical(parts, cleaned2)) {
    entry$before <- parts
    entry$after  <- cleaned2
    
    changed_idx <- which(parts != cleaned2)
    sample_idx <- head(changed_idx, 5)
    entry$examples <- data.frame(
      before = parts[sample_idx],
      after  = cleaned2[sample_idx]
    )
  }
  
  if (length(entry) > 0) {
    recode_log[[varname]] <<- entry
  }
  
  cleaned2
}

# Build recode map 
recode_map <- list()
for (i in seq_along(adv_description$recode_ACE)) {
  var <- adv_description$variable_unique[i]
  recode_map[[var]] <- parse_recode_levels(adv_description$recode_ACE[i], var)
}

## This block is checking the data against recode_map (the cleaned recode_ACE)
## Any categories within variables that are in the data but not in recode_map are removed
## these are missing admin categories such as not completed, missing, -1 etc.

missing_by_var <- lapply(names(all_levels_by_var), function(v) {
  data_levels   <- all_levels_by_var[[v]]
  recode_levels <- recode_map[[v]]
  setdiff(data_levels, recode_levels)
})
names(missing_by_var) <- names(all_levels_by_var)

missing_categories <- sort(unique(unlist(missing_by_var)))

cat("==== DIAGNOSTIC: DATA LEVELS NOT IN recode_ACE ====\n")
for (v in names(missing_by_var)) {
  if (!is.null(missing_by_var[[v]]) && length(missing_by_var[[v]]) > 0) {
    cat("\nVariable:", v, "\n")
    cat("  Data levels not in recode_ACE:\n")
    cat("   -", paste(missing_by_var[[v]], collapse = "\n   - "), "\n")
  }
}
cat("\nTotal unique missing categories across all variables:\n")
print(missing_categories)
cat("=====================================================\n\n")

## CLEAN MISSING ADMIN CATEGORIES (PER VARIABLE, WITH LOGGING)
# set missing admin categories to NA
removed_log <- list()

for (v in ACE_var) {
  if (v %in% names(alspac.table_ACE) && is.factor(alspac.table_ACE[[v]])) {
    
    missing_for_v <- missing_by_var[[v]]
    
    if (!is.null(missing_for_v) && length(missing_for_v) > 0) {
      
      x <- as.character(alspac.table_ACE[[v]])
      
      # Identify which rows will be set to NA
      to_remove <- which(x %in% missing_for_v)
      
      if (length(to_remove) > 0) {
        
        # Log details
        removed_log[[v]] <- list(
          removed_categories = missing_for_v,
          n_removed = length(to_remove),
          examples = head(data.frame(
            before = x[to_remove],
            row = to_remove
          ), 5)
        )
        
        # Apply removal
        x[to_remove] <- NA
      }
      
      alspac.table_ACE[[v]] <- factor(x)
    }
  }
}

## Print summary
cat("==== BLOCK 7: VALUES REMOVED PER VARIABLE ====\n")

if (length(removed_log) == 0) {
  cat("No values were removed in any variable.\n")
} else {
  for (v in names(removed_log)) {
    cat("\nVariable:", v, "\n")
    cat("  Categories removed:\n")
    cat("   -", paste(removed_log[[v]]$removed_categories, collapse = "\n   - "), "\n")
    cat("  Number of values set to NA:", removed_log[[v]]$n_removed, "\n")
    cat("  Example rows removed:\n")
    print(removed_log[[v]]$examples)
  }
}

cat("\n==============================================\n\n")

# Detect numeric categories mismatches

auto_fix_numeric_mismatches <- FALSE

numeric_mismatch_report <- list()

for (i in seq_len(nrow(adv_description))) {
  
  var <- adv_description$variable_unique[i]
  
  if (!var %in% names(alspac.table_ACE)) next
  
  x <- alspac.table_ACE[[var]]
  recode_levels <- recode_map[[var]]
  
  # Only check numeric or numeric-coded-as-character variables
  if (is.numeric(x) || all(grepl("^[0-9]+$", na.omit(as.character(x))))) {
    
    data_levels <- sort(unique(as.character(x)))
    recode_levels_chr <- trimws(as.character(recode_levels))
    
    mismatches <- setdiff(data_levels, recode_levels_chr)
    
    if (length(mismatches) > 0) {
      
      numeric_mismatch_report[[var]] <- mismatches
      
      cat("\n==== NUMERIC MISMATCH DETECTED ====\n")
      cat("Variable:", var, "\n")
      cat("Data levels not in recode_ACE:\n")
      cat("  -", paste(mismatches, collapse = "\n  - "), "\n")
      cat("===================================\n")
      
      # Optional auto-fix
      if (auto_fix_numeric_mismatches) {
        cat("  -> Auto-fixing: converting these values to NA\n")
        alspac.table_ACE[[var]] <- as.character(alspac.table_ACE[[var]])
        alspac.table_ACE[[var]][
          alspac.table_ACE[[var]] %in% mismatches
        ] <- NA
        alspac.table_ACE[[var]] <- factor(alspac.table_ACE[[var]])
      }
    }
  }
}

# Summary (no file writing)
cat("\n==== SUMMARY: NUMERIC MISMATCH REPORT ====\n")

if (length(numeric_mismatch_report) == 0) {
  cat("No numeric mismatches detected.\n")
} else {
  cat("Variables with mismatches:", length(numeric_mismatch_report), "\n\n")
  print(numeric_mismatch_report)
}

cat("==========================================\n\n")

## -----------------------------------------------------------------------------
## BLOCK 8 - PREPARE VARIABLES BEFORE BINARY RECODING 
## -----------------------------------------------------------------------------

# Overview:
  # Prepare data for recoding:
  # A:Duplicate variables encoding two different exposure time periods
  # B:Factor levels in correct order 
  # C:Add "FALSE / non-exposed" to skipped questions
  # Ensures that values missing due to questionnaire skip logic
  # are not treated as genuinely missing data
  # D:Apply final level ordering from metadata
  # Uses factor_level_after_recode column
  # E:Check data is either factor or numeric

## A: add duplicate variables (variable encoding two different time periods)
table(duplicated(ACE_var))#unique id
alspac.table_ACE<-cbind(alspac.table_ACE,
                        alspac.table_ACE[,gsub('_dup','',adv_description$variable_unique[
                          duplicated(gsub('_dup','',adv_description$variable_unique))])])
#   add _dup to names of variables encoding 2 time periods
names(alspac.table_ACE)[duplicated(names(alspac.table_ACE))]<-
  paste0(names(alspac.table_ACE)[duplicated(names(alspac.table_ACE))],'_dup')
names(alspac.table_ACE)[names(alspac.table_ACE)%in% gsub('_dup','',adv_description$variable_unique[!adv_description$variable_unique%in%names(alspac.table_ACE)])] <- paste0(names(alspac.table_ACE)[names(alspac.table_ACE)%in% gsub('_dup','',adv_description$variable_unique[!adv_description$variable_unique%in%names(alspac.table_ACE)])], '_dup')

## B: Variables need to be ordered properly 
# Change for variables with factor levels in wrong order (ie reverse coding is marked as special)

var_special <- adv_description$variable_unique[grepl('special', adv_description$reverse_scale)]

cat('\n\n', paste0(Sys.time()),
    ' Start changing levels for variables with factor levels in wrong order 
    (ie reverse coding is marked as special)\n')

for(var in var_special){
  
  cat("\n==============================\n")
  cat("Variable:", var, "\n")
  
  ## BEFORE
  cat("Before:", length(levels(factor(alspac.table_ACE[[var]]))), "levels:\n  ",
      paste(levels(factor(alspac.table_ACE[[var]])), collapse=", "), "\n")
  
  ## Original recode_ACE
  raw_recode <- adv_description[adv_description$variable_unique==var, 'recode_ACE']
  cat("Original recode_ACE string:\n  ", raw_recode, "\n")
  
  ## Split recode_ACE
  recode <- unlist(strsplit(raw_recode, '];'))
  cat("Split recode (length =", length(recode), "):\n  ",
      paste(recode, collapse=" | "), "\n")
  
  ## Existing logic (unchanged)
  new_recode_ACE <-
    if(length(recode)==3){
      paste0(recode[[3]],';',recode[[1]],'];',recode[[2]])
    } else if(length(recode)==4){
      paste0(recode[[3]],';',recode[[1]],'];',recode[[2]],';',recode[[4]])
    } else {
      stop(paste("ERROR: recode length =", length(recode), "for variable", var))
    }
  
  cat("Reconstructed recode_ACE:\n  ", new_recode_ACE, "\n")
  
  ## Store updated recode_ACE
  adv_description[adv_description$variable_unique==var, 'recode_ACE'] <- new_recode_ACE
  
  ## Clean and split again
  recode <- unlist(strsplit(gsub(']','', new_recode_ACE), ';'))
  cat("Final recode levels (after gsub + split):\n  ",
      paste(recode, collapse=" | "), "\n")
  
  ## Apply new levels
  alspac.table_ACE[[var]] <- as.character(alspac.table_ACE[[var]])
  alspac.table_ACE[[var]] <- factor(alspac.table_ACE[[var]], levels = recode)
  
  ## Update reverse_scale
  if(grepl('yes', adv_description[adv_description$variable_unique==var, 'reverse_scale'])){
    adv_description[adv_description$variable_unique==var, 'reverse_scale'] <- 'yes'
  } else {
    adv_description[adv_description$variable_unique==var, 'reverse_scale'] <- 'no'
  }
  
  ## AFTER
  cat("After:", length(levels(factor(alspac.table_ACE[[var]]))), "levels:\n  ",
      paste(levels(factor(alspac.table_ACE[[var]])), collapse=", "), "\n")
  
  cat("==============================\n")
  
  rm(recode)
}

##C: Add in non-exposed for variables with skips: questions where answers depend on previous question
var_follow_up <- adv_description$variable_unique[
  which(!is.na(adv_description$followup_inc_other_question) & 
          adv_description$followup_inc_other_question != 'no')]
cat('\n\n', paste0(Sys.time()),
    'Start changing levels for variables that are skips: questions where answers depend on previous question\n')

for(var in var_follow_up){
  tryCatch({
    # Debug: Start processing variable
    cat("\nProcessing variable:", var, "\n")  # Print the variable name
    
    # Debug: Print the table of current variable levels before modification
    cat("\nCurrent levels for variable", var, ":\n")
    print(table(alspac.table_ACE[[var]]))
    
    # Generate non-missing condition based on follow-up question
    non_miss <- !is.na(alspac.table_ACE[, gsub('_.*$', '', adv_description$followup_inc_other_question[
      which(adv_description$variable_unique == var)])])
    
    # Convert the variable to character to allow adding FALSE values
    alspac.table_ACE[[var]] <- as.character(alspac.table_ACE[[var]])
    alspac.table_ACE[non_miss & is.na(alspac.table_ACE[[var]]), var] <- FALSE
    
    if (adv_description$reverse_scale[which(adv_description$variable_unique == var)] == 'yes') {
      
      # Debug: Print recode_ACE before modification
      cat("\nOriginal recode_ACE for", var, ":", adv_description$recode_ACE[which(adv_description$variable_unique == var)], "\n")
      
      adv_description[which(adv_description$variable_unique == var), 'recode_ACE'] <- 
        paste0(adv_description$recode_ACE[which(adv_description$variable_unique == var)], ";FALSE")
      
      # Debug: After modification of recode_ACE
      cat("\nUpdated recode_ACE for", var, ":", adv_description$recode_ACE[which(adv_description$variable_unique == var)], "\n")
      
      cutoff_pre <- lapply(strsplit(adv_description$recode_ACE[which(adv_description$variable_unique == var)],
                                    '];'), strsplit, split = ';')[[1]]
      
      # Debug: Print the raw split of recode_ACE
      cat("\nSplit recode_ACE for", var, ":\n")
      print(cutoff_pre)
      
      new.levels <- unlist(lapply(1:length(cutoff_pre), function(i, ...) {
        v <- abs(rep(i - length(cutoff_pre), length(cutoff_pre[[i]])))
        names(v) <- cutoff_pre[[i]]
        v
      }))
      
      # Ensure new.levels is properly aligned with existing factor levels
      if (!all(names(new.levels) %in% levels(factor(alspac.table_ACE[[var]])))) {
        cat("\nAligning new.levels with existing factor levels...\n")
        new.levels <- new.levels[levels(factor(alspac.table_ACE[[var]]))]
      }
      
      # Debug: Print out the lengths of new.levels and factor levels to compare
      cat("\nLength of new.levels:", length(new.levels), "\n")
      cat("Length of factor levels:", length(levels(factor(alspac.table_ACE[[var]]))), "\n")
      
      # Check if the levels are consistent
      stopifnot(length(new.levels) == length(levels(factor(alspac.table_ACE[[var]]))))  # This is where the error occurs
      
      if (!all(new.levels == levels(factor(alspac.table_ACE[[var]])))) {
        cat("\nUpdating factor levels for", var, "...\n")
        alspac.table_ACE[[var]] <- factor(alspac.table_ACE[[var]], levels = names(new.levels))
      }
    } else {
      
      # If no reverse scale, modify recode_ACE to include FALSE at the beginning
      if (grepl(']', adv_description[which(adv_description$variable_unique == var), 'recode_ACE'])) {
        adv_description[which(adv_description$variable_unique == var), 'recode_ACE'] <- 
          paste0("FALSE;", adv_description$recode_ACE[which(adv_description$variable_unique == var)])
      } else {
        adv_description[which(adv_description$variable_unique == var), 'recode_ACE'] <- 
          paste0("FALSE];", adv_description$recode_ACE[which(adv_description$variable_unique == var)])
      }
      
      # Debug: Print recode_ACE after modification
      cat("\nUpdated recode_ACE for", var, ":", adv_description$recode_ACE[which(adv_description$variable_unique == var)], "\n")
      
      cutoff_pre <- lapply(strsplit(adv_description$recode_ACE[which(adv_description$variable_unique == var)],
                                    '];'), strsplit, split = ';')[[1]]
      
      # Debug: Print the raw split of recode_ACE
      cat("\nSplit recode_ACE for", var, ":\n")
      print(cutoff_pre)
      
      new.levels <- unlist(lapply(1:length(cutoff_pre), function(i, ...) {
        v <- rep(i, length(cutoff_pre[[i]])) - 1
        names(v) <- cutoff_pre[[i]]
        v
      }))
      
      # Ensure new.levels is properly aligned with existing factor levels
      if (!all(names(new.levels) %in% levels(factor(alspac.table_ACE[[var]])))) {
        cat("\nAligning new.levels with existing factor levels...\n")
        new.levels <- new.levels[levels(factor(alspac.table_ACE[[var]]))]
      }
      
      # Debug: Print out the lengths of new.levels and factor levels to compare
      cat("\nLength of new.levels:", length(new.levels), "\n")
      cat("Length of factor levels:", length(levels(factor(alspac.table_ACE[[var]]))), "\n")
      
      # Check if the levels are consistent
      stopifnot(length(new.levels) == length(levels(factor(alspac.table_ACE[[var]]))))  # This is where the error occurs
      
      if (!all(new.levels == levels(factor(alspac.table_ACE[[var]])))) {
        cat("\nUpdating factor levels for", var, "...\n")
        alspac.table_ACE[[var]] <- factor(alspac.table_ACE[[var]], levels = names(new.levels))
      }
    }
    
    # Debug: Print the table of variable levels after modification
    cat("\nTable after modifying levels for", var, ":\n")
    print(table(alspac.table_ACE[[var]]))
    
    # Clean up variables to save memory
    rm(cutoff_pre, new.levels, non_miss)
    
  }, error = function(e) {
    # Capture the error and print the variable that caused it
    message("Error occurred while processing variable: ", var)
    message("Error message: ", e$message)
  })
}

## D: Apply factor_level_after_recode ordering for ALL factor variables 

cat("\n=== APPLYING factor_level_after_recode TO ALL VARIABLES ===\n")

for (v in adv_description$variable_unique) {
  
  if (!v %in% names(alspac.table_ACE)) next
  if (!is.factor(alspac.table_ACE[[v]])) next
  
  cat("\n--- Variable:", v, "---\n")
  
  x <- alspac.table_ACE[[v]]
  
  cat("Levels BEFORE:\n")
  print(levels(x))
  
  fl <- adv_description$factor_level_after_recode[
    adv_description$variable_unique == v
  ]
  
  # Use _, ; and ] as separators
  # 1) remove closing brackets
  fl_clean <- gsub("]", "", fl)
  # 2) split on _ or ;
  correct_levels <- unlist(strsplit(fl_clean, "[_;]"))
  correct_levels <- trimws(correct_levels)
  correct_levels <- correct_levels[correct_levels != ""]
  
  alspac.table_ACE[[v]] <- factor(x, levels = correct_levels)
  
  cat("Levels AFTER:\n")
  print(levels(alspac.table_ACE[[v]]))
}

cat("\n=== factor_level_after_recode APPLICATION COMPLETE ===\n")

##E: Check data is either factor or numeric
if(!all(sapply(alspac.table_ACE[,ACE_var],
               class)%in%c('factor','numeric'))){
  cat('\nalspac.table_ACE dataframe contains other type of data (not factor or numeric):',
      sapply(alspac.table_ACE[,ACE_var],
             class)[!sapply(alspac.table_ACE[,ACE_var],
                            class)%in%c('factor','numeric')])
}

## -----------------------------------------------------------------------------
## BLOCK 9 - RECODE TO BINARY ACE VARIABLES
## -----------------------------------------------------------------------------

# Overview:
  # A:define cutoff (see recode_ACE column in xlsx sheet 2)
  # B:Recode to binary
  # C:Save excel sheet of binary variables with counts and %s
  # D:Add non-dichotomised variables (id, auxiliary imputation)
  # E:Save

##A: define cutoff based on info in recode_ACE column of the Excel file with description variables 
lookup_cutoff<-lapply(strsplit(adv_description$recode_ACE,'];'),strsplit,split=';')
names(lookup_cutoff)<-adv_description$variable_unique
create_cutoff<-
  function(x,reverse_scale,...)
    if(reverse_scale=='yes'){
      rev(unlist( lapply(1:length(x), function(i,...){
        v<- abs(rep(i-length(x),length(x[[i]])))
        names(v)<-x[[i]]
        v
      })))} else{
        unlist(lapply(1:length(x), function(i,...){
          v<- rep(i,length(x[[i]]))-1
          names(v)<-x[[i]]
          v}))
      } 
cutoff_levels<-
  mapply(create_cutoff,lookup_cutoff,reverse_scale = adv_description$reverse_scale)
names(cutoff_levels)<-adv_description$variable_unique
new_labels<-cutoff_levels
new_levels<-cutoff_levels

##B: recode dataset to binary: 
## store mismatches
mismatch_summary <- list()

binary_alspacKids_ACE_data <-
  data.frame(sapply(adv_description$variable_unique, function(var)
  {
    
    if(!all(names(new_levels[[var]]) %in% levels(factor(alspac.table_ACE[[var]])))){
      
      new_levels[[var]] <- new_levels[[var]][levels(factor(alspac.table_ACE[[var]]))]
      
      new_labels[[var]] <-
        gsub(';','_', unlist(strsplit(adv_description$recode_ACE[
          which(adv_description$variable_unique == var)], '];')))
      
      new_labels[[var]] <-
        new_labels[[var]][grepl(paste0(names(new_levels[[var]]), collapse='|'), new_labels[[var]])]
      
    } else {
      
      new_labels[[var]] <-
        gsub(';','_', unlist(strsplit(adv_description$recode_ACE[
          which(adv_description$variable_unique == var)], '];')))
    }
    
    print(var)
    print(new_labels[[var]])
    print(names(new_levels[[var]]))
    
    ## DETECT mismatch
    data_lvls <- levels(factor(alspac.table_ACE[[var]]))
    meta_lvls <- names(new_levels[[var]])
    
    missing_data_levels <- setdiff(data_lvls, meta_lvls)
    
    if(length(missing_data_levels) > 0){
      
      mismatch_summary[[var]] <<- list(
        data_levels = data_lvls,
        metadata_levels = meta_lvls,
        missing_in_metadata = missing_data_levels
      )
    }
    
    ## mapping
    alspac.table_ACE[[var]] <- factor(
      new_levels[[var]][factor(alspac.table_ACE[[var]])],
      labels = new_labels[[var]]
    )
    
    ## convert to logical
    alspac.table_ACE[[var]] <- as.numeric(alspac.table_ACE[[var]]) == 2
    
  }))

## PRINT SUMMARY AT END

cat("\n\n==== MISMATCH SUMMARY ====\n")

if(length(mismatch_summary) == 0){
  
  cat("\n✅ No mismatches detected.\n")
  
} else {
  
  for(v in names(mismatch_summary)){
    
    cat("\n--------------------------------\n")
    cat("Variable:", v, "\n\n")
    
    cat("Data levels:\n")
    cat("  -", paste(mismatch_summary[[v]]$data_levels, collapse = "\n  - "), "\n\n")
    
    cat("Metadata levels:\n")
    cat("  -", paste(mismatch_summary[[v]]$metadata_levels, collapse = "\n  - "), "\n\n")
    
    cat("Levels in data NOT found in metadata:\n")
    cat("  -", paste(mismatch_summary[[v]]$missing_in_metadata, collapse = "\n  - "), "\n")
    
  }
  
  cat("\n================================\n")
}

## C: Save excel sheet of binary variables with counts and %s
# 1. Extract variable names and factor levels
cat("\n=== BUILDING ACE SUMMARY EXCEL SHEET WITH FORMATTED BINARY COUNTS ===\n")
var_list   <- adv_description$variable_unique
levels_cutoff <- adv_description$recode_ACE
level_list <- adv_description$factor_level_after_recode

# 2. Binary dataset variable names
binary_vars <- names(binary_alspacKids_ACE_data)

# 3. Function to find matching binary variable
match_binary_var <- function(v) {
  # direct match
  if (v %in% binary_vars) return(v)
  
  # duplicated variable (time-period duplicates)
  dup_name <- paste0(v, "_dup")
  if (dup_name %in% binary_vars) return(dup_name)
  
  # nothing else exists in your dataset
  return(NA)
}

# 4. Build summary table
binary_summary <- lapply(var_list, function(v) {
  
  bvar <- match_binary_var(v)
  
  if (is.na(bvar)) {
    return(c(FALSE_summary = NA, TRUE_summary = NA))
  }
  
  x <- binary_alspacKids_ACE_data[[bvar]]
  x <- as.factor(x)
  
  n_false <- sum(x == "FALSE", na.rm = TRUE)
  n_true  <- sum(x == "TRUE",  na.rm = TRUE)
  n_total <- n_false + n_true
  
  pct_false <- ifelse(n_total > 0, round(100 * n_false / n_total, 1), NA)
  pct_true  <- ifelse(n_total > 0, round(100 * n_true  / n_total, 1), NA)
  
  false_fmt <- ifelse(!is.na(n_false), paste0(n_false, " (", pct_false, "%)"), NA)
  true_fmt  <- ifelse(!is.na(n_true),  paste0(n_true,  " (", pct_true,  "%)"), NA)
  
  c(FALSE_summary = false_fmt, TRUE_summary = true_fmt)
})

binary_summary_df <- do.call(rbind, binary_summary)

# 5. Combine into final export table
ACE_export_table <- data.frame(
  variable_unique = var_list,
  recode_ACE = levels_cutoff,
  factor_level_after_recode = level_list,
  FALSE_summary = binary_summary_df[, "FALSE_summary"],
  TRUE_summary  = binary_summary_df[, "TRUE_summary"],
  stringsAsFactors = FALSE
)

# 6. Write Excel file
write.xlsx(
  ACE_export_table,
  file = "ACE_variable_summary.xlsx",
  sheetName = "ACE Summary",
  rowNames = FALSE
)
cat("\n=== EXCEL FILE WRITTEN: ACE_variable_summary.xlsx ===\n")

##D: add non-dichotomised variables
#alspac do file data 
binary_alspacKids_ACE_data[,alspac_do_file]<- 
  alspac.table_ACE[,alspac_do_file]
#this includes IDs
head(binary_alspacKids_ACE_data[,c('aln', 'qlet')])
#survived 1st year (kz011b)
table(binary_alspacKids_ACE_data$kz011b)
#as well as whether the child was part of a twin (mz010a)
table(binary_alspacKids_ACE_data$mz010a)

#for imputation also add the original variable for the 
#pregnancy adversities and ses
table(paste0(impute_ACE,'_org')%in%names(binary_alspacKids_ACE_data))
#should be FALSE for all 111 variables
binary_alspacKids_ACE_data[,paste0(impute_ACE,'_org')]<-
  alspac.table_ACE[,impute_ACE]
table(paste0(impute_ACE,'_org')%in%names(binary_alspacKids_ACE_data))
#should be TRUE for all 111 variables
#NOTE: to distinguish the non-binary version have added _org to the name
# So for variable 'c804', the original version is
table(binary_alspacKids_ACE_data$b608_org)
# dichotomised:
table(binary_alspacKids_ACE_data$b608)
head(binary_alspacKids_ACE_data[,c('aln', 'qlet')])

##E: Save
save(binary_alspacKids_ACE_data,file="binary_alspacKids_ACE_data.RData")

################################################################################
#End of script 
# Script_1_Recode_the_original_variables_to_binary_2026.R
################################################################################