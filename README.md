24.11.2026

These files relate to the original Houtepen 2018 Wellcome Open Research paper, and have since been updated in 2025/2026 by Annie Herbert, Kim Burrows, Bushra Farooq, and Laura Howe to:
1) include additional variables that have been collected in subsequent ALSPAC data collections
2) account for changes to auxilliary variable names in the core ALSPAC data files
3) provide additional information around how to work with the original variables and syntax.

In this GitHub repository, there are three R scripts and two Excel files that can be used to derive the ACE variables and perform multiple imputation, and a Word document to guide the user. 

The files are:
- Notes for data use_ALSPAC ACEs July 2026 - this document is intended to guide the user through building the dataset, and then using the three R scripts to format the data, derive ACEs, and carry out imputation.

- 0_setup_required variables.xls - includes names of all core variables used to capture ACEs as well as all auxilliary variables used in imputation. An ALSPAC Direct User needs to create a dataset that includes these variables as a first step. If an external user, these are the minimum variables that should be requested from the ALSPAC team.

- SI_data1_ACE_definitions_overtime_2026.xls - including names of all core variables used to capture ACEs, and some meta-data about them (e.g. time-periods they cover, how asked, and when). This excel file is called upon within the R scripts.
  
- Script_1_Recode_the_original_variables_to_binary_2026.R - this file includes syntax that formats the data to correspond with their expected format for Scripts 2 (derive ACEs) and 3 (imputation). The resulting data file at the end of Script 1 is called  ‘binary_alspacKids_ACE_data.RData’

- Script_2_Derive_binary_ACEs_2026.R - this file includes syntax to derive the inidivdual binary ACE variables (classic and extended types), and counts of how many ACEs each individual experiences. The resulting data file at the end is ‘alspacKids_ACE_0_16.RData’ (unless you change the object ‘fileid_out’).

- Script_3_multiple_imputation_ACE_constructs_2026.R - this file carries out multiple imputation. The resulting data file at the end of Script 3 is called  ‘imputed_combined_[today's date].RData’

If you discover any inaccuracies/inconsistencies while using these files please contact annie.herbert@bristol.ac.uk and bushra.farooq@bristol.ac.uk.
