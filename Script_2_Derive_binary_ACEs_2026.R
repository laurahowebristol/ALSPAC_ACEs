################################################################################

### Script 2 : Create 19 binary adverse childhood experiences (ACE) variables  ###
# (Script_2_Derive_binary_ACEs_2026.R)

# Last edited by Bushra Farooq July 2026 #

################################################################################

# REQUIRED INPUT FILES:

# 1. Excel file with description variables 
#   - 'SI_data1_ACE_definitions_overtime_2026.xlsx' 
#
# 2. Data file of binary(TRUE/FALSE) version of the variables 
#   described in the excel file, derived in Script 1:
#   'binary_alspacKids_ACE_data.RData'

# REQUIRED EDITS TO SCRIPT:

# A. Specify time period of ACE exposure (e.g. 0-16 years)
# B. Exclude certain ACE variables
# C. Threshold for number of ACE questions answered
# D. Reduce sample size by excluding alive at 1 yrs
# E. Reduce sample by excluding one of each twins

################################################################################


rm(list=ls()) # Clear workspace to avoid contamination


## -----------------------------------------------------------------------------
## BLOCK 1 - SETUP: Load packages, working directories, time periods
## -----------------------------------------------------------------------------

# Overview:
  # A:Load packages
  # B:Set up input and output file locations
  # C:Specify ACE exposure time period (Edit according to period of interest)
  # D:Exclude ACE vars
  # E:Specify threshold for number of questions answered 
  # F:Restrict sample size
  # G:Change the id added to all output

# A: Load packages
library(foreign)
library(readxl)
library(matrixStats)
library(tableone)

# B: Specifiy file locations 
# Change to location where input files are stored:
loc_inp= 'path/to/input/files/'

# Change to location where output should be stored:
loc_out= 'path/to/output/files/'

#Changed the loc_inp and loc_out to own folders.
setwd(loc_inp)
setwd(loc_out)

# Exact spelling for name of the excel file with description variables
name_excel<-'SI_data1_ACE_definitions_overtime_2026.xlsx'

# Exact spelling for name of the data file containing original variables to binary  
# Derived in Script 1
name_data<-'binary_alspacKids_ACE_data.RData'

# C: Specify time period, this includes variables based on the
#   time_yrs1 and time_yrs2 columns in the excel input file 
timeperiod<-c(0,16) # Default 0-16 yrs, which means ACE exposure from birth-16yrs

# D: Exclude variables for ACEs by specifying their names 
# List of ACE variable names in the column 'variables' in the excel input file). 
exclude_var<-NA  # Default NA

# Different method to exclude the variables for the ACE 'SES'.
#    Different because these variables are already derived from ALSPAC questions, 
#    so not in ALSPAC data dictionary.
exclude_sc<-NA # Default is NA

# E: Set a threshold for number of questions answered by participants
#    Default is, everyone needs to have answered 10% of the x ACE questions
threshold_proportion_answered_ACE0_16<-0.1 # numerical value between 0-1
#NOTE: Even setting to 0 still excludes participants that did not answer any questions,
#   This is done because there is no information for the imputation model

# F: Restrict to ALSPAC children dataset mentioned in Boyd et al.
sample_Boyd<-TRUE # Change to FALSE if you want to keep full sample

# Exclude one of each twin pair
exclude_twin<-TRUE # Change to TRUE if you want to remove.

# G: Change the id added to all output.
#   Default is alspacKids_ACE and whatever you specified as time period
fileid_out<-paste0('alspacKids_ACE_',timeperiod[1],'_',timeperiod[2])

## -----------------------------------------------------------------------------
## BLOCK 2 - Load data and metadata
## -----------------------------------------------------------------------------

# Overview:
  # A:Metadata - Excel file with description variables 
  # B:Data -  Binary version of the original variables (from Script 1)


# Custom function to output everything in a log file
catf <- function(..., file=paste0(loc_out,fileid_out,'_ACEderivation_log.txt'), append=TRUE){
  cat(..., file=file, append=append)
}
catf('\n\n',paste0(Sys.time()),
     'Start code block 2: Read in essential data\n')

# A: Metadata - Excel file with description variables 
adv_description<-data.frame(readxl::read_excel(
  paste0(loc_inp,name_excel),
  sheet = 'ACE variables',col_names=TRUE))

# B: Data file containing dataframe of binary variables described in the excel file
load(paste0(loc_inp,name_data))
dim(binary_alspacKids_ACE_data)  

#Keep global environment clear by removing no longer necessary objects
rm(name_data,name_excel)

## -----------------------------------------------------------------------------
## BLOCK 3 - Implement inclusion/exclusion criteria (time period, exclude variables)
## -----------------------------------------------------------------------------

# Overview:
  # A:Specify time period
  # B:Exclude certain ACE variables
  # C:Threshold for number ACE questions answered
  # D:Exclude participants based on alive after 1st year (Boyd et al., 2013)
  # E:Exclude one child per twin pair

catf('\n\n',paste0(Sys.time()),
     'Code block 3: Read in optional data and incorporate optional options\n')

# A: Specify time period
if(timeperiod[1]==0 & timeperiod[2]==16){#default
  ACE_var<-(adv_description$variable_unique[
    which(adv_description$time_yrs1>=timeperiod[1] &
            adv_description$time_yrs2<=timeperiod[2])
  ])
}else{#if chosen own
  ACE_var<-(adv_description$variable_unique[
    which(adv_description$time_yrs1>=timeperiod[1] &
            adv_description$time_yrs2<=timeperiod[2]
    )])
  catf('\nYou have chosen your own time period from',timeperiod[1],'till',timeperiod[2],'yrs,',
       'which results in the removal of',sum(adv_description$time_yrs1>=timeperiod[1],
                                             adv_description$time_yrs2<=timeperiod[2]),'variables.
Leaving',length(ACE_var),'ACE variables in the analysis\n'
  )
}

# B: Exclude certain ACE variables
if(!is.na(exclude_var)){
  catf('\nYou have chosen to exclude the following ACE variables',exclude_var,
       'which results in the removal of',sum(exclude_var%in%ACE_var),'variables
leaving',length(ACE_var[!ACE_var%in%exclude_var]),'ACE variables in the analysis')
  ACE_var<-ACE_var[!ACE_var%in%exclude_var]
}

if(!is.na(exclude_sc)){
  rm_sc<-c(grep('^sc_household',names(binary_alspacKids_ACE_data),value=T),
           adv_description$variable[adv_description$ACE%in%c('ses')])
  catf('\nYou have chosen to exclude the social class variables',rm_sc,
       'which results in the removal of',sum(rm_sc%in%ACE_var),'variables
leaving',length(ACE_var[!ACE_var%in%rm_sc]),'ACE variables in the analysis')
  ACE_var<-ACE_var[!ACE_var%in%rm_sc]
}

catf('\nFinal',length(ACE_var),'variables in the analyses are:\n',ACE_var,
     '\n\n\nThe distribution over the ACE categories is:\n',paste0(
       names(table(adv_description$ACE[adv_description$variable_unique%in%ACE_var])),' ',
       table(adv_description$ACE[adv_description$variable%in%ACE_var]),' variables',collapse='\n'),
     '\n')

# Excel files of variables include an excluded based on time period of interest
write.csv(adv_description[adv_description$variable_unique%in%ACE_var,],
          file=paste0(loc_out,fileid_out,"included_variables.csv"),row.names = F)
write.csv(adv_description[!adv_description$variable_unique%in%ACE_var,],
          file=paste0(loc_out,fileid_out,"excluded_variables.csv"),row.names = F)

# C: Exclude participants based on threshold nr ACE questions answered
ACE_var0_16<-unique(adv_description$variable[
  #making sure it's questions answered by using variable NOT variable_unique
  which(adv_description$time_yrs1>=0 &
          adv_description$time_yrs2<=16) 
])
jpeg('hist_prop_answeredACE.jpg')
hist(abs((rowSums(is.na(binary_alspacKids_ACE_data[,ACE_var0_16]))/length(ACE_var0_16))-1),
     main='',xlab='Proportion answered ACE questions',ylab='Number of individuals')
dev.off()

sum(!(rowSums(is.na(binary_alspacKids_ACE_data[,ACE_var0_16]))/
        length(ACE_var0_16))>=(1-threshold_proportion_answered_ACE0_16))
binary_alspacKids_ACE_data<-
  binary_alspacKids_ACE_data[
    !(rowSums(is.na(binary_alspacKids_ACE_data[,ACE_var0_16]))/
        length(ACE_var0_16))>=(1-threshold_proportion_answered_ACE0_16),]

# D: Exclude participants based on alive after 1st year (Boyd 2013)
if(sample_Boyd){
  binary_alspacKids_ACE_data<-
    binary_alspacKids_ACE_data[grepl('Yes',binary_alspacKids_ACE_data$kz011b),]
}

# E: Exclude one child per twin pair (but only if both twins survived all the other cutoffs above)
if(exclude_twin){
  binary_alspacKids_ACE_data<-binary_alspacKids_ACE_data[
    !(grepl('Multiple',binary_alspacKids_ACE_data$mz010a)&
        grepl('B',binary_alspacKids_ACE_data$qlet)&
        duplicated(binary_alspacKids_ACE_data$aln)),]
}

catf('\nFinal sample size analyses is:\n',nrow(binary_alspacKids_ACE_data),'\n\n')

# Keep global environment clear by removing no longer necessary objects
rm(exclude_var,exclude_sc,exclude_twin,sample_Boyd,
   ACE_var0_16,threshold_proportion_answered_ACE0_16)

## -----------------------------------------------------------------------------
## BLOCK 4 - Derive binary ACE constructs (yes/no)
## -----------------------------------------------------------------------------

# Overview:
  # This block transforms individual questionnaire variables into binary
  # ACE constructs and summary scores.

# Steps:
# 1. Define the set of ACE constructs (e.g. physical abuse, sexual abuse etc).
# 2. For each ACE:
#    - Identify all underlying questionnaire variables from metadata.
#    - Combine them into a single binary indicator (1 = exposed, 0 = not exposed).
#    - Apply missingness rules (set to NA if insufficient questions answered).
# 3. Remove ACE constructs with no available data.
# 4. Append time-period suffix to all ACE variable names.
# 5. Output diagnostics:
#    - % missing per ACE
#    - % exposed per ACE
# 6. Calculate summary ACE scores:
#    - Extended ACE score (all ACEs)
#    - Classic ACE score (subset of traditional ACEs)
# 7. Merge ACE constructs back into the main dataset.
# 8. Keep only relevant variables for analysis.
# 9. Ensure ID variable (aln) is numeric for merging.
# 10. Save the final dataset in R and Stata formats.
#
# KEY LOGIC:
# - Multiple variables per ACE exposure = any "yes" (rowSums > 0)
# - Missingness rule → set to NA if >50% of questions unanswered
#
# Classic 10 ACEs are: physical abuse, sexual abuse, emotional abuse,
# emotional neglect, bullying, violence between parents, household substance abuse,
# parental mental illness, parental conviction, parental separation

# Extended ACEs are: low social class, financial difficulties, bad neighbourhood,
# social support child, social support parent, violence between child and their partner,
# physical illness of child, physical illness of parent, parent-child bond

# See Houtepen et al (2018) for definitions 

catf('\n\n', Sys.time(),
     'Code block 4: Derive binary ACE constructs (yes/no)\n')

catf('\nDeriving adversity exposure (yes/no) between ',
     timeperiod[1], '–', timeperiod[2], ' years\n')

## -----------------------------------------------------------------------------
## 1. Define ACE constructs
## -----------------------------------------------------------------------------

ACEmeasures <- c(
  "ACEscore_classic", 
  "physical_abuse", "sexual_abuse", "emotional_abuse", "emotional_neglect",
  "bullying", "violence_between_parents",
  "substance_household",
  "mental_health_problems_or_suicide", "parent_convicted_offence",
  "parental_separation",
  "ACEscore_extended", 
  "social_class", "financial_difficulties", "neighbourhood",
  "social_support_child", "social_support_parent",
  "violence_between_child_and_partner", "physical_illness_child",
  "physical_illness_parent", "parent_child_bond"
)

ACEs <- ACEmeasures[c(2:11, 13:21)]
classicACEs <- ACEs[1:10]

## -----------------------------------------------------------------------------
## 2. Function to derive each ACE
## -----------------------------------------------------------------------------

derive_ACE_original <- function(ACE, ACE_var, data, metadata) {
  
  cat(ACE, ' ')
  
  
  vars_ACE <- metadata$variable_unique[
    metadata$variable_unique %in% ACE_var &
      metadata$ACE %in% ACE
  ]
  
  
  quest_ACE <- metadata$variable_unique[
    metadata$variable_unique %in% ACE_var[ACE_var %in% metadata$variable] &
      metadata$ACE %in% ACE
  ]
  
  print(quest_ACE)
  
  # Case 1: No variables
  if (length(vars_ACE) == 0) {
    catf('\n\nNo data available on', ACE,
         'Remove', ACE, 'from ACEs vector')
    x <- rep(NA, nrow(data))
    
    # Case 2: Single variable
  } else if (length(vars_ACE) == 1) {
    
    x <- data[, vars_ACE]
    
    # Case 3: Multiple variables (KEY LOGIC)
  } else {
    
    x <- rowSums(data[, vars_ACE], na.rm = TRUE) > 0
    
    miss <- rowSums(is.na(data[, quest_ACE]))
    
    print(dim(data[, quest_ACE]))
    
    # Apply missingness rule (must answer 50% questions)
    x[miss > (length(quest_ACE) / 2)] <- NA
  }
  
  return(x)
}

## -----------------------------------------------------------------------------
## 3. Generate ACE dataset 
## -----------------------------------------------------------------------------

ACE_time1_time2 <- data.frame(
  sapply(ACEs, derive_ACE_original,
         ACE_var = ACE_var,
         data = binary_alspacKids_ACE_data,
         metadata = adv_description)
)

## -----------------------------------------------------------------------------
## 4. Remove ACEs with all missing
## -----------------------------------------------------------------------------

ACE_time1_time2 <- ACE_time1_time2[
  , !colSums(is.na(ACE_time1_time2)) == nrow(ACE_time1_time2)
]

## -----------------------------------------------------------------------------
## 5. Add time-period suffix
## -----------------------------------------------------------------------------

colnames(ACE_time1_time2) <-
  paste0(colnames(ACE_time1_time2),
         '_', timeperiod[1], '_', timeperiod[2], 'yrs')

## -----------------------------------------------------------------------------
## 6. Diagnostics 
## -----------------------------------------------------------------------------

catf('\nPercentage missing:\n',
     paste0(names(ACE_time1_time2), '=',
            round(colSums(is.na(ACE_time1_time2)) /
                    nrow(ACE_time1_time2) * 100, 2),
            '%\n'))

catf('\nPercentage adversity exposed(=yes):\n',
     paste0(names(ACE_time1_time2), '=',
            round(colSums(ACE_time1_time2, na.rm = TRUE) /
                    colSums(!is.na(ACE_time1_time2)) * 100, 2),
            '%\n'))

## -----------------------------------------------------------------------------
## 7. Calculate ACE scores 
## -----------------------------------------------------------------------------

catf('\nCalculate extended ACE score for',
     sum(grepl(paste0(ACEs, collapse='|'),
               names(ACE_time1_time2))),
     'ACEs:',
     names(ACE_time1_time2)[
       grepl(paste0(ACEs, collapse='|'),
             names(ACE_time1_time2))], '\n')

# Identify ONLY ACE components (exclude score variables)
ACE_components <- ACE_time1_time2[
  , !grepl("^ACEscore", names(ACE_time1_time2))
]

# Convert TRUE/FALSE to 1/0
ACE_components <- as.data.frame(
  lapply(ACE_components, as.integer)
)

# Extended ACE score
ACE_time1_time2[[paste0('ACEscore_extended_',
                        timeperiod[1], '_',
                        timeperiod[2], 'yrs')]] <-
  rowSums(ACE_components, na.rm = TRUE)

catf('\nCalculate classic ACE score for',
     sum(grepl(paste0(classicACEs, collapse='|'),
               names(ACE_components))),
     'ACEs:',
     names(ACE_components)[
       grepl(paste0(classicACEs, collapse='|'),
             names(ACE_components))], '\n')

# Classic ACE score
ACE_time1_time2[[paste0('ACEscore_classic_',
                        timeperiod[1], '_',
                        timeperiod[2], 'yrs')]] <-
  rowSums(
    ACE_components[
      , grepl(paste0(classicACEs, collapse='|'),
              names(ACE_components))
    ],
    na.rm = TRUE
  )

## -----------------------------------------------------------------------------
## 8. Restrict ACE lists 
## -----------------------------------------------------------------------------

ACEs <- ACEs[ACEs %in%
               unique(adv_description$ACE[adv_description$variable_unique %in% ACE_var])]

classicACEs <- classicACEs[classicACEs %in%
                             unique(adv_description$ACE[adv_description$variable_unique %in% ACE_var])]

## -----------------------------------------------------------------------------
## 9. Merge with dataset
## -----------------------------------------------------------------------------

alspacKids_ACE_data_2026 <- cbind(binary_alspacKids_ACE_data,
                                  ACE_time1_time2)

## -----------------------------------------------------------------------------
## 10. Select variables 
## -----------------------------------------------------------------------------

select <- unique(c("aln", "qlet", names(ACE_time1_time2),
                   'kz021','mult','in_core','in_alsp',
                   'in_phase2','in_phase3','tripquad','mz010a','mz014',
                   grep('org', names(binary_alspacKids_ACE_data), value = TRUE),
                   gsub('_org','',
                        grep('org', names(binary_alspacKids_ACE_data), value = TRUE))
))

alspacKids_ACE_data_2026 <-
  alspacKids_ACE_data_2026[
    , select[select %in% names(alspacKids_ACE_data_2026)]
  ]

## -----------------------------------------------------------------------------
## 11. Clean dataset
## -----------------------------------------------------------------------------

alspacKids_ACE_data_2026 <- droplevels(alspacKids_ACE_data_2026)

if (!class(alspacKids_ACE_data_2026$aln) == 'numeric') {
  catf('\nOriginally aln was not numeric.')
  alspacKids_ACE_data_2026$aln <-
    as.numeric(as.character(alspacKids_ACE_data_2026$aln))
}


## -----------------------------------------------------------------------------
## 11. Save output
## -----------------------------------------------------------------------------

save(alspacKids_ACE_data_2026,
     file = paste0(loc_out, fileid_out, ".RData"))

write.dta(alspacKids_ACE_data_2026,
          file = paste0(loc_out, fileid_out, ".dta"))


## -----------------------------------------------------------------------------
## Create summary table (n and % exposed/unexposed)
## -----------------------------------------------------------------------------

ACE_summary <- data.frame(
  ACE = names(ACE_time1_time2),
  exposed_n = colSums(ACE_time1_time2 == 1, na.rm = TRUE),
  unexposed_n = colSums(ACE_time1_time2 == 0, na.rm = TRUE),
  total_n = colSums(!is.na(ACE_time1_time2))
)

# Percentages (based on non-missing total)
ACE_summary$exposed_pct <-
  round(ACE_summary$exposed_n / ACE_summary$total_n * 100, 2)

ACE_summary$unexposed_pct <-
  round(ACE_summary$unexposed_n / ACE_summary$total_n * 100, 2)

# Reorder columns as requested
ACE_summary <- ACE_summary[
  , c("ACE", "exposed_n", "exposed_pct",
      "unexposed_n", "unexposed_pct", "total_n")
]

## Save to Excel
# You may need: install.packages("writexl")
library(writexl)
writexl::write_xlsx(
  ACE_summary,
  path = paste0(loc_out, fileid_out, "_ACE_summary.xlsx")
)


################################################################################
#End of script 
# Script_2_Derive_binary_ACEs_2026.R
################################################################################