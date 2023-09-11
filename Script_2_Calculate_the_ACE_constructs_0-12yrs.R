##Lotte Houtepen last edited 29th May 2018 
# using ALSPAC data extracted on 13th April 2018 and 
# dichotomised with the script 'Script_1_Recode_the_original_variables_to_binary.R'


############################################################ 
#All code needed to create 19 binary adverse childhood experiences (ACE) variables
#for ALSPAC kids (n=15455) 
#
#After adapting the first bit of code, the additional 'code blocks' should run automatically. 
#
#Minimum required input is:
#1. Excel file with description variables 
#   - 'SI_data1_ACE_definitions_overtime.xlsx' 
#
#2. Data file for 15445 kids with binary(TRUE/FALSE) version of the variables 
#   described in the excel file
#   - 'binary_alspacKids_ACE_data.RData'
#NOTE: you can derive the binary file yourself with the script:
# Script_1_Recode_the_original_variables_to_binary.R
#
#Optional input:
#A. Specify time period
#B. Exclude certain ACE variables
#C. Threshold for nr questions answered
#D. Reduce sample size by excluding alive at 1 yrs
#E. Reduce sample by excluding one of each twins
#F. Specify tag used in all output
###############
rm(list=ls())#clear global R environment

##############################
###Adapt according to your analysis
##############################
##HAVE TO CHANGE 0-2 (or the script will fail)

#0. Set locations
#  change to location where input files are stored:
loc_inp<-'add filepath'
#   change to location output should be stored (make sure this folder exists):
loc_out<-'add filepath'
#   To check if you specified locations correctly, run in R:
setwd(loc_inp)
setwd(loc_out)
#   If either of the two lines above generate an Error, 
#   the location was misspecified/ folder does not exist.

#1. Exact spelling for name of the excel file with description variables
#Spelling is case sensitive
name_excel<-'SI_data1_ACE_definitions_overtime.xlsx'

#2. Exact spelling for name of the data file containing dataframe for 
#   15445 kids on the variables described in the excel file
#   Spelling is case sensitive
name_data<-'binary_alspacKids_ACE_data.RData'


##Optional to change the following four (A,B1,B2,C,D,E,F)

#A. Specify time period, this includes variables based on the
#   time_yrs1 and time_yrs2 columns in the excel input file (see NOTE below).
timeperiod<-c(0,16)#Default 0-16 yrs, which means 
#all variables reporting on birth till 16, 
#NOTE: this uses time_yrs1>=0 & time_yrs2<=16 
#      which excludes variables reporting on 12-18yrs
#      (as time_yrs2 will be 18 for these variables, which is higher than 16)

#B1. Exclude variables for most ACEs by specifying their names 
#(list of ACE variable names in the column 'variables' in the excel input file). 
exclude_var<-NA#Default NA

#B2. Added on 20180213, different method to exclude the variables for the ACE 'SES'.
#    Different because these variables are already derived from ALSPAC questions, 
#    so not in ALSPAC data dictionary.
#IF you want to exclude, set the following to TRUE:
exclude_sc<-NA#Default is NA

#C.  Added on 20180426, set a threshold for nr questions answered by participants
#    This is based on the proportion of questions on ACE exposure 0-18yrs
#    Default is, everyone needs to have answered 10% of the ... ACE questions
threshold_proportion_answered_ACE0_18<-0.1#numerical value between 0-1
#NOTE: Even setting to 0 still excludes participants that did not answer any questions,
#     This is done because there is no information for the imputation model

#D. Restrict to alspac children dataset mentioned in Boyd et al, n=14701 
#   or in our case n=14691 as we already removed tripquad
sample_Boyd<-TRUE#Change to FALSE if you want to keep n=15455

#E.Exclude one of each twin pair
exclude_twin<-FALSE#Change to TRUE if you want to remove

#F. Change the id added to all output.
#   Default is alspacKids_ACE and whatever you specified as time period
fileid_out<-paste0('alspacKids_ACE_',timeperiod[1],'_',timeperiod[2])

############################
#Code block 1: Load packages (if necessary will try to install from bioconductor)
############################
packages<-c('foreign','readxl','matrixStats','tableone')
source("http://bioconductor.org/biocLite.R")
for(pkg in packages){
if(!require(pkg,character.only=T)){
biocLite(pkg,suppressUpdates=TRUE)
library(pkg,character.only=T)
}
}
rm(pkg,packages)

catf <- function(..., file=paste0(loc_out,fileid_out,'_ACEderivation_log.txt'), append=TRUE){
cat(..., file=file, append=append)
}#custom function to output everything in a log file

############################
#Code block 2: Read in essential data
############################
#1. Excel file with description variables 
#2. Data file for ALSPAC kids with binary version of the variables described in the excel file

catf('\n\n',paste0(Sys.time()),
'Start code block 2: Read in essential data\n')

#1. Excel file with description variables 
adv_description<-data.frame(readxl::read_excel(
paste0(loc_inp,name_excel),
sheet = 'ACE variables',col_names=TRUE))

#2. Data file containing dataframe for 15445 kids on the variables described in the excel file
load(paste0(loc_inp,name_data))
dim(binary_alspacKids_ACE_data)#15445 observations for 811 variables

#Keep global environment clear by removing no longer necessary objects
rm(name_data,name_excel)

##############################
#Code block 3: Incorporate optional options (time period, exclude certain variables)
##############################
#Optional input:
#A. Specify time period
#B. Exclude certain ACE variables
#C-E. Exclude participants based on Boyd et al 2013, twins, threshold nr ACE questions answered

catf('\n\n',paste0(Sys.time()),
     'Code block 3: Read in optional data and incorporate optional options\n')

#A. Specify time period
if(timeperiod[1]==0& timeperiod[2]==16){#default
  ACE_var<-tolower(adv_description$variable_unique[
    which(adv_description$time_yrs1>=timeperiod[1] &
            adv_description$time_yrs2<=timeperiod[2])
    ])
}else{#if chosen own
  ACE_var<-tolower(adv_description$variable_unique[
    which(adv_description$time_yrs1>=timeperiod[1] &
            adv_description$time_yrs2<=timeperiod[2]
    )])
  catf('\nYou have chosen your own time period from',timeperiod[1],'till',timeperiod[2],'yrs,',
       'which results in the removal of',sum(adv_description$time_yrs1>=timeperiod[1],
                                             adv_description$time_yrs2<=timeperiod[2]),'variables.
Leaving',length(ACE_var),'ACE variables in the analysis\n'
  )
}

#B. Exclude certain ACE variables
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

write.csv(adv_description[adv_description$variable_unique%in%ACE_var,],
file=paste0(loc_out,fileid_out,"included_variables.csv"),row.names = F)
write.csv(adv_description[!adv_description$variable_unique%in%ACE_var,],
file=paste0(loc_out,fileid_out,"excluded_variables.csv"),row.names = F)

#C.  Exclude participants based on threshold nr ACE questions answered
ACE_var0_18<-unique(tolower(adv_description$variable[
#making sure it's questions answered by using variable NOT variable_unique
which(adv_description$time_yrs1>=0 &
        adv_description$time_yrs2<=18)
]))
jpeg('hist_prop_answeredACE.jpg')
hist(abs((rowSums(is.na(binary_alspacKids_ACE_data[,ACE_var0_18]))/length(ACE_var0_18))-1),
     main='',xlab='Proportion answered ACE questions',ylab='Number of individuals')
dev.off()

sum(!(rowSums(is.na(binary_alspacKids_ACE_data[,ACE_var0_18]))/
        length(ACE_var0_18))>=(1-threshold_proportion_answered_ACE0_18))
binary_alspacKids_ACE_data<-
  binary_alspacKids_ACE_data[
    !(rowSums(is.na(binary_alspacKids_ACE_data[,ACE_var0_18]))/
        length(ACE_var0_18))>=(1-threshold_proportion_answered_ACE0_18),]

#D. Exclude participants based on alive after 1st year (Boyd 2013)
if(sample_Boyd){
  binary_alspacKids_ACE_data<-
    binary_alspacKids_ACE_data[grepl('Yes',binary_alspacKids_ACE_data$kz011b),]
}

#E. Exclude one child per twin pair (but only if both twins survived all the other cutoffs above)
if(exclude_twin){
  binary_alspacKids_ACE_data<-binary_alspacKids_ACE_data[
    !(grepl('Multiple',binary_alspacKids_ACE_data$mz010a)&
        grepl('B',binary_alspacKids_ACE_data$qlet)&
        duplicated(binary_alspacKids_ACE_data$aln)),]
}

catf('\nFinal sample size analyses is:\n',nrow(binary_alspacKids_ACE_data),'\n\n')

#Keep global environment clear by removing no longer necessary objects
rm(exclude_var,exclude_sc,exclude_twin,sample_Boyd,
   ACE_var0_18,threshold_proportion_answered_ACE0_18)

######################
#Code block 4: Derive binary ACE constructs(yes/no)
######################
catf('\n\n',paste0(Sys.time()),
     'Code block 4: Derive binary ACE constructs(yes/no)\n')

#Derive construct :
catf('\nDeriving adversity exposure (yes/no) between',timeperiod[1],'yrs -',timeperiod[2],'yrs')
ACEmeasures<-#ACEs in correct order
  c("ACEscore_classic", "ACEcat_classic",
    "physical_abuse", "sexual_abuse", "emotional_abuse", "emotional_neglect", 
    "bullying","violence_between_parents", "substance_household", 
    "mental_health_problems_or_suicide","parent_convicted_offence",
    "parental_separation", 
    "ACEscore_extended", "ACEcat_extended",
    "social_class", "financial_difficulties", "neighbourhood", 
    "social_support_child", "social_support_parent", 
    "violence_between_child_and_partner","physical_illness_child", 
    "physical_illness_parent","parent_child_bond")
ACEs<-ACEmeasures[c(3:12,15:23)]
classicACEs<-ACEs[1:10]#ACEs in correct order
ACE_time1_time2<-data.frame(
  sapply(ACEs,function(ACE,ACE_var,...){
    cat(ACE,' ')
    vars_ACE<-adv_description$variable_unique[adv_description$variable_unique%in%ACE_var &
                                                adv_description$ACE%in%ACE]
    quest_ACE<-adv_description$variable_unique[
      adv_description$variable_unique%in%ACE_var[ACE_var%in%adv_description$variable] &
        adv_description$ACE%in%ACE]
    if(length(vars_ACE)==0){catf('\n\nNo data available on', ACE,
                                 'Remove',ACE ,'from ACEs vector');x<-rep(NA,nrow(binary_alspacKids_ACE_data))
    } else if(length(vars_ACE)==1){
      x<-binary_alspacKids_ACE_data[,vars_ACE]
    } else {
      x<-rowSums(binary_alspacKids_ACE_data[,vars_ACE],na.rm=T)>0
      miss<-rowSums(is.na(binary_alspacKids_ACE_data[,quest_ACE]))
      x[miss> (length(quest_ACE)/2)]<-NA#need to have completed 50% questions
    } 
    return(x)
  },ACE_var=ACE_var))
#remove ACEs with all NA (missing)
ACE_time1_time2<-ACE_time1_time2[,!colSums(is.na(ACE_time1_time2))==nrow(ACE_time1_time2)]
#add time period indicator to ACE variable name:
colnames(ACE_time1_time2)<-paste0(colnames(ACE_time1_time2),'_',timeperiod[1],'_',timeperiod[2],'yrs')
#Percentage missingness per construct
catf('\nPercentage missing:\n',paste0(names(ACE_time1_time2),'=',
                                      round(colSums(is.na(ACE_time1_time2))/nrow(ACE_time1_time2)*100,2),
                                      '%\n'))
catf('\nPercentage adversity exposed(=yes):\n',
     paste0(names(ACE_time1_time2),'=',
            round(colSums(ACE_time1_time2,na.rm=T)/colSums(!is.na(ACE_time1_time2))*100,2),
            '%\n'))

#calculate ACE scores
catf('\nCalculate extended ACE score for',sum(grepl(paste0(ACEs,collapse='|'),names(ACE_time1_time2))),'ACEs:',
     names(ACE_time1_time2)[grepl(paste0(ACEs,collapse='|'),names(ACE_time1_time2))],'\n')
ACE_time1_time2[[paste0('ACEscore_extended_',timeperiod[1],'_',timeperiod[2],'yrs')]]<-
  rowSums(ACE_time1_time2)
#create binary score:
ACE_time1_time2[[paste0('ACEcat_extended_',timeperiod[1],'_',timeperiod[2],'yrs')]]<-
  cut(ACE_time1_time2[[paste0('ACEscore_extended_',timeperiod[1],'_',timeperiod[2],'yrs')]],
      breaks=c(-1,1,2,5,20),labels=c('0_1low','2lomid','3_5midhi','6+high'))
#classic ACEs
catf('\nCalculate classic ACE score for',sum(grepl(paste0(classicACEs,collapse='|'),names(ACE_time1_time2))),'ACEs:',
     names(ACE_time1_time2)[grepl(paste0(classicACEs,collapse='|'),names(ACE_time1_time2))],'\n')
ACE_time1_time2[[paste0('ACEscore_classic_',timeperiod[1],'_',timeperiod[2],'yrs')]]<-
  rowSums(ACE_time1_time2[,grepl(paste0(classicACEs,collapse='|'),names(ACE_time1_time2))])
#create binary score:
ACE_time1_time2[[paste0('ACEcat_classic_',timeperiod[1],'_',timeperiod[2],'yrs')]]<-
  cut(ACE_time1_time2[[paste0('ACEscore_classic_',timeperiod[1],'_',timeperiod[2],'yrs')]],
      breaks=c(-1,0,1,3,20),labels=c('0','1','2_3','4+'))

#subset to available data
ACEs<-ACEs[ACEs%in%
             unique(adv_description$ACE[adv_description$variable_unique%in%ACE_var])]
classicACEs<-classicACEs[classicACEs%in%
                           unique(adv_description$ACE[adv_description$variable_unique%in%ACE_var])]

#add binary constructs to overall data
alspacKids_ACE_data_2018<-cbind(binary_alspacKids_ACE_data,
                                ACE_time1_time2)
#limit to variables of interest (so drop individual questions),
#20180413: added in the imputation variables (denoted by _org, vars for SES&adversity before birth)
select<-unique(c("aln", "qlet",names(ACE_time1_time2),
                 'kz021','mult','in_core','in_alsp',
                 'in_phase2','in_phase3','tripquad','mz010a','mz014',
                 grep('org',names(binary_alspacKids_ACE_data),value=T),
                 gsub('_org','',grep('org',names(binary_alspacKids_ACE_data),value=T))
))
alspacKids_ACE_data_2018<-
  alspacKids_ACE_data_2018[,select[select%in%names(alspacKids_ACE_data_2018)]]
ncol(alspacKids_ACE_data_2018)#Should include 253 variables

##Drop unused factor levels
alspacKids_ACE_data_2018<-droplevels(alspacKids_ACE_data_2018)

#check aln is numeric, this is necessary to be able to merge ALSPAC stata files
if(!class(alspacKids_ACE_data_2018$aln)=='numeric'){
  catf('\nOriginally aln was not numeric.
       Check the alspacKids_ACE_data_2018 file if it is after conversion')
  alspacKids_ACE_data_2018$aln<-
  as.numeric(as.character(alspacKids_ACE_data_2018$aln))}

#save file in different formats
#Preferred format is the RData file
save(alspacKids_ACE_data_2018,
     file=paste0(loc_out,fileid_out,".RData"))
#also saved as .dta for stata
write.dta(alspacKids_ACE_data_2018,
          file=paste0(loc_out,fileid_out,".dta"))
#and spss
write.foreign(alspacKids_ACE_data_2018, 
              paste0(loc_out,fileid_out,".txt"), 
              paste0(loc_out,fileid_out,".sps"),   
              package="SPSS")

#all data necessary for imputation
save(alspacKids_ACE_data_2018,ACEmeasures,ACEs,classicACEs,
     file=paste0(loc_out,fileid_out,"_imputation.RData"))

################
#End of script Script_2_Calculate_the_ACE_constructs.R
################