##   MAKING THE TABLEAU FILE FROM THE PSNU-IM FACTVIEW
##   Noah Bartlett
##   Date: Decmeber 20, 2017
##   Updated: April 25, 2018


###########
### NOTES
###########

############################################################################################
# Before you run this R code, run the data through the "Consolidate IP Names" code.
############################################################################################

# Packages to install
library(tidyverse)

# This increases the memory in R so it can run big files
memory.limit(size=56000)


# Set-up working directory (Change as needed)
setwd("C:/Users/nbartlett/Documents/ICPI Data/R/ICPIScripts")

# Import data (Change as needed)
# Uses the PSNU_IM file, already run through the Consolidate IP names code
data=read.csv("ICPI_MER_Structured_Dataset_PSNU_IM_20180323_v2_1_FV_Clean.txt",sep="\t",header = T)


#########################################################################################
# For non-q4 data, add a YTD column
# q1 is easy. q2 and q3 will be more complicated (point-in-time vs. cumulatuive data)
#########################################################################################

# data$fy2018apr=data$fy2018q1 # <- For Q1
data$fy2018apr <- ifelse(data$indicator == "TX_CURR", data$fy2018q2, data$fy2018q1 + data$fy2018q2) # <- For Q2

#########################################################################################
# Select a subset of indicators to be included in the Tableau tool
#########################################################################################

data <- data[(data$indicator %in% c("GEND_GBV", "HRH_PRE", "HTS_SELF", "HTS_TST",
                                    "HTS_TST_NEG", "HTS_TST_POS", "KP_MAT", "KP_PREV",
                                    "OVC_SERV", "OVC_SERV_OVER_18", "OVC_SERV_UNDER_18", 
                                    "PMTCT_ART", "PMTCT_EID", "PMTCT_EID_Less_Equal_Two_Months",
                                    "PMTCT_EID_Two_Twelve_Months","PMTCT_EID_POS", "PMTCT_STAT", 
                                    "PMTCT_STAT_KnownatEntry_POSITIVE" , "PMTCT_STAT_NewlyIdentified_Negative", 
                                    "PMTCT_STAT_NewlyIdentified_POSITIVE", "PMTCT_STAT_POS", "PP_PREV",
                                    "PrEP_NEW", "TB_ART", "TB_STAT", "TX_CURR", "TX_NEW", "TX_PVLS",
                                    "TX_RET", "TX_TB", "VMMC_CIRC")),] 

###################################
# Add DREAMS Districts to file
###################################

# List of DREAMS Districts
dreams<-c(
  # Kenya
  "Homa Bay", "Kisumu", "Nairobi County", "Siaya",
  # Lesotho
  "Berea", "Maseru", 
  # Malawi
  "Machinga District", "Zomba District",
  # Mozambique
  "Chokwe", "Cidade Da Beira", "Cidade De Quelimane", "Cidade De Xai-Xai","Xai-Xai",
  # South Africa
  "gp City of Johannesburg Metropolitan Municipality", "gp Ekurhuleni Metropolitan Municipality",
  "kz eThekwini Metropolitan Municipality", "kz uMgungundlovu District Municipality",
  "kz Umkhanyakude District Municipality",
  # Swaziland
  "Hhohho", "Lubombo", "Manzini" , "Shiselweni", 
  # Tanzania
  "Kahama DC", "Kahama TC", "Kyela DC","Mbeya CC","Msalala DC","Shinyanga MC","Temeke MC","Ushetu DC",
  # Uganda
  "Bukomansimbi District", "Gomba District", "Gulu District", "Lira District", "Mityana District",
  "Mubende District", "Mukono District", "Oyam District", "Rakai District", "Sembabule District",
  # Zambia
  "Chingola District", "Lusaka Urban District", "Ndola District", 
  # Zimbabwe
  "Bulawayo", "Chipinge", "Gweru" , "Makoni", "Mazowe", "Mutare")


# Creates a TRUE/FALSE column if PSNU is listed above as a DREAMS district
data$dreams <- data$psnu %in% dreams

#########################
# Add TX_NET_NEW to file
#########################

# Create new dataframe with just TX_CURR
net_new= data %>%
  filter(indicator=="TX_CURR")                    

# Calculate TX_NET_NEW and creates new columns for each Q and/or FY
net_new<-     
  net_new %>% 
  mutate_at(vars(starts_with("fy2")),funs(ifelse(is.na(.),0,.))) %>%   
  mutate(indicator="TX_NET_NEW",
         y2015q2=fy2015q2,
         y2015q3=0,
         y2015q4=fy2015q4-fy2015q2,
         y2015apr=fy2015q4,
         y2016_targets=fy2016_targets-fy2015apr,
         y2016q1=0,
         y2016q2=fy2016q2-fy2015apr,
         y2016q3=0,
         y2016q4=fy2016q4-fy2016q2,
         y2016apr=fy2016q4-fy2015apr,
         y2017_targets=fy2017_targets-fy2016apr,
         y2017q1=fy2017q1-fy2016q4,
         y2017q2=fy2017q2-fy2017q1,
         y2017q3=fy2017q3-fy2017q2,
         y2017q4=fy2017q4-fy2017q3,
         y2017apr=fy2017q4-fy2016apr,
         y2018q1=fy2018q1-fy2017q4,
         y2018apr=fy2018q1-fy2017apr)  # <- ADD NECESSARY VARIABLES EACH QUARTER #


# Delete old columns
net_new=
  net_new %>% 
  select(-starts_with("fy20"))

# Rename new columns with correct names (i.e. fy2016q1)
names(net_new) <- gsub("y2", "fy2", names(net_new))

# Corrects for an error involving vectors in R
data$indicator <- as.character(data$indicator)

# Adds TX_NET_NEW dataframe to FactView dataframe
data.netnew=bind_rows(data,net_new)


################################# 
# Reshape data for use in Tableau
################################# 

# Create three dataframes - Results, Targets and APR - to be combined.

# These are the columns which will be included in Tableau
# This list can be modified as needed.
TableauColumns<-c("operatingunit", "snu1", "snu1uid", "psnu", "psnuuid", "currentsnuprioritization", "dreams",
                  "primepartner", "fundingagency","implementingmechanismname", "mechanismuid",
                  "indicator","numeratordenom", "indicatortype","standardizeddisaggregate", 
                  "age","sex","resultstatus","otherdisaggregate","modality", "ismcad")

# Create results dataframe. Only collects quarterly data starting in FY2015Q3
results<- data.netnew %>%
  select(-fy2015q2, -contains("targets"), -contains("apr")) %>% 
  
  # Columns that will be used in Tableau.
  group_by_at(TableauColumns) %>%
  
  # Creates one values column and one period column (e.g. FY2017Q3)
  summarize_at(vars(starts_with("fy2")), funs(sum(., na.rm=TRUE))) %>%
  ungroup %>%
  gather(period, values, starts_with("fy2")) %>% 
  filter(values !=0)


# Create targets dataframe for FY16 and FY17 targets
targets<- data.netnew %>%
  select(-contains("apr"),-contains("Q")) %>% 
  
  # Columns that will be used in Tableau.
  group_by_at(TableauColumns) %>%
  
  # Creates one values column and one period column (e.g. FY2016_Targets)
  summarize_at(vars(starts_with("fy2")), funs(sum(., na.rm=TRUE))) %>%
  ungroup %>%
  gather(period, values, starts_with("fy2")) %>%
  filter(values !=0)

# Create APR dataframe for FY15, FY16, FY17 APR results
apr<- data.netnew %>%
  select(-contains("targets"),-contains("Q") ) %>% 
  
  # Columns that will be used in Tableau.
  group_by_at(TableauColumns) %>%
  
  # Creates one values column and one period column (e.g. FY2016APR)
  summarize_at(vars(starts_with("fy2")), funs(sum(., na.rm=TRUE))) %>%
  ungroup %>%
  gather(period, values, starts_with("fy2")) %>%
  filter(values !=0)

# Creates a column in each dataframe to label values either Results or Targets
results$ResultsOrTargets<-"Quarterly Results"
targets$ResultsOrTargets<-"Targets"
apr$ResultsOrTargets<-"Annual Results"


# Changes quarters into dates - PART 1 
#     Will change back to quarters in Tableau
#     Why do we have to do this? Because Tableau assumes that Q1 starts in January. 
#     Although you can set Tableau to have fiscal years have an October start, by that 
#     time, Tableau has already assigned the quarterly data to have a January start
#     and your dates will all be off by one quarter. 

results$period<- gsub("fy20", "",results$period)
targets$period<- gsub("fy20", "",targets$period)
apr$period<- gsub("fy20", "",apr$period)
targets$period<- gsub("_targets", "q1",targets$period)
apr$period<- gsub("apr", "q1",apr$period)


# Combines all three dataframes into one
finaldata=bind_rows(results, targets, apr)

# Changes quarters into dates - PART 2
#     YES - I know there are better ways to do this. But this works. And frankly, finding
#     another solution was harder than it should have been. 

finaldata$period[finaldata$period=="15q1"] <- "10/1/2014"
finaldata$period[finaldata$period=="15q2"] <- "1/1/2015"
finaldata$period[finaldata$period=="15q3"] <- "4/1/2015"
finaldata$period[finaldata$period=="15q4"] <- "7/1/2015"
finaldata$period[finaldata$period=="16q1"] <- "10/1/2015"
finaldata$period[finaldata$period=="16q2"] <- "1/1/2016"
finaldata$period[finaldata$period=="16q3"] <- "4/1/2016"
finaldata$period[finaldata$period=="16q4"] <- "7/1/2016"
finaldata$period[finaldata$period=="17q1"] <- "10/1/2016"
finaldata$period[finaldata$period=="17q2"] <- "1/1/2017"
finaldata$period[finaldata$period=="17q3"] <- "4/1/2017"
finaldata$period[finaldata$period=="17q4"] <- "7/1/2017"
finaldata$period[finaldata$period=="18q1"] <- "10/1/2017"
#finaldata$period[finaldata$period=="18q2"] <- "1/1/2018"
#finaldata$period[finaldata$period=="18q3"] <- "4/1/2018"

#####################################
# RENAME THE HIV TESTING MODALITIES 
#####################################

finaldata$modality <- ifelse(finaldata$modality == "OtherMod", "Other Community",
                             ifelse(finaldata$modality == "IndexMod", "Community Index Testing",
                                    ifelse(finaldata$modality == "VCTMod", "Community VCT",
                                           ifelse(finaldata$modality == "VMMC", "VMMC",
                                                  ifelse(finaldata$modality == "VCT", "VCT",
                                                         ifelse(finaldata$modality == "OtherPITC", "Other PITC",
                                                                ifelse(finaldata$modality == "Index", "Facility Index Testing",
                                                                       ifelse(finaldata$modality == "MobileMod", "Community Mobile Testing",
                                                                              ifelse(finaldata$modality == "PMTCT ANC", "PMTCT ANC",
                                                                                     ifelse(finaldata$modality == "Pediatric", "Pediatrics",
                                                                                            ifelse(finaldata$modality == "TBClinic", "TB Clinics",
                                                                                                   ifelse(finaldata$modality == "Malnutrition", "Malnutrition Facilities",
                                                                                                          ifelse(finaldata$modality == "STI Clinic", "STI Clinics",
                                                                                                                 ifelse(finaldata$modality == "Emergency Ward", "Emergency Wards",
                                                                                                                        ifelse(finaldata$modality == "Inpat", "Inpatient",
                                                                                                                               ifelse(finaldata$modality == "HomeMod", "Community Home-Based",
                                                                                                                                      ifelse(finaldata$modality == "KeyPop", "Key Populations",
                                                                                                                                             "")))))))))))))))))



################################################
# RENAME THE COLUMN HEADINGS FROM ALL LOWERCASE
################################################

names(finaldata)[names(finaldata) == 'operatingunit'] <- 'Operating Unit'
names(finaldata)[names(finaldata) == 'snu1'] <- 'SNU'
names(finaldata)[names(finaldata) == 'psnu'] <- 'PSNU'
names(finaldata)[names(finaldata) == 'currentsnuprioritization'] <- 'Current SNU Prioritization'
names(finaldata)[names(finaldata) == 'dreams'] <- 'DREAMS'
names(finaldata)[names(finaldata) == 'primepartner'] <- 'Prime Partner'
names(finaldata)[names(finaldata) == 'fundingagency'] <- 'Funding Agency'
names(finaldata)[names(finaldata) == 'implementingmechanismname'] <- 'Implementing Mechanism Name'
names(finaldata)[names(finaldata) == 'numeratordenom'] <- 'Numerator Denom'
names(finaldata)[names(finaldata) == 'standardizeddisaggregate'] <- 'Standardized Disaggregate'
names(finaldata)[names(finaldata) == 'resultstatus'] <- 'Result Status'
names(finaldata)[names(finaldata) == 'otherdisaggregate'] <- 'Other Disaggregate'


#finaldata$values<-format(finaldata$values, digits=1)
finaldata = mutate_if(finaldata, is.numeric, as.integer)

write_tsv(finaldata, "FY18Q1.PSNU.IM.2018.04.25.txt")

