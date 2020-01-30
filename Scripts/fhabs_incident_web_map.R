## Script to create data for FHABs Incidents web map on the CA HABs Portal


## The script standardizes the language in the "current advisory" field so that
## current advisories can be displayed as colors on the map
## The script also changes the advisory and size of the point based on the time since last update


message("Running Rscript to make Tableau CSV")

## Load Libraries
suppressMessages(library(tidyverse))
suppressMessages(library(lubridate))

## Data is stored on the S: drive
setwd("S:/OIMA/SHARED/Freshwater HABs Program/FHABs Database/Python_Output")

## Download Bloom report CSV
blooms <- suppressMessages(read_csv("FHAB_BloomReport.csv")) %>% 
  mutate(TypeofSign= tolower(TypeofSign)) 

# Calculate number of days ago bloom was last observed
days_ago <- as.duration(blooms$BloomLastVerifiedOn %--% Sys.Date()) %>% 
  as.numeric(., "days")

## Function to clean up the bloom advisory language and labels
revise_advisory_labels <- function(df, column){
df_new <- df %>% 
  mutate(TypeofSign_new= TypeofSign) %>% 
  mutate(TypeofSign_new= ifelse(str_detect(TypeofSign_new, "(cau|cua|advis)"), "Caution", TypeofSign_new)) %>% # CAUTION
  mutate(TypeofSign_new= ifelse(str_detect(TypeofSign_new, "warn"), "Warning", TypeofSign_new)) %>% # WARNING
  mutate(TypeofSign_new= ifelse(str_detect(TypeofSign_new, "dan"), "Danger", TypeofSign_new)) %>% # DANGER
  mutate(TypeofSign_new= ifelse(str_detect(TypeofSign_new, "(non|no |n\\/a)"), "None", TypeofSign_new)) %>%  # NONE
  mutate(TypeofSign_new= ifelse(str_detect(TypeofSign_new, "(close)"), "Danger", TypeofSign_new)) %>%  # DANGER
  mutate(TypeofSign_new= ifelse(str_detect(TypeofSign_new, "usace"), "Awareness sign", TypeofSign_new)) %>%  # USACE
  mutate(TypeofSign_new= ifelse(str_detect(TypeofSign_new, "invest"), "Under investigation", TypeofSign_new)) %>%  # Under investigation requestion from RB1
  mutate(TypeofSign_new= ifelse(str_detect(TypeofSign_new, "current|progress|unknown|notifying"), "See incident report", TypeofSign_new)) %>% # MISCELLANEOUS
  mutate(TypeofSign_new= ifelse(is.na(TypeofSign_new), "See incident report", TypeofSign_new)) # No data
return(df_new)
}

blooms_newLabels <- revise_advisory_labels(blooms)

#table(blooms_newLabels$TypeofSign_new, exclude= NULL)
#table(blooms_newLabels$TypeofSign, exclude= NULL)


## Apply time cutoffs to revise the advisories displayed on the map
## >14 days and <90 days with no updated bloom observation, status changes to "Suspected bloom"
## >90 days with no updated bloom observation, status changes to "None"

blooms_newLabels_timeCutoff <- blooms_newLabels %>% 
  mutate(TypeofSign_new= ifelse(days_ago > 21 & days_ago <= 90 & TypeofSign_new != "None", "Suspected bloom", TypeofSign_new)) %>% 
  mutate(TypeofSign_new= ifelse(days_ago > 90, "Historical", TypeofSign_new)) %>% 
  mutate(days_ago_label= "days_ago",
         days_ago_label= ifelse(days_ago <= 7, "<7 days", days_ago_label)) %>%
  mutate(days_ago_label= ifelse(days_ago > 7 & days_ago <= 30, "8-30 days", days_ago_label)) %>%
  mutate(days_ago_label= ifelse(days_ago > 7 & days_ago <= 30, "8-30 days", days_ago_label)) %>%
  mutate(days_ago_label= ifelse(days_ago > 30 & days_ago <= 90, "31-90 days", days_ago_label)) %>%
  mutate(days_ago_label= ifelse(days_ago > 90, ">90 days", days_ago_label))


#table(blooms_newLabels_timeCutoff$TypeofSign_new, exclude= NA)

message("Writing file: FHAB_BloomReport_Tableau.csv")
write_csv(blooms_newLabels_timeCutoff, "FHAB_BloomReport_Tableau.csv")

