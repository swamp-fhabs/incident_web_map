
## Load Libraries
library(tidyverse)
library(lubridate)


## Download Bloom report CSV
blooms <- read_csv("fhab_bloomreport.csv") %>% 
  mutate(TypeofSign= tolower(TypeofSign)) 

# Calculate number of days ago bloom was last observed
days_ago <- as.duration(blooms$ObservationDate %--% Sys.Date()) %>% 
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
  mutate(TypeofSign_new= ifelse(str_detect(TypeofSign_new, "usace"), "General notification", TypeofSign_new)) %>%  # USACE
  mutate(TypeofSign_new= ifelse(str_detect(TypeofSign_new, "current|progress|unknown|notifying"), "See incident report", TypeofSign_new)) %>% # MISCELLANEOUS
  mutate(TypeofSign_new= ifelse(is.na(TypeofSign_new), "See incident report", TypeofSign_new)) # No data
return(df_new)
}

blooms_newLabels <- revise_advisory_labels(blooms)


table(blooms_newLabels$TypeofSign_new, exclude= NULL)
table(blooms_newLabels$TypeofSign, exclude= NULL)


## Apply time cutoffs to revise the advisories displayed on the map
## >14 days and <90 days with no updated bloom observation, status changes to "Suspected bloom"
## >90 days with no updated bloom observation, status changes to "None"
blooms_newLabels_timeCutoff <- blooms_newLabels %>% 
  mutate(TypeofSign_new= ifelse(days_ago > 14 & days_ago < 90, "Suspected bloom", TypeofSign_new)) %>% 
  mutate(TypeofSign_new= ifelse(days_ago > 90, "None", TypeofSign_new))


table(blooms_newLabels_timeCutoff$TypeofSign_new, exclude= NA)



