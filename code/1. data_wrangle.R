
library(foreign)
library(summarytools)
library(dplyr)
library(tidyr)
library(stringr)
library(VIM)
library(ggplot2)
library(readxl)
# library(lubridate)

# helper function
repl_na <- function(d){
  if(is.character(d)) d = replace_na(d,"No")
  if(is.numeric(d)) d = replace_na(d,0)
  d
}

# 
stations <- read_excel("../data/Reporting Station.xlsx") %>% 
  select(Reporting_Station, Operation_Type)

Racial_Tab <- read_excel("../data/rac_cat.xlsx", trim_ws = TRUE) %>% 
  transmute(RacialApp = RacialAppearance,
            RacialCat = RacAppNew,
            Racialiased = `RacApp-Binary`)


## Read in data and wrangle without filtering
vicpol_readin<- read_excel("../data/2018-2019 VicPol Search Data in Excel.xlsx", trim_ws = TRUE, 
                           guess_max  = 27466) %>% 
  dplyr::rename_all(funs(make.names(.))) %>% # clean names
  select_if(colSums(!is.na(.)) > 0) %>% # remove empty columns
  
# create variables for each Contact Type (this takes several minutes)
  rowwise() %>%
  mutate(VEHICLE_CHECK = any(c_across(Contact.Type.1:Contact.Type.6) %in% c("VEHICLE CHECK")),
         PERSON_CHECK = any(c_across(Contact.Type.1:Contact.Type.6) %in% c("PERSON CHECK")),
         SEARCH_WITHOUT_WARRANT = any(c_across(Contact.Type.1:Contact.Type.6) %in% c("SEARCH WITHOUT WARRANT")),
         SEARCH_WARRANT = any(c_across(Contact.Type.1:Contact.Type.6) %in% c("SEARCH WARRANT")),
         VEHICLE_IMPOUNDMENT = any(c_across(Contact.Type.1:Contact.Type.6) %in% c("VEHICLE IMPOUNDMENT")),
         DPCSA_S_81_SRCH_WITH_WARRANT = any(c_across(Contact.Type.1:Contact.Type.6) %in% c("DPCSA-S.81-SRCH WITH WARRANT")),
         CCO_CONTRAVENTION = any(c_across(Contact.Type.1:Contact.Type.6) %in% c("CCO CONTRAVENTION")),
         DEMERIT_POINT_NOTIFICATION = any(c_across(Contact.Type.1:Contact.Type.6) %in% c("DEMERIT POINT NOTIFICATION" )),
         MOVE_ON_DIRECTION_ISSUED = any(c_across(Contact.Type.1:Contact.Type.6) %in% c("MOVE ON DIRECTION ISSUED")),
         SEIZED_PLATES = any(c_across(Contact.Type.1:Contact.Type.6) %in% c("SEIZED PLATES")),
         ADULT_PAROLE_BREACH = any(c_across(Contact.Type.1:Contact.Type.6) %in% c("ADULT PAROLE BREACH")),
         YOUTH_PAROLE = any(c_across(Contact.Type.1:Contact.Type.6) %in% c("YOUTH PAROLEE")),
         DRUG_DRINK_DRIVER_BAN = any(c_across(Contact.Type.1:Contact.Type.6) %in% c("DRUG/DRINK DRIVER BAN")),
         WEAPONS_SRCH_WO_W = any(c_across(Contact.Type.1:Contact.Type.6) %in% c("WEAPONS-SRCH WO/W"))) %>%
  ungroup() %>%
  select(-(Contact.Type.1:Contact.Type.6)) %>%

# Clean indigenous status and Sex
  mutate(Indigeneous.Status = case_when(Indigeneous.Status %in% c("ABORIGINAL","BOTH TSI & ABORIGINAL","TORRES STRAIT ISLANDER (TSI)") ~ "ABORIGINAL and/or TSI",
                                        Indigeneous.Status %in% c("NO INFO PROVIDED", "NOT STATED / UNKNOWN") ~ NA_character_,
                                        TRUE ~ Indigeneous.Status),
         Sex = ifelse(Sex == "U", NA, Sex),
         Contact.ID = as.factor(Contact.ID),
         Contacting.Member.Rank = case_when(Contacting.Member.Rank == "RECRUT" ~ "CONST",
                                            TRUE ~ Contacting.Member.Rank),
         Contacting.Member.Rank = as.factor(Contacting.Member.Rank), 
         Contacting.Member.Rank = droplevels(Contacting.Member.Rank),
         Search.Reason =  case_when(X001.CONTROL.OF.WEAPONS.ACT == 1 |
                                      X001.FIREARMS.ACT == 1 |
                                      X001.FPO.NO.INTENT.TO.CHARGE == 1|
                                      X001.FPO.INTENT.TO.CHARGE == 1   ~ "Weapons",
                                    X001.DP.CS.S.82 == 1 |
                                      X001.VOLATILE.SUB.U.18.60E == 1 |
                                      X001.VOLATILE.SUB.18..60F == 1 ~ "Drugs",
                                    X001.GRAFFITI.PREVENTION.ACT == 1 ~ "Graffiti",
                                    TRUE ~ NA_character_),
         PACKAGE_or_THING = case_when(X003.PACKAGE == 1 |X003.THING == 1 ~ 1, 
                                      TRUE ~ 0)) %>% 
  
  mutate(Age.of.Contact = ifelse(Age.of.Contact > 80, 80, Age.of.Contact)) %>% 

  # clean variable types and values
  mutate_at(vars(SEARCH.W.O.WARRANT.TYPES:X024.SOME), ~repl_na(.)) %>% # replace NA with 0 or "No" (see top of code)
  mutate_at(vars(- Contact.Date, - Contact.Time, - Age.of.Contact), as.factor)  %>% #Make all except factors
  
# create categories for racial appearance and reporting station
  left_join(Racial_Tab, by = c( "Racial.Appearance" = "RacialApp")) %>% # categorise racial appearance 
  left_join(stations, by = c(  "Reporting.Station" = "Reporting_Station")) %>% #categorise reporting station 

# calculate found variable 
  mutate(Found = if_any(c(PROHIBITED.WEAPONS, DANGEROUS.ARTICLES, 
                          FIREARMS,CONTROLLED.WEAPONS,
                          OTHER.ARTICLES,GRAFFITI.IMPLEMENTS, 
                          VOLATILE.SUBSTANCES.TYPES), ~ . == "Yes"))


print(dfSummary(vicpol_readin), file = "results/datachecks/vicpol_readin_summary.html")

write.csv(vicpol_readin, file  = "../data/vicpol_readin.csv", row.names = FALSE)

