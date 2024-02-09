
library(foreign)
library(summarytools)
library(dplyr)
library(tidyr)
library(stringr)
library(VIM)
library(ggplot2)
library(readxl)
library(lubridate)

# 
rank_order = c("PSO", "CONST", "SCONST", "SGT", "SSGT")

cat_levels= c("CAUCASIAN","ASIAN", "LATIN AMERICAN", "MEDITERRANEAN" ,
              "INDIAN SUBCONTINENTAL", "PACIFIC IS", "AFRICAN/MIDDLE-EASTERN",
              "ABORIGINAL/T.S. ISLANDER", "OTHER")

vicpol_readin <- read.csv( "../data/vicpol_readin.csv") %>% 
  
# remove search warrant  (<1%)  
  filter(SEARCH_WARRANT == FALSE, 
         DPCSA_S_81_SRCH_WITH_WARRANT == FALSE,
         Person.Reports == "Person data exists") %>% 

# clean rank and racial appearance order 
  mutate(Contacting.Member.Rank = factor(Contacting.Member.Rank, levels = c(rank_order))) %>% # clean rank
  mutate(RacialCat = factor(RacialCat, levels = cat_levels ))  %>% # clean RacialCat

# make everything but these factors
  mutate_at(vars(-Contact.Date, - Contact.Time, -Age.of.Contact), as.factor) %>% 
  
# clean date
  mutate(Contact.Date = ymd(Contact.Date),
         Year = as.factor(year(Contact.Date)))
  


# Choosing variable for importation of racial appearance

# 1 - start with variables in the model
imp_vars = c("Found", "Age.of.Contact", "Sex", "RacialCat", "Search.Reason")  # add "Search_Reason"

# 2 - don't want variables with too many missing (>50%)

missing_gp <- vicpol_readin %>% 
  summarise_all(~ mean(is.na(.)))

missing_gp[which(missing_gp>.50)]

# Do not use these
# Complexion.of.contact Hair.colour.of.contact Hair.style.1.of.contact Hair.style.2.of.contact 
#0.8943456              0.8914158             0.8949315                0.8929539          

# But use this anyway
# Indigeneous.Status, 
# 0.6986743

imp_vars <- c(imp_vars, "Indigeneous.Status")
imp_vars

#3. Don't want variables with almost all in one category (more than 90%)
fact_largest_gp <- vicpol_readin %>% 
  select_if(is.factor) %>% 
  summarise_all(~max(prop.table(table(., useNA = "always"))))

colnames(fact_largest_gp)[which(fact_largest_gp<0.90)]

# [1] "Contact.ID"               "Reporting.Station"        "Contacting.Member.Rank"  
# [4] "Racial.Appearance"        "Sex"                      "Complexion.of.contact"   
# [7] "Hair.colour.of.contact"   "Hair.style.1.of.contact"  "Hair.style.2.of.contact" 
# [10] "Indigeneous.Status"       "X001.DP.CS.S.82"          "X003.VEHICLE"            
# [13] "X003.PACKAGE"             "X003.THING"               "X003.NO.OBJECTS.SEARCHED"
# [16] "VEHICLE_CHECK"            "PERSON_CHECK"             "Search.Reason"           
# [19] "PACKAGE_or_THING"         "RacialCat"                "Racialiased"             
# [22] "Operation_Type"           "Found"                    "Year" 


# we will not use
# - ContactID (one row per ContactID)
# - Racial.Appearance (since we are using RacialCat)
# -"Complexion.of.contact", "Hair.colour.of.contact",
#   "Hair.style.1.of.contact", "Hair.style.2.of.contact" (too many missing as above)
# - # "SRCH.W.O.WARRANT.SEARCH.TYPES" #"X021.OTHER", "OTHER.ARTICLES" "X003.NO.OBJECTS.SEARCHED"
#    these describe what search methods was used as the dominant category is "other", 
#    and so contain no useful information for imputation (essentially too many missing)
# - # "OTHER.ARTICLES"  because this is already in Found response
# - "X001.DP.CS.S.82" this is in search reason
# "X003.PACKAGE"             "X003.THING"  not including because combined in PaCKAGE_OR_THING
# - "Racialiased", same infor as Racial Cat


# we will use
# - "Sex" - already there
# - "Indigeneous.Status" - already there
# - "X003.VEHICLE"
# - "VEHICLE_CHECK", "PERSON_CHECK",  - (these are from Contact.Type.1 to 6)
# - "Year" 
# - Operation_Type
imp_vars <- c(imp_vars, "X003.VEHICLE", "VEHICLE_CHECK", "PERSON_CHECK",
              "Year", "Operation_Type",
              'Reporting.Station', "PACKAGE_or_THING", "Contacting.Member.Rank")
imp_vars



# non-factors
vicpol_readin %>% 
  select_if(Negate(is.factor)) %>% 
  head()

# Age.of.Contact, already there

# create dataset of just variables for imputation
for_impute <- vicpol_readin %>% 
  select(any_of(imp_vars))

print(dfSummary(for_impute), file = "results/datachecks/for_impute.html")

### RacialCat UNKNOWN still there, check

model.matrix(~ . -1 - Reporting.Station, data = for_impute) %>% 
  cor() %>% 
  round(digits = 2) %>% 
  write.csv(file = "results/datachecks/for_impute.html_cov.csv")

# all seems okay


VIM::aggr(for_impute, numbers=FALSE, sortVars=TRUE,cex.axis = .7,
          combined = TRUE, labels=substr(names(for_impute),1,10))

# barMiss(vicpol_readin[,c("Contacting.Member.Rank","Racial.Appearance")])
# histMiss(vicpol_readin[,c("Age.of.Contact","Racial.Appearance")])
# barMiss(vicpol_readin[,c("Sex","Racial.Appearance")])

save(for_impute, file = "for_impute.Rdata")

