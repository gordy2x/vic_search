
library(labelled)
library(tidyverse) # for data manipulation
library(lubridate) # to manage dates
library(mitml)
library(jomo)
library(tictoc)
library(VIM)
library(mice)
library(glmmTMB)
library(broom.mixed)
library(emmeans)
library(naniar)


load(file = "for_impute.Rdata")

VIM::aggr(for_impute, numbers=FALSE, sortVars=TRUE,cex.axis = .7,
          combined = TRUE, labels=substr(names(for_impute),1,10))

fml <-  Indigeneous.Status +  RacialCat + Search.Reason +
  Age.of.Contact + Sex   ~  
  X003.VEHICLE + VEHICLE_CHECK + PERSON_CHECK+ Year + Operation_Type +
  PACKAGE_or_THING + Contacting.Member.Rank + (1|Reporting.Station)


for_little <- for_impute %>% 
  select(Indigeneous.Status,RacialCat,Search.Reason,Age.of.Contact,Sex,
         X003.VEHICLE,VEHICLE_CHECK,PERSON_CHECK,Year,Operation_Type,
         PACKAGE_or_THING,Contacting.Member.Rank)

mcar_test(for_little)

# start <- Sys.time()
# imp <- jomoImpute(for_impute , formula=fml,
#                   n.burn = 10, n.iter = 100, m = 2)
# time <- Sys.time() - start
# time
# Time difference of 34.03009 mins

# 
# start2 <- Sys.time()
# imp <- jomoImpute(for_impute , formula=fml,
#                   n.burn = 10, n.iter = 100, m = 3)
# time2 <- Sys.time() - start2
# time2
# Time difference of 47.99054 mins

# if one burn in these should be similar
# time / (10+100*2)
# Time difference of 0.1620481 mins
# time2 / (10+100*3)
# Time difference of 0.1548082 mins

# # they are so
# an_iter = 0.1620481
# 
# # best guess is nineteen hours
# an_iter*(5000 + 20*100)/60 


# 
# imp <- jomoImpute(for_impute , formula=fml,
#                   n.burn = 2, n.iter = 2, m = 20)



start3 <- Sys.time()
imp <- jomoImpute(for_impute , formula=fml,
                  n.burn = 5000, n.iter = 100, m = 20)
time3 <- Sys.time() - start3
time3

save(imp, file = "data/imputed_20")

# convergence checks
summary(imp)
plot(imp, pos = "beta")
# probably okay



#### datasets

imputed_datasets <- mitmlComplete(imp, print = "all")

## create imputed datset as mids object for later emmeans
mids_data <- imputed_datasets %>% 
  bind_rows(.id = "imp") %>% 
  mutate(imp = as.numeric(imp)) %>% 
  bind_rows(complete.cases) %>% 
  as.mids(.imp = "imp")

save(mids_data, file = "data/imputed_20_mids")


