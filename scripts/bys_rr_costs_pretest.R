#*###*###*###*###*###*###*###*###*###*###*##*#
#*##                                      #*##
####    Bystander: Costs - Wrangling      ####
#*##                                      #*##
#*###*###*###*###*###*###*###*###*###*###*##*#

#*###*###*###*###*###*###*###*###*###*###*##*#
#*##                                      #*##
####    Used packages                     ####
#*##                                      #*##
#*###*###*###*###*###*###*###*###*###*###*##*#


if(!require(formattable)){install.packages('formattable')}
if(!require(janitor)){install.packages('janitor')}
if(!require(tidyverse)){install.packages('tidyverse')}


#*###*###*###*###*###*###*###*###*###*###*##*#
#*##                                      #*##
####    Importing the data from Git       ####
#*##                                      #*##
#*###*###*###*###*###*###*###*###*###*###*##*#

ds_raw <- read.csv("https://raw.githubusercontent.com/dfusocialpsy/bys_costs_rr/main/data/bys_pics_pretest_raw.csv", header = TRUE)

### Loading the flile manually ###
# ds_raw <- read.csv(file.choose())


#*###*###*###*###*###*###*###*###*###*###*##*#
#*##                                      #*##
####    Raw data to working data set      ####
#*##                                      #*##
#*###*###*###*###*###*###*###*###*###*###*##*#

ds <- ds_raw
ds_1 <- ds 


#*###*###*###*###*###*###*###*###*###*###*##*#
#*##                                      #*##
####        Preparing the dataset         ####
#*##                                      #*##
#*###*###*###*###*###*###*###*###*###*###*##*#

library(tidyverse)

glimpse(ds)
str(ds)
class(ds$zb01x01)

#*##*###*###*###*###*###*###*###*###*###*###
#*#                                      #*#
###    Columnnames, Remove, & add id     ###
#*#                                      #*#
#*##*###*###*###*###*###*###*###*###*###*###

## Add id column and move it to the first column ##

# Add id
ds$id <- 1:(nrow(ds))

# Move id to first column
ds %>% relocate(id) -> ds


## Create clean columnnames ##
names(ds)
ds <-  janitor::clean_names(ds)
names(ds)

## Remove unnecessary columns ##
glimpse(ds)

match("started", names(ds))
match("zb01_cp", names(ds))
match("time001", names(ds))
match("finished", names(ds))

ds[, c(2:6, 8, 560:570)] <- NULL

glimpse(ds)


## Remove unnecessary suffixes from columnaes ##
names(ds) <- gsub("[_]01", "", names(ds)) 
names(ds) <- gsub("01x", "", names(ds)) 

## Create tidy dataset ##

names(ds)

# Combine "zbs" to one "zb" column, 
# keep: id, age, gender, & attention check
# add stim_seq = stimulus sequence variable 

ds %>% 
  select(id, ic02, df02, df01, 
         starts_with("zb")) %>% 
  rename(gen = df02,
         age = df01,
         ac = ic02) %>% 
  pivot_longer(cols = -c(id, gen, age, ac),
               values_to = "stimulus") %>% 
  group_by(id) %>% # this is necessary for creating stim_seq
  mutate(stim_seq = 1:n()) %>% # creates stim_seq (1:10) for every particpant
  ungroup() %>%  
  arrange(stimulus, id) %>%   # otherwise, values of "zb" would not fit participants answers
  select(-name) -> a # value (here: stimulus because tidy organizes "er" ascendending,
  
# Prepare columns in long format for each DV #

# Erkennbarkeit
ds %>% 
  select(id, starts_with("er")) %>% 
  pivot_longer(-id,
               values_drop_na = TRUE) %>% 
  arrange(name, id) %>% # arrange by name (i.e., original columnn names )  
  select(value) %>% # leads to that value (situmlus) matches variable
  rename(er = value) -> b

# Gefährlichkeit Bystander
ds %>% 
  select(id, starts_with("gb")) %>% 
  pivot_longer(-id,
               values_drop_na = TRUE) %>% 
  arrange(name, id) %>% # arrange by name (i.e., original columnn names )  
  select(value) %>% # leads to that value (situmlus) matches variable
  rename(gb = value) -> c


# Gefährlichkeit Opfer
ds %>% 
  select(id, starts_with("go")) %>% 
  pivot_longer(-id,
               values_drop_na = TRUE) %>% 
  arrange(name, id) %>% # arrange by name (i.e., original columnn names )  
  select(value) %>% # leads to that value (situmlus) matches variable
  rename(go = value) -> d

# Pluralistische Ignorrance
ds %>% 
  select(id, starts_with("pi")) %>% 
  pivot_longer(-id,
               values_drop_na = TRUE) %>% 
  arrange(name, id) %>% # arrange by name (i.e., original columnn names )  
  select(value) %>% # leads to that value (situmlus) matches variable
  rename(pi = value) -> e

# Verantwortungsdiffuion 
ds %>% 
  select(id, starts_with("di")) %>% 
  pivot_longer(-id,
               values_drop_na = TRUE) %>% 
  arrange(name, id) %>% # arrange by name (i.e., original columnn names )  
  select(value) %>% # leads to that value (situmlus) matches variable
  rename(di = value) -> f

# Bewertungsangst
ds %>% 
  select(id, starts_with("ba")) %>% 
  pivot_longer(-id,
               values_drop_na = TRUE) %>% 
  arrange(name, id) %>% # arrange by name (i.e., original columnn names )  
  select(value) %>% # leads to that value (situmlus) matches variable
  rename(ba = value) -> g

# Qualitative description
ds %>% 
  select(id, starts_with("be")) %>% 
  pivot_longer(-id,
               values_drop_na = TRUE) %>% 
  arrange(name, id) %>% # arrange by name (i.e., original columnn names )  
  select(value) %>% # leads to that value (situmlus) matches variable
  rename(be = value) -> h

# Bind single objects (a:h) to one dataframe 
ds_ready <- cbind(a, b, c, d, e, f, g)


#*###*###*###*###*###*###*###*###*###*###*##*##*#
#*##                                         #*##
####  Compute descripitve values for images  ####
#*##                                         #*##
#*###*###*###*###*###*###*###*###*###*###*##*##*#

ds_ready %>% 
  arrange(id, stim_seq) %>% 
  group_by(stimulus) %>% 
  summarise(across(er:ba, 
                   .fns = list(mean = mean, sd = sd), na.rm = TRUE, 
                   .names = "{col}_{fn}")) %>% 
  ungroup() %>% 
  select(stimulus, starts_with(c("er", "gb", "go"))) -> ds_means_sds
  
## Arrange images by "er" (clarity) - all stimuli ##
ds_means_sds %>% 
  arrange(-er_mean) %>% 
  round(digits = 2) %>% 
  rename(Stimulus = stimulus,
         Recognizability_mean = er_mean,
         Recognizability_sd = er_sd,
         Severity_for_bystander_mean = gb_mean,
         Severity_for_bystander_sd = gb_sd,
         Severity_for_victim_mean = go_mean,
         Severity_for_victim_sd = go_sd) %>% 
  formattable::formattable() 


## Arrange images by "er" (clarity) - all stimuli ##
ds_means_sds %>% 
  filter(er_mean > 5.5) %>% 
  arrange(-er_mean) %>%  
  round(digits = 2) %>% 
  rename(Stimulus = stimulus,
         Recognizability_mean = er_mean,
         Recognizability_sd = er_sd,
         Severity_for_bystander_mean = gb_mean,
         Severity_for_bystander_sd = gb_sd,
         Severity_for_victim_mean = go_mean,
         Severity_for_victim_sd = go_sd) %>% 
  formattable::formattable() 




