# Current year bycatch caluclations - ONLY for current year
# work with Ben Daly on this request.
# Work to automate bycatch expansion currently performed for BSAI crab fisheries

# Currently set up for smbkc but should be transferable to other stocks

# K.Palof    ADF&G 
# katie.palof@alaska.gov
# 2019-6-10


# load ---
source('./code/packages.R')

cur_yr <- 2018   #needs to be changed if year changes
stock <- 'smbkc' #change with each stock of interest - stock of interest is the stock that is being caught or bycatch occured

# data -----
# Data is from the Kodiak wikki - http://kodweb.fishgame.state.ak.us/
# The data is accessed via: Data Access – Shellfish – Biological Data – Crab Observer – 
#                                    Reports & Info - 
#    
#  Fish ticket data is from Ben Daly - need to ask him where this is stored.

#  This is for one species - 
#  Look at all open fishery in that year  - here current year 2018. Should only show up in 
#                                             snow, tanner, and directed fisheries/

# pot summary ---------
# Species Composition Reports - Sample pot summary - Fishery: QO16/ - Species: SM blue king crab
#         Fishery will change - look at all those in current year but species will always be the same - species of interest
# example: 
files_pots <- dir(paste0('data/', cur_yr, '/', stock,'/potsum'), pattern = '*.csv') # creates a list with file names
sampled_pots <- files_pots %>% 
                  map(function(x) read_csv(file.path(paste0('data/', cur_yr, '/', stock,'/potsum'), x))) %>% 
                  reduce(rbind) # takes those file name from above and reads all of them into 1 data frame
head(sampled_pots) #check to make sure things were read in ok.

# Data dump --------
# Data dumps - Crab Detail Data - Fishery: QO16 - Species: snow crab - Sex: all 
files_cdata <- dir(paste0('data/', cur_yr, '/', stock,'/datadump'), pattern = '*.csv')
crab_data <- files_cdata %>% 
  map(function(x) read_csv(file.path(paste0('data/', cur_yr, '/', stock,'/datadump'), x))) %>% 
  reduce(rbind) # see comments above same code just different folder/files

# Data on this relationship from NMFS tech memo July 2016 - Bob Foy
weight_length <- read.csv('data/weight_length.csv') # using these values and size results in average weight in grams.   
# this file is created from Bob's data. 
head(weight_length) # added fishery_code column (matches the 2 letter codes out of the wiki)
# head () or tail() gives you a glimpse of the beginning or end of the data frame

# landed pounds in directed fishery SMBKC QP - where to update this from ???? **fix**
landed_lb <- read_excel("data/SMBKC/FT summary SMBKC Multiple Seasons By Stat Area With CDQ-.xlsx", 
                       sheet = 'landed')
## Fishery directed effort - Ben D. summarizes this from WBT Directed-Incidental Calculations_17-18.xlsx
# where to update this from ???? **fix**
fishery_effort <- read_excel("data/SMBKC/SMBKC_bycatch_ests.xlsx", 
                       sheet = 'Sheet1')

# summary stats  ----------
# number of pots sampled ------
sampled_pots %>% 
  unite(Trip_spn, Trip, Spn, sep ="-") -> pots2 # combines trip and spn to give unique ID for each row

pots2 %>% 
  group_by(Fishery) %>% 
  summarise(no_pots = length(unique(`Trip_spn`))) -> pots # summarizes the number of pots observed in each fishery

# count in pots ------
# count for females, sublegal, legalret and legalNR # from potSummary
sampled_pots %>% 
  gather("component", "n", Female:LegalNR) -> sampled_pots2 
#### groups data differently for easier summarization

sampled_pots2 %>% 
  group_by(Fishery, component) %>% 
  summarise(number = sum(n)) -> numbers # summarises the numbers observed for each fishery and "component"
                                        #   components are females, sublegal

numbers %>% 
  left_join(pots) -> samp_pots

# calculate CPUE from sampled pots -----------
samp_pots %>% 
  mutate(cpue = number/no_pots) -> samp_pots
# pull year out from fishery designation
library(stringr)
numextract <- function(string){ 
  str_sub(string, 3, 4)
} 
chrextract <- function(string){ 
  str_sub(string, 1, 2)
}
samp_pots %>% 
  mutate(year = as.numeric(numextract(Fishery)) + 2000, 
         fishery = chrextract(Fishery), 
         species = ifelse(fishery == "QO", "snow", 
                    ifelse(fishery == "QT", "TannerW", 
                     ifelse(fishery == "QP", "SMBKC")))) -> samp_pots

# total effort from fishery ---
# stored in excel and with calcs there so needs to be edited for each area for the rows included
head(fishery_effort)
fishery_effort %>% 
  select(year = Year, species = Fishery, Fishery_directed_effort) %>% 
  right_join(samp_pots) -> summary1
# add effort to sampled pots summary 


# catch number -------
# extrapolated from cpue and total fishery effort 
summary1 %>% 
  mutate(catch_no = cpue*Fishery_directed_effort) -> summary1

# size comp, avg size and weight ---------------------------
# use crab_data here    - sampling at sea NOT dockside
crab_data %>% 
  group_by(fishery, size, legal, sex, shell) %>% 
  summarise(n = n()) %>% 
  mutate(component = ifelse(sex ==1 & legal ==0, "Sublegal", 
                            ifelse(sex ==1 & legal ==1, "LegalRet", 
                                   ifelse(sex ==1 & legal ==2, "LegalNR", 
                                          ifelse(sex ==2, "Female", " "))))) %>% 
  as.data.frame-> by_size

# total crab of each category sampled not just those that have recorded shell and size 
component_list <- c("Female", "Sublegal", "LegalRet", "LegalNR")
by_size %>% 
  group_by(fishery, component) %>% 
  summarise(n = sum(n)) %>% 
  filter(component %in% component_list) -> samp_numbers_by_component

# Item 2 tabe 1 males and females weighted average -----------------
shell_cond <- c(1,2,3,4)
by_size %>% 
  filter(!is.na(shell) & !is.na(size)) %>% 
  filter(shell %in% shell_cond) %>% 
  group_by(fishery, component) %>% 
  summarise(avg_size = weighted.mean(size, n, na.rm = T), n = sum(n)) %>% 
  as.data.frame -> by_sex
# 
# my total for males here does NOT match Ben's Item2 spreadsheet....females does match????
# look into this - I believe this is due to including those without shell conditions, removed shell = NA
# use totals from samp_numbers_by_component - they include all individuals sampled. 

# summary avg wt ---------------
# calculate average weight by applying the weight-length relationship and then averaging weight...how different is this?

by_size %>% 
  separate(fishery, into = c("fishery_code", "year"), sep = "(?<=[A-Z a-z])(?=[0-9])") %>% 
  mutate(year = as.numeric(year) +2000) -> by_size2

weight_length %>% 
  filter(Species == "st matthew bkc") %>% 
  ungroup() %>% 
  select(-Species, -fishery_code) -> wl_smbkc_only

Males = c("Sublegal", "LegalRet", "LegalNR")
by_size2 %>% 
  left_join(wl_smbkc_only) %>% # here I always want to use the relationship for SMBKC regardless
                            # of the directed fishery
  select(-legal, -sex, -shell) %>% 
  mutate(wt_gram = alpha*(size^(beta)), 
         wt_kg = wt_gram/1000, 
         wt_lb = wt_kg*2.20462262, 
         component2 = ifelse(component %in% Males, "Male", "Female")) %>% 
  group_by(fishery_code, year, component2) %>% 
  summarise(avg_wt_g = weighted.mean(wt_gram, n, na.rm = T), 
            avg_wt_lb = weighted.mean(wt_lb, n, na.rm = T), n = sum(n) ) %>% 
  as.data.frame -> avg_weight

# add catch biomass to summary1 -----------
head(summary1) # number here is total count in pots
# need this by males and females

summary1 %>% 
  mutate(component2 = ifelse(component %in% Males, "Male", "Female")) %>% 
  group_by(Fishery, year, species, fishery, Fishery_directed_effort, no_pots,component2) %>% 
  summarise(number = sum(number)) %>% 
  mutate(cpue = number/no_pots, 
         catch_no = cpue*Fishery_directed_effort) -> summary2

# need this by males and females
head(avg_weight)

summary2 %>% 
  rename(fishery_code = fishery) %>%
  left_join(avg_weight) %>% 
  select(year, species, Fishery, Fishery_directed_effort, no_pots, 
         component2, number, cpue, catch_no, avg_wt_lb) %>% 
  mutate(catch_biomass = catch_no*avg_wt_lb)-> SMBKC_all
# **fix** what to do about component / fishery sections that don't have length or weight data

write.csv(SMBKC_all, file = 'results/SMBKC_allfisheries.csv')

## summarize for excel output comparison ------
SMBKC_all %>% 
  ungroup() %>%
  select(-Fishery, -fishery_code) %>% 
  group_by(species, year) %>% 
  summarise(total_catch_biomass = sum(catch_biomass, na.rm = TRUE)) -> SMBKC_all_total #matches Ben D.'s!!
  
  
head(landed_lb)
landed_lb %>% 
  group_by(Season) %>% 
  summarise(landed_pounds = sum(Whole.Weight..sum., na.rm = TRUE)) %>% 
  mutate(year = as.numeric(str_sub(Season, 1, 4)), species = "SMBKC")-> smbkc_landed_lb

SMBKC_all_total %>% 
  left_join(smbkc_landed_lb) %>% 
  select(-Season) %>% 
  mutate(landed_pounds = replace_na(landed_pounds, 0), 
         total_bycatch_lb = total_catch_biomass - landed_pounds, 
         total_bycatch_mort_lb = total_bycatch_lb*0.2) %>% 
  write.csv(file = 'results/SMBKC_total.csv')

