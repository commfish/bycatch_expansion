# SMBKC bycatch from 2010 to current year - calculated by request for plan team in Sept. 2018
# work with Ben Daly on this request.
# Work to automate bycatch expansion currently performed for BSAI crab fisheries

# K.Palof    ADF&G 
# katie.palof@alaska.gov
# 2018-09-06


# load ---
source('./code/packages.R')

# data -----
# Data is from the Kodiak wikki - http://kodweb.fishgame.state.ak.us/
# The data is accessed via: Data Access – Shellfish – Biological Data – Crab Observer – 
  #                                                                             Reports & Info
# Fish ticket data is from Ben Daly - need to ask him where this is stored.

# This is for one species - SMBKC St.Matts Blue King Crab. 
#  Look at all open fishery in that year  - here need 2010 to 2018. Should only show up in 
#                                             snow, tanner, and directed fisheries/
# Species Composition Reports - Sample pot summary - Fishery: QO16/ - Species: SM blue king crab
files_pots <- dir('data/SMBKC/potsum', pattern = '*.csv')
sampled_pots <- files_pots %>% 
                  map(function(x) read_csv(file.path('data/SMBKC/potsum', x))) %>% 
                  reduce(rbind) 
# From Ben D, need to figure out where this comes from **fix**
# each fishery has a tab here, read in all applicable fisheries
#fish_tkt <- read.xlsx("data/FishTicketsummaries 2016-17.xlsx", sheetName = 'QO16', startRow = 3, 
#                      endRow = 53)
#fish_tkt2 <- read.xlsx("data/FishTicketsummaries 2016-17.xlsx", sheetName = 'QT17', startRow = 3, 
#                       endRow = 53) # edit start and end rows.
#fish_tkt3 <- read.xlsx("data/FishTicketsummaries 2016-17.xlsx", sheetName = 'TR17', startRow = 3, 
#                   endRow = 53) # edit start and end rows.

#fish_tkt1 %>% 
#  rbind(fish_tkt2) %>% 
#  rbind(fish_tkt3)
# Data dumps - Crab Detail Data - Fishery: QO16 - Species: snow crab - Sex: all 
files_cdata <- dir('data/SMBKC/datadump', pattern = '*.csv')
crab_data <- files_cdata %>% 
  map(function(x) read_csv(file.path('data/SMBKC/datadump', x))) %>% 
  reduce(rbind) 

# Data on this relationship from NMFS tech memo July 2016 - Bob Foy
weight_length <- read.csv('data/weight_length.csv') #using these values and size results in average
#                  weight in grams.   

# landed pounds in directed fishery SMBKC QP
landed_lb <- read.xlsx("data/SMBKC/FT summary SMBKC Multiple Seasons By Stat Area With CDQ-.xlsx", 
                       sheetName = 'Sheet1', startRow = 3, endRow = 55)
## Fishery directed effort - Ben D. summarizes this from WBT Directed-Incidental Calculations_17-18.xlsx
fishery_effort <- read.xlsx("data/SMBKC/SMBKC_bycatch_ests.xlsx", 
                       sheetName = 'Sheet1')

# summary stats  ----------
# number of pots sampled ------
sampled_pots %>% 
  unite(Trip-spn, Trip, Spn, sep ="-") -> pots2

pots2 %>% 
  group_by(Fishery) %>% 
  summarise(no_pots = length(unique(`Trip - spn`))) -> pots

# count in pots ------
# count for females, sublegal, legalret and legalNR # from potSummary
sampled_pots %>% 
  gather("component", "n", Female:LegalNR) -> sampled_pots2

sampled_pots2 %>% 
  group_by(Fishery, component) %>% 
  summarise(number = sum(n)) -> numbers

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

Males = c("Sublegal", "LegalRet", "LegalNR")
by_size2 %>% 
  left_join(weight_length) %>% 
  select(-Species, -legal, -sex, -shell) %>% 
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

write.csv(SMBKC_all, file = 'resultsSMBKC_allfisheries.csv')

## summarize for excel output comparison ------
SMBKC_all %>% 
  ungroup() %>%
  select(-Fishery, -fishery_code) %>% 
  filter(species == "SMBKC") %>% 
  as.data.frame()




