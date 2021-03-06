# Work to automate bycatch expansion currently performed for BSAI crab fisheries

# K.Palof    ADF&G 
# katie.palof@alaska.gov
# 2018-06-04


# load ---
source('./code/packages.R')

# data -----
# Data is from the Kodiak wikki - http://kodweb.fishgame.state.ak.us/
# The data is accessed via: Data Access – Shellfish – Biological Data – Crab Observer – Reports & Info
# Fish ticket data is from Ben Daly - need to ask him where this is stored.

# This is for one species - snow crab. Look at all open fishery in that year (2017)
# Species Composition Reports - Sample pot summary - Fishery: QO16/ - Species: snow crab
files_pots <- dir('data/pots', pattern = '*.csv')
sampled_pots <- files_pots %>% 
                  map(function(x) read_csv(file.path('data/pots', x))) %>% 
                  reduce(rbind) 
# From Ben D, need to figure out where this comes from **fix**
# each fishery has a tab here, read in all applicable fisheries
fish_tkt <- read.xlsx("data/FishTicketsummaries 2016-17.xlsx", sheetName = 'QO16', startRow = 3, 
                      endRow = 53)
fish_tkt2 <- read.xlsx("data/FishTicketsummaries 2016-17.xlsx", sheetName = 'QT17', startRow = 3, 
                       endRow = 53) # edit start and end rows.
fish_tkt3 <- read.xlsx("data/FishTicketsummaries 2016-17.xlsx", sheetName = 'TR17', startRow = 3, 
                   endRow = 53) # edit start and end rows.

fish_tkt1 %>% 
  rbind(fish_tkt2) %>% 
  rbind(fish_tkt3)
# Data dumps - Crab Detail Data - Fishery: QO16 - Species: snow crab - Sex: all 
files_cdata <- dir('data/datadump', pattern = '*.csv')
crab_data <- files_cdata %>% 
  map(function(x) read_csv(file.path('data/datadump', x))) %>% 
  reduce(rbind) 

# Data on this relationship from NMFS tech memo July 2016 - Bob Foy
weight_length <- read.csv('data/weight_length.csv') #using these values and size results in average
#                  weight in grams.   

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
  gather("component", "n", 16:19) -> sampled_pots2

sampled_pots2 %>% 
  group_by(Fishery, component) %>% 
  summarise(number = sum(n)) -> numbers

numbers %>% 
  left_join(pots) -> samp_pots

# calculate CPUE from sampled pots -----------
samp_pots %>% 
  mutate(cpue = number/no_pots) -> samp_pots

# total effort from fishery ---
# stored in excel and with calcs there so needs to be edited for each area for the rows included
head(fish_tkt)
# add effort to sampled pots summary 
samp_pots %>% 
  merge(sum(fish_tkt$Effort..sum.)) %>% 
  rename(fishery_effort = y) -> summary1 # **fix** currently not correct, need fishery effort for other fisheries
                                          # only QO16 here

# catch number -------
# extrapolated from cpue and total fishery effort 
summary1 %>% 
  mutate(catch_no = cpue*fishery_effort) -> summary1

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

# summary avg size and wt ---------------
by_sex %>% 
  mutate(Fishery = fishery) %>% 
  separate(fishery, into = c("fishery_code", "year"), sep = "(?<=[A-Z a-z])(?=[0-9])") ->by_component

by_component %>% 
  left_join(weight_length) %>% 
  select(-Species) -> by_component2


by_component2 %>% 
  filter(component %in% component_list) %>% 
  rename(fishery = Fishery) %>% 
  mutate(avg_wt = alpha*(avg_size^(beta)), 
         avg_wt_kg = avg_wt/1000) -> EBSsnow

EBSsnow %>% 
  select(-n) %>% 
  left_join(samp_numbers_by_component) ->EBSsnow
write.csv(EBSsnow, file = 'results/EBSsnow_weight_length_all_fisheries.csv')

# add catch biomass to summary1 -----------
head(summary1) # number here is total count in pots
head(EBSsnow)
summary1 %>% 
  rename(fishery = Fishery) %>%
  left_join(EBSsnow) %>% 
  select(fishery, component, number, no_pots, cpue, fishery_effort, catch_no, avg_size, 
         avg_wt_kg, n) %>% 
  mutate(catch_biomass = catch_no*avg_wt_kg)-> EBSsnow_all
# **fix** what to do about component / fishery sections that don't have length or weight data
# **update** need to update with fishery_effort data for the other fisheries...right now all are QO16..

write.csv(EBSsnow_all, file = 'results/EBSsnowcrab_allfisheries.csv')

## side question ----
# calculate average weight by applying the weight-length relationship and then averaging weight...how different is this?

head(by_size)
head(weight_length)

by_size %>% 
  mutate(Fishery = fishery) %>% 
  separate(fishery, into = c("fishery_code", "year"), sep = "(?<=[A-Z a-z])(?=[0-9])") -> by_size2

by_size2 %>% 
  left_join(weight_length) %>% 
  select(-Species, -legal, -sex, -shell) %>% 
  mutate(wt_gram = alpha*(size^(beta))) %>% 
  group_by(Fishery, component) %>% 
  summarise(avg_wt_g = weighted.mean(wt_gram, n, na.rm = T), n = sum(n) ) %>% 
  mutate(avg_wt_kg = avg_wt_g/1000) %>% 
  as.data.frame -> avg_weight
