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
fish_tkt <- read.xlsx("data/FishTicketsummaries 2016-17.xlsx", sheetName = 'QO16', startRow = 3, 
                      endRow = 53)
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
  mutate(cpue = number/pots) -> cpue_summary

# total effort from fishery ---
# stored in excel and with calcs there so needs to be edited for each area for the rows included
head(fish_tkt)
# add effort to cpue summary
cpue_summary %>% 
  merge(sum(fish_tkt$Effort..sum.)) %>% 
  rename(fishery_effort = y) ->summary1

# catch number -------
# extrapolated from cpue and total fishery effort 
summary1 %>% 
  mutate(catch_no = cpue*fishery_effort) -> summary2
