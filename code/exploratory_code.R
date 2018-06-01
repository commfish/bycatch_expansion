# work in progress
# work to automate bycatch expansion currently performed for BSAI fisheries

# K.Palof    ADF&G 
# katie.palof@alaska.gov
# 2018-05-30

# load ---
source('./code/packages.R')

# data -----
# Data is from the Kodiak wikki - 
# http://kodweb.fishgame.state.ak.us/
#  The data is accessed via: Data Access – Shellfish – Biological Data – Crab Observer – Reports & Info
# see e-mail from Ben Daly for right now (paste explaination in here at a later time)

# just for QO16
sampled_pots <- read.csv('data/potSummary_QO16.csv') 
fish_tkt <- read.xlsx("data/FishTicketsummaries 2016-17.xlsx", sheetName = 'QO16', startRow = 3, 
                      endRow = 53)
crab_data <- read.csv('data/crabDatadump_QO16.csv') 

# calcs ----
# exploratory right now - edit and re-order at a later time

# no pots sampled ----------
# from potSummary
sampled_pots %>% 
  unite(Trip-spn, Trip, Spn, sep ="-") -> pots2
length(unique(pots2$`Trip - spn`)) ->no_pots

# count in pots ------
# count for females, sublegal, legalret and legalNR # from potSummary
sampled_pots %>% 
  summarise(T_female = sum(Female), T_sublegal = sum(Sublegal), T_legalret = sum(LegalRet), 
            T_legalNR = sum(LegalNR)) -> pot_summary

merge(no_pots, pot_summary) -> samp_pots
samp_pots %>% 
  rename(pots = x) -> samp_pots
samp_pots

# calculate CPUE from sampled pots -----------
samp_pots %>% 
  mutate(female_cpue = T_female/pots, sub_cpue = T_sublegal/pots, 
         legalret_cpue = T_legalret/pots, legalNR_cpue = T_legalNR/pots) -> cpue_summary

# total effort from fishery ---
# stored in excel and with calcs there so needs to be edited for each area for the rows included
head(fish_tkt)
# add effort to cpue summary
merge(cpue_summary, sum(fish_tkt$Effort..sum.)) 

cpue_summary %>% 
  merge(sum(fish_tkt$Effort..sum.)) %>% 
  rename(fishery_effort = y) ->summary1

# catch number -------
# extrapolated from cpue and total fishery effort 
summary1 %>% 
  mutate(female_catch = round(fishery_effort*female_cpue,0), 
         sub_catch = round(fishery_effort*sub_cpue,0),
         legalret_catch = round(fishery_effort*legalret_cpue,0),
         legalNR_catch = round(fishery_effort*legalNR_cpue,0)
         ) -> summary2

# size comp, avg size and weight -----
# crabDatadump_QO16.csv   - sampling at sea NOT dockside
crab_data %>% 
  group_by(size, legal, sex, shell) %>% 
  summarise(n = n()) %>% 
  as.data.frame-> by_size

#write.csv(by_size, file = 'results/by_size_at_sea.csv')

# Item 2 tabe 1 males and females weighted average -----------------
by_size %>% 
  filter(!is.na(shell) & !is.na(size)) %>% 
  group_by(sex) %>% 
  summarise(wtg_avg = weighted.mean(size, n, na.rm = T), n = sum(n)) %>% 
  as.data.frame -> by_sex
# **save** need to save females wtg_avg and n here 

# my total for males here does NOT match Ben's Item2 spreadsheet....females does match????
# look into this **fix**  
# I believe this is due to including those without shell conditions, removed shell = NA

# Item 2 tab 2 legal retained/non-retained -------------
crab_data %>% 
  filter(sex == 1) 
  

# Item 2 tab 3 legal / sublegal males ---------------------
by_size %>% 
  filter(sex == 1 & !is.na(shell) & !is.na(size)) %>% 
  group_by(legal) %>% 
  summarise(wtg_avg = weighted.mean(size, n, na.rm = T), n = sum(n))

# all crab by size and shell condition -----
crab_data %>% 
  group_by(size, shell) %>% 
  summarise(n = n()) %>% 
  spread(key = shell, value = n) -> all_size

all_size %>% 
  colSums(na.rm = T)

# males by size and shell condition -----
by_size %>% 
  filter(sex == 1) -> males
males %>% 
  spread(key = shell, value = n) %>% 
  as.data.frame -> males2

males2 %>% 
  colSums(na.rm = T)

write.csv(males2, file = 'results/by_size_at_sea_males.csv')
