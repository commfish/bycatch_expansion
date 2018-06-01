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
weight_length <- read.csv('data/weight_length.csv') #using these values and size results in average
#                  weight in grams.   


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
  gather("component", "n", 16:19) -> sampled_pots2

sampled_pots2 %>% 
  group_by(component) %>% 
  summarise(number = sum(n)) -> pots_by

merge(no_pots, pots_by) -> samp_pots
samp_pots %>% 
  rename(pots = x) -> samp_pots
samp_pots

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
  mutate(catch_no = round(cpue*fishery_effort,0)) -> summary2

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
by_size %>% 
  filter(sex == 1 & !is.na(shell) & !is.na(size)) %>% 
  group_by(legal) %>% 
  summarise(wtg_avg = weighted.mean(size, n, na.rm = T), n = sum(n)) %>% 
  as.data.frame -> by_retained
# **save** need to save legal 0, 1, 2 here - there are sublegal, legalRet, and legal NR and n's

# Item 2 tab 3 legal / sublegal males ---------------------
by_size %>% 
  filter(sex == 1 & !is.na(shell) & !is.na(size)) %>% 
  mutate(status = ifelse(legal == 0, 'sub', 
                         ifelse(legal == 1, 'Leg', ifelse(legal == 2, 'Leg', legal)))) %>% 
  group_by(status) %>% 
  summarise(wtg_avg = weighted.mean(size, n, na.rm = T), n = sum(n)) %>% 
  as.data.frame ->by_legal
# **save** need to save sublegal and legal here -  and n's

# summary avg size and wt ---------------
component <- c("Female", "Sublegal", "LegalRet", "LegalNR")
EBSsnow <- data.frame(component)
EBSsnow$avg_size <- round(c(by_sex[2,2], by_retained[1,2], by_retained[2,2], by_retained[3,2]),1)
EBSsnow$n <- c(by_sex[2,3], by_retained[1,3], by_retained[2,3], by_retained[3,3])
EBSsnow$alpha <- c(weight_length[11,2], weight_length[10,2], weight_length[10,2], weight_length[10,2])
EBSsnow$beta <- c(weight_length[11,3], weight_length[10,3], weight_length[10,3], weight_length[10,3])

EBSsnow %>% 
  mutate(avg_wt = alpha*(avg_size^(beta)), 
         avg_wt_kg = round(avg_wt/1000,3)) -> EBSsnow

# add catch biomass to summary2 -----------
head(summary2)
head(EBSsnow)
summary2 %>% 
  left_join(EBSsnow) %>% 
  select(component, number, pots, cpue, fishery_effort, catch_no, 
         avg_wt_kg, n) -> EBSsnow2

EBSsnow2 %>% 
  mutate(catch_biomass = catch_no*avg_wt_kg)


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
