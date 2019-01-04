# bbrkc bycatch expansion time series - from XXXX to 2017 - calculated by to compare to Ben D.'s values
# 
# Work to automate bycatch expansion currently performed for BSAI crab fisheries

# K.Palof    ADF&G 
# katie.palof@alaska.gov
# 2018-12-21


# load ---
source('./code/packages.R')

# data -----
# Data is from the Kodiak wikki - http://kodweb.fishgame.state.ak.us/
# The data is accessed via: Data Access – Shellfish – Biological Data – Crab Observer – 
#                                                                             Reports & Info
# Fish ticket data is from Ben Daly - need to ask him where this is stored.

# This is for one species - BBRKC - Bristol Bay Red King crab 
#  Normally need to look at all open fisheries in each year. Here there was a data dump provided for all years.

##### BEN sent me a summary of sampled obsever pots ONLY for directed and cost recovery... why ?? **FIX**
# Species Composition Reports - Sample pot summary - Fishery: all fisheries/ - Species: BBRKC

# Species Composition Reports - Sample pot summary - Fishery: TR or CR or XR/ - Species: red king crab
files_pots <- dir('data/time_series/bbrkc/samp_pots', pattern = '*.csv')
sampled_pots <- files_pots %>% 
  map(function(x) read_csv(file.path('data/time_series/bbrkc/samp_pots', x))) %>% 
  reduce(rbind) 

# From Ben D, need to figure out where this comes from **fix**
fish_tkt <- read.csv('data/time_series/bbrkc/FT_numcrab_landed.csv')
# total landings and effort in directed fishery - bbrkc??? check with Ben

# Data dumps - Crab Detail Data - Fishery: all - Species: bbrkc - Sex: all 
crab_data <-  read.csv('data/time_series/bbrkc/RKC_crab_dump_921_3.csv')

# dockside samples --
dockside <- read.csv('data/time_series/bbrkc/RKC-BBR_EBS_dockside_crab-3.csv')
# Data on this relationship from NMFS tech memo July 2016 - Bob Foy
weight_length <- read.csv('data/weight_length.csv') #using these values and size results in average
#                  weight in grams.   

## landed pounds in directed fishery bbrkc TR -  see fish_tkt above.
## Fishery directed effort and observed effort - summarized by Ben.
fishery_effort <- read.xlsx("data/time_series/bbrkc/Jie_BBRKC_data_request_MarchApril2018_pre96included.xlsx", 
                            sheetName = 'effort')

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

# pull year out from fishery designation
library(stringr)
numextract <- function(string){ 
  str_sub(string, 3, 4)
} 
chrextract <- function(string){ 
  str_sub(string, 1, 2)
}
samp_pots %>% 
  mutate(year = as.numeric(numextract(Fishery)), 
         fishery = chrextract(Fishery), 
         species = ifelse(fishery == "CR", "red king", 
                          ifelse(fishery == "TR", "red king", 
                                 ifelse(fishery == "XR", "CR red king")))) -> samp_pots
samp_pots %>% 
  mutate(year = ifelse(year > 25, year + 1900, year + 2000)) -> samp_pots

### group directed red crab together (TR and CR)
samp_pots %>% 
  group_by(year, species, component) %>% 
  summarise(number = sum(number), no_pots = sum(no_pots)) -> samp_pots
  

# calculate CPUE from sampled pots -----------
samp_pots %>% 
  mutate(cpue = number/no_pots) -> samp_pots

# total effort from fishery ---
# stored in excel and with calcs there so needs to be edited for each area for the rows included
head(fishery_effort)
fishery_effort %>% 
  select(year, dir_fishery_effort, CR_fishery_effort) %>%
  gather("type", "direct_effort", dir_fishery_effort:CR_fishery_effort) %>% 
  mutate(species = ifelse(type == "dir_fishery_effort", "red king", "CR red king")) %>% 
  select(-type) %>% 
  right_join(samp_pots) -> summary1
# add effort to sampled pots summary 


# catch number -------
# extrapolated from cpue and total fishery effort 
summary1 %>% 
  mutate(catch_no = cpue*direct_effort) -> summary1

### 2016 check -----
# see discard estimates4.xlsx 
summary1 %>% filter(year == 2016) %>% 
  group_by(component) %>% 
  summarise(fish_effort = sum(direct_effort), 
            number = sum(number), 
            obs_effort = sum(no_pots)) %>% 
  filter(component == "LegalNR" | component == "LegalRet") %>% 
  group_by(fish_effort, obs_effort) %>% 
  summarise(legal_no = sum(number)) %>% 
  mutate(expand_legal_no = legal_no/obs_effort * fish_effort)
# not the same as Bill's....???? **FIX**

### legal NR (numbers) from data and subtraction --------------------
# uses numbers NOT weight 
head(summary1)
# summarise catch_no by year
summary1 %>% 
  filter(year > 1989) %>% 
  group_by(year, component) %>% 
  summarise(fish_effort = sum(direct_effort), 
            number = sum(number), 
            obs_effort = sum(no_pots)) %>% 
  mutate(expand_no = (number/obs_effort)*fish_effort) -> summary1_annual
fish_tkt %>% 
  select(year, numcrab_landed, wt_landed_lbs) -> bbrkc_fish_tkt_sum

summary1_annual %>% 
  left_join(bbrkc_fish_tkt_sum) -> summary1_annual_catch

### Legal NR percent ----------
summary1_annual_catch %>% 
  mutate(percent = expand_no/numcrab_landed) %>% 
  filter(component == "LegalNR") -> percent_LegNR_no

### Substraction method ----------
summary1_annual_catch %>% 
  mutate(component2 = ifelse(component == "LegalNR", "Legal", 
                             ifelse(component == "LegalRet", "Legal", component))) %>% 
  group_by(year, component2, fish_effort, obs_effort, numcrab_landed, wt_landed_lbs) %>% 
  summarise(number = sum(number)) %>% 
  mutate(expand_no = (number/obs_effort)*fish_effort,  
         percent_sub = (expand_no-numcrab_landed)/expand_no) %>% 
  filter(component2 == "Legal") -> percent_LegNR_subtraction_no
percent_LegNR_subtraction_no %>% 
  ungroup() %>% 
  select(-component2) -> merge_no

#### file for numbers -----
percent_LegNR_no %>% 
  ungroup() %>% 
  select(-component, -number, -expand_no) %>% 
  right_join(merge_no) %>% 
  write.csv(file = 'results/bbrkc/bbrkc_numbers.csv')

# percent is from data collected as LegalNR and percent_sub is from subtraction method using Legals in observed
#   pots expaned and the numcrab_landed

# GRAPH numbers ----------
percent_LegNR_no %>% 
  select(year, percent) %>% 
  right_join(percent_LegNR_subtraction_no) %>% 
  select(year, percent, percent_sub) %>% 
  mutate(LegalNR = percent*100, Subtraction = percent_sub*100) %>% 
  gather("method", "percentage", LegalNR:Subtraction) %>% 
  ggplot(aes(year, percentage, fill = method, width = 0.5)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    scale_fill_grey() +
    geom_text(aes(label = round(percentage, digits = 1)), vjust = -0.6, 
              position = position_dodge(0.9), size = 2.5) +
    ggtitle("Percentage of Legal discards in numbers") +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_y_continuous(name = "Percentage of Legal discards", breaks = seq(-60, 100, 20)) +
    scale_x_continuous(name = "Year", breaks = seq(1990, 2018, 2)) +
    ggsave('./results/bbrkc/discard_numbers.png', dpi = 300, width = 8.0, 
           height = 4.0, unit = "in")

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
  mutate(year = as.numeric(numextract(fishery)), 
         fishery = chrextract(fishery), 
         species = ifelse(fishery == "CR", "red king", 
                               ifelse(fishery == "TR", "red king", 
                               ifelse(fishery == "XR", "CR red king", "not"))), 
         year = ifelse(year > 25, year + 1900, year + 2000)) -> by_size2
# remove non directed bbkrc fisheries
by_size2 %>% 
  filter(species != "not") -> by_size2

## weight conversion -------------
weight_length %>% 
  filter(Species == "red king") %>% 
  ungroup() %>% 
  select(-Species, -fishery_code, -description) -> wl_bbrkc_only

Males = c("Sublegal", "LegalRet", "LegalNR")
Legal = c("LegalRet", "LegalNR")
by_size2 %>% 
  left_join(wl_bbrkc_only) %>% # here I always want to use the relationship for bbrkc regardless of fishery
   # of the directed fishery
  select(-legal, -sex, -shell) %>% 
  filter(component != " ") %>% 
  mutate(wt_gram = alpha*(size^(beta)), 
         wt_kg = wt_gram/1000, 
         wt_lb = wt_kg*2.20462262, 
         component2 = ifelse(component %in% Legal, "Legal",
                             ifelse(component == "Sublegal", "Male", "Female"))) %>% 
  group_by(year, species, component2) %>% 
  summarise(avg_wt_g = weighted.mean(wt_gram, n, na.rm = T), 
            avg_wt_lb = weighted.mean(wt_lb, n, na.rm = T), n = sum(n) ) %>% 
  ungroup() %>% 
  select(-species) %>% 
  as.data.frame -> avg_weight  ## !!!!fix!!!! this is by male or female since components are not adequately 
#                                                   sampled in each year
# I need LEGAL males here!

# add catch biomass to summary1 -----------
head(summary1_annual_catch) # number here is total count in pots by component
# and includes catch data 

summary1_annual_catch %>% 
  mutate(component2 = ifelse(component %in% Legal, "Legal",
                             ifelse(component == "Sublegal", "Male", "Female")), 
         obs_cpue = number/obs_effort, 
         fish_cpue = numcrab_landed/fish_effort) -> summary2

# need this components -- eventually just focus on legalNR and then just legal in general
head(avg_weight)

### adding weight by male or female ----------
summary2 %>% 
  left_join(avg_weight) %>% 
  select(-avg_wt_g) %>% 
  mutate(NRexpand_biomass = expand_no*avg_wt_lb)-> bbrkc_wt_1
  
# **fix** what to do about component / fishery sections that don't have length or weight data

### LegalNR weight ----------
bbrkc_wt_1 %>% 
  mutate(percent = NRexpand_biomass/wt_landed_lbs) %>% 
  filter(component == "LegalNR") -> percent_LegNR_lb # data input for weights is missing 16 and 17

### subtraction weight -------
bbrkc_wt_1 %>% 
  group_by(year, component2, fish_effort, obs_effort, numcrab_landed, wt_landed_lbs, avg_wt_lb) %>% 
  summarise(number = sum(number)) %>% 
  mutate(obs_cpue = number/obs_effort, 
         fish_cpue = numcrab_landed/fish_effort,
         expand_lb = ((number/obs_effort)*fish_effort)*avg_wt_lb,  
         percent_sub = (expand_lb-wt_landed_lbs)/expand_lb) %>% 
  filter(component2 == "Legal") -> percent_LegNR_subtraction_lb


#### file for weight-- lbs -----
percent_LegNR_subtraction_lb %>% 
  ungroup() %>% 
  select(year, percent_sub, avg_wt_lb, obs_cpue, fish_cpue, expand_lb) -> merge_lb

percent_LegNR_lb %>% 
  ungroup() %>% 
  select(year, fish_effort, obs_effort, numcrab_landed, wt_landed_lbs, NRexpand_biomass, percent) %>% 
  left_join(merge_lb) %>% 
  write.csv(file = 'results/bbrkc/bbrkc_weights.csv')

# percent is from data collected as LegalNR and percent_sub is from subtraction method using Legals in observed
#   pots expaned and the numcrab_landed


# GRAPH pounds weight ----------
percent_LegNR_lb %>% 
  select(year, percent) %>% 
  right_join(percent_LegNR_subtraction_lb) %>% 
  select(year, percent, percent_sub) %>% 
  mutate(LegalNR = percent*100, Subtraction = percent_sub*100) %>% 
  gather("method", "percentage", LegalNR:Subtraction) %>% 
  ggplot(aes(year, percentage, fill = method, width = 0.5)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_grey() +
  geom_text(aes(label = round(percentage, digits = 1)), vjust = -0.6, 
            position = position_dodge(0.9), size = 3.5) +
  ggtitle("Percentage of Legal discards in biomass (lb)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(name = "Percentage of Legal discards", breaks = seq(-60, 100, 20)) +
  scale_x_continuous(name = "Year", breaks = seq(1990, 2018, 2)) +
  ggsave('./results/bbrkc/discard_pounds.png', dpi = 300, width = 8.0, 
         height = 4.0, unit = "in")





#write.csv(SMBKC_all, file = 'results/SMBKC_allfisheries.csv')





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
