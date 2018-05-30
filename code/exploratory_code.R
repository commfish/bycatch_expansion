# work in progress
# work to automate bycatch expansion currently performed for BSAI fisheries

# K.Palof    ADF&G 
# katie.palof@alaska.gov
# 2018-05-30

# load ---
source('./code/packages.R')

# data -----
# Data is from the Kodiak wikki - see e-mail from Ben Daly for right now (paste explaination in here at a later
# time)

# just for QO16
sampled_pots <- read.csv('data/potSummary_QO16.csv') 
fish_tkt <- read.xlsx("data/FishTicketsummaries 2016-17.xlsx", sheetName = 'QO16', startRow = 3, 
                      endRow = 53)


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
# this is stored in excel....see about extracted it from there

