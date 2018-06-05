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
crab_data <- read.csv('data/crabDatadump_QO16.csv') 
# Data on this relationship from NMFS tech memo July 2016 - Bob Foy
weight_length <- read.csv('data/weight_length.csv') #using these values and size results in average
#                  weight in grams.   
