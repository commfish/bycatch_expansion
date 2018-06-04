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

# for one fishery  QO16 currently
# Species Composition Reports - Sample pot summary - Fishery: QO16 - Species: snow crab
sampled_pots <- read.csv('data/potSummary_QO16.csv') 
# 
fish_tkt <- read.xlsx("data/FishTicketsummaries 2016-17.xlsx", sheetName = 'QO16', startRow = 3, 
                      endRow = 53)
# Data dumps - Crab Detail Data - Fishery: QO16 - Species: snow crab - Sex: all 
crab_data <- read.csv('data/crabDatadump_QO16.csv') 
# Data on this relationship from NMFS tech memo July 2016 - Bob Foy
weight_length <- read.csv('data/weight_length.csv') #using these values and size results in average
#                  weight in grams.   
