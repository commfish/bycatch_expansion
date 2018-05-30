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

pots <- read.csv('data/potSummary_QO16.csv') 





# calcs ----
# exploratory right now - edit and re-order at a later time

# no pots sampled ----------
pots %>% 
  unite(Trip-spn, Trip, Spn, sep ="-") -> pots2
length(unique(pots2$`Trip - spn`)) ->no_pots

         