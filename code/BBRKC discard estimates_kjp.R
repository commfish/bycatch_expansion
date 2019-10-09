# BBRKC bycatch expansion 
# updated to be universal to species / fishery

# K.Palof 
# katie.palof@alaska.gov
# 2019-10-9

# load -------
source('./code/packages.R')

# globals ----
DM <- 0.2 #discard mortality

# data -----
# specifics on where this data comes from  **FIX**
obs_dump <- read.csv("./data/bbrkc/RKC-1990-2018_crab_dump.csv") # now reads in as a data frame 
potsum <- read.csv("./data/bbrkc/RKC-1990-2018_potsum.csv")
ret_catch <-read.csv("./data/bbrkc/RKC-retained_catch.csv")
dock <-read.csv("./data/bbrkc/RKC-1990-2018_dockside.csv")
metadata <- read.csv("./data/metadata.csv")
metadata2 <- read.csv("./data/metadata2.csv")
weight_length <- read.csv('./data/weight_length.csv') #using these values and size results in average
#   weight in grams.   

# data clean-up ---------
weight_length %>% 
  filter(species == "red king crab", 
         component %in% c("LegalRet", "Female", "Immature_F")) %>% 
  mutate(component = case_when(sex == 1 ~ "male", 
                               clutch == -9 ~ "immatureF",
                               sex != 1 & is.na(clutch) ~ "female")) %>% 
  select(species, component, sex, alpha, beta) -> rkc_wt
  
  
# assigns species, area fished, and weights - just directed and CR in the end
obs_dump %>% 
  mutate(year = as.numeric(substring(fishery, 3, 4)),
         year = ifelse(year <= 75, year + 2000, year + 1900),
         target = substring(fishery, 2, 2), 
         fishery_type = substring(fishery, 1, 1)) %>% 
  left_join(metadata) %>% 
  left_join(metadata2) %>% 
  mutate(dir_cr = case_when(fishery_type == "X" ~ "CR",
                            fishery_type == "Q" ~ "NA", 
                            fishery_type %in% c("C", "E", "T") ~ "directed"), 
         component = case_when(sex == 1 ~ "male", 
                               clutch == -9 ~ "immatureF",
                               sex != 1 & clutch != -9 ~ "female")) %>% 
  left_join(rkc_wt) %>% 
  mutate(wt_kg = (alpha * size^beta) / 1000, 
         wt_lb = wt_kg * 2.20462262) %>% 
  filter(dir_cr != "NA",
         species == "red king crab",
         sex %in% c(1, 2)) -> obs_dump2

# create mean weight data frame, add years with fishery closures
obs_dump2 %>%
  group_by(year) %>%
  summarize(female_wt = mean(wt_lb[sex == 2], na.rm = T),
            male_wt = mean(wt_lb[sex == 1], na.rm = T),
            sublegal_male_wt = mean(wt_lb[sex == 1 & legal ==0], na.rm = T),
            lnr_male_wt = mean(wt_lb[sex == 1 & (legal == 2 | (year < 1998 & legal == 1))], na.rm = T),
            legal_male_wt = mean(wt_lb[sex == 1 & legal %in% c(1 ,2)], na.rm=T)) %>%
  add_row(year = 1994:1995,.before = 4) -> mean_wt

# # create fields to parse data
# sp_fish - species
# dir_cr - fishery area/type?
# year
# tot_male - total males (sublegal and legal)
# only retain males and females in Bristol Bay fisheries
potsum %>% 
  mutate(year = as.numeric(substring(fishery, 3, 4)),
       year = ifelse(year <= 75, year + 2000, year + 1900),
       target = substring(fishery, 2, 2), 
       fishery_type = substring(fishery, 1, 1)) %>% 
  left_join(metadata) %>% 
  left_join(metadata2) %>% 
  mutate(dir_cr = case_when(fishery_type == "X" ~ "CR",
                            fishery_type == "Q" ~ "NA", 
                            fishery_type %in% c("C", "E", "T") ~ "directed"), 
         tot_male = sublegal + tot_legal) %>% 
  filter(dir_cr != "NA",
         species == "red king crab") -> potsum2

# create observer cpue data frame, add years with fishery closures
potsum2 %>%
  group_by(year) %>%
  summarize(obs_num_fem = sum(female, na.rm = T),
            obs_num_male = sum(tot_male, na.rm = T), 
            obs_num_sublegal_male = sum(sublegal, na.rm = T), 
            obs_num_lnr_male = sum(legal_nr, na.rm = T),
            obs_num_legal_male = sum(tot_legal, na.rm = T), 
            obs_effort = n()) %>%
  as.data.frame() %>%
  mutate(obs_fem_cpue = obs_num_fem / obs_effort,
         obs_male_cpue = obs_num_male / obs_effort,
         obs_sublegal_male_cpue = obs_num_sublegal_male / obs_effort,
         obs_lnr_male_cpue = obs_num_lnr_male / obs_effort,
         obs_legal_male_cpue = obs_num_legal_male / obs_effort) %>%
  select(1,2,8,3,9,4,10,5,11,6,12) %>%
  add_row(year = 1994:1995, .before = 4) -> obs_cpue

# dockside sampling data, add year and fishery
dock %>%
  mutate(year = as.numeric(substring(fishery, 3, 4)),
         year = ifelse(year <= 75, year + 2000, year + 1900),
         target = substring(fishery, 2, 2), 
         fishery_type = substring(fishery, 1, 1)) %>% 
  left_join(metadata) %>% 
  left_join(metadata2) %>% 
  mutate(dir_cr = case_when(fishery_type == "X" ~ "CR",
                            fishery_type == "Q" ~ "NA", 
                            fishery_type %in% c("C", "E", "T") ~ "directed"),
         shell_text = case_when(shell %in% c(0:2, 9) ~ "New",
                                shell %in% c(3:5) ~ "Old")) -> dock



