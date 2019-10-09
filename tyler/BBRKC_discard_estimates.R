# notes----
# Updated, more annotated, and streamlined script for BBRKC observer catch estimation based on "BBKRC discard estimates v2.0 - Ben Daly"
# author: Tyler Jackson
# contact
# date: 10/9/2019

# load ----
library(tidyverse)
library(FNGr)
theme_set(theme_sleek())
source("./scripts/tickr2.R")

# globals ----
DM <- 0.2 #discard mortality

# data ----
obs_dump <- read_csv("data\\RKC-1990-2018_crab_dump.csv")
potsum <- read_csv("data\\RKC-1990-2018_potsum.csv")
ret_catch <-read_csv("data\\RKC-retained_catch.csv")
dock <-read_csv("data\\RKC-1990-2018_dockside.csv")

# data cleaning and parsing ----

# create fields to parse data
# sp_fish - species
# dir_cr - fishery area/type?
# year
# wt_kg, wt_lbs
# only retain males and females in Bristol Bay fisheries
obs_dump %>%
  mutate(sp_fish = ifelse(substring(fishery, 2, 2) == "R", "RKC", "Tanner"),
         dir_cr = substring(fishery, 1, 1), 
         dir_cr = case_when(dir_cr == "X" ~ "CR",
                            dir_cr == "Q" ~ "NA", 
                            dir_cr %in% c("C", "E", "T") ~ "directed"), 
         year = as.numeric(substring(fishery, 3, 4)),
         year = ifelse(year <= 18, year + 2000, year + 1900), 
         wt_kg = case_when(sex == 1 ~ (0.000403 * size^3.141334) / 1000,
                           clutch == -9 ~ (0.000408 * size^3.127956) / 1000,
                           sex != 1 & clutch != -9 ~ (0.003593 * size^2.666076) / 1000),
         wt_lb = wt_kg * 2.20462262) %>%
  filter(dir_cr != "NA",
         sp_fish == "RKC",
         sex %in% c(1, 2)) -> obs_dump

# create mean weight data frame, add years with fishery closures
obs_dump %>%
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
  mutate(sp_fish = ifelse(substring(fishery, 2, 2) == "R", "RKC", "Tanner"),
         dir_cr = substring(fishery, 1, 1), 
         dir_cr = case_when(dir_cr == "X" ~ "CR",
                            dir_cr == "Q" ~ "NA", 
                            dir_cr %in% c("C", "E", "T") ~ "directed"),
         year = as.numeric(substring(fishery, 3, 4)),
         year = ifelse(year <= 18, year + 2000, year + 1900),
         tot_male = sublegal + tot_legal) %>%
  filter(dir_cr != "NA",
         sp_fish == "RKC") -> potsum

# create observer cpue data frame, add years with fishery closures
potsum %>%
  group_by(year) %>%
  summarize(obs_num_fem = sum(female, na.rm = T),
            obs_num_male = sum(tot_male, na.rm = T), 
            obs_num_sublegal_male = sum(sublegal, na.rm = T), 
            obs_num_lnr_male = sum(legal_nr, na.rm = T),
            obs_num_legal_male = sum(tot_legal, na.rm = T), 
            obs_effort = n()) %>%
  ungroup() %>%
  mutate(obs_fem_cpue = obs_num_fem / obs_effort,
         obs_male_cpue = obs_num_male / obs_effort,
         obs_sublegal_male_cpue = obs_num_sublegal_male / obs_effort,
         obs_lnr_male_cpue = obs_num_lnr_male / obs_effort,
         obs_legal_male_cpue = obs_num_legal_male / obs_effort) %>%
  select(1,2,8,3,9,4,10,5,11,6,12) %>%
  add_row(year = 1994:1995, .before = 4) -> obs_cpue

# dockside sampling data, add year and fishery
dock %>%
  mutate(dir_cr = substring(fishery, 1, 1), 
         dir_cr = case_when(dir_cr == "X" ~ "CR",
                   dir_cr == "Q" ~ "NA", 
                   dir_cr %in% c("C", "E", "T") ~ "directed"),
         year = as.numeric(substring(fishery, 3, 4)),
         year = ifelse(year <= 18, year + 2000, year + 1900),
         Shell_text = case_when(shell %in% c(0:2, 9) ~ "New",
                                shell %in% c(3:5) ~ "Old")) -> dock

            
# output summary tables ----

# summarize retained catch and join to observer cpue data
ret_catch %>%
  group_by(year) %>%
  summarise(ret_cat_lb = sum(ret_cat_lb, na.rm = T), 
            ret_cat_crabs = sum(ret_cat_crabs, na.rm = T),
            fishing_effort = sum(effort, na.rm = T)) %>%
  ungroup() %>%
  left_join(obs_cpue, by="year") -> obs_cpue
         
write.csv(obs_cpue, "./output/BBRKC_observer_cpue_1990_2018.csv", row.names = F)
         
# discard estimates using subtraction method
obs_cpue %>%
  left_join(mean_wt, by="year") %>%
  mutate(male_tot_catch_wt_lb = obs_male_cpue * fishing_effort * male_wt,
         legal_male_tot_catch_wt_lb = obs_legal_male_cpue * fishing_effort * legal_male_wt,
         male_discard_wt_lb = male_tot_catch_wt_lb - ret_cat_lb,
         legal_male_discard_wt_lb = legal_male_tot_catch_wt_lb - ret_cat_lb,
         female_discard_wt_lb = obs_fem_cpue * fishing_effort * female_wt,
         male_dm_wt_lb = male_discard_wt_lb * DM,
         female_dm_wt_lb = female_discard_wt_lb * DM,
         tot_dm_wt_lb = male_dm_wt_lb + female_dm_wt_lb,
         tot_dm_wt_mil_lb = tot_dm_wt_lb / 1000000,
         dm_rate = tot_dm_wt_lb / ret_cat_lb,
         legal_male_discard_rate = legal_male_discard_wt_lb / legal_male_tot_catch_wt_lb) %>%
  select(1, 20:30) -> sub_discard_est

write.csv(sub_discard_est, "./output/BBRKC_subtraction_discard_esimates_1990_2018.csv", row.names=F)

# compute legal nt reatined discards
obs_cpue %>%
  left_join(mean_wt, by="year") %>%
  mutate(sublegal_male_catch_wt_lb = obs_sublegal_male_cpue * fishing_effort * sublegal_male_wt,
         lnr_male_catch_wt_lb = obs_lnr_male_cpue * fishing_effort * lnr_male_wt, 
         male_not_retained_tot_catch_wt_lb = sublegal_male_catch_wt_lb + lnr_male_catch_wt_lb,
         female_tot_catch_wt_lb = obs_fem_cpue * fishing_effort * female_wt,
         tot_discard_wt_lb = male_not_retained_tot_catch_wt_lb + female_tot_catch_wt_lb,
         tot_dm_wt_lb = tot_discard_wt_lb * DM,
         tot_dm_wt_mil_lb =  tot_dm_wt_lb / 1000000,
         dm_rate = tot_dm_wt_lb / ret_cat_lb) %>%
  select(1:4, 20:27) %>%
  filter(year < 2018) -> lnr_discard_estimates

write.csv(lnr_discard_estimates, "./output/BBRKC_legal_not_retained_discard_estimates_1990_2017.csv", row.names = F)
  
# figures ----    

# Discard Mortality Rate by Year 
sub_discard_est %>%
  select(year, dm_rate) %>%
  mutate(Method = "Subtraction") %>%
  bind_rows(lnr_discard_estimates %>%
              select(year, dm_rate) %>%
              mutate(Method = "LNR")) %>%
  mutate(Rationalized = case_when(year < 2005 ~ F,
                                  year >= 2005 ~ T)) %>%
  ggplot(aes(x = year, y = dm_rate, color = Rationalized))+
  geom_point()+ 
  geom_line(aes(linetype = factor(Method, levels = c("Subtraction", "LNR"))))+
  scale_color_manual(values = c("blue", "red"), guide=F)+
  scale_y_continuous(limits = c(-0.01,0.5))+
  scale_x_continuous(breaks=tickr2(data=sub_discard_est, var="year", by=1, labs=2)$breaks, labels=tickr2(sub_discard_est, "year", 1, 2)$labels)+
  labs(x = NULL, y = "lb bycatch mortality per lb retained catch", linetype = "Estimation Method")+
  theme(legend.justification=c(1,1), legend.position = c(1,1)) -> dm_rate_year

png('./figures/BBRKC_dm_rate_year_1990_2018.png', width = 6, height = 4, units = "in", res = 300) 
dm_rate_year
dev.off()

# Discard mortality by year 
sub_discard_est %>%
    select(year, tot_dm_wt_mil_lb ) %>%
    mutate(Method = "Subtraction") %>%
    bind_rows(lnr_discard_estimates %>%
                select(year, tot_dm_wt_mil_lb) %>%
                mutate(Method = "LNR")) %>%
    mutate(Rationalized = case_when(year < 2005 ~ F,
                                year >= 2005 ~ T)) %>%
    ggplot(aes(x = year, y = tot_dm_wt_mil_lb, color = Rationalized))+
    geom_point()+ 
    geom_line(aes(linetype = factor(Method, levels = c("Subtraction", "LNR"))))+
    scale_color_manual(values = c("blue", "red"), guide=F)+
    scale_x_continuous(breaks=tickr2(data=sub_discard_est, var="year", by=1, labs=2)$breaks, labels=tickr2(sub_discard_est, "year", 1, 2)$labels)+
    labs(x = NULL, y = "Discard mortality (million lb)", linetype = "Estimation Method")+
    theme(legend.justification=c(1,1), legend.position = c(1,1)) -> dm_mil_lb_year

png('./figures/BBRKC_dm_mil_lb_year_1990_2018.png', width = 6, height = 4, units = "in", res = 300) 
dm_mil_lb_year
dev.off()

# Legal Discard Rate
sub_discard_est %>%
  mutate(Rationalized = case_when(year < 2005 ~ F,
                                  year >= 2005 ~ T)) %>%
  ggplot(aes(x = year, y = legal_male_discard_rate, color = Rationalized))+
  geom_point()+ 
  geom_line()+
  geom_hline(yintercept = 0, size=0.4)+
  scale_color_manual(values = c("blue", "red"), guide=F)+
  scale_x_continuous(breaks=tickr2(data=sub_discard_est, "year", 1, 2)$breaks, labels=tickr2(sub_discard_est, "year", 1, 2)$labels)+
  scale_y_continuous(breaks=seq(-1, 1, 0.2), limits = c(-0.5, 0.5))+
  labs(x = NULL, y = "lbs legal discards / lbs legal catch") -> legal_discard_rate_subtraction

png('./figures/BBRKC_legal_discard_rate_lb_year_1990_2018.png', width = 6, height = 4, units = "in", res = 300) 
legal_discard_rate_subtraction
dev.off()

# Proportion males that are sublegal
obs_cpue %>%
  ggplot(aes(x = year, y = obs_num_sublegal_male / obs_num_male))+
  geom_point()+
  geom_line()+
  labs(x=NULL, y = "Proportion sublegal in total males")+
  scale_x_continuous(breaks = tickr2(obs_cpue, "year", 1, 2)$breaks, labels = tickr2(obs_cpue, "year", 1, 2)$labels)+
  scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0,1)) -> prop_sublegal_male

png('./figures/BBRKC_sublegal_male_proportion_1990_2018.png', width = 6, height = 4, units = "in", res = 300) 
prop_sublegal_male
dev.off()

# Proportion legal old shell in all legal males
(obs_dump %>%
  filter(sex == 1 & legal == 1 & shell != -9) %>%
  mutate(Shell_text = case_when(shell %in% c(0:2, 9) ~ "New",
                                shell %in% c(3:5) ~ "Old")) %>%
  group_by(year) %>%
  mutate(tot_legal_male = n()) %>%
  group_by(year, Shell_text) %>%
  summarize(count = n(),
            tot_legal_male = mean(tot_legal_male)) %>%
  ungroup() %>%
  filter(Shell_text == "Old") %>%
  add_row(year = 1994:1995) -> obs_old_shell_prop) %>%
  ggplot(aes(x = year, y = count[Shell_text == "Old"] / tot_legal_male))+
  geom_point()+
  geom_line()+
  scale_x_continuous(breaks = tickr2(obs_cpue, "year", 1, 2)$breaks, labels = tickr2(obs_cpue, "year", 1, 2)$labels)+
  scale_y_continuous(breaks = seq(0, 0.5, 0.1), limits = c(0, 0.5))+
  labs(x = NULL, y = "Proportion old shell in legal males") -> prop_OS_male
  
png('./figures/BBRKC_oldshell_male_proportion_1990_2018.png', width = 6, height = 4, units = "in", res = 300) 
prop_OS_male
dev.off() 

# dockside vs fishery proportion old shell male
dock %>%
  group_by(year) %>%
  mutate(total = sum(numcrab)) %>%
  group_by(year, Shell_text) %>%
  summarize(count = sum(numcrab),
            total = mean(total)) %>%
  ungroup() %>%
  filter(Shell_text == "Old") %>%
  add_row(year = 1994:1995) %>%
  left_join(obs_old_shell_prop, c("year", "Shell_text")) %>%
  unite("Dockside", count.x, total) %>%
  unite("Observer", count.y, tot_legal_male) %>%
  gather("Source","value", c(3, 4)) %>%
  separate(value, c("count", "total")) %>%
  mutate_at(4:5, as.numeric) %>%
  mutate(Source = factor(Source, levels = c("Observer", "Dockside"))) %>%
  ggplot(aes(x = year, y = count / total, linetype=Source))+
  geom_point()+
  geom_line()+
  scale_x_continuous(breaks = tickr2(obs_cpue, "year", 1, 2)$breaks, labels = tickr2(obs_cpue, "year", 1, 2)$labels)+
  scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1))+
  labs(x = NULL, y = "Proportion old shell", linetype=NULL)+
  theme(legend.justification=c(1,1), legend.position = c(1,1)) -> prop_OS_dock_obs

png('./figures/BBRKC_oldshell_male_proportion_1990_2018 _dock&obs.png', width = 6, height = 4, units = "in", res = 300) 
prop_OS_dock_obs
dev.off() 

