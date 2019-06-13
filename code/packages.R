# packages to load generic
# Katie Palof
# katie.palof@alaska.gov
# 2018-3-23

# load -----
library(tidyverse)
library(xlsx)
library(readxl)
library(reshape2)
library(scales)
library(extrafont)
library(grid)
library(knitr)
library(pander)
library(data.table)
library(readr)
library(purrr)
library(stringr)
options(scipen=9999) # remove scientific notation

loadfonts(device="win")
windowsFonts(Times=windowsFont("TT Times New Roman"))
theme_set(theme_bw(base_size=12,base_family='Times New Roman')+ 
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank()))

# functions ------------
