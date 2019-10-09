# BBRKC bycatch expansion 

# K.Palof 
# katie.palof@alaska.gov
# 2019-10-1

# load -------
source('./code/packages.R')

# data -----

# specifics on where this data comes from  **FIX**
data_dump<-read.csv("./data/from_tyler/RKC-1990-2018_crab_dump.csv")
metadata <- read.csv("./data/metadata.csv")

# data clean-up ---------
# need a key for two letter codes and what fisheries they belong to....if we have this we could merge this with
#    this data file and remove all the ifelse statements
data_dump %>% 
  mutate(year = as.numeric(numextract(fishery)), 
         fishery = chrextract(fishery), 
         species = ifelse(fishery == "CR", "red king", 
                         ifelse(fishery == "TR", "red king", 
                                ifelse(fishery == "XR", "CR red king", "Tanner"))), 
         dir_CR = ifelse(fishery == "XR" , "CR", 
                         ifelse(fishery == "Q", "NA", "directed"))) -> data_dump2 #%>% 
  select(!is.na(dir_CR))


  data_dump<-read.csv("RKC-1990-2018_crab_dump.csv")
  x<-str_sub(data_dump$fishery, 2,2)
  sp_fish<-ifelse(x=="R","RKC", "Tanner")
  X<-str_sub(data_dump$fishery, 1,1)
  dir_CR<-ifelse(X=="X", "CR", ifelse(X=="Q", "NA", "directed"))
  data_dump<-cbind(data_dump, sp_fish, dir_CR)
data_dump<-data_dump[!data_dump$dir_CR=="NA",]##removes non-BB fisheries###
data_dump<-subset(data_dump, sp_fish=="RKC")

x<-data_dump$fishery
y<-str_sub(x, 3)
Y<-as.numeric(as.character(y))
yy<-ifelse(y>18, 1900, 2000)
year<-Y+yy
data_dump<-cbind(data_dump, year)
