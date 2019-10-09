########used R 3.4.3.#############

wd<-"V:/Z- Ben/Data and Ests to CPT SAFE authors/2019/BBRKC"
setwd(wd)

library(stringr)
library(plyr)
library(dplyr)
library(miscTools)

############observer data dump#################

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

wt_kg<-ifelse(data_dump$sex==1, ((0.000403*data_dump$size^3.141334)/1000), ifelse(data_dump$clutch==-9, ((0.000408*data_dump$size^3.127956)/1000), ((0.003593*data_dump$size^2.666076)/1000)))
data_dump<-cbind(data_dump, wt_kg)
wt_lb<-data_dump$wt_kg*2.20462262
data_dump<-cbind(data_dump, wt_lb)
data_dump<-subset(data_dump, sex==1 | sex==2)###exclude records with unknow sex###
mean_wt<-aggregate(wt_lb~year+sex, data=data_dump, FUN=mean)
F_wt<-subset(mean_wt, sex==2)
M_wt<-subset(mean_wt, sex==1)
mean_wt2<-aggregate(wt_lb~year+sex+legal, data=data_dump, FUN=mean)
subLm_wt<-subset(mean_wt2, sex==1 & legal==0)
LNRm_wt<-subset(mean_wt2, sex==1 & legal==2)
LNRm_wt2<-subset(mean_wt2, year<1998 & sex==1 & legal==1)
LNRm_wt<-rbind(LNRm_wt,LNRm_wt2)
LNRm_wt<-LNRm_wt[order(LNRm_wt$year),]
Lm_wt<-subset(data_dump, sex==1 & legal==1 | legal==2)
Lm_wt2<-aggregate(wt_lb~year, data=Lm_wt, FUN=mean)
Mean_wt<-left_join(F_wt, M_wt, by="year")
Mean_wt<-left_join(Mean_wt, subLm_wt, by="year")
Mean_wt<-left_join(Mean_wt, LNRm_wt, by="year")
Mean_wt<-left_join(Mean_wt, Lm_wt2, by="year")
names(Mean_wt)[3]<-"F_wt"
names(Mean_wt)[5]<-"M_wt"
names(Mean_wt)[8]<-"subLm_wt"
names(Mean_wt)[11]<-"LNRm_wt"
names(Mean_wt)[12]<-"Lm_wt"

drops<-c("sex.x", "sex.y", "sex.x.x", "sex.y.y", "legal.x", "legal.y")
Mean_wt<-Mean_wt[, !(names(Mean_wt) %in% drops)]

Mean_wt<-as.matrix(Mean_wt)
fill<-rep(0, 5)
Mean_wt<-insertRow(Mean_wt, 28, c(1994, fill))###adds zeros for closed fisheries###
Mean_wt<-insertRow(Mean_wt, 29, c(1995, fill))
Mean_wt<-as.data.frame(Mean_wt)
Mean_wt<-Mean_wt[order(Mean_wt$year),]




##################################sample pot summary###########

Psum_data<-read.csv("RKC-1990-2018_potsum.csv")
x<-str_sub(Psum_data$fishery, 2,2)
sp_fish<-ifelse(x=="R","RKC", "Tanner")
X<-str_sub(Psum_data$fishery, 1,1)
dir_CR<-ifelse(X=="X", "CR", ifelse(X=="Q", "NA", "directed"))
Psum_data<-cbind(Psum_data, sp_fish, dir_CR)
Psum_data<-Psum_data[!Psum_data$dir_CR=="NA",]##removes non-BB fisheries###
Psum_data<-subset(Psum_data, sp_fish=="RKC")
total_male<-Psum_data$sublegal+Psum_data$tot_legal
Psum_data<-cbind(Psum_data, total_male)

x<-Psum_data$fishery
y<-str_sub(x, 3)
Y<-as.numeric(as.character(y))
yy<-ifelse(y>18, 1900, 2000)
year<-Y+yy
Psum_data<-cbind(Psum_data, year)

Obs_num_fem<-aggregate(Psum_data$female~year, data=Psum_data, FUN=sum)
names(Obs_num_fem)[2]<-"Obs_num_fem"
Obs_num_male<-aggregate(Psum_data$total_male~year, data=Psum_data, FUN=sum)
names(Obs_num_male)[2]<-"Obs_num_male"
Obs_num_subL_male<-aggregate(Psum_data$sublegal~year, data=Psum_data, FUN=sum)
names(Obs_num_subL_male)[2]<-"Obs_num_subL_male"
Obs_num_LNR_male<-aggregate(Psum_data$legal_nr~year, data=Psum_data, FUN=sum)
names(Obs_num_LNR_male)[2]<-"Obs_num_LNR_male"
names(Obs_num_subL_male)[2]<-"Obs_num_subL_male"
Obs_num_L_male<-aggregate(Psum_data$tot_legal~year, data=Psum_data, FUN=sum)
names(Obs_num_L_male)[2]<-"Obs_num_L_male"


Obs_effort<-aggregate(Psum_data$year~year, data=Psum_data, FUN=length)
names(Obs_effort)[2]<-"Obs_effort"
Obs_effort
Obs_fem_CPUE<-Obs_num_fem$Obs_num_fem/Obs_effort$Obs_effort
Obs_male_CPUE<-Obs_num_male$Obs_num_male/Obs_effort$Obs_effort
Obs_subL_male_CPUE<-Obs_num_subL_male$Obs_num_subL_male/Obs_effort$Obs_effort
Obs_LNR_male_CPUE<-Obs_num_LNR_male$Obs_num_LNR_male/Obs_effort$Obs_effort
Obs_L_male_CPUE<-Obs_num_L_male$Obs_num_L_male/Obs_effort$Obs_effort

CPUE_nums<-cbind(Obs_effort, Obs_num_fem$Obs_num_fem, Obs_fem_CPUE, Obs_num_male$Obs_num_male, Obs_male_CPUE, Obs_num_subL_male$Obs_num_subL_male, Obs_subL_male_CPUE, Obs_num_LNR_male$Obs_num_LNR_male, Obs_LNR_male_CPUE, Obs_num_L_male$Obs_num_L_male, Obs_L_male_CPUE)  
names(CPUE_nums)[3]<-"Obs_num_fem"
names(CPUE_nums)[5]<-"Obs_num_male"
names(CPUE_nums)[7]<-"Obs_num_subL_male"
names(CPUE_nums)[9]<-"Obs_num_LNR_male"
names(CPUE_nums)[11]<-"Obs_num_L_male"

CPUE_nums<-as.matrix(CPUE_nums)
fill<-rep(0, 11)
CPUE_nums<-insertRow(CPUE_nums, 28, c(1994, fill))
CPUE_nums<-insertRow(CPUE_nums, 29, c(1995, fill))
CPUE_nums<-as.data.frame(CPUE_nums)
CPUE_nums<-CPUE_nums[order(CPUE_nums$year),]

###################retained catch#############
catch<-read.csv("retained_catch.csv")
catch<-aggregate(. ~year, data=catch, FUN=sum)
names(catch)[5]<-"Fish_effort"
catch<-catch[, !(names(catch) %in% "dir_CR")]

CPUE_nums<-left_join(CPUE_nums, catch, by="year")
write.csv(CPUE_nums, "CPUE_nums.csv")

M_tot_catch_wt_lb<-CPUE_nums$Obs_male_CPUE*CPUE_nums$Fish_effort*Mean_wt$M_wt
M_disc_wt_lb<-M_tot_catch_wt_lb-CPUE_nums$ret_cat_lb
year<-seq(1990, 2018, 1)
sub_disc_ests<-cbind(year, M_tot_catch_wt_lb, M_disc_wt_lb)
sub_disc_ests<-as.data.frame(sub_disc_ests)
sub_disc_ests$M_disc_wt_lb[which(sub_disc_ests$year==1994)]=0
sub_disc_ests$M_disc_wt_lb[which(sub_disc_ests$year==1995)]=0

F_disc_wt_lb<-CPUE_nums$Obs_fem_CPUE*CPUE_nums$Fish_effort*Mean_wt$F_wt
M_disc_mort_wt_lb<-sub_disc_ests$M_disc_wt_lb*0.2
F_disc_mort_wt_lb<-F_disc_wt_lb*0.2
Tot_disc_mort_wt_lb<-M_disc_mort_wt_lb+F_disc_mort_wt_lb
disc_mort_rate<-Tot_disc_mort_wt_lb/CPUE_nums$ret_cat_lb

sub_disc_ests2<-cbind(year, F_disc_wt_lb, M_disc_mort_wt_lb, F_disc_mort_wt_lb, Tot_disc_mort_wt_lb, disc_mort_rate) 
sub_disc_ests2<-as.data.frame(sub_disc_ests2)
sub_disc_ests<-left_join(sub_disc_ests, sub_disc_ests2, by="year")

sub_disc_ests$Tot_disc_mort_wt_mill_lb<-sub_disc_ests$Tot_disc_mort_wt_lb/1000000

LM_tot_catch_wt_lb<-CPUE_nums$Obs_L_male_CPUE*CPUE_nums$Fish_effort*Mean_wt$Lm_wt
LM_disc_wt_lb<-LM_tot_catch_wt_lb-CPUE_nums$ret_cat_lb
sub_disc_ests<-cbind(sub_disc_ests, LM_tot_catch_wt_lb, LM_disc_wt_lb)
sub_disc_ests$LM_disc_wt_lb[which(sub_disc_ests$year==1994)]=0
sub_disc_ests$LM_disc_wt_lb[which(sub_disc_ests$year==1995)]=0
sub_disc_ests$LM_disc_rate<-sub_disc_ests$LM_disc_wt_lb/sub_disc_ests$LM_tot_catch_wt_lb
sub_disc_ests$LM_disc_rate[which(sub_disc_ests$year==1994)]=0
sub_disc_ests$LM_disc_rate[which(sub_disc_ests$year==1995)]=0

####LM_disc_mort_wt_lb<-LM_disc_wt_lb*0.2####
####LM_disc_mort_rate<-LM_disc_mort_wt_lb/CPUE_nums$ret_cat_lb####


write.csv(sub_disc_ests, "subtraction_disc_ests.csv")


###recompute LNR discards##########

M_subL_catch_wt_lb<-CPUE_nums$Obs_subL_male_CPUE*CPUE_nums$Fish_effort*Mean_wt$subLm_wt
M_LNR_catch_wt_lb<-CPUE_nums$Obs_LNR_male_CPUE*CPUE_nums$Fish_effort*Mean_wt$LNRm_wt
M_tot_catch_wt_lb<-M_subL_catch_wt_lb+M_LNR_catch_wt_lb
F_tot_catch_wt_lb<-CPUE_nums$Obs_fem_CPUE*CPUE_nums$Fish_effort*Mean_wt$F_wt
Tot_disc_wt_lb<-M_tot_catch_wt_lb+F_tot_catch_wt_lb
Tot_disc_mort_lb<-Tot_disc_wt_lb*0.2
Tot_disc_mort_mill_lb<-Tot_disc_mort_lb/1000000

year<-seq(1990, 2018, 1)
LNR_disc_ests<-cbind(year, M_subL_catch_wt_lb, M_LNR_catch_wt_lb, M_tot_catch_wt_lb, F_tot_catch_wt_lb, Tot_disc_wt_lb, Tot_disc_mort_lb, Tot_disc_mort_mill_lb)
LNR_disc_ests<-as.data.frame(LNR_disc_ests)
LNR_disc_ests<-left_join(LNR_disc_ests, catch, by="year")
LNR_disc_ests$disc_mort_rate<-LNR_disc_ests$Tot_disc_mort_lb/LNR_disc_ests$ret_cat_lb 
LNR_disc_ests<-subset(LNR_disc_ests, year<2018)

write.csv(LNR_disc_ests, "LNR_disc_ests.csv")

################################################################



#####plots#######
sub_pre_rat<-subset(sub_disc_ests, year<2005)
sub_post_rat<-subset(sub_disc_ests, year>2004)
LNR_pre_rat<-subset(LNR_disc_ests, year<2005)
LNR_post_rat<-subset(LNR_disc_ests, year>2004)

plot(sub_pre_rat$year, sub_pre_rat$disc_mort_rate, type="o", pch=16, lwd=2, col="blue", xlab="", xaxt="none", 
     ylab="lb bycatch mortality per lb retained catch", xlim=c(1990, 2018), ylim=c(0, 0.5), main="BBRKC discard mortality rate")
lines(sub_post_rat$year, sub_post_rat$disc_mort_rate, type="o", pch=16, lwd=2, col="red")
axis(1, seq(1990, 2018, 1), las=2)
lines(LNR_pre_rat$year, LNR_pre_rat$disc_mort_rate, type="o", pch=16, lwd=2, col="blue", lty=3)
lines(LNR_post_rat$year, LNR_post_rat$disc_mort_rate, type="o", pch=16, lwd=2, col="red", lty=3)

legend(2009, 0.49, legend=c("Subtraction", "LNR"), pch=16, lty=c(1,3), title="Estimation method", box.lty=0, col="black")

plot(sub_pre_rat$year, sub_pre_rat$Tot_disc_mort_wt_mill_lb, type="o", pch=16, lwd=2, col="blue", xlab="", xaxt="none", 
     ylab="Discard mortality (million lb)", xlim=c(1990, 2018), ylim=c(0, 5), main="BBRKC discard mortality", lty=1)
lines(sub_post_rat$year, sub_post_rat$Tot_disc_mort_wt_mill_lb, type="o", pch=16, lwd=2, col="red", lty=1)
lines(LNR_pre_rat$year, LNR_pre_rat$Tot_disc_mort_mill_lb, type="o", pch=16, lwd=2, col="blue", lty=3)
lines(LNR_post_rat$year, LNR_post_rat$Tot_disc_mort_mill_lb, type="o", pch=16, lwd=2, col="red", lty=3)
axis(1, seq(1990, 2018, 1), las=2)

legend(2009, 4.9, legend=c("Subtraction", "LNR"), pch=16, lty=c(1,3), title="Estimation method", box.lty=0, col="black")

plot(sub_pre_rat$year, sub_pre_rat$LM_disc_rate, type="o", pch=16, lwd=2, col="blue", xlab="", xaxt="none", 
     ylab="Legal discards/Total legal catch", xlim=c(1990, 2018), ylim=c(-0.5, 0.5), main="BBRKC Legal discard rate: subtraction method")
lines(sub_post_rat$year, sub_post_rat$LM_disc_rate, type="o", pch=16, lwd=2, col="red")
axis(1, seq(1990, 2018, 1), las=2)
abline(h=0, col="black", lty=1)



##############explore sublegal+legal CPUE trends##########

subL_ratio<-CPUE_nums$Obs_num_subL_male/CPUE_nums$Obs_num_male
year<-seq(1990, 2018, 1)


plot(year, subL_ratio, type="o", pch=16, lwd=2, col="black", xlab="", xaxt="none", 
     ylab="subL_M/Total_M", xlim=c(1990, 2018), ylim=c(0, 1), main="BBRKC Observer pots: subL_male/total male", lty=1)
axis(1, seq(1990, 2018, 1), las=2)

data_dump_SC<-subset(data_dump, sex==1 & legal==1)
data_dump_SC<-aggregate(data_dump_SC$year~year+shell,  data=data_dump_SC, FUN=length)
names(data_dump_SC)[3]<-"count"
data_dump_SC<-subset(data_dump_SC, shell>-1)
data_dump_SC$new_old<-ifelse(data_dump_SC$shell==0 | data_dump_SC$shell==1 | data_dump_SC$shell==9 | data_dump_SC$shell==2,"new","old")
data_dump_SC<-aggregate(data_dump_SC$count~year+new_old, data=data_dump_SC, FUN=sum)
names(data_dump_SC)[3]<-"count"
OS<-subset(data_dump_SC, new_old=="old")
NS<-subset(data_dump_SC, new_old=="new")
ALL<-OS$count+NS$count
ratio_OS<-OS$count/ALL
Y<-seq(1990, 2018, 1)
Y<-Y[Y!=c(1994,1995)]

plot(Y, ratio_OS, type="o", pch=16, lwd=2, col="black", xlab="", xaxt="none", 
     ylab="Legal_OS/All legal", xlim=c(1990, 2018), ylim=c(0, 0.5), main="BBRKC Observer pots: Legal_OS/All legal", lty=1)
axis(1, seq(1990, 2018, 1), las=2)




lines(Y, OS_sel$dock_ratio_OS, type="o", pch=16, lwd=2, col="black", lty=3)

dock_data<-read.csv("RKC-1990-2018_dockside.csv")
X<-str_sub(dock_data$fishery, 1,1)
dir_CR<-ifelse(X=="X", "CR", ifelse(X=="Q", "NA", "directed"))
x<-dock_data$fishery
y<-str_sub(x, 3)
Y<-as.numeric(as.character(y))
yy<-ifelse(y>18, 1900, 2000)
year<-Y+yy
dock_data<-cbind(dock_data, dir_CR, year)
dock_data$new_old<-ifelse(dock_data$shell==0 | dock_data$shell==1 | dock_data$shell==9 | dock_data$shell==2,"new","old")
dock_data<-aggregate(dock_data$numcrab~year+new_old, data=dock_data, FUN=sum)
names(dock_data)[3]<-"count"

dock_OS<-subset(dock_data, new_old=="old")
dock_NS<-subset(dock_data, new_old=="new")
dock_ALL<-dock_OS$count+dock_NS$count
dock_ratio_OS<-dock_OS$count/dock_ALL
Y<-seq(1990, 2018, 1)
Y<-Y[Y!=c(1994,1995)]
plot(Y, ratio_OS, type="o", pch=16, lwd=2, col="black", xlab="", xaxt="none", 
     ylab="Legal_OS/All legal", xlim=c(1990, 2018), ylim=c(0, 0.9), main="BBRKC ratio: Legal_OS/All legal", lty=1)
axis(1, seq(1990, 2018, 1), las=2)
lines(Y,dock_ratio_OS, type="o", pch=16, lwd=2, col="black", xlab="", lty=3)

#####OS selectivity calcs######


surv_data<-read.csv("survey_L_OS.csv")
surv_data<-subset(surv_data, year>1989)
surv_data<-surv_data[!surv_data$year %in% c(1994,1995),]
lines(surv_data$year, surv_data$surv_prop_L_OS, type="o", pch=16, lwd=2, col="red", xlab="", lty=1)

legend(2009, 0.89, legend=c("At-sea obs. pots", "dockside", "NOAA survey"), pch=16, col=c("black", "black", "red"), lty=c(1,3, 1), title="", box.lty=0)


OS_sel<-cbind(surv_data, ratio_OS, dock_ratio_OS)
OS_sel$OS_sel<-OS_sel$dock_ratio_OS*(OS_sel$surv_prop_L_OS-1)/(OS_sel$surv_prop_L_OS*OS_sel$dock_ratio_OS-OS_sel$surv_prop_L_OS)

plot(Y, OS_sel$OS_sel, type="o", pch=16, lwd=2, col="black", xlab="", xaxt="none", 
     ylab="Oldshell selectivity", xlim=c(1990, 2018), ylim=c(0, 1.5), main="BBRKC oldshell selectivity (retained catch)", lty=1)
axis(1, seq(1990, 2018, 1), las=2)
m<-mean(OS_sel$OS_sel)
abline(h=m, col="red", lty=3)

##########estimate vessel level discards############

Psum_data<-read.csv("RKC-1990-2018_potsum.csv")
x<-str_sub(Psum_data$fishery, 2,2)
sp_fish<-ifelse(x=="R","RKC", "Tanner")
X<-str_sub(Psum_data$fishery, 1,1)
dir_CR<-ifelse(X=="X", "CR", ifelse(X=="Q", "NA", "directed"))
Psum_data<-cbind(Psum_data, sp_fish, dir_CR)
Psum_data<-Psum_data[!Psum_data$dir_CR=="NA",]##removes non-BB fisheries###
Psum_data<-subset(Psum_data, sp_fish=="RKC")
total_male<-Psum_data$sublegal+Psum_data$tot_legal
Psum_data<-cbind(Psum_data, total_male)

x<-Psum_data$fishery
y<-str_sub(x, 3)
Y<-as.numeric(as.character(y))
yy<-ifelse(y>18, 1900, 2000)
year<-Y+yy
Psum_data<-cbind(Psum_data, year)
CY<-2018
Psum_data<-subset(Psum_data, year==CY)
Obs_effort_ves<-aggregate(Psum_data$year~adfg, data=Psum_data, FUN=length)
names(Obs_effort_ves)[2]<-"Obs_effort_ves"

Obs_num_fem<-aggregate(Psum_data$female~adfg, data=Psum_data, FUN=sum)
names(Obs_num_fem)[2]<-"Obs_num_fem"
Obs_num_male<-aggregate(Psum_data$total_male~adfg, data=Psum_data, FUN=sum)
names(Obs_num_male)[2]<-"Obs_num_male"
Obs_num_subL_male<-aggregate(Psum_data$sublegal~adfg, data=Psum_data, FUN=sum)
names(Obs_num_subL_male)[2]<-"Obs_num_subL_male"
Obs_num_LNR_male<-aggregate(Psum_data$legal_nr~adfg, data=Psum_data, FUN=sum)
names(Obs_num_LNR_male)[2]<-"Obs_num_LNR_male"
names(Obs_num_subL_male)[2]<-"Obs_num_subL_male"
Obs_num_L_male<-aggregate(Psum_data$tot_legal~adfg, data=Psum_data, FUN=sum)
names(Obs_num_L_male)[2]<-"Obs_num_L_male"

Obs_fem_CPUE<-Obs_num_fem$Obs_num_fem/Obs_effort_ves$Obs_effort_ves
Obs_male_CPUE<-Obs_num_male$Obs_num_male/Obs_effort_ves$Obs_effort_ves
Obs_subL_male_CPUE<-Obs_num_subL_male$Obs_num_subL_male/Obs_effort_ves$Obs_effort_ves
Obs_LNR_male_CPUE<-Obs_num_LNR_male$Obs_num_LNR_male/Obs_effort_ves$Obs_effort_ves
Obs_L_male_CPUE<-Obs_num_L_male$Obs_num_L_male/Obs_effort_ves$Obs_effort_ves

CPUE_nums<-cbind(Obs_effort_ves, Obs_num_fem$Obs_num_fem, Obs_fem_CPUE, Obs_num_male$Obs_num_male, Obs_male_CPUE, Obs_num_subL_male$Obs_num_subL_male, Obs_subL_male_CPUE, Obs_num_LNR_male$Obs_num_LNR_male, Obs_LNR_male_CPUE, Obs_num_L_male$Obs_num_L_male, Obs_L_male_CPUE) 

names(CPUE_nums)[3]<-"Obs_num_fem"
names(CPUE_nums)[5]<-"Obs_num_male"
names(CPUE_nums)[7]<-"Obs_num_subL_male"
names(CPUE_nums)[9]<-"Obs_num_LNR_male"
names(CPUE_nums)[11]<-"Obs_num_L_male"

vessels<-read.csv("Vessels.csv")
FT_vessel<-read.csv("FT_vessel.csv")
FT_vessel$adfg<-vessels$adfg[match(FT_vessel$vessel, vessels$vessel)]
CPUE_nums$ret_cat_lb<-FT_vessel$ret_cat_lb[match(CPUE_nums$adfg, FT_vessel$adfg)]
CPUE_nums$effort<-FT_vessel$effort[match(CPUE_nums$adfg, FT_vessel$adfg)]
CPUE_nums$name<-vessels$vessel[match(CPUE_nums$adfg, FT_vessel$adfg)]

names(CPUE_nums)[14]<-"Fish_effort_ves"


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

wt_kg<-ifelse(data_dump$sex==1, ((0.000403*data_dump$size^3.141334)/1000), ifelse(data_dump$clutch==-9, ((0.000408*data_dump$size^3.127956)/1000), ((0.003593*data_dump$size^2.666076)/1000)))
data_dump<-cbind(data_dump, wt_kg)
wt_lb<-data_dump$wt_kg*2.20462262
data_dump<-cbind(data_dump, wt_lb)

data_dump<-subset(data_dump, sex==1 & year==2018)
M_mean_wt<-aggregate(wt_lb~adfg, data=data_dump, FUN=mean)
data_dump<-subset(data_dump, sex==1 & year==2018 & legal==1)
LM_mean_wt<-aggregate(wt_lb~adfg, data=data_dump, FUN=mean)

CPUE_nums<-left_join(CPUE_nums, M_mean_wt, by="adfg")
CPUE_nums<-left_join(CPUE_nums, LM_mean_wt, by="adfg")
names(CPUE_nums)[16]<-"M_mean_wt"
names(CPUE_nums)[17]<-"LM_mean_wt"

CPUE_nums$M_tot_catch_wt_lb<-CPUE_nums$Obs_male_CPUE*CPUE_nums$Fish_effort_ves*CPUE_nums$M_mean_wt
CPUE_nums$M_disc_wt_lb<-CPUE_nums$M_tot_catch_wt_lb-CPUE_nums$ret_cat_lb
CPUE_nums$M_disc_rate<-CPUE_nums$M_disc_wt_lb/CPUE_nums$M_tot_catch_wt_lb
CPUE_nums$M_disc_mort_lb<-CPUE_nums$M_disc_wt_lb*0.2
CPUE_nums$M_disc_mort_rate<-CPUE_nums$M_disc_mort_lb/CPUE_nums$ret_cat_lb

CPUE_nums$LM_tot_catch_wt_lb<-CPUE_nums$Obs_L_male_CPUE*CPUE_nums$Fish_effort_ves*CPUE_nums$LM_mean_wt
CPUE_nums$LM_disc_wt_lb<-CPUE_nums$LM_tot_catch_wt_lb-CPUE_nums$ret_cat_lb
CPUE_nums$LM_disc_rate<-CPUE_nums$LM_disc_wt_lb/CPUE_nums$LM_tot_catch_wt_lb
CPUE_nums$LM_disc_mort_lb<-CPUE_nums$LM_disc_wt_lb*0.2
CPUE_nums$LM_disc_mort_rate<-CPUE_nums$LM_disc_mort_lb/CPUE_nums$ret_cat_lb

write.csv(CPUE_nums, "vessel_disc_ests.csv")

par(mar=c(7,4,4,2))  ##+0.1,mgp=c(5,1,0))###
barplot(CPUE_nums$LM_disc_rate, names.arg=CPUE_nums$adfg, las=2, main="2018/19 Legal discard rate", 
        ylab="Legal discards/Total legal catch", xlab="", col="red")
title(xlab="vessel code", cex.lab=1.5, line=4.5)

par(mar=c(7,4,4,2))  ##+0.1,mgp=c(5,1,0))###
barplot(CPUE_nums$M_disc_rate, names.arg=CPUE_nums$adfg, las=2, main="2018/19 Male discard rate", 
        ylab="Male discards/Total male catch", xlab="", col="red", ylim=c(-0.15, 0.6))
title(xlab="vessel code", cex.lab=1.5, line=4.5)

par(mar=c(7,4,4,2))  ##+0.1,mgp=c(5,1,0))###
CPUEs<-cbind(CPUE_nums$Obs_subL_male_CPUE, CPUE_nums$Obs_L_male_CPUE)
CPUEs<-t(CPUEs)
rownames(CPUEs)=c("Sublegal CPUE", "Legal CPUE")
colnames(CPUEs)=CPUE_nums$adfg
barplot(CPUEs, col=c("darkblue", "red"), border="white", beside=T, main="2018/19 Observer-pot CPUE", legend=rownames(CPUEs), xlab="", ylim=c(0, 70), ylab="CPUE", las=2, font.lab=2)
title(xlab="Vessel code", cex.lab=1, font.lab=2, line=4.5)



