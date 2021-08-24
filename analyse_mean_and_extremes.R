library(ggplot2)
library(timeDate)
library(data.table)
library(tidyverse)

# load the ncdf4 package
library(ncdf4)

# le but est d'afficher les valeurs moyennes pour chaque pixel ("station") par mois 
# et les probas de depassement de seuil et de comparer les 3 scenarios: climat 2000, 2050 RCP45 et RCP85

yrstart=2050
yrstop=2079

pixel_name = "tubuai"
#hereheretue tubuai maupiti teahupoo

# set path and filename pour climat 2000
ncpath <- "/Users/gablellouch/Documents/Meteo_France/ENM/ienm3/houles_dirpf/postprocessing/analyse_4pixels/"
ncname <- paste("h2T_PolynsGL_",pixel_name,".nc",sep="")  
ncfname <- paste(ncpath, ncname, sep="")
dname <- "energy" 

# path figures
figpath <- "/Users/gablellouch/Documents/Meteo_France/ENM/ienm3/houles_dirpf/Figures/postprocessing/analyse_4pixels/"

# open a netCDF file
ncin <- nc_open(ncfname)
#print(ncin)

# get time
time <- ncvar_get(ncin,"time")
#time

tunits <- ncatt_get(ncin,"time","units")
nt <- dim(time)

time_posixct <- seq(ISOdatetime(2050,1,1,3,0,0,tz="UTC"),ISOdatetime(2079,12,31,21,0,0,tz="UTC"), by="3 hours")

# get h2T
h2T <- ncvar_get(ncin,dname)
dim(h2T)

# make dataframe
df = data.frame(time,time_posixct,h2T)

df$month = month(time_posixct)
df$month_abb=month.abb[df$month]

df$month_abb <-factor(df$month_abb, 
                      levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                 "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

# Moyennes mensuelles sur la chronique
df_mean = aggregate(h2T ~ month_abb,df,mean)
df_mean$Exp = "Climat 2000"

# Max mensuelles sur la chronique
df_max = aggregate(h2T ~ month_abb,df,max)
df_max$Exp = "Climat 2000"

# Preparation a la probabilite de depassement
df$over160 = 0
df[df$h2T>=160,"over160"] = 1
df_proba_160 = aggregate(over160 ~ month_abb,df,mean)
df_proba_160$Exp = "Climat 2000"

df$over224 = 0
df[df$h2T>=224,"over224"] = 1
df_proba_224 = aggregate(over224 ~ month_abb,df,mean)
df_proba_224$Exp = "Climat 2000"

df$over288 = 0
df[df$h2T>=288,"over288"] = 1
df_proba_288 = aggregate(over288 ~ month_abb,df,mean)
df_proba_288$Exp = "Climat 2000"

# climat 2050, RCP45
ncname_rcp45 <- paste("h2T_PolynsGL_",pixel_name,"_rcp45.nc",sep="")  
ncfname_rcp45 <- paste(ncpath, ncname_rcp45, sep="")
ncin_rcp45 <- nc_open(ncfname_rcp45)
h2T_rcp45 <- ncvar_get(ncin_rcp45,dname)
df_rcp45 = data.frame(time,time_posixct,h2T_rcp45)
colnames(df_rcp45)[3] = "h2T"

df_rcp45$month = month(time_posixct)
df_rcp45$month_abb=month.abb[df_rcp45$month]
df_rcp45$month_abb <-factor(df_rcp45$month_abb, 
                      levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                 "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

# Moyennes mensuelles sur la chronique
df_rcp45_mean = aggregate(h2T ~ month_abb,df_rcp45,mean)
df_rcp45_mean$Exp = "RCP4.5 2050"

# Max mensuelles sur la chronique
df_rcp45_max = aggregate(h2T ~ month_abb,df_rcp45,max)
df_rcp45_max$Exp = "RCP4.5 2050"

# Preparation a la probabilite de depassement
df_rcp45$over160 = 0
df_rcp45[df_rcp45$h2T>=160,"over160"] = 1
df_rcp45_proba_160 = aggregate(over160 ~ month_abb,df_rcp45,mean)
df_rcp45_proba_160$Exp = "RCP4.5 2050"

df_rcp45$over224 = 0
df_rcp45[df_rcp45$h2T>=224,"over224"] = 1
df_rcp45_proba_224 = aggregate(over224 ~ month_abb,df_rcp45,mean)
df_rcp45_proba_224$Exp = "RCP4.5 2050"

df_rcp45$over288 = 0
df_rcp45[df_rcp45$h2T>=288,"over288"] = 1
df_rcp45_proba_288 = aggregate(over288 ~ month_abb,df_rcp45,mean)
df_rcp45_proba_288$Exp = "RCP4.5 2050"


# climat 2050, RCP85
ncname_rcp85 <- paste("h2T_PolynsGL_",pixel_name,"_rcp85.nc",sep="")  
ncfname_rcp85 <- paste(ncpath, ncname_rcp85, sep="")
ncin_rcp85 <- nc_open(ncfname_rcp85)
h2T_rcp85 <- ncvar_get(ncin_rcp85,dname)
df_rcp85 = data.frame(time,time_posixct,h2T_rcp85)
colnames(df_rcp85)[3] = "h2T"

df_rcp85$month = month(time_posixct)
df_rcp85$month_abb=month.abb[df_rcp85$month]
df_rcp85$month_abb <-factor(df_rcp85$month_abb, 
                            levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                       "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

# Moyennes mensuelles sur la chronique
df_rcp85_mean = aggregate(h2T ~ month_abb,df_rcp85,mean)
df_rcp85_mean$Exp = "RCP8.5 2050"

# Max mensuelles sur la chronique
df_rcp85_max = aggregate(h2T ~ month_abb,df_rcp85,max)
df_rcp85_max$Exp = "RCP8.5 2050"

# Preparation a la probabilite de depassement
df_rcp85$over160 = 0
df_rcp85[df_rcp85$h2T>=160,"over160"] = 1
df_rcp85_proba_160 = aggregate(over160 ~ month_abb,df_rcp85,mean)
df_rcp85_proba_160$Exp = "RCP8.5 2050"

df_rcp85$over224 = 0
df_rcp85[df_rcp85$h2T>=224,"over224"] = 1
df_rcp85_proba_224 = aggregate(over224 ~ month_abb,df_rcp85,mean)
df_rcp85_proba_224$Exp = "RCP8.5 2050"

df_rcp85$over288 = 0
df_rcp85[df_rcp85$h2T>=288,"over288"] = 1
df_rcp85_proba_288 = aggregate(over288 ~ month_abb,df_rcp85,mean)
df_rcp85_proba_288$Exp = "RCP8.5 2050"

# prepare les plots
df_mean_ALL = rbind(df_mean,df_rcp45_mean,df_rcp85_mean)
df_max_ALL = rbind(df_max,df_rcp45_max,df_rcp85_max)
df_proba_160_ALL = rbind(df_proba_160,df_rcp45_proba_160,df_rcp85_proba_160)
df_proba_224_ALL = rbind(df_proba_224,df_rcp45_proba_224,df_rcp85_proba_224)
df_proba_288_ALL = rbind(df_proba_288,df_rcp45_proba_288,df_rcp85_proba_288)


# Plots
gg=ggplot(df_mean_ALL,aes(x=month_abb,y=h2T,group=Exp,fill=Exp,color=Exp)) +
  geom_point(shape=21,size=3,colour="black") +
  scale_fill_manual(values=c("black","blue","red")) +
  geom_line() +
  scale_color_manual(values=c("black","blue","red")) +
  ggtitle(paste("MEAN Hs²Tm: ",str_to_upper(pixel_name),sep="")) +
  xlab("DATE") + ylab("Hs²Tm (m²s)") +
  ylim(20, 90) + 
  theme(text = element_text(size=20),legend.title=element_blank(),legend.position = c(10.2,10.8),legend.direction = "vertical" )
print(gg)
ggsave(gg, file=paste(figpath,"ts_mean_h2T_",pixel_name,"_",yrstart,"_",yrstop,"_v1.png",sep=""), width = 18, height = 12, units = "cm")

ggp160=ggplot(df_proba_160_ALL,aes(x=month_abb,y=over160,group=Exp,fill=Exp,color=Exp)) +
  geom_point(shape=21,size=3,colour="black") +
  scale_fill_manual(values=c("black","blue","red")) +
  geom_line() +
  scale_color_manual(values=c("black","blue","red")) +
  ggtitle(paste("p(Hs²Tm>160): ",str_to_upper(pixel_name),sep="")) +
  xlab("DATE") + ylab("LIKELIHOOD") +
  ylim(0, 0.018) + 
  theme(text = element_text(size=20),legend.title=element_blank(),legend.position = c(0.2,0.8),legend.direction = "vertical" )
print(ggp160)
ggsave(ggp160, file=paste(figpath,"ts_proba_over_160_h2T_",pixel_name,"_",yrstart,"_",yrstop,"_v1.png",sep=""), width = 18, height = 12, units = "cm")

ggp224=ggplot(df_proba_224_ALL,aes(x=month_abb,y=over224,group=Exp,fill=Exp,color=Exp)) +
  geom_point(shape=21,size=3,colour="black") +
  scale_fill_manual(values=c("black","blue","red")) +
  geom_line() +
  scale_color_manual(values=c("black","blue","red")) +
  ggtitle(paste("p(Hs²Tm>224): ",str_to_upper(pixel_name),sep="")) +
  xlab("DATE") + ylab("LIKELIHOOD") +
  ylim(0, 0.018) + 
  theme(text = element_text(size=20),legend.position = c(10.2,10.8),legend.direction = "vertical" )
print(ggp224)
ggsave(ggp224, file=paste(figpath,"ts_proba_over_224_h2T_",pixel_name,"_",yrstart,"_",yrstop,"_v1.png",sep=""), width = 18, height = 12, units = "cm")

ggp288=ggplot(df_proba_288_ALL,aes(x=month_abb,y=over288,group=Exp,fill=Exp,color=Exp)) +
  geom_point(shape=21,size=3,colour="black") +
  scale_fill_manual(values=c("black","blue","red")) +
  geom_line() +
  scale_color_manual(values=c("black","blue","red")) +
  ggtitle(paste("p(Hs²Tm>288): ",str_to_upper(pixel_name),sep="")) +
  xlab("DATE") + ylab("LIKELIHOOD") +
  ylim(0, 0.018) + 
  theme(text = element_text(size=20),legend.position = c(10.2,10.8),legend.direction = "vertical" )
print(ggp288)
ggsave(ggp288, file=paste(figpath,"ts_proba_over_288_h2T_",pixel_name,"_",yrstart,"_",yrstop,"_v1.png",sep=""), width = 18, height = 12, units = "cm")

