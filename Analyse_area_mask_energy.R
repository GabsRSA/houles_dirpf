library(ggplot2)
library(mapdata)
library(timeDate)
library(data.table)
library(tidyverse)
library(utils)

# extract hist info
## Convenience function
get_hist <- function(p) {
  d <- ggplot_build(p)$data[[1]]
  data.frame(x = d$x, xmin = d$xmin, xmax = d$xmax, y = d$y)
}

# pick time series peaks (i did a function to reuse it) 
ts_peaks <- function(ts,time,time_posixct,nino34) {
  i = 1
  j = 1
  ind_start = c()
  ind_stop = c()
  N_peak = c()
  ind_max = c()
  while(i < dim(ts)) {
    if(ts[i]!=0){
      ind_start[j] = i
      i = i+1
      while(ts[i]!=0){
        i = i+1
      }
      ind_stop[j] = i
      N_peak[j] = max(ts[ind_start[j]:ind_stop[j]])
      ind_max[j] = ind_start[j] -1 + which.max(ts[ind_start[j]:ind_stop[j]])
      j = j+1}
    i = i+1
  }

  time_posixct_event = time_posixct[ind_max]
  # find if event is nino or nina
  time_yrmo_event = format(time_posixct_event,"%Y-%m")
  time_yrmo_nino34 = format(nino34[["time_posixct"]],"%Y-%m")
  ind = c()
  for (i in seq(1,length(time_yrmo_event))){
    ind[i] = which(time_yrmo_nino34==time_yrmo_event[i])
  }
  Episode = nino34[ind,c(7)] # column 7 is the episode type : nino/nina/Neutre
  
  dataDate = format(time_posixct_event,"%Y%m%d")
  dataTime = paste(as.numeric(format(time_posixct_event,"%H")),"00",sep="")
  # ind_max is used for mask.nc and time_max for output.grib/outpul_PolynsGL.grib
  time_max = time[ind_max] 
  time_start = time[ind_start] 
  time_stop = time[ind_stop] 
  delta_event = ind_stop - ind_start + 1
  
  df_event = data.frame(N_peak,ind_max,ind_start,ind_stop,delta_event,time_posixct_event,time_max,time_start,time_stop,dataDate,dataTime,Episode)
  df_event$month = month(time_posixct_event)
  df_event$month_abb=month.abb[df_event$month]
  
  df_event$month_abb <-factor(df_event$month_abb, 
                              levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                         "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
  return(df_event)
}


# load the ncdf4 package
library(ncdf4)

yrstart=2050
yrstop=2079

# Analyse du fichier sum_mask_timpctl99_h2T_PolynsGL (sur 2050-2079)
# L'objectif etait d'abord d'identifier les evenenements intenses en termes d'energie pour les comparer avec ceux obtenus avec SWH

# Maintenant l objectif est de faire la comparaison avec les fichiers sum_mask_timpctl99_h2T_PolynsGL_rcp45_vs_clim2000.nc 
# et sum_mask_timpctl99_h2T_PolynsGL_rcp85_vs_clim2000.nc

# donnees climat 2000
ncpath <- "/Users/gablellouch/Documents/Meteo_France/ENM/ienm3/houles_dirpf/postprocessing/calib2_analyse_swh_hautes/2050_2079/"
ncname <- "sum_mask_timpctl99_h2T_PolynsGL.nc"
ncfname <- paste(ncpath, ncname, sep="")
dname <- "energy" 

# open netCDF file
ncin <- nc_open(ncfname)
time <- ncvar_get(ncin,"time")

# get h2T
n_mask_h2T<- ncvar_get(ncin,dname)
dim(n_mask_h2T)

# path figures
figpath <- paste("/Users/gablellouch/Documents/Meteo_France/ENM/ienm3/houles_dirpf/Figures/postprocessing/calib2_analyse_swh_hautes/",yrstart,"_",yrstop,"/",sep="")

# load nino34 index
nino34 = read.table(file="/Users/gablellouch/Documents/Meteo_France/ENM/ienm3/houles_dirpf/Belenos/Nino34/Info_NinoNina_CE0.txt",header=T,sep=";")
nino34["time_posixct"] = as.Date(nino34[["time_posixct"]],"%Y-%m-%d")

# define time vector
time_posixct <- seq(ISOdatetime(2050,1,1,3,0,0,tz="UTC"),ISOdatetime(2079,12,31,21,0,0,tz="UTC"), by="3 hours")

# all times and only peaks for n_mask_h2T (climat 2000)
df = data.frame(time,time_posixct,n_mask_h2T)
df_event = ts_peaks(n_mask_h2T,time,time_posixct,nino34)
df_event$Exp. = "2000"

# Filter out events shorter than 2 time stamps (2*3 = 6h)
df_event_filt_time = df_event[df_event$delta_event>2,]

# check
gg=ggplot(NULL) +
  geom_line(data=df,aes(x=time_posixct,y=n_mask_h2T), size=0.4, colour="red") +
  #  geom_point(data=df_event,aes(x=time_posixct_event,y=N_peak), shape = 21, colour = "black", fill = "blue", size = 2, stroke = 0.5) +
  ggtitle(paste("Mask area (PolynsGL) ",yrstart,"-",yrstop,sep="")) +
  xlab("Date") + ylab(paste("Mask size (Nb of pixels)",sep="")) +
#  ylim(0, 350) +
#  xlim(time_posixct[1368], time_posixct[1568]) + 
  #  ylim(0, 200) +
  #  xlim(time_posixct[800], time_posixct[900]) + 
  #  ylim(0, 1000) +
  #  xlim(time_posixct[1], time_posixct[2920]) + 
  #  ylim(0, 1500) +
   # xlim(time_posixct[72700], time_posixct[73700]) + # pour l evenement le plus long en temps
    xlim(time_posixct[12800], time_posixct[13600]) + # pour l evenement le plus fort en intensite
  theme(text = element_text(size=15))
#print(gg)

# calculate hist of delta event
hdelta=ggplot(df_event) +
  geom_histogram(aes(x=delta_event), breaks=seq(0,184,by=2), color="red", position="dodge", alpha=0.5) +
  labs(title=paste("Delta event",yrstart,"-",yrstop,sep=" "),x="Event duration (time unit)", y = "Count") +
  theme(text = element_text(size=15))
#print(hdelta)

# calculate hist
h=ggplot(df_event) +
  geom_histogram(aes(x=N_peak), breaks=seq(0,2800,by=2), color="red", position="dodge", alpha=0.5) +
  labs(title=paste("N peak",yrstart,"-",yrstop,sep=" "),x="N peak (m)", y = "Count") +
  theme(text = element_text(size=15))
#  print(h)

hfilt=ggplot(df_event_filt_time) +
  geom_histogram(aes(x=N_peak), breaks=seq(0,2800,by=2), color="blue", position="dodge", alpha=0.5) +
  labs(title=paste("N peak",yrstart,"-",yrstop,sep=" "),x="N peak (m)", y = "Count") +
  theme(text = element_text(size=15))
#  print(h)

# calculate the cumulative curve (ccdf)
dfhist = get_hist(h)
dfhist_filt = get_hist(hfilt)

dfhist["ycomp"]=sum(dfhist[,"y"])-cumsum(dfhist[,"y"])
dfhist_filt["ycomp"]=sum(dfhist_filt[,"y"])-cumsum(dfhist_filt[,"y"])

ggc = ggplot(NULL) + 
  geom_line(data=dfhist,aes(x=x, ycomp), size=0.4, colour="red") +
  geom_line(data=dfhist_filt,aes(x=x, ycomp), size=0.4, colour="blue") +
  #  geom_point(aes(x=x,y=ycomp), shape = 21, colour = "black", fill = "blue", size = 2, stroke = 0.5) +
  labs(title=paste("CCDF N peak",yrstart,"-",yrstop,sep=" "),x="N peak (m)", y = "Count") +
  theme(text = element_text(size=15))
#print(ggc)
#ggsave(ggc, file=paste(figpath,"ccdf_npeak_",yrstart,"_",yrstop,".png",sep=""), width = 12, height = 9, units = "cm")


# Filter out events with N_peak < 3 (to have about 30 events per year 900 per 30 years)
df_event_filt_time_h2T = df_event_filt_time[df_event_filt_time$N_peak>3,]

# Filter out events with N_peak < 1430 (to have about 30 events over the 30 years)
df_event_filt_time_h2T_red = df_event_filt_time[df_event_filt_time$N_peak>1430,]

# write date of intense events
#write.table(df_event_filt_time_h2T_red$time_posixct_event, paste("/Users/gablellouch/Documents/Meteo_France/ENM/ienm3/houles_dirpf/postprocessing/calib2_analyse_swh_hautes/time_32events_h2T.txt",sep=""), sep = ";", quote = T, col.names = T, row.names = F)
#write.table(df_event_filt_time_h2T$time_posixct_event, paste("/Users/gablellouch/Documents/Meteo_France/ENM/ienm3/houles_dirpf/postprocessing/calib2_analyse_swh_hautes/time_898events_h2T.txt",sep=""), sep = ";", quote = T, col.names = T, row.names = F)


# scatter plot : event duration vs event coverage 
colours = c("Nina"="blue","Nino"="red","Neutre"="black","NA"="grey")
colScale = scale_fill_manual(name="",values=colours,labels=c("Neutre","Niña","Niño","NA"))
ggs = ggplot(df_event) +
  geom_point(aes(x=delta_event,y=N_peak,fill=factor(Episode)), shape = 21, size = 2, stroke = 0.5, colour="black") +
  colScale +
  labs(fill="Nino3.4",title=paste("CLIMAT 2000"),x=expression(Delta[event]*" (time unit, 3h)"), y = expression("N"[peak])) +
  xlim(0,145) +
  ylim(0,3700) +
  theme(text = element_text(size=20), legend.title=element_blank(), legend.position = c(0.15,0.8),legend.direction = "vertical"  )
print(ggs)
ggsave(ggs, file=paste(figpath,"spat_vs_temp_",yrstart,"_",yrstop,"_h2T_2000_v1.png",sep=""), width = 15, height = 12, units = "cm")

ggsred = ggplot(df_event_filt_time_h2T_red) +
  geom_point(aes(x=delta_event,y=N_peak,fill=factor(Episode)), shape = 21, size = 2, stroke = 0.5, colour = "black") +
  colScale +
  labs(fill="Nino3.4",title=paste("Climat 2000: Intense swells",sep=" "),x="Duration (time unit, 3h)", y = "Coverage (nb of pixels)") +
  xlim(0,145) +
  ylim(0,3700) +
  theme(text = element_text(size=15), plot.subtitle = element_text(face="italic"))
print(ggsred)
ggsave(ggsred, file=paste(figpath,"spat_vs_temp_filtred_",yrstart,"_",yrstop,"_h2T_2000.png",sep=""), width = 15, height = 12, units = "cm")


gg=ggplot(NULL) +
  geom_line(data=df,aes(x=time_posixct,y=n_mask_h2T), size=0.4, colour="red") +
#  geom_point(data=df_event,aes(x=time_posixct_event,y=N_peak), shape = 21, colour = "black", fill = "blue", size = 2, stroke = 0.5) +
#  geom_point(data=df_event_filt_time_h2T,aes(x=time_posixct_event,y=N_peak), shape = 21, colour = "black", fill = "pink", size = 2, stroke = 0.5) +
  ggtitle(paste("Climat 2000 (PolynsGL) ",yrstart,"-",yrstop,sep="")) +
#  labs(subtitle="h2T") +
  xlab("Date") + ylab(paste("Mask area",sep="")) +
#  ylim(0, 1300) +
#  xlim(time_posixct[22060], time_posixct[22440]) + 
  theme(text = element_text(size=20), plot.subtitle = element_text(face="italic"))
print(gg)
ggsave(gg, file=paste(figpath,"ts_mask_h2T","_",yrstart,"_",yrstop,".png",sep=""), width = 12, height = 9, units = "cm")
#ggsave(gg, file=paste(figpath,"ts_mask_h2T","_",yrstart,"_",yrstop,"_zoom20570720_20570906.png",sep=""), width = 12, height = 9, units = "cm")
#ggsave(gg, file=paste(figpath,"ts_mask_h2T_peak","_",yrstart,"_",yrstop,"_zoom20570720_20570906.png",sep=""), width = 12, height = 9, units = "cm")
#ggsave(gg, file=paste(figpath,"ts_mask_h2T_filtpeak","_",yrstart,"_",yrstop,"_zoom20570720_20570906.png",sep=""), width = 12, height = 9, units = "cm")


# work on climat 2050 sur rcp45 et rcp85
# rcp45
ncname_rcp45 <- "sum_mask_timpctl99_h2T_PolynsGL_rcp45_vs_clim2000.nc"
ncfname_rcp45 <- paste("/Users/gablellouch/Documents/Meteo_France/ENM/ienm3/houles_dirpf/postprocessing/calib2_rcp45_rcp85/", ncname_rcp45, sep="")
# open netCDF file
ncin_rcp45 <- nc_open(ncfname_rcp45)
time_rcp45 <- ncvar_get(ncin_rcp45,"time")
# get h2T
n_mask_h2T_rcp45<- ncvar_get(ncin_rcp45,dname)
dim(n_mask_h2T_rcp45)
# path figures
figpath_rcp45_rcp85 <- paste("/Users/gablellouch/Documents/Meteo_France/ENM/ienm3/houles_dirpf/Figures/postprocessing/calib2_rcp45_rcp85/",sep="")
# load nino34 index
nino34_rcp45 = read.table(file="/Users/gablellouch/Documents/Meteo_France/ENM/ienm3/houles_dirpf/Belenos/Nino34/Info_NinoNina_CE1.txt",header=T,sep=";")
nino34_rcp45["time_posixct"] = as.Date(nino34[["time_posixct"]],"%Y-%m-%d")
# only peaks for n_mask_h2T_rcp45 (climat 2050 rcp45)
df_event_rcp45 = ts_peaks(n_mask_h2T_rcp45,time_rcp45,time_posixct,nino34_rcp45)
df_event_rcp45$Exp. = "RCP4.5 2050"
# filter by duration min, and N_peak values
df_event_rcp45_filt_time = df_event_rcp45[df_event_rcp45$delta_event>2,]
df_event_rcp45_filt_time_h2T = df_event_rcp45_filt_time[df_event_rcp45_filt_time$N_peak>3,]
df_event_rcp45_filt_time_h2T_red = df_event_rcp45_filt_time[df_event_rcp45_filt_time$N_peak>1430,]

# scatter plot : event duration vs event coverage 
colours = c("Nina"="blue","Nino"="red","Neutre"="black","NA"="grey")
colScale = scale_fill_manual(name="Episode",values=colours,labels=c("Neutre","Niña","Niño","NA"))
ggs_rcp45 = ggplot(df_event_rcp45) +
  geom_point(aes(x=delta_event,y=N_peak,fill=factor(Episode)), shape = 21, size = 2, stroke = 0.5, colour="black") +
  colScale +
  labs(fill="Nino3.4",title=paste("CLIMAT 2050 RCP4.5"),x=expression(Delta[event]*" (time unit, 3h)"), y = expression("N"[peak])) +
  xlim(0,145) +
  ylim(0,3700) +
  theme(text = element_text(size=20), legend.title=element_blank(), legend.position = c(10.15,10.8),legend.direction = "vertical"  )
print(ggs_rcp45)
ggsave(ggs_rcp45, file=paste(figpath,"spat_vs_temp_",yrstart,"_",yrstop,"_h2T_2050_RCP45_v1.png",sep=""), width = 15, height = 12, units = "cm")

ggsred_rcp45 = ggplot(df_event_rcp45_filt_time_h2T_red) +
  geom_point(aes(x=delta_event,y=N_peak,fill=factor(Episode)), shape = 21, size = 2, stroke = 0.5, colour = "black") +
  colScale +
  labs(fill="Nino3.4",title=paste("Climat 2050 RCP4.5: Intense swells",sep=" "),x="Duration (time unit, 3h)", y = "Coverage (nb of pixels)") +
  xlim(0,145) +
  ylim(0,3700) +
  theme(text = element_text(size=15), plot.subtitle = element_text(face="italic"))
print(ggsred_rcp45)
ggsave(ggsred_rcp45, file=paste(figpath,"spat_vs_temp_filtred_",yrstart,"_",yrstop,"_h2T_2050_RCP45.png",sep=""), width = 15, height = 12, units = "cm")


# rcp85
ncname_rcp85 <- "sum_mask_timpctl99_h2T_PolynsGL_rcp85_vs_clim2000.nc"
ncfname_rcp85 <- paste("/Users/gablellouch/Documents/Meteo_France/ENM/ienm3/houles_dirpf/postprocessing/calib2_rcp45_rcp85/", ncname_rcp85, sep="")
# open netCDF file
ncin_rcp85 <- nc_open(ncfname_rcp85)
time_rcp85 <- ncvar_get(ncin_rcp85,"time")
# get h2T
n_mask_h2T_rcp85<- ncvar_get(ncin_rcp85,dname)
dim(n_mask_h2T_rcp85)
# load nino34 index
nino34_rcp85 = read.table(file="/Users/gablellouch/Documents/Meteo_France/ENM/ienm3/houles_dirpf/Belenos/Nino34/Info_NinoNina_CE2.txt",header=T,sep=";")
nino34_rcp85["time_posixct"] = as.Date(nino34[["time_posixct"]],"%Y-%m-%d")
# only peaks for n_mask_h2T_rcp85 (climat 2050 rcp85)
df_event_rcp85 = ts_peaks(n_mask_h2T_rcp85,time_rcp85,time_posixct,nino34_rcp85)
df_event_rcp85$Exp. = "RCP8.5 2050"
# filter by duration min, and N_peak values
df_event_rcp85_filt_time = df_event_rcp85[df_event_rcp85$delta_event>2,]
df_event_rcp85_filt_time_h2T = df_event_rcp85_filt_time[df_event_rcp85_filt_time$N_peak>3,]
df_event_rcp85_filt_time_h2T_red = df_event_rcp85_filt_time[df_event_rcp85_filt_time$N_peak>1430,]

colours = c("Nina"="blue","Nino"="red","Neutre"="black","NA"="grey")
colScale = scale_fill_manual(name="Episode",values=colours,labels=c("Neutre","Niña","Niño","NA"))
ggs_rcp85 = ggplot(df_event_rcp85) +
  geom_point(aes(x=delta_event,y=N_peak,fill=factor(Episode)), shape = 21, size = 2, stroke = 0.5, colour="black") +
  colScale +
  labs(fill="Nino3.4",title=paste("CLIMAT 2050 RCP8.5"),x=expression(Delta[event]*" (time unit, 3h)"), y = expression("N"[peak])) +
  xlim(0,145) +
  ylim(0,3700) +
  theme(text = element_text(size=20), legend.title=element_blank(), legend.position = c(10.15,10.8),legend.direction = "vertical"  )
print(ggs_rcp85)
ggsave(ggs_rcp85, file=paste(figpath,"spat_vs_temp_",yrstart,"_",yrstop,"_h2T_2050_RCP85_v1.png",sep=""), width = 15, height = 12, units = "cm")

ggsred_rcp85 = ggplot(df_event_rcp85_filt_time_h2T_red) +
  geom_point(aes(x=delta_event,y=N_peak,fill=factor(Episode)), shape = 21, size = 2, stroke = 0.5, colour = "black") +
  colScale +
  labs(fill="Nino3.4",title=paste("Climat 2050 RCP8.5: Intense swells",sep=" "),x="Duration (time unit, 3h)", y = "Coverage (nb of pixels)") +
  xlim(0,145) +
  ylim(0,3700) +
  theme(text = element_text(size=15), plot.subtitle = element_text(face="italic"))
print(ggsred_rcp85)
ggsave(ggsred_rcp85, file=paste(figpath,"spat_vs_temp_filtred_",yrstart,"_",yrstop,"_h2T_2050_RCP85.png",sep=""), width = 15, height = 12, units = "cm")


# preparation pour les graphes: reunion des dataframe
df_event_ALL = rbind(df_event,df_event_rcp45,df_event_rcp85)
df_event_filt_time_h2T_ALL = rbind(df_event_filt_time_h2T,df_event_rcp45_filt_time_h2T,df_event_rcp85_filt_time_h2T)
df_event_filt_time_h2T_red_ALL = rbind(df_event_filt_time_h2T_red,df_event_rcp45_filt_time_h2T_red,df_event_rcp85_filt_time_h2T_red)

# plot bar graph, all events
colours = c("Nina"="blue","Nino"="red","Neutre"="black","NA"="grey")

# plot histograms
gghist1 = ggplot(df_event_ALL, aes(x=month_abb,fill=Episode)) +
  geom_bar(stat = "count",position="dodge") +
  scale_x_discrete(drop=FALSE) + 
  scale_fill_manual(name="",values=colours,labels=c("Neutre","Niña","Niño","NA"),drop=FALSE) +
  labs(title="ALL INTENSE SWELLS", x = "Month", y = "Count")  +
  ylim(0,170) +
  theme(text = element_text(size=20)) +
  geom_text(stat='count', position=position_dodge(width=0.95), aes(label=..count.., y=..count..), size=5, vjust=-0.5) + 
  facet_grid(vars(Exp.))
print(gghist1)
ggsave(gghist1, file=paste(figpath_rcp45_rcp85,"1hist_intense_swells_nino_nina","_",yrstart,"_",yrstop,"_v1.png",sep=""), width = 36, height = 24, units = "cm")

# calcul des valeurs affichées par l'hist pour mettre dans le rapport
# 2000
## total
# annee
81+46+58+73+86+122+151+140+102+68+63+64
# hiver austral MJJAS
86+122+151+140+102
# ete austral ONDJFMA
68+63+64+81+46+58+73

## neutre
# annee
37+23+20+29+63+63+99+80+59+38+26+21
# hiver austral MJJAS
63+63+99+80+59
# ete austral ONDJFMA
38+26+21+37+23+20+29

## nino
# annee
27+14+28+29+15+29+22+14+17+15+19+27
# hiver austral
15+29+22+14+17
# ete austral
15+19+27+27+14+28+29

## nina
# annee
15+9+10+15+8+30+30+46+26+15+18+16
# hiver austral
8+30+30+46+26
# ete austral
15+18+16+15+9+10+15


# 2050 RCP4.5
## total
# annee
63+47+62+80+86+112+118+130+94+54+42+69
# hiver austral MJJAS
86+112+118+130+94
# ete austral ONDJFMA
54+42+69+63+47+62+80
## neutre
# annee
28+20+29+32+58+70+70+81+52+24+25+31
# hiver austral MJJAS
58+70+70+81+52
# ete austral ONDJFMA
24+25+31+28+20+29+32
## nino
# annee
20+17+21+29+11+19+16+25+22+11+12+20
# hiver austral
11+19+16+25+22
# ete austral
11+12+20+20+17+21+29
## nina
# annee
13+10+12+19+17+23+32+24+20+19+5+18
# hiver austral
17+23+32+24+20
# ete austral
19+5+18+13+10+12+19

# 2050 RCP8.5
## total
# annee
64+44+38+55+95+127+127+144+106+83+45+70
# hiver austral MJJAS
95+127+127+144+106
# ete austral ONDJFMA
83+45+70+64+44+38+55
## neutre
# annee
22+18+16+25+58+61+67+81+49+45+21+25
# hiver austral MJJAS
58+61+67+81+49
# ete austral ONDJFMA
45+21+25+22+18+16+25
## nino
# annee
21+16+17+22+24+32+19+23+31+20+14+23
# hiver austral
24+32+19+23+31
# ete austral
20+14+23+21+16+17+22
## nina
# annee
18+10+5+8+13+34+41+40+26+18+10+22
# hiver austral
13+34+41+40+26
# ete austral
18+10+22+18+10+5+8

gghist2 = ggplot(df_event_filt_time_h2T_ALL, aes(x=month_abb,fill=Episode)) +
  geom_bar(stat = "count",position="dodge") +
  scale_x_discrete(drop=FALSE) + 
  scale_fill_manual(name="Episode",values=colours,labels=c("Niña","Niño","Neutre","NA"),drop=FALSE) +
  labs(title="INTENSE SWELLS (filtered in time/intensity)", x = "Month", y = "Count")  +
  ylim(0,170) +
  theme(text = element_text(size=20)) +
  geom_text(stat='count', position=position_dodge(width=0.95), aes(label=..count.., y=..count..), size=5, vjust=-0.5) + 
  facet_grid(vars(Exp.))
print(gghist2)
ggsave(gghist2, file=paste(figpath_rcp45_rcp85,"2hist_intense_swells_nino_nina_filt_time_h2T","_",yrstart,"_",yrstop,"_v1.png",sep=""), width = 36, height = 24, units = "cm")

colours = c("Nina"="blue","Nino"="red","Neutre"="black")

gghist3 = ggplot(df_event_filt_time_h2T_red_ALL, aes(x=month_abb,fill=Episode)) +
  geom_bar(stat = "count",position="dodge") +
  scale_x_discrete(drop=FALSE) + 
  coord_cartesian(ylim=c(0, 10)) + 
  scale_y_continuous(breaks=seq(0, 8, 2)) +
  scale_fill_manual(name="Episode",values=colours,labels=c("Neutre","Niña","Niño"),drop=FALSE) +
  labs(title="VERY INTENSE SWELLS (filtered in time/intensity)", x = "Month", y = "Count")  +
  theme(text = element_text(size=20)) +
  geom_text(stat='count', position=position_dodge(width=0.95), aes(label=..count.., y=..count..), size=5, vjust=-0.5) + 
#  geom_text(stat='count', aes(group=month_abb, label=..count.., y=..count..), size=5, vjust=-0.5) + 
  facet_grid(vars(Exp.))
print(gghist3)
ggsave(gghist3, file=paste(figpath_rcp45_rcp85,"3hist_intense_swells_nino_nina_filt_strongintensity","_",yrstart,"_",yrstop,"_v2.png",sep=""), width = 36, height = 24, units = "cm")

# calcul des valeurs affichées par l'hist pour mettre dans le rapport
# 2000
## total
# annee
5+1+0+1+3+5+2+9+2+2+0+2
# hiver austral MJJAS
3+5+2+9+2
# ete austral ONDJFMA
2+0+2+5+1+0+1

## neutre
# annee
1+0+0+0+2+3+1+9+2+2+0+0
# hiver austral MJJAS
2+3+1+9+2
# ete austral ONDJFMA
2+0+0+1+0+0+0

## nino
# annee
3+0+0+1+1+1+1+0+0+0+0+1
# hiver austral
1+1+1+0+0
# ete austral
0+0+1+3+0+0+1

## nina
# annee
1+1+0+0+0+1+0+0+0+0+0+1
# hiver austral
0+1+0+0+0
# ete austral
0+0+1+1+1+0+0

# 2050 RCP4.5
## total
# annee
3+0+0+0+2+4+9+5+1+0+0+3
# hiver austral MJJAS
2+4+9+5+1
# ete austral ONDJFMA
0+0+3+3+0+0+0

## neutre
# annee
1+0+0+0+1+4+5+5+1+0+0+2
# hiver austral MJJAS
1+4+5+5+1
# ete austral ONDJFMA
0+0+2+1+0+0+0

## nino
# annee
2+0+0+0+0+0+2+0+0+0+0+1
# hiver austral
2
# ete austral
3

## nina
# annee
3
# hiver austral
3
# ete austral
0

# 2050 RCP8.5
## total
# annee
2+1+1+3+0+8+7+7+2+2+0+5
# hiver austral MJJAS
0+8+7+7+2
# ete austral ONDJFMA
2+0+5+2+1+1+3

## neutre
# annee
20
# hiver austral MJJAS
16
# ete austral ONDJFMA
4

## nino
# annee
11
# hiver austral
4
# ete austral
7

## nina
# annee
7
# hiver austral
4
# ete austral
3



# let's analyse the 3 most intense(N_peak)/longest(delta_event) events for all 3 experiments in terms of energy and FF010/PMER
# first find the most intense events in terms of N_peak
N_peak_sorted_df_event_filt_time_h2T_red = sort(df_event_filt_time_h2T_red$N_peak, index.return=TRUE, decreasing = T)
N_peak_sorted_df_event_rcp45_filt_time_h2T_red = sort(df_event_rcp45_filt_time_h2T_red$N_peak, index.return=TRUE, decreasing = T)
N_peak_sorted_df_event_rcp85_filt_time_h2T_red = sort(df_event_rcp85_filt_time_h2T_red$N_peak, index.return=TRUE, decreasing = T)

most_intense_N_peak_time = df_event_filt_time_h2T_red[N_peak_sorted_df_event_filt_time_h2T_red$ix,"time_posixct_event"]
most_intense_N_peak_time_rcp45 = df_event_rcp45_filt_time_h2T_red[N_peak_sorted_df_event_rcp45_filt_time_h2T_red$ix,"time_posixct_event"]
most_intense_N_peak_time_rcp85 = df_event_rcp85_filt_time_h2T_red[N_peak_sorted_df_event_rcp85_filt_time_h2T_red$ix,"time_posixct_event"]

# secondly find the most intense events in terms of delta_event
delta_event_sorted_df_event_filt_time = sort(df_event_filt_time$delta_event, index.return=TRUE, decreasing = T)
delta_event_sorted_df_event_rcp45_filt_time = sort(df_event_rcp45_filt_time$delta_event, index.return=TRUE, decreasing = T)
delta_event_sorted_df_event_rcp85_filt_time = sort(df_event_rcp85_filt_time$delta_event, index.return=TRUE, decreasing = T)

most_intense_delta_event_time = df_event_filt_time[delta_event_sorted_df_event_filt_time$ix,"time_posixct_event"]
most_intense_delta_event_time_rcp45 = df_event_rcp45_filt_time[delta_event_sorted_df_event_rcp45_filt_time$ix,"time_posixct_event"]
most_intense_delta_event_time_rcp85 = df_event_rcp85_filt_time[delta_event_sorted_df_event_rcp85_filt_time$ix,"time_posixct_event"]


# write shell files to execute on belenos 
text1 = "grib_copy -w dataDate="
text2 = ",dataTime="
text3_Np = " ../output extraits_Npeak/output_"
text3_de = " ../output extraits_deltaevent/output_"

# h2T (calibration_2/MFWAM_pacific_ext/post/ and calibration_2/MFWAM_rcp45/post and calibration_2/MFWAM_rcp85/post)
text_lista = c()
text_lista_rcp45 = c()
text_lista_rcp85 = c()
date = c()
date_rcp45 = c()
date_rcp85 = c()
text_lista_de = c()
text_lista_de_rcp45 = c()
text_lista_de_rcp85 = c()
date_de = c()
date_de_rcp45 = c()
date_de_rcp85 = c()
k = 1
for (i in seq(1,20)){
  for (j in seq(1,8)){
    # N_peak
    date[j] = format(most_intense_N_peak_time[i]-as.difftime(6-j, unit="days"),"%Y%m%d")
    date_rcp45[j] = format(most_intense_N_peak_time_rcp45[i]-as.difftime(6-j, unit="days"),"%Y%m%d")
    date_rcp85[j] = format(most_intense_N_peak_time_rcp85[i]-as.difftime(6-j, unit="days"),"%Y%m%d")
    text_lista[k]=paste(text1,date[j],text3_Np,date[j],".grib &",sep="")
    text_lista_rcp45[k]=paste(text1,date_rcp45[j],text3_Np,date_rcp45[j],".grib &",sep="")
    text_lista_rcp85[k]=paste(text1,date_rcp85[j],text3_Np,date_rcp85[j],".grib &",sep="")
    
    # delta_event
    date_de[j] = format(most_intense_delta_event_time[i]-as.difftime(6-j, unit="days"),"%Y%m%d")
    date_de_rcp45[j] = format(most_intense_delta_event_time_rcp45[i]-as.difftime(6-j, unit="days"),"%Y%m%d")
    date_de_rcp85[j] = format(most_intense_delta_event_time_rcp85[i]-as.difftime(6-j, unit="days"),"%Y%m%d")
    text_lista_de[k]=paste(text1,date_de[j],text3_de,date_de[j],".grib &",sep="")
    text_lista_de_rcp45[k]=paste(text1,date_de_rcp45[j],text3_de,date_de_rcp45[j],".grib &",sep="")
    text_lista_de_rcp85[k]=paste(text1,date_de_rcp85[j],text3_de,date_de_rcp85[j],".grib &",sep="")
    k = k+1
  }
}

write.table(text_lista, file = "batch_h2T_Npeak_20events.sh", row.names = FALSE, col.names = FALSE, quote = FALSE)
write.table(text_lista_rcp45, file = "batch_h2T_Npeak_20events_rcp45.sh", row.names = FALSE, col.names = FALSE, quote = FALSE)
write.table(text_lista_rcp85, file = "batch_h2T_Npeak_20events_rcp85.sh", row.names = FALSE, col.names = FALSE, quote = FALSE)

write.table(text_lista_de, file = "batch_h2T_deltaevent_20events.sh", row.names = FALSE, col.names = FALSE, quote = FALSE)
write.table(text_lista_de_rcp45, file = "batch_h2T_deltaevent_20events_rcp45.sh", row.names = FALSE, col.names = FALSE, quote = FALSE)
write.table(text_lista_de_rcp85, file = "batch_h2T_deltaevent_20events_rcp85.sh", row.names = FALSE, col.names = FALSE, quote = FALSE)

# h2T (calibration_2/MFWAM_pacific_ext/post/)

#on transforme avec cdo:
# cat *.grib > all_files.grib
# cdo -f nc copy all_files.grib all_files.nc
# cdo expr,energy=var100*var100*var201 all_files.nc h2T_all_files.nc

# (FF010/PMER)

text_listb = c()
text_listb_rcp45 = c()
text_listb_rcp85 = c()
text_listc = c()
text_listc_rcp45 = c()
text_listc_rcp85 = c()
date = c()
date_rcp45 = c()
date_rcp85 = c()
year = c()
year_rcp45 = c()
year_rcp85 = c()

text_listb_de = c()
text_listb_de_rcp45 = c()
text_listb_de_rcp85 = c()
text_listc_de = c()
text_listc_de_rcp45 = c()
text_listc_de_rcp85 = c()
date_de = c()
date_de_rcp45 = c()
date_de_rcp85 = c()
year_de = c()
year_de_rcp45 = c()
year_de_rcp85 = c()

k = 1
for (i in seq(1,20)){
  for (j in seq(1,8)){
    date[j] = format(most_intense_N_peak_time[i]-as.difftime(6-j, unit="days"),"%Y%m%d")
    date_rcp45[j] = format(most_intense_N_peak_time_rcp45[i]-as.difftime(6-j, unit="days"),"%Y%m%d")
    date_rcp85[j] = format(most_intense_N_peak_time_rcp85[i]-as.difftime(6-j, unit="days"),"%Y%m%d")
    year[j] = year(most_intense_N_peak_time[i]-as.difftime(6-j, unit="days"))
    year_rcp45[j] = year(most_intense_N_peak_time_rcp45[i]-as.difftime(6-j, unit="days"))
    year_rcp85[j] = year(most_intense_N_peak_time_rcp85[i]-as.difftime(6-j, unit="days"))
    text_listb[k]=paste(text1,date[j]," ../CE0/FF010CE0PacifExt",year[j],".grib ","extraits_CE0/extraits_Npeak/FF010CE0PacifExt_",date[j],".grib"," &",sep="")
    text_listb_rcp45[k]=paste(text1,date_rcp45[j]," ../CE1/FF010CE1PacifExt",year_rcp45[j],".grib ","extraits_CE1/extraits_Npeak/FF010CE1PacifExt_",date_rcp45[j],".grib"," &",sep="")
    text_listb_rcp85[k]=paste(text1,date_rcp85[j]," ../CE2/FF010CE2PacifExt",year_rcp85[j],".grib ","extraits_CE2/extraits_Npeak/FF010CE2PacifExt_",date_rcp85[j],".grib"," &",sep="")
    text_listc[k]=paste(text1,date[j]," ../CE0/PMERCE0PacifExt",year[j],".grib ","extraits_CE0/extraits_Npeak/PMERCE0PacifExt_",date[j],".grib"," &",sep="")
    text_listc_rcp45[k]=paste(text1,date_rcp45[j]," ../CE1/PMERCE1PacifExt",year_rcp45[j],".grib ","extraits_CE1/extraits_Npeak/PMERCE1PacifExt_",date_rcp45[j],".grib"," &",sep="")
    text_listc_rcp85[k]=paste(text1,date_rcp85[j]," ../CE2/PMERCE2PacifExt",year_rcp85[j],".grib ","extraits_CE2/extraits_Npeak/PMERCE2PacifExt_",date_rcp85[j],".grib"," &",sep="")
    
    date_de[j] = format(most_intense_delta_event_time[i]-as.difftime(6-j, unit="days"),"%Y%m%d")
    date_de_rcp45[j] = format(most_intense_delta_event_time_rcp45[i]-as.difftime(6-j, unit="days"),"%Y%m%d")
    date_de_rcp85[j] = format(most_intense_delta_event_time_rcp85[i]-as.difftime(6-j, unit="days"),"%Y%m%d")
    year_de[j] = year(most_intense_delta_event_time[i]-as.difftime(6-j, unit="days"))
    year_de_rcp45[j] = year(most_intense_delta_event_time_rcp45[i]-as.difftime(6-j, unit="days"))
    year_de_rcp85[j] = year(most_intense_delta_event_time_rcp85[i]-as.difftime(6-j, unit="days"))
    text_listb_de[k]=paste(text1,date_de[j]," ../CE0/FF010CE0PacifExt",year_de[j],".grib ","extraits_CE0/extraits_deltaevent/FF010CE0PacifExt_",date_de[j],".grib"," &",sep="")
    text_listb_de_rcp45[k]=paste(text1,date_de_rcp45[j]," ../CE1/FF010CE1PacifExt",year_de_rcp45[j],".grib ","extraits_CE1/extraits_deltaevent/FF010CE1PacifExt_",date_de_rcp45[j],".grib"," &",sep="")
    text_listb_de_rcp85[k]=paste(text1,date_de_rcp85[j]," ../CE2/FF010CE2PacifExt",year_de_rcp85[j],".grib ","extraits_CE2/extraits_deltaevent/FF010CE2PacifExt_",date_de_rcp85[j],".grib"," &",sep="")
    text_listc_de[k]=paste(text1,date_de[j]," ../CE0/PMERCE0PacifExt",year_de[j],".grib ","extraits_CE0/extraits_deltaevent/PMERCE0PacifExt_",date_de[j],".grib"," &",sep="")
    text_listc_de_rcp45[k]=paste(text1,date_de_rcp45[j]," ../CE1/PMERCE1PacifExt",year_de_rcp45[j],".grib ","extraits_CE1/extraits_deltaevent/PMERCE1PacifExt_",date_de_rcp45[j],".grib"," &",sep="")
    text_listc_de_rcp85[k]=paste(text1,date_de_rcp85[j]," ../CE2/PMERCE2PacifExt",year_de_rcp85[j],".grib ","extraits_CE2/extraits_deltaevent/PMERCE2PacifExt_",date_de_rcp85[j],".grib"," &",sep="")
    
    k = k+1
  }
}

write.table(text_listb, file = "batch_FF010_Npeak_20events.sh", row.names = FALSE, col.names = FALSE, quote = FALSE)
write.table(text_listb_rcp45, file = "batch_FF010_Npeak_20events_rcp45.sh", row.names = FALSE, col.names = FALSE, quote = FALSE)
write.table(text_listb_rcp85, file = "batch_FF010_Npeak_20events_rcp85.sh", row.names = FALSE, col.names = FALSE, quote = FALSE)
write.table(text_listc, file = "batch_PMER_Npeak_20events.sh", row.names = FALSE, col.names = FALSE, quote = FALSE)
write.table(text_listc_rcp45, file = "batch_PMER_Npeak_20events_rcp45.sh", row.names = FALSE, col.names = FALSE, quote = FALSE)
write.table(text_listc_rcp85, file = "batch_PMER_Npeak_20events_rcp85.sh", row.names = FALSE, col.names = FALSE, quote = FALSE)

write.table(text_listb_de, file = "batch_FF010_deltaevent_20events.sh", row.names = FALSE, col.names = FALSE, quote = FALSE)
write.table(text_listb_de_rcp45, file = "batch_FF010_deltaevent_20events_rcp45.sh", row.names = FALSE, col.names = FALSE, quote = FALSE)
write.table(text_listb_de_rcp85, file = "batch_FF010_deltaevent_20events_rcp85.sh", row.names = FALSE, col.names = FALSE, quote = FALSE)
write.table(text_listc_de, file = "batch_PMER_deltaevent_20events.sh", row.names = FALSE, col.names = FALSE, quote = FALSE)
write.table(text_listc_de_rcp45, file = "batch_PMER_deltaevent_20events_rcp45.sh", row.names = FALSE, col.names = FALSE, quote = FALSE)
write.table(text_listc_de_rcp85, file = "batch_PMER_deltaevent_20events_rcp85.sh", row.names = FALSE, col.names = FALSE, quote = FALSE)



# (OLD) let's consider a number of examples (in red in the file comparaison_vents_swh_vs_h2T.ods) (including the 5 days preceding the max and 2 days following)
timetotest=c(ISOdatetime(2050,7,6,21,0,0,tz="UTC"), # h2T
             ISOdatetime(2052,8,22,21,0,0,tz="UTC"), # swh
             ISOdatetime(2053,11,13,21,0,0,tz="UTC"),ISOdatetime(2053,12,10,18,0,0,tz="UTC"), # swh
             ISOdatetime(2056,3,13,21,0,0,tz="UTC"), # swh
             ISOdatetime(2059,3,28,15,0,0,tz="UTC"),ISOdatetime(2059,4,28,9,0,0,tz="UTC"), # h2T
             ISOdatetime(2059,3,4,0,0,0,tz="UTC"),ISOdatetime(2059,5,14,15,0,0,tz="UTC"), # swh
             ISOdatetime(2071,3,15,21,0,0,tz="UTC"), # h2T
             ISOdatetime(2071,2,27,18,0,0,tz="UTC"), # swh
             ISOdatetime(2075,1,13,15,0,0,tz="UTC"), # swh
             ISOdatetime(2075,3,28,6,0,0,tz="UTC"), # h2T
             ISOdatetime(2077,7,8,6,0,0,tz="UTC"),ISOdatetime(2077,7,19,9,0,0,tz="UTC"),ISOdatetime(2077,8,2,9,0,0,tz="UTC"), # swh
             ISOdatetime(2078,1,30,21,0,0,tz="UTC") # h2T
)

# SWH (calibration_2/MFWAM_pacific_ext/post/)
text_lista = c()
date = c()
year = c()
k = 1
for (i in seq(1,length(timetotest))){
  for (j in seq(1,8)){
    date[j] = format(timetotest[i]-as.difftime(6-j, unit="days"),"%Y%m%d")
    text_lista[k]=paste(text1,date[j],text3,date[j],".grib &",sep="")
    k = k+1
  }
}

write.table(text_lista, file = "batch_SWH_compa_h2T_vs_swh_17events.sh", row.names = FALSE, col.names = FALSE, quote = FALSE)

# h2T (calibration_2/MFWAM_pacific_ext/post/)

#on transforme avec cdo:
# cat *.grib > all_files.grib
# cdo -f nc copy all_files.grib all_files.nc
# cdo expr,energy=var100*var100*var201 all_files.nc h2T_all_files.nc


# (FF010/PMER)

text4 = " CE0/FF010CE0PacifExt"
text5 = " CE0/PMERCE0PacifExt"

text_listb = c()
text_listc = c()
date = c()
year = c()
k = 1
for (i in seq(1,length(timetotest))){
  for (j in seq(1,8)){
    date[j] = format(timetotest[i]-as.difftime(6-j, unit="days"),"%Y%m%d")
    year[j] = year(timetotest[i]-as.difftime(6-j, unit="days"))
    text_listb[k]=paste(text1,date[j],text4,year[j],".grib ","extraits_swh_vs_h2T/FF010CE0PacifExt_",date[j],".grib"," &",sep="")
    text_listc[k]=paste(text1,date[j],text5,year[j],".grib ","extraits_swh_vs_h2T/PMERCE0PacifExt_",date[j],".grib"," &",sep="")
    k = k+1
  }
}

write.table(text_listb, file = "batch_FF010_compa_h2T_vs_swh_17events.sh", row.names = FALSE, col.names = FALSE, quote = FALSE)
write.table(text_listc, file = "batch_PMER_compa_h2T_vs_swh_17events.sh", row.names = FALSE, col.names = FALSE, quote = FALSE)





# ma question sur stackoverflow 3/07/2021
month = c("Jan","Jan","Feb","Mar","Apr","May","Jun","Jul","Jul","Jul","Aug","Aug","Sep","Oct","Nov","Dec","Dec")
Episode = c("Nino","ras","Nino","Nina","Nina","Nina","Nino","Nina","Nino","Nina","Nina","Nino","ras","Nino","ras","Nino","Nina")
df1 = data.frame(month,Episode)
df1$Exp = "2000"
df1$month <-factor(df1$month,levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

month = c("Jan","Feb","Feb","Feb","Mar","Apr","May","Jun","Jun","Jun","Jun","Jul","Aug","Sep","Sep","Oct","Oct","Oct","Nov","Nov","Nov","Dec")
Episode = c("ras","Nino","Nino","Nina","Nina","Nina","ras","ras","Nino","Nino","ras","ras","ras","Nino","Nina","Nina","Nino","Nino","Nina","Nino","ras","ras")
df2 = data.frame(month,Episode)
df2$Exp = "RCP45"
df2$month <-factor(df2$month,levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

month = c("Jan","Jan","Jan","Jan","Feb","Mar","Apr","Apr","May","May","Jun","Jul","Aug","Sep","Oct","Nov","Nov","Dec","Dec")
Episode = c("Nina","ras","Nino","Nino","ras","Nino","Nino","Nino","Nina","ras","ras","Nina","ras","ras","Nino","Nino","Nino","Nina","Nina")
df3 = data.frame(month,Episode)
df3$Exp = "RCP85"
df3$month <-factor(df3$month,levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

colours = c("Nina"="blue","Nino"="red","ras"="black","NA"="grey")
colScale = scale_fill_manual(name="Episode",values=colours)

gghist = ggplot(df3, aes(x=month,fill=Episode)) +
  geom_bar(stat = "count") +
  scale_x_discrete(drop=FALSE) + 
  scale_fill_manual(name="Episode",values=colours,drop=FALSE) +
  labs(title=("Distribution of extreme events"), x = "Month")  +
  theme(text = element_text(size=15))
print(gghist)

df_4test = rbind(df1,df2,df3)


month = c("Jan","Jan","Feb","Mar","Apr","May","Jun","Jul","Jul","Jul","Aug","Aug","Sep","Oct","Nov","Dec","Dec")
Episode = c("Nino","ras","Nino","Nina","Nina","Nina","Nino","Nina","Nino","Nina","Nina","Nino","ras","Nino","ras","Nino","Nina")

month = c("Jan","Feb","Feb","Feb","Mar","Apr","May","Jun","Jun","Jun","Jun","Jul","Aug","Sep","Sep","Oct","Oct","Oct","Nov","Nov","Nov","Dec")
Episode = c("ras","Nino","Nino","Nina","Nina","Nina","ras","ras","Nino","Nino","ras","ras","ras","Nino","Nina","Nina","Nino","Nino","Nina","Nino","ras","ras")

month = c("Jan","Jan","Jan","Jan","Feb","Mar","Apr","Apr","May","May","Jun","Jul","Aug","Sep","Oct","Nov","Nov","Dec","Dec")
Episode = c("Nina","ras","Nino","Nino","ras","Nino","Nino","Nino","Nina","ras","ras","Nina","ras","ras","Nino","Nino","Nino","Nina","Nina")

  
