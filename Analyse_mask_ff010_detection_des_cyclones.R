library(ggplot2)
library(mapdata)
library(timeDate)
library(data.table)
library(tidyverse)
library(utils)
library(lubridate)

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
yrstop=2449

# Analyse des 3 fichiers 
# sum_maskxxxms_FF010CE0PolynsGL_Ext2050_2449.nc / sum_maskxxxms_FF010CE1PolynsGL_Ext2050_2449.nc / sum_maskxxxms_FF010CE2PolynsGL_Ext2050_2449;NC  (sur 2050-2449)

# L'objectif est d'identifier les cyclones extremes (FF010 > 40m/s CAT NAT) et de les denombrer pour comparer les 3 exp. Climat 2000/RCP45/RCP85 et comparer avec les exp. de Martine

# set path and filename.
seuil = 40
ncpath <- "/Users/gablellouch/Documents/Meteo_France/ENM/ienm3/houles_dirpf/postprocessing/analyse_cyclones_signature_FF010/"
ncname_ff010_CE0 <- paste("sum_mask",seuil,"ms_FF010CE0PolynsGL_Ext2050_2449.nc",sep="")
ncname_ff010_CE1 <- paste("sum_mask",seuil,"ms_FF010CE1PolynsGL_Ext2050_2449.nc",sep="")
ncname_ff010_CE2 <- paste("sum_mask",seuil,"ms_FF010CE2PolynsGL_Ext2050_2449.nc",sep="")
ncfname_ff010_CE0 <- paste(ncpath, ncname_ff010_CE0, sep="")
ncfname_ff010_CE1 <- paste(ncpath, ncname_ff010_CE1, sep="")
ncfname_ff010_CE2 <- paste(ncpath, ncname_ff010_CE2, sep="")
dname_ff010 <- "var33" 

# path figures
figpath <- paste("/Users/gablellouch/Documents/Meteo_France/ENM/ienm3/houles_dirpf/Figures/postprocessing/analyse_cyclones_signature_FF010/",sep="")

# open netCDF files
ncin_ff010_CE0 <- nc_open(ncfname_ff010_CE0)
ncin_ff010_CE1 <- nc_open(ncfname_ff010_CE1)
ncin_ff010_CE2 <- nc_open(ncfname_ff010_CE2)

time <- ncvar_get(ncin_ff010_CE0,"time")
tunits <- ncatt_get(ncin_ff010_CE0,"time","units")
nt <- dim(time)

# define time vector (attention on commence le 1/1/2050 a 0H)
time_posixct <- seq(ISOdatetime(2050,1,1,0,0,0,tz="UTC"),ISOdatetime(2449,12,31,0,0,0,tz="UTC"), by="1 day")

# get data for ff010
n_mask_ff010_CE0 <- ncvar_get(ncin_ff010_CE0,dname_ff010)
n_mask_ff010_CE1 <- ncvar_get(ncin_ff010_CE1,dname_ff010)
n_mask_ff010_CE2 <- ncvar_get(ncin_ff010_CE2,dname_ff010)
dim(n_mask_ff010_CE0)

# load nino information (will be useful to link cyclone events with nino/nina/Neutre episodes)
nino34 = read.table(file="/Users/gablellouch/Documents/Meteo_France/ENM/ienm3/houles_dirpf/Belenos/Nino34/Info_NinoNina_CE0.txt",header=T,sep=";")
nino34["time_posixct"] = as.Date(nino34[["time_posixct"]],"%Y-%m-%d")

nino34_rcp45 = read.table(file="/Users/gablellouch/Documents/Meteo_France/ENM/ienm3/houles_dirpf/Belenos/Nino34/Info_NinoNina_CE1.txt",header=T,sep=";")
nino34_rcp45["time_posixct"] = as.Date(nino34[["time_posixct"]],"%Y-%m-%d")

nino34_rcp85 = read.table(file="/Users/gablellouch/Documents/Meteo_France/ENM/ienm3/houles_dirpf/Belenos/Nino34/Info_NinoNina_CE2.txt",header=T,sep=";")
nino34_rcp85["time_posixct"] = as.Date(nino34[["time_posixct"]],"%Y-%m-%d")

# make dataframes
df_event = ts_peaks(n_mask_ff010_CE0,time,time_posixct,nino34)
df_event$Exp. = "2000"
df_event_rcp45 = ts_peaks(n_mask_ff010_CE1,time,time_posixct,nino34_rcp45)
df_event_rcp45$Exp. = "RCP4.5 2050"
df_event_rcp85 = ts_peaks(n_mask_ff010_CE2,time,time_posixct,nino34_rcp85)
df_event_rcp85$Exp. = "RCP8.5 2050"

# analyse des evenements les + forts (comme fait pour l'energy)
N_peak_sorted_df_event = sort(df_event$N_peak, index.return=TRUE, decreasing = T)
N_peak_sorted_df_event_rcp45 = sort(df_event_rcp45$N_peak, index.return=TRUE, decreasing = T)
N_peak_sorted_df_event_rcp85 = sort(df_event_rcp85$N_peak, index.return=TRUE, decreasing = T)

most_intense_N_peak_time = df_event[N_peak_sorted_df_event$ix,]
most_intense_N_peak_time_rcp45 = df_event_rcp45[N_peak_sorted_df_event_rcp45$ix,]
most_intense_N_peak_time_rcp85 = df_event_rcp85[N_peak_sorted_df_event_rcp85$ix,]



df_event_ALL = rbind(df_event,df_event_rcp45,df_event_rcp85)

colours = c("Nina"="blue","Nino"="red","Neutre"="black","NA"="grey")
colScale = scale_fill_manual(name="Episode",values=colours)

# identify Nino/Nina/Neutre conditions
gghist = ggplot(df_event_ALL, aes(x=month_abb,fill=Episode)) +
  geom_bar(stat = "count",position="dodge") +
  scale_x_discrete(drop=FALSE) + 
  scale_fill_manual(name="Episode",values=colours,drop=FALSE) +
  labs(title=("400 years: distribution of cyclones in French Polynesia"), x = "Month", y = "Count")  +
  ylim(0,50) +
  theme(text = element_text(size=20)) +
  geom_text(stat='count', position=position_dodge(width=0.95), aes(label=..count.., y=..count..), size=5, vjust=-0.5) + 
  facet_grid(vars(Exp.))
print(gghist)
ggsave(gghist, file=paste(figpath,"hist_cyclones_nino_nina_o40ms","_",yrstart,"_",yrstop,"_v1.png",sep=""), width = 26, height = 24, units = "cm")

# scatter plot : event duration vs event coverage 
colours = c("Nina"="blue","Nino"="red","Neutre"="black","NA"="grey")
colScale = scale_fill_manual(name="Episode",values=colours,labels=c("Neutre","Niña","Niño","NA"))
ggs = ggplot(df_event) +
  geom_point(aes(x=delta_event,y=N_peak,fill=factor(Episode)), shape = 21, size = 4, stroke = 0.5, colour="black") +
  colScale +
  labs(fill="Nino3.4",title=paste("CLIMAT 2000"),x=expression(Delta[event]*" (time unit, 24h)"), y = expression("N"[peak])) +
  xlim(0,13) +
  ylim(0,68) +
  theme(text = element_text(size=20), legend.title=element_blank(), legend.position = c(0.87,0.15),legend.direction = "vertical"  )
print(ggs)
ggsave(ggs, file=paste(figpath,"spat_vs_temp_",yrstart,"_",yrstop,"_FF010_2000_v1.png",sep=""), width = 15, height = 12, units = "cm")

ggs_rcp45 = ggplot(df_event_rcp45) +
  geom_point(aes(x=delta_event,y=N_peak,fill=factor(Episode)), shape = 21, size = 4, stroke = 0.5, colour="black") +
  colScale +
  labs(fill="Nino3.4",title=paste("Climat 2050 RCP4.5"),x=expression(Delta[event]*" (time unit, 24h)"), y = expression("N"[peak])) +
  xlim(0,13) +
  ylim(0,68) +
  theme(text = element_text(size=20), legend.title=element_blank(), legend.position = c(10.15,10.8),legend.direction = "vertical"  )
print(ggs_rcp45)
ggsave(ggs_rcp45, file=paste(figpath,"spat_vs_temp_",yrstart,"_",yrstop,"_FF010_2050_rcp45_v1.png",sep=""), width = 15, height = 12, units = "cm")

ggs_rcp85 = ggplot(df_event_rcp85) +
  geom_point(aes(x=delta_event,y=N_peak,fill=factor(Episode)), shape = 21, size = 4, stroke = 0.5, colour="black") +
  colScale +
  labs(fill="Nino3.4",title=paste("Climat 2050 RCP8.5"),x=expression(Delta[event]*" (time unit, 24h)"), y = expression("N"[peak])) +
  xlim(0,13) +
  ylim(0,68) +
  theme(text = element_text(size=20), legend.title=element_blank(), legend.position = c(10.15,10.8),legend.direction = "vertical"  )
print(ggs_rcp85)
ggsave(ggs_rcp85, file=paste(figpath,"spat_vs_temp_",yrstart,"_",yrstop,"_FF010_2050_rcp85_v1.png",sep=""), width = 15, height = 12, units = "cm")



# write shell files to execute on belenos 
text1 = "grib_copy -w dataDate="

# (ff010)
# j'inclus les PMER correspondant pour avoir un suivi du lien PMER / FF010

text_list = c()
text_list_rcp45 = c()
text_list_rcp85 = c()
text_listb = c()
text_listb_rcp45 = c()
text_listb_rcp85 = c()
date = c()
date_rcp45 = c()
date_rcp85 = c()
year = c()
year_rcp45 = c()
year_rcp85 = c()

k = 1
for (i in seq(1,dim(df_event)[1])){
  for (j in seq(1,10)){
    date[j] = format(df_event$time_posixct_event[i]-as.difftime(7-j, unit="days"),"%Y%m%d")
    year[j] = year(df_event$time_posixct_event[i]-as.difftime(7-j, unit="days"))
    text_list[k]=paste(text1,date[j]," ../CE0/FF010CE0PolynsGL_Ext",year[j],".grib ","CE0/FF010CE0PolynsGL_Ext",date[j],".grib"," &",sep="")
    text_listb[k]=paste(text1,date[j]," ../../../../PMER/analyse_cyclones/CE0/PMERCE0PolynsGL_Ext",year[j],".grib ","CE0/PMERCE0PolynsGL_Ext",date[j],".grib"," &",sep="")
    k = k+1
  }
}

k = 1
for (i in seq(1,dim(df_event_rcp45)[1])){
  for (j in seq(1,10)){
    date_rcp45[j] = format(df_event_rcp45$time_posixct_event[i]-as.difftime(7-j, unit="days"),"%Y%m%d")
    year_rcp45[j] = year(df_event_rcp45$time_posixct_event[i]-as.difftime(7-j, unit="days"))
    text_list_rcp45[k]=paste(text1,date_rcp45[j]," ../CE1/FF010CE1PolynsGL_Ext",year_rcp45[j],".grib ","CE1/FF010CE1PolynsGL_Ext",date_rcp45[j],".grib"," &",sep="")
    text_listb_rcp45[k]=paste(text1,date_rcp45[j]," ../../../../PMER/analyse_cyclones/CE1/PMERCE1PolynsGL_Ext",year_rcp45[j],".grib ","CE1/PMERCE1PolynsGL_Ext",date_rcp45[j],".grib"," &",sep="")
    k = k+1
  }
}

k = 1
for (i in seq(1,dim(df_event_rcp85)[1])){
  for (j in seq(1,10)){
    date_rcp85[j] = format(df_event_rcp85$time_posixct_event[i]-as.difftime(7-j, unit="days"),"%Y%m%d")
    year_rcp85[j] = year(df_event_rcp85$time_posixct_event[i]-as.difftime(7-j, unit="days"))
    text_list_rcp85[k]=paste(text1,date_rcp85[j]," ../CE2/FF010CE2PolynsGL_Ext",year_rcp85[j],".grib ","CE2/FF010CE2PolynsGL_Ext",date_rcp85[j],".grib"," &",sep="")
    text_listb_rcp85[k]=paste(text1,date_rcp85[j]," ../../../../PMER/analyse_cyclones/CE2/PMERCE2PolynsGL_Ext",year_rcp85[j],".grib ","CE2/PMERCE2PolynsGL_Ext",date_rcp85[j],".grib"," &",sep="")
    k = k+1
  }
}

write.table(text_list, file = "batch_FF010_cyclones_40ms_400ans.sh", row.names = FALSE, col.names = FALSE, quote = FALSE)
write.table(text_list_rcp45, file = "batch_FF010_cyclones_40ms_400ans_rcp45.sh", row.names = FALSE, col.names = FALSE, quote = FALSE)
write.table(text_list_rcp85, file = "batch_FF010_cyclones_40ms_400ans_rcp85.sh", row.names = FALSE, col.names = FALSE, quote = FALSE)

write.table(text_listb, file = "batch_FF010_correspPMER_cyclones_40ms_400ans.sh", row.names = FALSE, col.names = FALSE, quote = FALSE)
write.table(text_listb_rcp45, file = "batch_FF010_correspPMER_cyclones_40ms_400ans_rcp45.sh", row.names = FALSE, col.names = FALSE, quote = FALSE)
write.table(text_listb_rcp85, file = "batch_FF010_correspPMER_cyclones_40ms_400ans_rcp85.sh", row.names = FALSE, col.names = FALSE, quote = FALSE)




