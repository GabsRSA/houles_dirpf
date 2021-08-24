library(ggplot2)
library(timeDate)
library(data.table)
library(tidyverse)

# load the ncdf4 package
library(ncdf4)

# le but est d'afficher les valeurs pour les 4 pixels sur des evenements intenses 
# detectes par Analyse_mask_energy dans les 3 scenarios: climat 2000, 2050 RCP45 et RCP85

yrstart=2050
yrstop=2079

pixel_name1 = "hereheretue"
pixel_name2 = "tubuai"
pixel_name3 = "maupiti"
#pixel_name4 = "teahupoo"

# paths
ncpath <- "/Users/gablellouch/Documents/Meteo_France/ENM/ienm3/houles_dirpf/postprocessing/analyse_4pixels/"
figpath <- "/Users/gablellouch/Documents/Meteo_France/ENM/ienm3/houles_dirpf/Figures/postprocessing/analyse_4pixels/evenements_intenses/"

# climat
climatid = "_rcp85"
# rien (mais il faut ajuster ievent à 1 à la fin) ou _rcp45 ou _rcp85

# events
events = c("2054-06-23","2062-08-05","2066-08-06")
events_rcp45 = c("2062-08-23","2074-07-21","2078-01-28")
events_rcp85 = c("2079-09-03","2075-01-13","2079-06-01")
event_list = data.frame(events,events_rcp45,events_rcp85)

# set filename
ncname <- paste("h2T_PolynsGL_",c(pixel_name1,pixel_name2,pixel_name3),climatid,".nc",sep="")  
ncfname <- paste(ncpath, ncname, sep="")
dname <- "energy" 

# open a netCDF file
ncin1 <- nc_open(ncfname[1])
ncin2 <- nc_open(ncfname[2])
ncin3 <- nc_open(ncfname[3])
#ncin4 <- nc_open(ncfname[4])

# get h2T
h2T1 <- ncvar_get(ncin1,dname)
h2T2 <- ncvar_get(ncin2,dname)
h2T3 <- ncvar_get(ncin3,dname)
#h2T4 <- ncvar_get(ncin4,dname)

time_posixct <- seq(ISOdatetime(2050,1,1,3,0,0,tz="UTC"),ISOdatetime(2079,12,31,21,0,0,tz="UTC"), by="3 hours")
time_YMD = format(time_posixct,"%Y-%m-%d")

# make dataframe
df1 = data.frame(time_posixct,h2T1)
df1$Site = "HEREHERETUE"
colnames(df1)[2] = "h2T"
df2 = data.frame(time_posixct,h2T2)
df2$Site = "TUBUAI"
colnames(df2)[2] = "h2T"
df3 = data.frame(time_posixct,h2T3)
df3$Site = "MAUPITI"
colnames(df3)[2] = "h2T"
#df4 = data.frame(time_posixct,h2T4)
#df4$Site = "Teahupoo"
#colnames(df4)[2] = "h2T"

# prepare les plots
df_ALL = rbind(df1,df2,df3)#,df4)

# Plots
ievent = grep(paste("events",climatid,sep=""),names(event_list))
#ievent = 1
i = 1
for (j in seq(1,3)) {
  timei = event_list[j,ievent]
  tim_ref = min(which(time_YMD==timei))
  gg=ggplot(df_ALL,aes(x=time_posixct,y=h2T,group=Site,fill=Site,color=Site)) +
    geom_point(shape=21,size=2,colour="black") +
    scale_fill_manual(values=c("green","pink","blue","red")) +
    geom_line() +
    scale_color_manual(values=c("green","pink","blue","red")) +
    ggtitle(timei) +
    xlab("DATE") + ylab("Hs²Tm (m²s)") +
#    ylim(0, 1250) + 
    xlim(time_posixct[tim_ref-40], time_posixct[tim_ref+40]) +
    theme(text = element_text(size=20),legend.title=element_blank(),legend.position = c(10.75,10.85),legend.direction = "vertical" )
  print(gg)
  ggsave(gg, file=paste(figpath,"ts_h2T_intense_event_NB_",i,"_",timei,climatid,"_v1.png",sep=""), width = 15, height = 12, units = "cm")
  i = i+1
}
