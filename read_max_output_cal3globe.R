library(ggplot2)
library(timeDate)
library(data.table)
library(tidyverse)

# load the ncdf4 package
library(ncdf4)

yrstart=2050
yrstop=2079

# set path and filename
ncpath <- "/Users/gablellouch/Documents/Meteo_France/ENM/ienm3/houles_dirpf/postprocessing/v2_calib3_calib4/"
ncname <- "max_output_cal3globe_lim_PolynsGL.nc"  
ncfname <- paste(ncpath, ncname, sep="")
dname <- "var100" 

# path figures
figpath <- "/Users/gablellouch/Documents/Meteo_France/ENM/ienm3/houles_dirpf/Figures/postprocessing/v2_calib3_calib4/"

# open a netCDF file
ncin <- nc_open(ncfname)
print(ncin)

# get time
time <- ncvar_get(ncin,"time")
time

tunits <- ncatt_get(ncin,"time","units")
nt <- dim(time)

time_posixct <- seq(ISOdatetime(2050,1,1,3,0,0,tz="UTC"),ISOdatetime(2079,12,31,21,0,0,tz="UTC"), by="3 hours")

# get swh
swh_array <- ncvar_get(ncin,dname)
dim(swh_array)

# make dataframe
df = data.frame(time,time_posixct,swh_array)

# extract hist info
## Convenience function
get_hist <- function(p) {
  d <- ggplot_build(p)$data[[1]]
  data.frame(x = d$x, xmin = d$xmin, xmax = d$xmax, y = d$y)
}


# loop over years
#yrstart=2050
#yrstop=2054
for (yr in yrstart:yrstop) {
  
  dfsub = df %>%
    filter(time_posixct >= as.Date(paste(yr,"-01-01",sep="")) & time_posixct < as.Date(paste(yr+1,"-01-01",sep="")))
  
  print(paste(yr,dim(dfsub)[1],sep=" "))
  
  # plot
  # gg=ggplot(dfsub,aes(x=time_posixct)) +
  #   geom_line(aes(y=swh_array), size=0.4, colour="red") +
  #   ggtitle(paste("SWH",yr,"-",yr+1,sep=" ")) +
  #   xlab("Date") + ylab(paste("SWH","(m)",sep=" ")) +
  #   theme(text = element_text(size=15))
  # print(gg)
  
  h=ggplot(dfsub) +
    geom_histogram(aes(x=swh_array), breaks=seq(0,25,by=0.5), color="red", position="dodge", alpha=0.5) +
    geom_vline(aes(xintercept=mean(swh_array)), color="red", linetype="dashed", size=1) +
    #  ggplot2::annotate("text", 35, 75, label=sprintf("mean(F_max)=%0.2f",mean(df4[["F_max"]])), color = "red", hjust = 0, size=7) +
    labs(title=paste("SWH",yr,"-",yr+1,sep=" "),x="SWH (m)", y = "Count") +
    theme(text = element_text(size=15))
    # print(h)
  
  if (yr == yrstart) {
    dfhist = get_hist(h)
  }else {
    dfhist_tmp = get_hist(h)
    dfhist$y = dfhist$y + dfhist_tmp$y
  }
  
  rm(dfsub)
}

# calculate mean value
dfhist$y = dfhist$y/(yrstop-yrstart+1)

# plot mean hist
gg=ggplot(dfhist) +
  geom_line(aes(x=x,y=y), size=0.4, colour="red") +
  ggtitle(paste("Hs MAX (PolynsGL) ",yrstart,"-",yrstop,sep="")) +
  xlab(paste("Hs MAX (m)",sep=" ")) + ylab("Count per year (PDF)") +
  theme(text = element_text(size=15))
print(gg)
ggsave(gg, file=paste(figpath,"hist_mean_globe_lim_PolynsGL","_",yrstart,"_",yrstop,"_v1.png",sep=""), width = 12, height = 9, units = "cm")

# plot complementary cumulative curve
dfhist["ycomp"]=sum(dfhist[,"y"])-cumsum(dfhist[,"y"])
ggc = ggplot(dfhist) + 
  geom_line(aes(x=x, ycomp), size=0.4, colour="red") +
  ggtitle(paste("Hs MAX (PolynsGL) ",yrstart,"-",yrstop,sep="")) +
  xlab(paste("Hs MAX (m)",sep=" ")) + ylab("Count per year (CCDF)") +
  theme(text = element_text(size=15))
print(ggc)
ggsave(ggc, file=paste(figpath,"ccdf_mean_globe_lim_PolynsGL","_",yrstart,"_",yrstop,"_v1.png",sep=""), width = 12, height = 9, units = "cm")


# set to 0 indices when SWH max < 8.75 (about 28 per year)
dfhigh = df
dfhigh$swh_array[dfhigh$swh_array<8.75] = 0

# subset df with SWH max > 8.75 (about 28 per year)
ind_higher_875 = df$swh_array>=8.75
dfhigher = df[ind_higher_875,]

# time for indices with SWH max > 8.75 (about 28 per year)
time_ind = df[ind_higher_875,"time"]

# export indices with SWM max > 8.75
ind_875 = as.numeric(rownames(dfhigher))
#write.table(t(ind_875), file = "ind_875.txt", sep = ",",row.names = FALSE, col.names = FALSE)


# plot
gg1=ggplot(df,aes(x=time_posixct)) +
  geom_line(aes(y=swh_array), size=0.4, colour="red") +
  geom_hline(aes(yintercept=8.75), color="black", size=0.5) +
  ggtitle(paste("Hs MAX (PolynsGL) ",yrstart,"-",yrstop,sep="")) +
  xlab("Date") + ylab(paste("Hs MAX (m)",sep="")) +
  ylim(0, 25) + 
  theme(text = element_text(size=15))
print(gg1)
ggsave(gg1, file=paste(figpath,"ts_SWH_MAX_lim_PolynsGL","_",yrstart,"_",yrstop,"_v1.png",sep=""), width = 14, height = 9, units = "cm")


# plot
gg2=ggplot(dfhigh,aes(x=time_posixct)) +
  geom_point(aes(y=swh_array), size=0.4, colour="blue") +
  geom_hline(aes(yintercept=8.75), color="black", size=0.5) +
  ggtitle(paste("SWH MAX > 8.75 (PolynsGL) ",yrstart,"-",yrstop,sep="")) +
  xlab("Date") + ylab(paste("SWH MAX (m)",sep="")) +
  ylim(0, 25) + 
  theme(text = element_text(size=15))
print(gg2)
ggsave(gg2, file=paste(figpath,"ts_SWH_MAX_sup875_lim_PolynsGL","_",yrstart,"_",yrstop,".png",sep=""), width = 14, height = 9, units = "cm")


# plot
gg3=ggplot(dfhigher,aes(x=time_posixct)) +
  geom_line(aes(y=swh_array), size=0.4, colour="green4") +
  geom_hline(aes(yintercept=8.75), color="black", size=0.5) +
  ggtitle(paste("Hs MAX > 8.75 (PolynsGL) ",yrstart,"-",yrstop,sep="")) +
  xlab("Date") + ylab(paste("Hs MAX (m)",sep="")) +
  ylim(0, 25) + 
  theme(text = element_text(size=15))
print(gg3)
ggsave(gg3, file=paste(figpath,"ts_SWH_MAX_onlysup875_lim_PolynsGL","_",yrstart,"_",yrstop,"_v1.png",sep=""), width = 14, height = 9, units = "cm")





