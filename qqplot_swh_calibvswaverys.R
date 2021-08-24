library(ggplot2)
library(timeDate)
library(data.table)
library(tidyverse)
library(abind)

# load the ncdf4 package
library(ncdf4)

# quantile a considerer
qq=c(5,seq(10,90,10),95,99)

# path figures
figpath <- "/Users/gablellouch/Documents/Meteo_France/ENM/ienm3/houles_dirpf/Figures/postprocessing/qqplot/"

for (i in qq) {
  
  print(i)
  # set path and filename
  ncpath <- "/Users/gablellouch/Documents/Meteo_France/ENM/ienm3/houles_dirpf/postprocessing/qqplot/"
  # dataname
  dname <- "var100"
  
  # open waverys
  ncwav <- paste("timpctl",i,"_output_waverys_lim_polynsGL.nc",sep="")
  ncwavname <- paste(ncpath, ncwav, sep="")
  ncwavin <- nc_open(ncwavname)
  #print(ncwavin)
  swh_wav <- ncvar_get(ncwavin,dname)
  #dim(swh_wav)
  
  # get lon / lat
  lon <- ncvar_get(ncwavin,"lon")
  lat <- ncvar_get(ncwavin,"lat")
  lonunits <- ncatt_get(ncwavin,"lon","units")
  latunits <- ncatt_get(ncwavin,"lat","units")
  
  # open mfwam cal 1/2/3/4
  nccal1 <- paste("timpctl",i,"_output_cal1globe_lim_polynsGL.nc",sep="") 
  nccal1name <- paste(ncpath, nccal1, sep="")
  nccal1in <- nc_open(nccal1name)
  swh_cal1 <- ncvar_get(nccal1in,dname)
  
  nccal2 <- paste("timpctl",i,"_output_cal2globe_lim_polynsGL.nc",sep="") 
  nccal2name <- paste(ncpath, nccal2, sep="")
  nccal2in <- nc_open(nccal2name)
  swh_cal2 <- ncvar_get(nccal2in,dname)
  
  nccal3 <- paste("timpctl",i,"_output_cal3globe_lim_polynsGL.nc",sep="") 
  nccal3name <- paste(ncpath, nccal3, sep="")
  nccal3in <- nc_open(nccal3name)
  swh_cal3 <- ncvar_get(nccal3in,dname)
  
  nccal4 <- paste("timpctl",i,"_output_cal4globe_lim_polynsGL.nc",sep="") 
  nccal4name <- paste(ncpath, nccal4, sep="")
  nccal4in <- nc_open(nccal4name)
  swh_cal4 <- ncvar_get(nccal4in,dname)
    
  if (i == 5) {
    swh_wav_3D = swh_wav
    swh_cal1_3D = swh_cal1
    swh_cal2_3D = swh_cal2
    swh_cal3_3D = swh_cal3
    swh_cal4_3D = swh_cal4
  }else {
    swh_wav_3D = abind(swh_wav_3D,swh_wav,along=3)
    swh_cal1_3D = abind(swh_cal1_3D,swh_cal1,along=3)
    swh_cal2_3D = abind(swh_cal2_3D,swh_cal2,along=3)
    swh_cal3_3D = abind(swh_cal3_3D,swh_cal3,along=3)
    swh_cal4_3D = abind(swh_cal4_3D,swh_cal4,along=3)
  }
}

# point nord
ind_lat1=which(lat==-5)
ind_lon1=which(lon==210)

x1 = swh_wav_3D[ind_lon1,ind_lat1,]
y1cal1 = swh_cal1_3D[ind_lon1,ind_lat1,]
y1cal2 = swh_cal2_3D[ind_lon1,ind_lat1,]
y1cal3 = swh_cal3_3D[ind_lon1,ind_lat1,]
y1cal4 = swh_cal4_3D[ind_lon1,ind_lat1,]

# point sud
ind_lat2=which(lat==-27)
ind_lon2=which(lon==210)

x2 = swh_wav_3D[ind_lon2,ind_lat2,]
y2cal1 = swh_cal1_3D[ind_lon2,ind_lat2,]
y2cal2 = swh_cal2_3D[ind_lon2,ind_lat2,]
y2cal3 = swh_cal3_3D[ind_lon2,ind_lat2,]
y2cal4 = swh_cal4_3D[ind_lon2,ind_lat2,]

# moyenne sur PolynsGL
x3 = apply(swh_wav_3D, 3, mean)
y3cal1 = apply(swh_cal1_3D, 3, mean)
y3cal2 = apply(swh_cal2_3D, 3, mean)
y3cal3 = apply(swh_cal3_3D, 3, mean)
y3cal4 = apply(swh_cal4_3D, 3, mean)

# moyenne sur bande nord de PolynsGL
ind_latnord = which(lat==-8)
x4 = apply(swh_wav_3D[,1:ind_latnord,], 3, mean)
y4cal1 = apply(swh_cal1_3D[,1:ind_latnord,], 3, mean)
y4cal2 = apply(swh_cal2_3D[,1:ind_latnord,], 3, mean)
y4cal3 = apply(swh_cal3_3D[,1:ind_latnord,], 3, mean)
y4cal4 = apply(swh_cal4_3D[,1:ind_latnord,], 3, mean)

# moyenne sur bande sud de PolynsGL
ind_latsud = which(lat==-27)
nlat = length(lat)
x5 = apply(swh_wav_3D[,ind_latsud:nlat,], 3, mean)
y5cal1 = apply(swh_cal1_3D[,ind_latsud:nlat,], 3, mean)
y5cal2 = apply(swh_cal2_3D[,ind_latsud:nlat,], 3, mean)
y5cal3 = apply(swh_cal3_3D[,ind_latsud:nlat,], 3, mean)
y5cal4 = apply(swh_cal4_3D[,ind_latsud:nlat,], 3, mean)

# make dataframes
nqq = length(qq)
df1 = data.frame(rep(qq,4),rep(x1,4),c(y1cal1,y1cal2,y1cal3,y1cal4),
                 c(rep("1",nqq),rep("2",nqq),rep("3",nqq),rep("4",nqq)))
colnames(df1) = c("quant","wav","mfwam","calib")

df2 = data.frame(rep(qq,4),rep(x2,4),c(y2cal1,y2cal2,y2cal3,y2cal4),
                  c(rep("1",nqq),rep("2",nqq),rep("3",nqq),rep("4",nqq)))
colnames(df2) = c("quant","wav","mfwam","calib")

df3 = data.frame(rep(qq,4),rep(x3,4),c(y3cal1,y3cal2,y3cal3,y3cal4),
                  c(rep("1",nqq),rep("2",nqq),rep("3",nqq),rep("4",nqq)))
colnames(df3) = c("quant","wav","mfwam","calib")

df4 = data.frame(rep(qq,4),rep(x4,4),c(y4cal1,y4cal2,y4cal3,y4cal4),
                  c(rep("1",nqq),rep("2",nqq),rep("3",nqq),rep("4",nqq)))
colnames(df4) = c("quant","wav","mfwam","calib")

df5 = data.frame(rep(qq,4),rep(x5,4),c(y5cal1,y5cal2,y5cal3,y5cal4),
                  c(rep("1",nqq),rep("2",nqq),rep("3",nqq),rep("4",nqq)))
colnames(df5) = c("quant","wav","mfwam","calib")


# plot QQ graph
gg1=ggplot(df1, aes(x=wav, y=mfwam, group=calib,color=calib)) + geom_line(linetype="dashed") + geom_point() +
  scale_x_continuous(limits = c(1,6.5)) + 
  scale_y_continuous(limits = c(1,6.5)) +
  geom_abline(slope=1,intercept=0) +
  ggtitle("QQ SWH 2050-2079 (PolynsGL) - (5°S-210°E)") +
  labs(color = "Calib. ID") +
  xlab("SWH (WAVERYS) (m)") + ylab("SWH (MFWAM) (m)") +
  scale_color_manual(labels = c("1", "2", "3", "4"), values = c("red", "blue", "green", "gold")) +
  theme(text = element_text(size=15)) 
print(gg1)
ggsave(gg1, file=paste(figpath,"QQ_SWH_2050_2079_pt5S210E",".png",sep=""), width = 16, height = 10, units = "cm")


gg2=ggplot(df2, aes(x=wav, y=mfwam, group=calib,color=calib)) + geom_line(linetype="dashed") + geom_point() +
  scale_x_continuous(limits = c(1,6.5)) + 
  scale_y_continuous(limits = c(1,6.5)) +
  geom_abline(slope=1,intercept=0) +
  ggtitle("QQ SWH 2050-2079 (PolynsGL) - (27°S-210°E)") +
  labs(color = "Calib. ID") +
  xlab("SWH (WAVERYS) (m)") + ylab("SWH (MFWAM) (m)") +
  scale_color_manual(labels = c("1", "2", "3", "4"), values = c("red", "blue", "green", "gold")) +
  theme(text = element_text(size=15)) 
print(gg2)
ggsave(gg2, file=paste(figpath,"QQ_SWH_2050_2079_pt27S210E",".png",sep=""), width = 16, height = 10, units = "cm")


gg3=ggplot(df3, aes(x=wav, y=mfwam, group=calib,color=calib)) + geom_line(linetype="dashed") + geom_point() +
  scale_x_continuous(limits = c(1,6.5)) + 
  scale_y_continuous(limits = c(1,6.5)) +
  geom_abline(slope=1,intercept=0) +
  ggtitle("QQ SWH 2050-2079 (PolynsGL) - (moyenne)") +
  labs(color = "Calib. ID") +
  xlab("SWH (WAVERYS) (m)") + ylab("SWH (MFWAM) (m)") +
  scale_color_manual(labels = c("1", "2", "3", "4"), values = c("red", "blue", "green", "gold")) +
  theme(text = element_text(size=15)) 
print(gg3)
ggsave(gg3, file=paste(figpath,"QQ_SWH_2050_2079_moyenne",".png",sep=""), width = 16, height = 10, units = "cm")


gg4=ggplot(df4, aes(x=wav, y=mfwam, group=calib,color=calib)) + geom_line(linetype="dashed") + geom_point() +
  scale_x_continuous(limits = c(1,6.5)) + 
  scale_y_continuous(limits = c(1,6.5)) +
  geom_abline(slope=1,intercept=0) +
#  ggtitle("QQ SWH 2050-2079 (PolynsGL) - (moy 0°-8°S)") +
  ggtitle("Climat 2000: QQ Hs (PolynsGL) - (moy 0°-8°S)") +
  labs(color = "Calib. ID") +
  xlab("Hs (WAVERYS) (m)") + ylab("Hs (MFWAM) (m)") +
  scale_color_manual(labels = c("1", "2", "3", "4"), values = c("red", "blue", "green", "gold")) +
  theme(text = element_text(size=15)) 
print(gg4)
ggsave(gg4, file=paste(figpath,"QQ_SWH_2050_2079_moy0_8degsud","_v1.png",sep=""), width = 16, height = 10, units = "cm")

gg5=ggplot(df5, aes(x=wav, y=mfwam, group=calib,color=calib)) + geom_line(linetype="dashed") + geom_point() +
  scale_x_continuous(limits = c(1,6.5)) + 
  scale_y_continuous(limits = c(1,6.5)) +
  geom_abline(slope=1,intercept=0) +
#  ggtitle("QQ SWH 2050-2079 (PolynsGL) - (moy 27°S-30°S)") +
  ggtitle("Climat 2000: QQ Hs (PolynsGL) - (moy 27°S-30°S)") +
  labs(color = "Calib. ID") +
  xlab("Hs (WAVERYS) (m)") + ylab("Hs (MFWAM) (m)") +
  scale_color_manual(labels = c("1", "2", "3", "4"), values = c("red", "blue", "green", "gold")) +
  theme(text = element_text(size=15)) 
print(gg5)
ggsave(gg5, file=paste(figpath,"QQ_SWH_2050_2079_moy27_30degsud","_v1.png",sep=""), width = 16, height = 10, units = "cm")



