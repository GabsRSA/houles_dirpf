library(ggplot2)
library(mapdata)
library(timeDate)
library(data.table)
library(tidyverse)

# load the ncdf4 package
library(ncdf4)

# Generation des indices nino3.4 à partir des tempé moy mensuelles et des climatologies (moy sur 30 ans)
# L'objectif est de re-injecter ces infos dans l'analyse des fortes houles (Analyse_area_mask_houle_intense.R)

# On recupere d'abord les temp moy mens dans TSURCE0Nino34_2050_2449.nc (sur 2050-2079)
# L'objectif est ensuite d'identifier a quelle situation ENSO correspondent les evenenements intenses detectes

# On rajoute les cas de RCP45 (CE1) /RCP85 (CE2)

# path figures
figpath <- paste("/Users/gablellouch/Documents/Meteo_France/ENM/ienm3/houles_dirpf/Figures/Belenos/Nino34/",sep="")

# set path and filename temp moy
ncpath <- "/Users/gablellouch/Documents/Meteo_France/ENM/ienm3/houles_dirpf/Belenos/Nino34/"
ncnameCE0 <- "TSURCE0Nino34_2050_2449.nc"  
ncnameCE1 <- "TSURCE1Nino34_2050_2449.nc"
ncnameCE2 <- "TSURCE2Nino34_2050_2449.nc"

ncfnameCE0 <- paste(ncpath, ncnameCE0, sep="")
ncfnameCE1 <- paste(ncpath, ncnameCE1, sep="")
ncfnameCE2 <- paste(ncpath, ncnameCE2, sep="")
dname <- "var11" 

# open a netCDF file
ncinCE0 <- nc_open(ncfnameCE0)
ncinCE1 <- nc_open(ncfnameCE1)
ncinCE2 <- nc_open(ncfnameCE2)
#print(ncin)

# get time
time <- ncvar_get(ncinCE0,"time")
time = time[-c(4799,4800)] # on enleve les 2 dernieres valeurs qui represent NDJ et DJF pour 2449/2500

tunits <- ncatt_get(ncinCE0,"time","units")
nt <- dim(time)

time_posixct <- seq(ISOdatetime(2050,2,15,0,0,0,tz="UTC"),ISOdatetime(2449,11,15,0,0,0,tz="UTC"), by="1 month")

# get tmoy
tmoyCE0 <- ncvar_get(ncinCE0,dname) - 273.15
tmoyCE1 <- ncvar_get(ncinCE1,dname) - 273.15
tmoyCE2 <- ncvar_get(ncinCE2,dname) - 273.15
dim(tmoyCE0)
tmoyCE0 = tmoyCE0[-c(4799,4800)]
tmoyCE1 = tmoyCE1[-c(4799,4800)]
tmoyCE2 = tmoyCE2[-c(4799,4800)]

# make dataframe
dfCE0 = data.frame(time,time_posixct,tmoyCE0)
dfCE1 = data.frame(time,time_posixct,tmoyCE1)
dfCE2 = data.frame(time,time_posixct,tmoyCE2)

dfCE0_wide = dcast(dfCE0,year(time_posixct) ~ month(time_posixct)) 
colnames(dfCE0_wide) = c("year","DJF","JFM","FMA","MAM","AMJ","MJJ","JJA","JAS","ASO","SON","OND","NDJ")

dfCE1_wide = dcast(dfCE1,year(time_posixct) ~ month(time_posixct)) 
colnames(dfCE1_wide) = c("year","DJF","JFM","FMA","MAM","AMJ","MJJ","JJA","JAS","ASO","SON","OND","NDJ")

dfCE2_wide = dcast(dfCE2,year(time_posixct) ~ month(time_posixct)) 
colnames(dfCE2_wide) = c("year","DJF","JFM","FMA","MAM","AMJ","MJJ","JJA","JAS","ASO","SON","OND","NDJ")

# On recupere ensuite les climatologies
ncfnameCE0_clim = c();
ncfnameCE1_clim = c();
ncfnameCE2_clim = c();
i = 1
trimest = c("JFM","FMA","MAM","AMJ","MJJ","JJA","JAS","ASO","SON","OND","NDJ","DJF")
for (im in trimest) {
  ncfnameCE0_clim[i] <- paste(ncpath,"climato/CE0/","TSURCE0Nino34_2050_2449_",im,"_climato.nc",sep='')
  ncfnameCE1_clim[i] <- paste(ncpath,"climato/CE1/","TSURCE1Nino34_2050_2449_",im,"_climato.nc",sep='')
  ncfnameCE2_clim[i] <- paste(ncpath,"climato/CE2/","TSURCE2Nino34_2050_2449_",im,"_climato.nc",sep='')
  i = i+1
}

dname <- "var11" 
ncinCE0_clim1 <- nc_open(ncfnameCE0_clim[1])
ncinCE0_clim2 <- nc_open(ncfnameCE0_clim[2])
ncinCE0_clim3 <- nc_open(ncfnameCE0_clim[3])
ncinCE0_clim4 <- nc_open(ncfnameCE0_clim[4])
ncinCE0_clim5 <- nc_open(ncfnameCE0_clim[5])
ncinCE0_clim6 <- nc_open(ncfnameCE0_clim[6])
ncinCE0_clim7 <- nc_open(ncfnameCE0_clim[7])
ncinCE0_clim8 <- nc_open(ncfnameCE0_clim[8])
ncinCE0_clim9 <- nc_open(ncfnameCE0_clim[9])
ncinCE0_clim10 <- nc_open(ncfnameCE0_clim[10])
ncinCE0_clim11 <- nc_open(ncfnameCE0_clim[11])
ncinCE0_clim12 <- nc_open(ncfnameCE0_clim[12])

tmoyCE0clim1 <- rep(ncvar_get(ncinCE0_clim1,dname) - 273.15,c(40,rep(10,36),0))
tmoyCE0clim2 <- rep(ncvar_get(ncinCE0_clim2,dname) - 273.15,c(40,rep(10,36),0))
tmoyCE0clim3 <- rep(ncvar_get(ncinCE0_clim3,dname) - 273.15, c(40,rep(10,36),0))
tmoyCE0clim4 <- rep(ncvar_get(ncinCE0_clim4,dname) - 273.15, c(40,rep(10,36),0))
tmoyCE0clim5 <- rep(ncvar_get(ncinCE0_clim5,dname) - 273.15, c(40,rep(10,36),0))
tmoyCE0clim6 <- rep(ncvar_get(ncinCE0_clim6,dname) - 273.15, c(40,rep(10,36),0))
tmoyCE0clim7 <- rep(ncvar_get(ncinCE0_clim7,dname) - 273.15, c(40,rep(10,36),0))
tmoyCE0clim8 <- rep(ncvar_get(ncinCE0_clim8,dname) - 273.15, c(40,rep(10,36),0))
tmoyCE0clim9 <- rep(ncvar_get(ncinCE0_clim9,dname) - 273.15, c(40,rep(10,36),0))
tmoyCE0clim10 <- rep(ncvar_get(ncinCE0_clim10,dname) - 273.15, c(40,rep(10,36),0))
tmoyCE0clim11 <- rep(ncvar_get(ncinCE0_clim11,dname) - 273.15, c(40,rep(10,36),0))
tmoyCE0clim12 <- rep(ncvar_get(ncinCE0_clim12,dname) - 273.15, c(40,rep(10,36),0))

ncinCE1_clim1 <- nc_open(ncfnameCE1_clim[1])
ncinCE1_clim2 <- nc_open(ncfnameCE1_clim[2])
ncinCE1_clim3 <- nc_open(ncfnameCE1_clim[3])
ncinCE1_clim4 <- nc_open(ncfnameCE1_clim[4])
ncinCE1_clim5 <- nc_open(ncfnameCE1_clim[5])
ncinCE1_clim6 <- nc_open(ncfnameCE1_clim[6])
ncinCE1_clim7 <- nc_open(ncfnameCE1_clim[7])
ncinCE1_clim8 <- nc_open(ncfnameCE1_clim[8])
ncinCE1_clim9 <- nc_open(ncfnameCE1_clim[9])
ncinCE1_clim10 <- nc_open(ncfnameCE1_clim[10])
ncinCE1_clim11 <- nc_open(ncfnameCE1_clim[11])
ncinCE1_clim12 <- nc_open(ncfnameCE1_clim[12])

tmoyCE1clim1 <- rep(ncvar_get(ncinCE1_clim1,dname) - 273.15,c(40,rep(10,36),0))
tmoyCE1clim2 <- rep(ncvar_get(ncinCE1_clim2,dname) - 273.15,c(40,rep(10,36),0))
tmoyCE1clim3 <- rep(ncvar_get(ncinCE1_clim3,dname) - 273.15, c(40,rep(10,36),0))
tmoyCE1clim4 <- rep(ncvar_get(ncinCE1_clim4,dname) - 273.15, c(40,rep(10,36),0))
tmoyCE1clim5 <- rep(ncvar_get(ncinCE1_clim5,dname) - 273.15, c(40,rep(10,36),0))
tmoyCE1clim6 <- rep(ncvar_get(ncinCE1_clim6,dname) - 273.15, c(40,rep(10,36),0))
tmoyCE1clim7 <- rep(ncvar_get(ncinCE1_clim7,dname) - 273.15, c(40,rep(10,36),0))
tmoyCE1clim8 <- rep(ncvar_get(ncinCE1_clim8,dname) - 273.15, c(40,rep(10,36),0))
tmoyCE1clim9 <- rep(ncvar_get(ncinCE1_clim9,dname) - 273.15, c(40,rep(10,36),0))
tmoyCE1clim10 <- rep(ncvar_get(ncinCE1_clim10,dname) - 273.15, c(40,rep(10,36),0))
tmoyCE1clim11 <- rep(ncvar_get(ncinCE1_clim11,dname) - 273.15, c(40,rep(10,36),0))
tmoyCE1clim12 <- rep(ncvar_get(ncinCE1_clim12,dname) - 273.15, c(40,rep(10,36),0))

ncinCE2_clim1 <- nc_open(ncfnameCE2_clim[1])
ncinCE2_clim2 <- nc_open(ncfnameCE2_clim[2])
ncinCE2_clim3 <- nc_open(ncfnameCE2_clim[3])
ncinCE2_clim4 <- nc_open(ncfnameCE2_clim[4])
ncinCE2_clim5 <- nc_open(ncfnameCE2_clim[5])
ncinCE2_clim6 <- nc_open(ncfnameCE2_clim[6])
ncinCE2_clim7 <- nc_open(ncfnameCE2_clim[7])
ncinCE2_clim8 <- nc_open(ncfnameCE2_clim[8])
ncinCE2_clim9 <- nc_open(ncfnameCE2_clim[9])
ncinCE2_clim10 <- nc_open(ncfnameCE2_clim[10])
ncinCE2_clim11 <- nc_open(ncfnameCE2_clim[11])
ncinCE2_clim12 <- nc_open(ncfnameCE2_clim[12])

tmoyCE2clim1 <- rep(ncvar_get(ncinCE2_clim1,dname) - 273.15,c(40,rep(10,36),0))
tmoyCE2clim2 <- rep(ncvar_get(ncinCE2_clim2,dname) - 273.15,c(40,rep(10,36),0))
tmoyCE2clim3 <- rep(ncvar_get(ncinCE2_clim3,dname) - 273.15, c(40,rep(10,36),0))
tmoyCE2clim4 <- rep(ncvar_get(ncinCE2_clim4,dname) - 273.15, c(40,rep(10,36),0))
tmoyCE2clim5 <- rep(ncvar_get(ncinCE2_clim5,dname) - 273.15, c(40,rep(10,36),0))
tmoyCE2clim6 <- rep(ncvar_get(ncinCE2_clim6,dname) - 273.15, c(40,rep(10,36),0))
tmoyCE2clim7 <- rep(ncvar_get(ncinCE2_clim7,dname) - 273.15, c(40,rep(10,36),0))
tmoyCE2clim8 <- rep(ncvar_get(ncinCE2_clim8,dname) - 273.15, c(40,rep(10,36),0))
tmoyCE2clim9 <- rep(ncvar_get(ncinCE2_clim9,dname) - 273.15, c(40,rep(10,36),0))
tmoyCE2clim10 <- rep(ncvar_get(ncinCE2_clim10,dname) - 273.15, c(40,rep(10,36),0))
tmoyCE2clim11 <- rep(ncvar_get(ncinCE2_clim11,dname) - 273.15, c(40,rep(10,36),0))
tmoyCE2clim12 <- rep(ncvar_get(ncinCE2_clim12,dname) - 273.15, c(40,rep(10,36),0))

dfCE0_clim_wide = data.frame(unique(year(time_posixct)),tmoyCE0clim12,tmoyCE0clim1,tmoyCE0clim2,tmoyCE0clim3,tmoyCE0clim4,tmoyCE0clim5,tmoyCE0clim6,tmoyCE0clim7,tmoyCE0clim8,tmoyCE0clim9,tmoyCE0clim10,tmoyCE0clim11)
colnames(dfCE0_clim_wide) = c("year","DJF","JFM","FMA","MAM","AMJ","MJJ","JJA","JAS","ASO","SON","OND","NDJ")

dfCE1_clim_wide = data.frame(unique(year(time_posixct)),tmoyCE1clim12,tmoyCE1clim1,tmoyCE1clim2,tmoyCE1clim3,tmoyCE1clim4,tmoyCE1clim5,tmoyCE1clim6,tmoyCE1clim7,tmoyCE1clim8,tmoyCE1clim9,tmoyCE1clim10,tmoyCE1clim11)
colnames(dfCE1_clim_wide) = c("year","DJF","JFM","FMA","MAM","AMJ","MJJ","JJA","JAS","ASO","SON","OND","NDJ")

dfCE2_clim_wide = data.frame(unique(year(time_posixct)),tmoyCE2clim12,tmoyCE2clim1,tmoyCE2clim2,tmoyCE2clim3,tmoyCE2clim4,tmoyCE2clim5,tmoyCE2clim6,tmoyCE2clim7,tmoyCE2clim8,tmoyCE2clim9,tmoyCE2clim10,tmoyCE2clim11)
colnames(dfCE2_clim_wide) = c("year","DJF","JFM","FMA","MAM","AMJ","MJJ","JJA","JAS","ASO","SON","OND","NDJ")

tmoyCE0_clim = as.vector(t(dfCE0_clim_wide[,c(-1)]))
tmoyCE0_clim = tmoyCE0_clim[-c(4799,4800)]
dfCE0_clim = data.frame(time_posixct,tmoyCE0_clim)

tmoyCE1_clim = as.vector(t(dfCE1_clim_wide[,c(-1)]))
tmoyCE1_clim = tmoyCE1_clim[-c(4799,4800)]
dfCE1_clim = data.frame(time_posixct,tmoyCE1_clim)

tmoyCE2_clim = as.vector(t(dfCE2_clim_wide[,c(-1)]))
tmoyCE2_clim = tmoyCE2_clim[-c(4799,4800)]
dfCE2_clim = data.frame(time_posixct,tmoyCE2_clim)

# check
gg=ggplot(NULL) +
  geom_line(data=dfCE0,aes(x=time_posixct,y=tmoyCE0,colour="tmoyCE0"), size=0.8) +
  geom_line(data=dfCE0_clim,aes(x=time_posixct,y=tmoyCE0_clim,colour="tmoyCE0_clim"), size=0.8) +
  scale_colour_manual(name="",values=c("tmoyCE0"="red","tmoyCE0_clim"="darkblue"),labels=c("TSURF","TSURF CLIM")) + 
  geom_point(data=dfCE0,aes(x=time_posixct,y=tmoyCE0), shape = 21, colour = "black", fill = "red", size = 2, stroke = 0.5) +
  geom_point(data=dfCE0_clim,aes(x=time_posixct,y=tmoyCE0_clim), shape = 21, colour = "black", fill = "blue", size = 2, stroke = 0.5) +
  ggtitle(paste("TSURF niño 3.4",sep="")) +
  xlab("DATE") + ylab(paste("TSURF (°C)",sep="")) +
  #ylim(0, 350) +
  theme(text = element_text(size=15),legend.position = c(0.3,0.9), legend.direction = "horizontal"  ) +
  xlim(time_posixct[1], time_posixct[96])
print(gg)
ggsave(gg, file=paste(figpath,"TsurfCE0_nino34_vs_clim","_2050_2057","_v2.png",sep=""), width = 14, height = 9, units = "cm")



# calcul de l'indice nino 3.4
nino34CE0 = tmoyCE0 - tmoyCE0_clim
nino34CE1 = tmoyCE1 - tmoyCE1_clim
nino34CE2 = tmoyCE2 - tmoyCE2_clim
Nino_34_tri = data.frame(time_posixct,nino34CE0)

# récupéré du projet DETECT

# caracterisation des periodes El Nino / La Nina (seuil de +/-0.5 depasse pour 5 saisons consecutives)
Nino_34_tri[["CE0_Plus0_5"]] = ifelse(Nino_34_tri[["nino34CE0"]] >= 0.5, 1, 0)
Nino_34_tri[["CE0_Moins0_5"]] = ifelse(Nino_34_tri[["nino34CE0"]] <= -0.5, -1, 0)
Nino_34_tri[["CE0_Nino"]] = rep("NA",nrow(Nino_34_tri))
Nino_34_tri[["CE0_Nina"]] = rep("NA",nrow(Nino_34_tri))

for (i in 1:nrow(Nino_34_tri)) {
  # on regarde s’il y a 5 saisons consecutives ou le seuil +0.5 est depasse
  seq1=seq(max(1,i-4),min(max(1,i-4)+4,nrow(Nino_34_tri)))
  seq2=seq(max(1,i-3),min(max(1,i-3)+4,nrow(Nino_34_tri)))
  seq3=seq(max(1,i-2),min(max(1,i-2)+4,nrow(Nino_34_tri)))
  seq4=seq(max(1,i-1),min(max(1,i-1)+4,nrow(Nino_34_tri)))
  seq5=seq(max(1,i),min(max(1,i)+4,nrow(Nino_34_tri)))
  Nino_34_tri[i,5]=ifelse(sum(Nino_34_tri[seq1,3])==5 || sum(Nino_34_tri[seq2,3])==5 || sum(Nino_34_tri[seq3,3])==5 || sum(Nino_34_tri[seq4,3])==5 || sum(Nino_34_tri[seq5,3])==5, 1, 0)
  Nino_34_tri[i,6]=ifelse(sum(Nino_34_tri[seq1,4])==-5 || sum(Nino_34_tri[seq2,4])==-5 || sum(Nino_34_tri[seq3,4])==-5 || sum(Nino_34_tri[seq4,4])==-5 || sum(Nino_34_tri[seq5,4])==-5, 1, 0)
}

# indique si Nino, Nina ou rien dans une meme colonne
Nino_34_tri[["CE0_Episode"]] = ifelse(Nino_34_tri[["CE0_Nino"]] == 1, "Nino",ifelse(Nino_34_tri[["CE0_Nina"]] == 1, "Nina", "Neutre"))
Nino_34_tri[["CE0_EpisodeNum"]] = ifelse(Nino_34_tri[["CE0_Nino"]] == 1, 1,ifelse(Nino_34_tri[["CE0_Nina"]] == 1, -1, 0))


Nino_34_tri[["nino34CE1"]] = nino34CE1
# caracterisation des periodes El Nino / La Nina (seuil de +/-0.5 depasse pour 5 saisons consecutives)
Nino_34_tri[["CE1_Plus0_5"]] = ifelse(Nino_34_tri[["nino34CE1"]] >= 0.5, 1, 0)
Nino_34_tri[["CE1_Moins0_5"]] = ifelse(Nino_34_tri[["nino34CE1"]] <= -0.5, -1, 0)
Nino_34_tri[["CE1_Nino"]] = rep("NA",nrow(Nino_34_tri))
Nino_34_tri[["CE1_Nina"]] = rep("NA",nrow(Nino_34_tri))

for (i in 1:nrow(Nino_34_tri)) {
  # on regarde s’il y a 5 saisons consecutives ou le seuil +0.5 est depasse
  seq1=seq(max(1,i-4),min(max(1,i-4)+4,nrow(Nino_34_tri)))
  seq2=seq(max(1,i-3),min(max(1,i-3)+4,nrow(Nino_34_tri)))
  seq3=seq(max(1,i-2),min(max(1,i-2)+4,nrow(Nino_34_tri)))
  seq4=seq(max(1,i-1),min(max(1,i-1)+4,nrow(Nino_34_tri)))
  seq5=seq(max(1,i),min(max(1,i)+4,nrow(Nino_34_tri)))
  Nino_34_tri[i,12]=ifelse(sum(Nino_34_tri[seq1,10])==5 || sum(Nino_34_tri[seq2,10])==5 || sum(Nino_34_tri[seq3,10])==5 || sum(Nino_34_tri[seq4,10])==5 || sum(Nino_34_tri[seq5,10])==5, 1, 0)
  Nino_34_tri[i,13]=ifelse(sum(Nino_34_tri[seq1,11])==-5 || sum(Nino_34_tri[seq2,11])==-5 || sum(Nino_34_tri[seq3,11])==-5 || sum(Nino_34_tri[seq4,11])==-5 || sum(Nino_34_tri[seq5,11])==-5, 1, 0)
}

# indique si Nino, Nina ou rien dans une meme colonne
Nino_34_tri[["CE1_Episode"]] = ifelse(Nino_34_tri[["CE1_Nino"]] == 1, "Nino",ifelse(Nino_34_tri[["CE1_Nina"]] == 1, "Nina", "Neutre"))
Nino_34_tri[["CE1_EpisodeNum"]] = ifelse(Nino_34_tri[["CE1_Nino"]] == 1, 1,ifelse(Nino_34_tri[["CE1_Nina"]] == 1, -1, 0))


Nino_34_tri[["nino34CE2"]] = nino34CE2
# caracterisation des periodes El Nino / La Nina (seuil de +/-0.5 depasse pour 5 saisons consecutives)
Nino_34_tri[["CE2_Plus0_5"]] = ifelse(Nino_34_tri[["nino34CE2"]] >= 0.5, 1, 0)
Nino_34_tri[["CE2_Moins0_5"]] = ifelse(Nino_34_tri[["nino34CE2"]] <= -0.5, -1, 0)
Nino_34_tri[["CE2_Nino"]] = rep("NA",nrow(Nino_34_tri))
Nino_34_tri[["CE2_Nina"]] = rep("NA",nrow(Nino_34_tri))

for (i in 1:nrow(Nino_34_tri)) {
  # on regarde s’il y a 5 saisons consecutives ou le seuil +0.5 est depasse
  seq1=seq(max(1,i-4),min(max(1,i-4)+4,nrow(Nino_34_tri)))
  seq2=seq(max(1,i-3),min(max(1,i-3)+4,nrow(Nino_34_tri)))
  seq3=seq(max(1,i-2),min(max(1,i-2)+4,nrow(Nino_34_tri)))
  seq4=seq(max(1,i-1),min(max(1,i-1)+4,nrow(Nino_34_tri)))
  seq5=seq(max(1,i),min(max(1,i)+4,nrow(Nino_34_tri)))
  Nino_34_tri[i,19]=ifelse(sum(Nino_34_tri[seq1,17])==5 || sum(Nino_34_tri[seq2,17])==5 || sum(Nino_34_tri[seq3,17])==5 || sum(Nino_34_tri[seq4,17])==5 || sum(Nino_34_tri[seq5,17])==5, 1, 0)
  Nino_34_tri[i,20]=ifelse(sum(Nino_34_tri[seq1,18])==-5 || sum(Nino_34_tri[seq2,18])==-5 || sum(Nino_34_tri[seq3,18])==-5 || sum(Nino_34_tri[seq4,18])==-5 || sum(Nino_34_tri[seq5,18])==-5, 1, 0)
}

# indique si Nino, Nina ou rien dans une meme colonne
Nino_34_tri[["CE2_Episode"]] = ifelse(Nino_34_tri[["CE2_Nino"]] == 1, "Nino",ifelse(Nino_34_tri[["CE2_Nina"]] == 1, "Nina", "Neutre"))
Nino_34_tri[["CE2_EpisodeNum"]] = ifelse(Nino_34_tri[["CE2_Nino"]] == 1, 1,ifelse(Nino_34_tri[["CE2_Nina"]] == 1, -1, 0))


# affichage de l’anomalie Nino3.4 pour les periodes Nino et Nina uniquement
ggninoCE0 = ggplot(Nino_34_tri) + 
  geom_line(aes(x=time_posixct, y=nino34CE0, colour=CE0_Episode, group=1), size=1) + 
  scale_colour_manual(name="",values = c("Nino" = "red","Nina" = "blue", "Neutre" = "black"),labels=c("Neutre","Niña","Niño")) +
  xlab("DATE") + 
  ylab("ANOMALY TSURF NIÑO 3.4 (°C)") +
  xlim(time_posixct[1], time_posixct[360]) + 
  ylim(-2.8,2.8) +
  ggtitle(paste("CLIMAT 2000",sep="")) +
  theme(text = element_text(size=15), legend.position = c(0.3,0.9),legend.direction = "horizontal"  )
print(ggninoCE0)
ggsave(ggninoCE0, file=paste(figpath,"anom_nino34","_2050_2079_CE0","_v3.png",sep=""), width = 14, height = 9, units = "cm")

ggninoCE1 = ggplot(Nino_34_tri) + 
  geom_line(aes(x=time_posixct, y=nino34CE1, colour=CE1_Episode, group=1), size=1) + 
  scale_colour_manual(name="",values = c("Nino" = "red","Nina" = "blue", "Neutre" = "black"),labels=c("Neutre","Niña","Niño")) + 
  xlab("DATE") + 
  ylab("ANOMALY TSURF NIÑO 3.4 (°C)") +
  xlim(time_posixct[1], time_posixct[360]) + 
  ylim(-2.8,2.8) +
  ggtitle(paste("CLIMAT 2050, RCP4.5",sep="")) +
  theme(text = element_text(size=15), legend.position = c(10.3,10.9),legend.direction = "horizontal"  )
print(ggninoCE1)
ggsave(ggninoCE1, file=paste(figpath,"anom_nino34","_2050_2079_CE1","_v3.png",sep=""), width = 14, height = 9, units = "cm")

ggninoCE2 = ggplot(Nino_34_tri) + 
  geom_line(aes(x=time_posixct, y=nino34CE2, colour=CE2_Episode, group=1), size=1) + 
  scale_colour_manual(name="",values = c("Nino" = "red","Nina" = "blue", "Neutre" = "black"),labels=c("Neutre","Niña","Niño")) + 
  xlab("DATE") + 
  ylab("ANOMALY TSURF NIÑO 3.4 (°C)") +
  xlim(time_posixct[1], time_posixct[360]) + 
  ylim(-2.8,2.8) +
  ggtitle(paste("CLIMAT 2050, RCP8.5",sep="")) +
  theme(text = element_text(size=15), legend.position = c(10.3,10.9),legend.direction = "horizontal"  )
print(ggninoCE2)
ggsave(ggninoCE2, file=paste(figpath,"anom_nino34","_2050_2079_CE2","_v3.png",sep=""), width = 14, height = 9, units = "cm")


# decompte des episodes Nino/Nina par scenario
ninoCE0 = Nino_34_tri[Nino_34_tri$CE0_Episode=="Nino",]
ninaCE0 = Nino_34_tri[Nino_34_tri$CE0_Episode=="Nina",]
NBninoCE0 = sum(diff(as.numeric(rownames(ninoCE0)))>1)
NBninaCE0 = sum(diff(as.numeric(rownames(ninaCE0)))>1)

ninoCE1 = Nino_34_tri[Nino_34_tri$CE1_Episode=="Nino",]
ninaCE1 = Nino_34_tri[Nino_34_tri$CE1_Episode=="Nina",]
NBninoCE1 = sum(diff(as.numeric(rownames(ninoCE1)))>1)
NBninaCE1 = sum(diff(as.numeric(rownames(ninaCE1)))>1)

ninoCE2 = Nino_34_tri[Nino_34_tri$CE2_Episode=="Nino",]
ninaCE2 = Nino_34_tri[Nino_34_tri$CE2_Episode=="Nina",]
NBninoCE2 = sum(diff(as.numeric(rownames(ninoCE2)))>1)
NBninaCE2 = sum(diff(as.numeric(rownames(ninaCE2)))>1)

# construction des histogrammes NINO/NINA
Nino_34_tri$month = month(Nino_34_tri$time_posixct)
Nino_34_tri$month_abb=month.abb[Nino_34_tri$month]

Nino_34_tri$month_abb <-factor(Nino_34_tri$month_abb, 
                            levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                       "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))


Nino_34_tri_sub = Nino_34_tri[c(7,14,21,24)]
Nino_34_tri_melt = melt(Nino_34_tri_sub,id.vars="month_abb",measure.vars=c("CE0_Episode","CE1_Episode","CE2_Episode"))
colnames(Nino_34_tri_melt) = c("month_abb","Exp","Episode")

Nino_34_nino = Nino_34_tri_melt[Nino_34_tri_melt$Episode=="Nino",]
Nino_34_nina = Nino_34_tri_melt[Nino_34_tri_melt$Episode=="Nina",]

colours = c("CE0_Episode"="red","CE1_Episode"="green3","CE2_Episode"="blue")
colScale = scale_fill_manual(name="Exp.",values=colours, labels = c("2000","RCP4.5 2050","RCP8.5 2050"))
gghist1 = ggplot(Nino_34_nino,aes(x=month_abb,fill=Exp)) +
  geom_bar(position=position_dodge(width=0.9),width=0.9) +
  colScale +
  scale_x_discrete(labels=c("DJF","JFM","FMA","MAM","AMJ","MJJ","JJA","JAS","ASO","SON","OND","NDJ")) +
  theme(text = element_text(size=30), legend.title=element_blank(), legend.position = c(0.5,0.85),legend.direction = "vertical"  ) +
  geom_text(aes(label=..count..),stat='count',position=position_dodge(0.9), vjust=-0.2) + 
  labs(title=("Distribution of all Niño events (400 years)"), x = "Month", y = "Count") 
print(gghist1)
ggsave(gghist1, file=paste(figpath,"distri_nino_2000_vs_2050_rcp45_rcp85_400ans","_v2.png",sep=""), width = 28, height = 15, units = "cm")

# calcul des valeurs affichées par l'hist pour mettre dans le rapport
# 2000
## nino
# annee
126+131+133+129+80+39+43+58+81+113+117+120
# hiver austral
80+39+43+58+81
# ete austral
113+117+120+126+131+133+129

# 2050 RCP 4.5
## nino
# annee
133+138+141+134+82+40+59+80+105+127+128+132
# hiver austral
82+40+59+80+105
# ete austral
127+128+132+133+138+141+134

# 2050 RCP 8.5
## nino
# annee
125+126+130+127+88+48+60+85+114+125+128+127
# hiver austral
88+48+60+85+114
# ete austral
125+128+127+125+126+130+127


gghist2 = ggplot(Nino_34_nina,aes(x=month_abb,fill=Exp)) +
  geom_bar(position=position_dodge(width=0.9),width=0.9) +
  colScale +
  scale_x_discrete(labels=c("DJF","JFM","FMA","MAM","AMJ","MJJ","JJA","JAS","ASO","SON","OND","NDJ")) +
  theme(text = element_text(size=30), legend.title=element_blank(), legend.position = c(10.5,10.85),legend.direction = "vertical"  ) +
  geom_text(aes(label=..count..),stat='count',position=position_dodge(0.9), vjust=-0.2) + 
  labs(title=("Distribution of all Niña events (400 years)"), x = "Month", y = "Count") 
print(gghist2)
ggsave(gghist2, file=paste(figpath,"distri_nina_2000_vs_2050_rcp45_rcp85_400ans","_v2.png",sep=""), width = 28, height = 15, units = "cm")

# calcul des valeurs affichées par l'hist pour mettre dans le rapport
# 2000
## nina
# annee
122+99+60+39+49+92+126+131+133+132+133+129
# hiver austral
49+92+126+131+133
# ete austral
132+133+129+122+99+60+39

# 2050 RCP 4.5
## nina
# annee
126+109+66+45+49+93+118+127+132+135+136+133
# hiver austral
49+93+118+127+132
# ete austral
135+136+133+126+109+66+45
# 2050 RCP 8.5
## nina
# annee
134+112+66+45+46+82+115+124+139+143+146+144
# hiver austral
46+82+115+124+139
# ete austral
143+146+144+134+112+66+45

# analyse des intensites des evenements versus duree
# nino
ind_start = 1
nino34CE0_mean = c()
nino34CE0_duration = c()
for (i in seq(1,NBninoCE0)) {
  ind_stop = which(diff(as.numeric(rownames(ninoCE0)))>1)[i]
  nino34CE0_mean[i] = mean(ninoCE0[ind_start:ind_stop,"nino34CE0"])
  nino34CE0_duration[i] = ind_stop - ind_start + 1
  ind_start = ind_stop + 1
}

ind_start = 1
nino34CE1_mean = c()
nino34CE1_duration = c()
for (i in seq(1,NBninoCE1)) {
  ind_stop = which(diff(as.numeric(rownames(ninoCE1)))>1)[i]
  nino34CE1_mean[i] = mean(ninoCE1[ind_start:ind_stop,"nino34CE1"])
  nino34CE1_duration[i] = ind_stop - ind_start + 1
  ind_start = ind_stop + 1
}

ind_start = 1
nino34CE2_mean = c()
nino34CE2_duration = c()
for (i in seq(1,NBninoCE2)) {
  ind_stop = which(diff(as.numeric(rownames(ninoCE2)))>1)[i]
  nino34CE2_mean[i] = mean(ninoCE2[ind_start:ind_stop,"nino34CE2"])
  nino34CE2_duration[i] = ind_stop - ind_start + 1
  ind_start = ind_stop + 1
}

dfnino_intensite_duree_CE0 = data.frame(nino34CE0_mean,nino34CE0_duration)
dfnino_intensite_duree_CE0$Exp = "2000"
colnames(dfnino_intensite_duree_CE0) = c("mean","duration","Exp.")

dfnino_intensite_duree_CE1 = data.frame(nino34CE1_mean,nino34CE1_duration)
dfnino_intensite_duree_CE1$Exp = "RCP4.5 2050"
colnames(dfnino_intensite_duree_CE1) = c("mean","duration","Exp.")

dfnino_intensite_duree_CE2 = data.frame(nino34CE2_mean,nino34CE2_duration)
dfnino_intensite_duree_CE2$Exp = "RCP8.5 2050"
colnames(dfnino_intensite_duree_CE2) = c("mean","duration","Exp.")

dfnino_intensite_duree = rbind(dfnino_intensite_duree_CE0,dfnino_intensite_duree_CE1,dfnino_intensite_duree_CE2)

ggs = ggplot(dfnino_intensite_duree,aes(x=duration,y=mean,group=Exp.)) +
  geom_point(aes(shape=Exp.,color=Exp.,size=Exp.)) +
  scale_shape_manual(values=c(17,16,3)) +
  scale_color_manual(values=c("red","green3","blue")) +
  scale_size_manual(values=c(3,2,2)) +
  labs(title=paste("Niño events - 400 years"),x="Event duration (time unit, 1 month)", y = "Tsurf anomaly (°C)") +
#  xlim(0,200) +
  ylim(0.1,2.3) +
  theme(text = element_text(size=17), legend.title=element_blank(), legend.position = c(0.5,0.1),legend.direction = "horizontal"  ) 
  print(ggs)
ggsave(ggs, file=paste(figpath,"intensity_vs_duration_nino_2000_vs_2050_rcp45_rcp85_400ans","_v1.png",sep=""), width = 13, height = 9, units = "cm")



# nina
ind_start = 1
nina34CE0_mean = c()
nina34CE0_duration = c()
for (i in seq(1,NBninaCE0)) {
  ind_stop = which(diff(as.numeric(rownames(ninaCE0)))>1)[i]
  nina34CE0_mean[i] = mean(ninaCE0[ind_start:ind_stop,"nino34CE0"])
  nina34CE0_duration[i] = ind_stop - ind_start + 1
  ind_start = ind_stop + 1
}

ind_start = 1
nina34CE1_mean = c()
nina34CE1_duration = c()
for (i in seq(1,NBninaCE1)) {
  ind_stop = which(diff(as.numeric(rownames(ninaCE1)))>1)[i]
  nina34CE1_mean[i] = mean(ninaCE1[ind_start:ind_stop,"nino34CE1"])
  nina34CE1_duration[i] = ind_stop - ind_start + 1
  ind_start = ind_stop + 1
}

ind_start = 1
nina34CE2_mean = c()
nina34CE2_duration = c()
for (i in seq(1,NBninaCE2)) {
  ind_stop = which(diff(as.numeric(rownames(ninaCE2)))>1)[i]
  nina34CE2_mean[i] = mean(ninaCE2[ind_start:ind_stop,"nino34CE2"])
  nina34CE2_duration[i] = ind_stop - ind_start + 1
  ind_start = ind_stop + 1
}

dfnina_intensite_duree_CE0 = data.frame(nina34CE0_mean,nina34CE0_duration)
dfnina_intensite_duree_CE0$Exp = "2000"
colnames(dfnina_intensite_duree_CE0) = c("mean","duration","Exp.")

dfnina_intensite_duree_CE1 = data.frame(nina34CE1_mean,nina34CE1_duration)
dfnina_intensite_duree_CE1$Exp = "RCP4.5 2050"
colnames(dfnina_intensite_duree_CE1) = c("mean","duration","Exp.")

dfnina_intensite_duree_CE2 = data.frame(nina34CE2_mean,nina34CE2_duration)
dfnina_intensite_duree_CE2$Exp = "RCP8.5 2050"
colnames(dfnina_intensite_duree_CE2) = c("mean","duration","Exp.")

dfnina_intensite_duree = rbind(dfnina_intensite_duree_CE0,dfnina_intensite_duree_CE1,dfnina_intensite_duree_CE2)

ggs2 = ggplot(dfnina_intensite_duree,aes(x=duration,y=mean,group=Exp.)) +
  geom_point(aes(shape=Exp.,color=Exp.,size=Exp.)) +
  scale_shape_manual(values=c(17,16,3)) +
  scale_color_manual(values=c("red","green3","blue")) +
  scale_size_manual(values=c(3,2,2)) +
  labs(title=paste("Niña events - 400 years"),x="Event duration (time unit, 1 month)", y = "Tsurf anomaly (°C)") +
  #  xlim(0,200) +
  ylim(-2.3,-0.5) +
  theme(text = element_text(size=17), legend.title=element_blank(), legend.position = c(10.5,10.1),legend.direction = "horizontal"  ) 
  print(ggs2)
ggsave(ggs2, file=paste(figpath,"intensity_vs_duration_nina_2000_vs_2050_rcp45_rcp85_400ans","_v1.png",sep=""), width = 13, height = 9, units = "cm")


# Analyse de Tsurf sur la boite Nino3.4
df_Tsurf = data.frame(time_posixct,month(time_posixct),year(time_posixct),tmoyCE0,tmoyCE1,tmoyCE2)
colnames(df_Tsurf)[c(2,3)] = c("month","year")

mean_month = data.frame()
sd_month = data.frame()

for (i in seq(1,12)) {
  mean_month[i,c(1,2,3,4)] = as.numeric(colMeans(df_Tsurf[df_Tsurf$month==i,][c(2,4,5,6)]))
  sd_month[i,c(1)] = i
  sd_month[i,c(2,3,4)] = as.numeric(apply(df_Tsurf[df_Tsurf$month==i,][c(4,5,6)],2,sd))
}
colnames(mean_month) = c("month","2000","RCP4.5 2050","RCP8.5 2050")
colnames(sd_month) = c("month","2000","RCP4.5 2050","RCP8.5 2050")

df_Tsurf_stat = cbind(melt(mean_month,id.vars="month"),melt(sd_month,id.vars="month")[c(2,3)])
colnames(df_Tsurf_stat) = c("month","Exp.","mean","Exp.","sd")
df_Tsurf_stat = df_Tsurf_stat[-c(4)]
df_Tsurf_stat$month_abb = month.abb[df_Tsurf_stat$month]

df_Tsurf_stat$month_abb <-factor(df_Tsurf_stat$month_abb, 
                               levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                          "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

ggts = ggplot(df_Tsurf_stat,aes(x=month_abb,y=mean,group=Exp.,color=Exp.)) +
  geom_line(size=1) +
  geom_point() +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd),width=0.2) +
  scale_color_manual(values=c("red","green3","blue")) +
  ylim(25.4,30.2) +
  theme(text = element_text(size=15), legend.title=element_blank(), legend.position = c(0.35,0.9),legend.direction = "horizontal"  ) +
  labs(title=paste("400 YEARS MEAN TSURF niño 3.4"),x="MONTH", y = "TSURF (°C)") +
  scale_x_discrete(labels=c("DJF","JFM","FMA","MAM","AMJ","MJJ","JJA","JAS","ASO","SON","OND","NDJ"))
print(ggts)
ggsave(ggts, file=paste(figpath,"Tsurf_nino34_2000_vs_2050_rcp45_rcp85_400ans_averagepermonth","_v1.png",sep=""), width = 14, height = 9, units = "cm")


# creation du fichier Dates+Nino/Nina
Nino_34_tri_CE0 = Nino_34_tri[seq(1,8)]
Nino_34_tri_CE1 = Nino_34_tri[c(1,seq(9,15))]
Nino_34_tri_CE2 = Nino_34_tri[c(1,seq(16,22))]
write.table(Nino_34_tri_CE0, paste("/Users/gablellouch/Documents/Meteo_France/ENM/ienm3/houles_dirpf/Belenos/Nino34/Info_NinoNina_CE0.txt",sep=""), sep = ";", quote = T, col.names = T, row.names = F)
write.table(Nino_34_tri_CE1, paste("/Users/gablellouch/Documents/Meteo_France/ENM/ienm3/houles_dirpf/Belenos/Nino34/Info_NinoNina_CE1.txt",sep=""), sep = ";", quote = T, col.names = T, row.names = F)
write.table(Nino_34_tri_CE2, paste("/Users/gablellouch/Documents/Meteo_France/ENM/ienm3/houles_dirpf/Belenos/Nino34/Info_NinoNina_CE2.txt",sep=""), sep = ";", quote = T, col.names = T, row.names = F)
