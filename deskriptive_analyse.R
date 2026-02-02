#Vorbereitung
##Aktivierung von Paketen
library(rio)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(sf)
library(cartography)
library(sp)
library(rgdal) 
library(maptools)
library(raster)
library(haven)
library(gridExtra)
library(data.table)
library(ggpattern)
library(DescTools)
library(carData)
library(stargazer)
library(xtable)
library(plm)
library(fixest)

##Working Directory Setzen
setwd("C:/Users/User/Desktop/deskritoptive_analyse")
options(scipen = 999)

##Import Datensätze
municipal_network_charges <- read_dta("data/final_data_muni.dta")
municipal_network_charges <- municipal_network_charges %>%
  rename(AGS=muni_id) %>%
  filter (year<=2022)%>%
  filter (year>=2015)
municipal_network_charges$AGS <- ifelse(nchar(municipal_network_charges$AGS) < 8, paste0("0", municipal_network_charges$AGS), municipal_network_charges$AGS)
municipal_network_charges$AGS <- as.character(municipal_network_charges$AGS)

paneldatensatz_netzebene <- read_dta("data/final_data_net.dta")
paneldatensatz_netzebene$regelgebiet_num <- as.numeric (paneldatensatz_netzebene$regelgebiet_num)

inflation <- read_dta("data/inflation.dta")

muni_data <- read_dta("data/muni_data_2020.dta")
muni_data <- muni_data %>% 
  rename(AGS = code)%>%
  mutate(population=population/1000)

geodaten <-read_dta("data/Geodaten.dta") 
geodaten <- geodaten %>% 
  rename(AGS = muni_id)%>%
  mutate(traffic=as.numeric(traffic))%>%
  mutate(agriculture=as.numeric(agriculture))%>%
  dplyr::select(AGS,year,traffic,agriculture)

#Latitude
breitengrad <- import ("data/breitengrade.csv")
breitengrad$AGS <- sprintf("%08d", breitengrad$AGS)
add <- data.frame(AGS = "14522630", LAT_DEZ = 50.8619)
breitengrad <- breitengrad %>%
  dplyr::select(AGS, LAT_DEZ)%>%  
  mutate(AGS=as.character(AGS))%>%
  mutate (LAT_DEZ=LAT_DEZ-min(LAT_DEZ, na.rm = TRUE))%>%
  mutate(LAT_DEZ=LAT_DEZ*(100/max(LAT_DEZ, na.rm = TRUE))*0.01)
####################################################################

# (1) Rechnungen
## (a) Korrelation zwischen verschiedenen Berechnungsvarianten Netzentgelt
corhh1 <- cor(municipal_network_charges$hh_nw_charge_wmean,municipal_network_charges$hh_nw_charge_mean)
corhh2 <- cor(municipal_network_charges$hh_nw_charge_wmean,municipal_network_charges$hh_nw_charge_main)
corhh3 <- cor(municipal_network_charges$hh_nw_charge_mean,municipal_network_charges$hh_nw_charge_main)

(corhh1+corhh2+corhh3)/3

corbusi1 <- cor(municipal_network_charges$busi_nw_charge_wmean,municipal_network_charges$busi_nw_charge_mean)
corbusi2 <- cor(municipal_network_charges$busi_nw_charge_wmean,municipal_network_charges$busi_nw_charge_main)
corbusi3 <- cor(municipal_network_charges$busi_nw_charge_mean,municipal_network_charges$busi_nw_charge_main)

(corbusi1+corbusi2+corbusi3)/3

corind1 <- cor(municipal_network_charges$ind_nw_charge_wmean,municipal_network_charges$ind_nw_charge_mean)
corind2 <- cor(municipal_network_charges$ind_nw_charge_wmean,municipal_network_charges$ind_nw_charge_main)
corind3 <- cor(municipal_network_charges$ind_nw_charge_mean,municipal_network_charges$ind_nw_charge_main)

cor_mean <-(corind1+corind2+corind3)/3
rm (corbusi1, corbusi2, corbusi3, corhh1, corhh2, corhh3, corind1, corind2, corind3)

# (b) Rechnung Quartilsabstand
municipal_network_charges2013 <- filter (municipal_network_charges, year>=2013)
quartilsabstand <- municipal_network_charges2013 %>%
  group_by (year) %>%
  summarize (qhh   = quantile (hh_nw_charge_wmean, 0.75)-quantile(hh_nw_charge_wmean, 0.25),              qbusi = quantile (busi_nw_charge_wmean, 0.75)-quantile(busi_nw_charge_wmean, 0.25),              qind  = quantile (ind_nw_charge_wmean, 0.75)-quantile(ind_nw_charge_wmean, 0.25),              qhhr   = quantile (hh_nw_charge_wmean_real, 0.75)-quantile(hh_nw_charge_wmean_real, 0.25),              qbusir = quantile (busi_nw_charge_wmean_real, 0.75)-quantile(busi_nw_charge_wmean_real, 0.25),              qindr  = quantile (ind_nw_charge_wmean_real, 0.75)-quantile(ind_nw_charge_wmean_real, 0.25))

# (c) Konzentrationsmaße
konzentration <- municipal_network_charges2013 %>%
  group_by (year) %>%
  summarize (HFI  = sum((hh_nw_charge_wmean/sum(hh_nw_charge_wmean))^2)*10000, GINI= Gini(hh_nw_charge_wmean))

# (d) Erstellung einzelner Datensätze je Regelgebiet

charges_tennet <- municipal_network_charges %>%
  filter (regelgebiet_num==1) %>%
  filter (year>=2013)

setDT(charges_tennet)
filtered_data_tennet <- charges_tennet[year %in% c(2018, 2022)]
period_data_differenz_tennet <- filtered_data_tennet[, .(Differenz = hh_nw_charge_wmean[year == 2022] - hh_nw_charge_wmean_real[year == 2018]), by = "AGS"]
period_data_prozent_tennet <- filtered_data_tennet[, .(Prozent = hh_nw_charge_wmean[year == 2022] / hh_nw_charge_wmean_real[year == 2018]), by = AGS]
period_data_tennet <- left_join(period_data_differenz_tennet, period_data_prozent_tennet, by="AGS")
period_data_tennet$Prozent <- (period_data_tennet$Prozent -1)*100
rm (period_data_prozent_tennet, period_data_differenz_tennet)

sum(period_data_tennet$Prozent<0)
sum(period_data_tennet$Prozent<0)/length(period_data_tennet$Prozent)


charges_amprion <- municipal_network_charges%>%
  filter (regelgebiet_num==2) %>%
  filter (year>=2013)

setDT(charges_amprion)
filtered_data_amprion <- charges_amprion[year %in% c(2018, 2022)]
period_data_differenz_amprion <- filtered_data_amprion[, .(Differenz = hh_nw_charge_wmean[year == 2022] - hh_nw_charge_wmean_real[year == 2018]), by = "AGS"]
period_data_prozent_amprion <- filtered_data_amprion[, .(Prozent = hh_nw_charge_wmean[year == 2022] / hh_nw_charge_wmean_real[year == 2018]), by = AGS]
period_data_amprion <- left_join(period_data_differenz_amprion, period_data_prozent_amprion, by="AGS")
period_data_amprion$Prozent <- (period_data_amprion$Prozent -1)*100
rm (period_data_prozent_amprion, period_data_differenz_amprion)

sum(period_data_amprion$Prozent<0)
sum(period_data_amprion$Prozent<0)/length(period_data_amprion$Prozent)


charges_hertz <- municipal_network_charges %>%
  filter (regelgebiet_num==3) %>%
  filter (year>=2013)

setDT(charges_hertz)
filtered_data_hertz <- charges_hertz[year %in% c(2018, 2022)]
period_data_differenz_hertz <- filtered_data_hertz[, .(Differenz = hh_nw_charge_wmean[year == 2022] - hh_nw_charge_wmean_real[year == 2018]), by = "AGS"]
period_data_prozent_hertz <- filtered_data_hertz[, .(Prozent = hh_nw_charge_wmean[year == 2022] / hh_nw_charge_wmean_real[year == 2018]), by = AGS]
period_data_hertz <- left_join(period_data_differenz_hertz, period_data_prozent_hertz, by="AGS")
period_data_hertz$Prozent <- (period_data_hertz$Prozent -1)*100
rm (period_data_prozent_hertz, period_data_differenz_hertz)

sum(period_data_hertz$Prozent<0)
sum(period_data_hertz$Prozent<0)/length(period_data_hertz$Prozent)


charges_trans <- municipal_network_charges %>%
  filter (regelgebiet_num==4) %>%
  filter (year>=2013)

setDT(charges_trans)
filtered_data_trans <- charges_trans[year %in% c(2018, 2022)]
period_data_differenz_trans <- filtered_data_trans[, .(Differenz = hh_nw_charge_wmean[year == 2022] - hh_nw_charge_wmean_real[year == 2018]), by = "AGS"]
period_data_prozent_trans <- filtered_data_trans[, .(Prozent = hh_nw_charge_wmean[year == 2022] / hh_nw_charge_wmean_real[year == 2018]), by = AGS]
period_data_trans <- left_join(period_data_differenz_trans, period_data_prozent_trans, by="AGS")
period_data_trans$Prozent <- (period_data_trans$Prozent -1)*100
rm (period_data_prozent_trans, period_data_differenz_trans)

sum(period_data_trans$Prozent<0)
sum(period_data_trans$Prozent<0)/length(period_data_trans$Prozent)
####################################################################

# (2) Maps
## Import Shapefiles
germangemeinden <- st_read ("shapefiles/VG250_GEM.shp")
germancountry <- st_read("shapefiles/VG250_LAN.shp")
###delete sea territories
germancountry <- filter(germancountry, GF == 3 | GF == 4)

## (a11) Netzentgelte in Deutschland 2018 HH
municipal_network_charges2018 <- filter(municipal_network_charges, year == 2018)
mapdata2018 <- left_join (germangemeinden, municipal_network_charges2018, by="AGS")

germany2018hh<-ggplot(mapdata2018)+
  geom_sf(data=mapdata2018,
          mapping = aes(fill=hh_nw_charge_wmean),
          stat = "sf", color="NA")+
  scale_fill_gradient(low="gray90", high="gray20", na.value="grey50", limits = c(4, 20))+
  geom_sf(data=germancountry,
          mapping = aes(),
          stat = "sf", color="white", fill="NA")+  
  labs(x = NULL, y = NULL, fill = "ct/kWh") +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_blank())

ggsave(filename = "export/2A11-Charges_2018_HH.pdf", plot = germany2018hh, width = 21, height = 29.7, units = "cm")

## (a12) Netzentgelte in Deutschland 2018 HH Real
germany2018hhreal<-ggplot(mapdata2018)+
  geom_sf(data=mapdata2018,
          mapping = aes(fill=hh_nw_charge_wmean_real),
          stat = "sf", color="NA")+
  scale_fill_gradient(low="gray90", high="gray20", na.value="grey50", limits = c(4, 20))+
  geom_sf(data=germancountry,
          mapping = aes(),
          stat = "sf", color="white", fill="NA")+  
  labs(x = NULL, y = NULL, fill = "ct/kWh") +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_blank())

ggsave(filename = "export/2A12-Charges_2018_HH_real.pdf", plot = germany2018hhreal, width = 21, height = 29.7, units = "cm")
ggsave(filename = "export-png/2A12-Charges_2018_HH_real.png", plot = germany2018hhreal, width = 4.94, height = 7, units = "in")


## (a21) Netzentgelte in Deutschland 2018 Busi
germany2018busi<-ggplot(mapdata2018)+
  geom_sf(data=mapdata2018,
          mapping = aes(fill=busi_nw_charge_wmean),
          stat = "sf", color="NA")+
  scale_fill_gradient(low="gray90", high="gray20", na.value="grey50", limits = c(4, 20))+
  geom_sf(data=germancountry,
          mapping = aes(),
          stat = "sf", color="white", fill="NA")+  
  labs(x = NULL, y = NULL, fill = "ct/kWh") +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_blank())

ggsave(filename = "export/2A21-Charges_2018_Busi.pdf", plot = germany2018busi, width = 21, height = 29.7, units = "cm")

## (a22) Netzentgelte in Deutschland 2018 Busi Real
germany2018busireal<-ggplot(mapdata2018)+
  geom_sf(data=mapdata2018,
          mapping = aes(fill=busi_nw_charge_wmean_real),
          stat = "sf", color="NA")+
  scale_fill_gradient(low="gray90", high="gray20", na.value="grey50", limits = c(4, 20))+
  geom_sf(data=germancountry,
          mapping = aes(),
          stat = "sf", color="white", fill="NA")+  
  labs(x = NULL, y = NULL, fill = "ct/kWh") +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_blank()) 

ggsave(filename = "export/2A22-Charges_2018_Busi_real.pdf", plot = germany2018busireal, width = 21, height = 29.7, units = "cm")

## (a31) Netzentgelte in Deutschland 2018 Industry
germany2018ind<-ggplot(mapdata2018)+
  geom_sf(data=mapdata2018,
          mapping = aes(fill=hh_nw_charge_wmean),
          stat = "sf", color="NA")+
  scale_fill_gradient(low="gray90", high="gray20", na.value="grey50", limits = c(4, 20))+
  geom_sf(data=germancountry,
          mapping = aes(),
          stat = "sf", color="white", fill="NA")+  
  labs(x = NULL, y = NULL, fill = "ct/kWh") +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_blank()) 

ggsave(filename = "export/2A31-Charges_2018_Ind.pdf", plot = germany2018ind, width = 21, height = 29.7, units = "cm")

## (a32) Netzentgelte in Deutschland 2018 Industry Real
germany2018indreal<-ggplot(mapdata2018)+
  geom_sf(data=mapdata2018,
          mapping = aes(fill=hh_nw_charge_wmean_real),
          stat = "sf", color="NA")+
  scale_fill_gradient(low="gray90", high="gray20", na.value="grey50", limits = c(4, 20))+
  geom_sf(data=germancountry,
          mapping = aes(),
          stat = "sf", color="white", fill="NA")+  
  labs(x = NULL, y = NULL, fill = "ct/kWh") +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_blank()) 

ggsave(filename = "export/2A32-Charges_2018_Ind_real.pdf", plot = germany2018indreal, width = 21, height = 29.7, units = "cm")
ggsave(filename = "export-png/2A32-Charges_2018_Ind_real.png", plot = germany2018indreal, width = 4.94, height = 7, units = "in")

## (b1) Netzentgelte in Deutschland 2022 HH
municipal_network_charges2022 <- filter(municipal_network_charges, year == 2022)
mapdata2022 <- left_join (germangemeinden, municipal_network_charges2022, by="AGS")

germany2022hh <-ggplot(mapdata2022)+
  geom_sf(data=mapdata2022,
          mapping = aes(fill=hh_nw_charge_wmean),
          stat = "sf", color="NA")+
  scale_fill_gradient(low="gray90", high="gray20", na.value="grey50", limits = c(4, 20))+
  geom_sf(data=germancountry,
          mapping = aes(),
          stat = "sf", color="white", fill="NA")+  
  labs(x = NULL, y = NULL, fill = "ct/kWh") +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_blank()) 

ggsave(filename = "export/2B1-Charges_2022_HH.pdf", plot = germany2022hh, width = 21, height = 29.7, units = "cm")
ggsave(filename = "export-png/2B1-Charges_2022_HH.png", plot = germany2022hh, width = 4.94, height = 7, units = "in")

## (b2) Netzentgelte in Deutschland 2022 Business
germany2022busi <-ggplot(mapdata2022)+
  geom_sf(data=mapdata2022,
          mapping = aes(fill=hh_nw_charge_wmean),
          stat = "sf", color="NA")+
  scale_fill_gradient(low="gray90", high="gray20", na.value="grey50", limits = c(4, 20))+
  geom_sf(data=germancountry,
          mapping = aes(),
          stat = "sf", color="white", fill="NA")+  
  labs(x = NULL, y = NULL, fill = "ct/kWh") +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_blank()) 

ggsave(filename = "export/2B2-Charges_2022_Busi.pdf", plot = germany2022busi, width = 21, height = 29.7, units = "cm")

## (b3) Netzentgelte in Deutschland 2022 Industry
germany2022ind <-ggplot(mapdata2022)+
  geom_sf(data=mapdata2022,
          mapping = aes(fill=hh_nw_charge_wmean),
          stat = "sf", color="NA")+
  scale_fill_gradient(low="gray90", high="gray20", na.value="grey50", limits = c(4, 20))+
  geom_sf(data=germancountry,
          mapping = aes(),
          stat = "sf", color="white", fill="NA")+  
  labs(x = NULL, y = NULL, fill = "ct/kWh") +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_blank()) 

ggsave(filename = "export/2B3-Charges_2022_Ind.pdf", plot = germany2022ind, width = 21, height = 29.7, units = "cm")
ggsave(filename = "export-png/2B3-Charges_2022_Ind.png", plot = germany2022ind, width = 4.94, height = 7, units = "in")

## (c11)Darstellung der Karten 2018 und 2022 nebeneinander HH
germany2018_2022_hh <- grid.arrange(germany2018hh, germany2022hh, ncol = 2)

ggsave(filename = "export/2C11-Charges_2022_2018_HH.pdf", plot=germany2018_2022_hh, width = 29.7, height = 21, units = "cm")

## (c12)Darstellung der Karten 2018 und 2022 nebeneinander HH real
germany2018_2022_hh_real <- grid.arrange(germany2018hhreal, germany2022hh, ncol = 2)

ggsave(filename = "export/2C12-Charges_2022_2018_HH_real.pdf", plot=germany2018_2022_hh_real, width = 29.7, height = 21, units = "cm")

## (c21)Darstellung der Karten 2018 und 2022 nebeneinander Busi
germany2018_2022_busi <- grid.arrange(germany2018busi, germany2022busi, ncol = 2)

ggsave(filename = "export/2C21-Charges_2022_2018_Busi.pdf", plot=germany2018_2022_busi, width = 29.7, height = 21, units = "cm")

## (c22)Darstellung der Karten 2018 und 2022 nebeneinander Busi Real
germany2018_2022_busi_real <- grid.arrange(germany2018busireal, germany2022busi, ncol = 2)

ggsave(filename = "export/2C11-Charges_2022_2018_Busi_real.pdf", plot=germany2018_2022_busi_real, width = 29.7, height = 21, units = "cm")

## (c31)Darstellung der Karten 2018 und 2022 nebeneinander Ind
germany2018_2022_ind <- grid.arrange(germany2018ind, germany2022ind, ncol = 2)

ggsave(filename = "export/2C31-Charges_2022_2018_Busi.pdf", plot=germany2018_2022_ind, width = 29.7, height = 21, units = "cm")

## (c32)Darstellung der Karten 2018 und 2022 nebeneinander Ind Real
germany2018_2022_ind_real <- grid.arrange(germany2018indreal, germany2022ind, ncol = 2)

ggsave(filename = "export/2C11-Charges_2022_2018_Busi_real.pdf", plot=germany2018_2022_ind_real, width = 29.7, height = 21, units = "cm")

## (d11) Preisänderung 2018 bis 2022 HH
setDT(municipal_network_charges)
filtered_data <- municipal_network_charges[year %in% c(2018, 2022)]
period_data_differenz <- filtered_data[, .(Differenz = hh_nw_charge_wmean[year == 2022] - hh_nw_charge_wmean[year == 2018]), by = AGS]
period_data_prozent <- filtered_data[, .(Prozent = hh_nw_charge_wmean[year == 2022] / hh_nw_charge_wmean[year == 2018]), by = AGS]
period_data <- left_join(period_data_differenz, period_data_prozent, by="AGS")
period_data$Prozent <- (period_data$Prozent -1)*100
period_data$AGS <- as.character(period_data$AGS)
mapdata20182022 <- left_join (germangemeinden, period_data, by="AGS")

changehh<-ggplot(mapdata20182022)+
  geom_sf(data=mapdata20182022,
          mapping = aes(fill=Prozent),
          stat = "sf", color="NA")+
  scale_fill_gradient(low="gray90", high="gray20", na.value="grey50", limits = c(-100, 120))+
  geom_sf(data=germancountry,
          mapping = aes(),
          stat = "sf", color="white", fill="NA")+  
  labs(x = NULL, y = NULL, fill = "change in %") +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_blank()) 

rm (filtered_data, period_data_differenz, period_data_prozent)

ggsave(filename = "export/2D11-Change_HH.pdf", plot=changehh, width = 21, height = 29.7, units = "cm")

## (d12) Preisänderung 2018 bis 2022 HH Real
setDT(municipal_network_charges)
filtered_data <- municipal_network_charges[year %in% c(2018, 2022)]
period_data_differenz <- filtered_data[, .(Differenz = hh_nw_charge_wmean_real[year == 2022] - hh_nw_charge_wmean_real[year == 2018]), by = AGS]
period_data_prozent <- filtered_data[, .(Prozent = hh_nw_charge_wmean_real[year == 2022] / hh_nw_charge_wmean_real[year == 2018]), by = AGS]
period_data <- left_join(period_data_differenz, period_data_prozent, by="AGS")
period_data$Prozent <- (period_data$Prozent -1)*100
period_data$AGS <- as.character(period_data$AGS)
mapdata20182022 <- left_join (germangemeinden, period_data, by="AGS")

changehh_real <- ggplot(mapdata20182022)+
  geom_sf(data=mapdata20182022,
          mapping = aes(fill=Prozent),
          stat = "sf", color="NA")+
  scale_fill_gradient(low="gray90", high="gray20", na.value="grey50", limits = c(-100, 120))+
  geom_sf(data=germancountry,
          mapping = aes(),
          stat = "sf", color="white", fill="NA")+  
  labs(x = NULL, y = NULL, fill = "change in %") +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_blank()) 

rm (filtered_data, period_data_differenz, period_data_prozent)

ggsave(filename = "export/2D12-Change_HH_real.pdf", plot=changehh_real, width = 21, height = 29.7, units = "cm")
ggsave(filename = "export-png/2D12-Change_HH_real.png", plot = changehh_real, width = 4.94, height = 7, units = "in")

## (d21) Preisänderung 2018 bis 2022 Busi
setDT(municipal_network_charges)
filtered_data <- municipal_network_charges[year %in% c(2018, 2022)]
period_data_differenz <- filtered_data[, .(Differenz = busi_nw_charge_wmean[year == 2022] - busi_nw_charge_wmean[year == 2018]), by = AGS]
period_data_prozent <- filtered_data[, .(Prozent = busi_nw_charge_wmean[year == 2022] / busi_nw_charge_wmean[year == 2018]), by = AGS]
period_data <- left_join(period_data_differenz, period_data_prozent, by="AGS")
period_data$Prozent <- (period_data$Prozent -1)*100
period_data$AGS <- as.character(period_data$AGS)
mapdata20182022 <- left_join (germangemeinden, period_data, by="AGS")

changebusi <- ggplot(mapdata20182022)+
  geom_sf(data=mapdata20182022,
          mapping = aes(fill=Prozent),
          stat = "sf", color="NA")+
  scale_fill_gradient(low="gray90", high="gray20", na.value="grey50", limits = c(-100, 120))+
  geom_sf(data=germancountry,
          mapping = aes(),
          stat = "sf", color="white", fill="NA")+  
  labs(x = NULL, y = NULL, fill = "change in %") +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_blank()) 

rm (filtered_data, period_data_differenz, period_data_prozent)

ggsave(filename = "export/2D21-Change_Busi.pdf", plot=changebusi, width = 21, height = 29.7, units = "cm")

## (d22) Preisänderung 2018 bis 2022 Busi Real
setDT(municipal_network_charges)
filtered_data <- municipal_network_charges[year %in% c(2018, 2022)]
period_data_differenz <- filtered_data[, .(Differenz = busi_nw_charge_wmean_real[year == 2022] - busi_nw_charge_wmean_real[year == 2018]), by = AGS]
period_data_prozent <- filtered_data[, .(Prozent = busi_nw_charge_wmean_real[year == 2022] / busi_nw_charge_wmean_real[year == 2018]), by = AGS]
period_data <- left_join(period_data_differenz, period_data_prozent, by="AGS")
period_data$Prozent <- (period_data$Prozent -1)*100
period_data$AGS <- as.character(period_data$AGS)
mapdata20182022 <- left_join (germangemeinden, period_data, by="AGS")

changebusireal <- ggplot(mapdata20182022)+
  geom_sf(data=mapdata20182022,
          mapping = aes(fill=Prozent),
          stat = "sf", color="NA")+
  scale_fill_gradient(low="gray90", high="gray20", na.value="grey50", limits = c(-100, 120))+
  geom_sf(data=germancountry,
          mapping = aes(),
          stat = "sf", color="white", fill="NA")+  
  labs(x = NULL, y = NULL, fill = "change in %") +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_blank()) 

rm (filtered_data, period_data_differenz, period_data_prozent)

ggsave(filename = "export/2D22-Change_Busi_real.pdf", plot=changebusireal, width = 21, height = 29.7, units = "cm")
ggsave(filename = "export-png/2D22-Change_Busi_real.png", plot = changebusireal, width = 4.94, height = 7, units = "in")

## (d31) Preisänderung 2018 bis 2022 Ind
setDT(municipal_network_charges)
filtered_data <- municipal_network_charges[year %in% c(2018, 2022)]
period_data_differenz <- filtered_data[, .(Differenz = ind_nw_charge_wmean[year == 2022] - ind_nw_charge_wmean[year == 2018]), by = AGS]
period_data_prozent <- filtered_data[, .(Prozent = ind_nw_charge_wmean[year == 2022] / ind_nw_charge_wmean[year == 2018]), by = AGS]
period_data <- left_join(period_data_differenz, period_data_prozent, by="AGS")
period_data$Prozent <- (period_data$Prozent -1)*100
period_data$AGS <- as.character(period_data$AGS)
mapdata20182022 <- left_join (germangemeinden, period_data, by="AGS")

changeind <- ggplot(mapdata20182022)+
  geom_sf(data=mapdata20182022,
          mapping = aes(fill=Prozent),
          stat = "sf", color="NA")+
  scale_fill_gradient(low="gray90", high="gray20", na.value="grey50", limits = c(-100, 120))+
  geom_sf(data=germancountry,
          mapping = aes(),
          stat = "sf", color="white", fill="NA")+  
  labs(x = NULL, y = NULL, fill = "change in %") +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_blank())

rm (filtered_data, period_data_differenz, period_data_prozent)

ggsave(filename = "export/2D31-Change_Ind.pdf", plot=changeind, width = 21, height = 29.7, units = "cm")
ggsave(filename = "export-png/2D31-Change_Ind.png", plot = changeind, width = 4.94, height = 7, units = "in")

## (d32) Preisänderung 2018 bis 2022 Ind Real
setDT(municipal_network_charges)
filtered_data <- municipal_network_charges[year %in% c(2018, 2022)]
period_data_differenz <- filtered_data[, .(Differenz = ind_nw_charge_wmean_real[year == 2022] - ind_nw_charge_wmean_real[year == 2018]), by = AGS]
period_data_prozent <- filtered_data[, .(Prozent = ind_nw_charge_wmean_real[year == 2022] / ind_nw_charge_wmean_real[year == 2018]), by = AGS]
period_data <- left_join(period_data_differenz, period_data_prozent, by="AGS")
period_data$Prozent <- (period_data$Prozent -1)*100
period_data$AGS <- as.character(period_data$AGS)
mapdata20182022 <- left_join (germangemeinden, period_data, by="AGS")

changeindreal <- ggplot(mapdata20182022)+
  geom_sf(data=mapdata20182022,
          mapping = aes(fill=Prozent),
          stat = "sf", color="NA")+
  scale_fill_gradient(low="gray90", high="gray20", na.value="grey50", limits = c(-100, 120))+
  geom_sf(data=germancountry,
          mapping = aes(),
          stat = "sf", color="white", fill="NA")+  
  labs(x = NULL, y = NULL, fill = "change in %") +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_blank()) 

rm (filtered_data, period_data_differenz, period_data_prozent)

ggsave(filename = "export/2D32-Change_Ind_real.pdf", plot=changeindreal, width = 21, height = 29.7, units = "cm")
ggsave(filename = "export-png/2D32-Change_Ind_real.png", plot = changeindreal, width = 4.94, height = 7, units = "in")

## (e) Übertragunsgnetzbetreiber 2022
netze <- import ("data/nns_csv_windows_1252_Netze.csv", encoding = "Latin-1")
regelgebiete <- import ("data/nns_csv_windows_1252_Regelgebiete.csv", encoding = "Latin-1")
regelgebiete <- rename(regelgebiete, Regelgebiet_num=Regelgebiet_Nr)
netze <-left_join(netze, regelgebiete, by = "Regelgebiet_num")

plz_netze <- import ("data/nns_csv_windows_1252_PLZ_Netzbetreiber.csv", encoding = "Latin-1")
plz_netze$gueltig_bis <- as.Date(plz_netze$gueltig_bis, "%d.%m.%Y")
uenetze <- plz_netze[plz_netze$gueltig_bis >= as.Date("2022-06-11"), ]
uenetze <- left_join (uenetze, netze, by="Netz_Nr")
uenetze <- dplyr::select(uenetze, Gemeindekennziffer, Regelgebiet_num, Regelgebiet_Name)
uenetze <- uenetze[!duplicated(uenetze$Gemeindekennziffer), ]
uenetze <- rename(uenetze, AGS=Gemeindekennziffer)
uenetze$AGS <- as.character(uenetze$AGS)
uenetze$AGS <- ifelse(nchar(uenetze$AGS) < 8, paste0("0", uenetze$AGS), uenetze$AGS)
uenetze <- mutate(uenetze, Regelgebiet_Name = ifelse(Regelgebiet_Name == "virtuelle Regelzone Flensburg", "virtual control zone flensburg", Regelgebiet_Name))

uenetze <- mutate(uenetze, 
                  Regelgebiet_Name = ifelse(Regelgebiet_Name == "virtuelle Regelzone Flensburg", "virtual control zone flensburg", Regelgebiet_Name),
                  Regelgebiet_Name = ifelse(Regelgebiet_Name == "50Hertz Transmission GmbH", "50Hertz", Regelgebiet_Name),
                  Regelgebiet_Name = ifelse(Regelgebiet_Name == "Austrian Power Grid GmbH (APG)", "Austrian Power Grid", Regelgebiet_Name),
                  Regelgebiet_Name = ifelse(Regelgebiet_Name == "TenneT TSO GmbH", "TenneT", Regelgebiet_Name),
                  Regelgebiet_Name = ifelse(Regelgebiet_Name == "TransnetBW GmbH", "TransnetBW", Regelgebiet_Name),
                  Regelgebiet_Name = ifelse(Regelgebiet_Name == "Amprion GmbH", "Amprion", Regelgebiet_Name)
)
mapdata_uenetz <- left_join (germangemeinden, uenetze, by="AGS")

regelgebiete_farben <- c("black", "white", "gray", "lightgray", "darkgray", "gray60")

mapdata_uenetz$Regelgebiet_Name <- ifelse(mapdata_uenetz$Regelgebiet_Name %in% c("Austrian Power Grid", "virtual control zone flensburg"), NA, mapdata_uenetz$Regelgebiet_Name)


transmissionnet <- ggplot(mapdata_uenetz) +
  geom_sf(mapping = aes(fill = Regelgebiet_Name), color = "NA") +
  geom_sf(data = germancountry,
          mapping = aes(),
          stat = "sf", color = "black", fill = "NA") +
  labs(x = NULL, y = NULL, fill = "TSO") +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  scale_fill_manual(values = regelgebiete_farben) +  
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.5),
    panel.background = element_rect(fill = "white", color = "black"),
    panel.grid.major = element_blank()
  )


rm (plz_netze, regelgebiete, netze)

ggsave(filename = "export/2E_transmissionnet.pdf", plot=transmissionnet, width = 21, height = 29.7, units = "cm")
ggsave(filename = "export-png/2E_transmissionnet.png", plot = transmissionnet, width = 4.94, height = 7, units = "in")

####################################################################

# (3) Boxplots
## (a1) Boxplots Haushalte
municipal_network_charges2013 <- filter (municipal_network_charges, year>=2015)
boxplothh <- ggplot(municipal_network_charges2013, aes(x = as.factor(year), y = hh_nw_charge_wmean)) +
  stat_boxplot(geom = "errorbar",
               width = 0.6)+
  geom_boxplot(fill= "gray90", width = 0.6, coef=30) +
  labs(x = NULL , y = "network charge ct/kWh", caption = "excludes outside values") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major.y = element_line(color = "gray90"),
        panel.grid.major.x = element_blank(),
        plot.caption = element_text(hjust = 0))

ggsave(filename = "export/3A1_HH.pdf", plot=boxplothh, width = 29.7, height = 21, units = "cm")

## (a2) Boxplots Haushalte Real
boxplothhreal <- ggplot(municipal_network_charges2013, aes(x = as.factor(year), y = hh_nw_charge_wmean_real)) +
  stat_boxplot(geom = "errorbar",
               width = 0.6)+
  geom_boxplot(fill= "gray90", width = 0.6, coef=30) +
  labs(x = NULL , y = "network charge ct/kWh", caption = "excludes outside values") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major.y = element_line(color = "gray90"),
        panel.grid.major.x = element_blank(),
        plot.caption = element_text(hjust = 0, size = 14),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14)
  )

ggsave(filename = "export/3A2_HH_real.pdf", plot=boxplothhreal, width = 29.7, height = 21, units = "cm")
ggsave(filename = "export-png/3A2_HH_real.png", plot = boxplothhreal, width = 7, height = 4.94, units = "in")

## (b) Boxplots Gewerbe
boxplotbusi <- ggplot(municipal_network_charges2013, aes(x = as.factor(year), y = busi_nw_charge_wmean)) +
  stat_boxplot(geom = "errorbar",
               width = 0.6)+
  geom_boxplot(fill= "gray90", width = 0.6, coef=30) +
  labs(x = NULL, y = "network charge ct/kWh", caption = "excludes outside values") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major.y = element_line(color = "gray90"),
        panel.grid.major.x = element_blank(),
        plot.caption = element_text(hjust = 0))

ggsave(filename = "export/3B1_Busi.pdf", plot=boxplotbusi, width = 29.7, height = 21, units = "cm")

## (b2) Boxplots Gewerbe Real
boxplotbusireal <- ggplot(municipal_network_charges2013, aes(x = as.factor(year), y = busi_nw_charge_wmean_real)) +
  stat_boxplot(geom = "errorbar",
               width = 0.6)+
  geom_boxplot(fill= "gray90", width = 0.6, coef=30) +
  labs(x = NULL, y = "network charge ct/kWh", caption = "excludes outside values") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major.y = element_line(color = "gray90"),
        panel.grid.major.x = element_blank(),
        plot.caption = element_text(hjust = 0))

ggsave(filename = "export/3B2_Busi_real.pdf", plot=boxplotbusireal, width = 29.7, height = 21, units = "cm")

## (c1) Boxplots Industrie
boxplotind <- ggplot(municipal_network_charges2013, aes(x = as.factor(year), y = ind_nw_charge_wmean)) +
  stat_boxplot(geom = "errorbar",
               width = 0.6)+
  geom_boxplot(fill= "gray90", width = 0.6, coef=30) +
  labs(x = NULL, y = "network charge ct/kWh", caption = "excludes outside values") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major.y = element_line(color = "gray90"),
        panel.grid.major.x = element_blank(),
        plot.caption = element_text(hjust = 0))

ggsave(filename = "export/3C1_Ind.pdf", plot=boxplotind, width = 29.7, height = 21, units = "cm")

## (c2) Boxplots Industrie Real
boxplotindreal <- ggplot(municipal_network_charges2013, aes(x = as.factor(year), y = ind_nw_charge_wmean_real)) +
  stat_boxplot(geom = "errorbar",
               width = 0.6)+
  geom_boxplot(fill= "gray90", width = 0.6, coef=30) +
  labs(x = NULL, y = "network charge ct/kWh", caption = "excludes outside values") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major.y = element_line(color = "gray90"),
        panel.grid.major.x = element_blank(),
        plot.caption = element_text(hjust = 0, size = 14),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14)
  )

ggsave(filename = "export/3C2_Ind_Real.pdf", plot=boxplotindreal, width = 29.7, height = 21, units = "cm")
ggsave(filename = "export-png/3C2_Ind_Real.png", plot = boxplotindreal, width = 7, height = 4.94, units = "in")

####################################################################

# (4) Weitere Graphen
## (a1) Entwicklung Netzentgelt in den letzten Jahren
df_years <- municipal_network_charges %>%
  group_by(year) %>%
  filter (year>=2015) %>%
  summarize(hh_nw_charge_wmean = mean(hh_nw_charge_wmean, na.rm = TRUE), busi_nw_charge_wmean = mean(busi_nw_charge_wmean, na.rm = TRUE), ind_nw_charge_wmean = mean(ind_nw_charge_wmean, na.rm = TRUE), ms_2_ap_wmean = mean(ms_2_ap_wmean, na.rm = TRUE), ms_2_lp_wmean = mean(ms_2_lp_wmean, na.rm = TRUE), ns_o_lm_hh_ap_wmean = mean(ns_o_lm_hh_ap_wmean, na.rm = TRUE), ns_o_lm_hh_gp_wmean = mean(ns_o_lm_hh_gp_wmean, na.rm = TRUE), hh_nw_charge_wmean_real = mean(hh_nw_charge_wmean_real, na.rm = TRUE), busi_nw_charge_wmean_real = mean(busi_nw_charge_wmean_real, na.rm = TRUE), ind_nw_charge_wmean_real = mean(ind_nw_charge_wmean_real, na.rm = TRUE), ms_2_ap_wmean_real = mean(ms_2_ap_wmean_real, na.rm = TRUE), ms_2_lp_wmean_real = mean(ms_2_lp_wmean_real, na.rm = TRUE), ns_o_lm_hh_ap_wmean_real = mean(ns_o_lm_hh_ap_wmean_real, na.rm = TRUE), ns_o_lm_hh_gp_wmean_real = mean(ns_o_lm_hh_gp_wmean_real, na.rm = TRUE))

developmentcharge <- ggplot(df_years, aes(x = as.factor(year))) +
  geom_point(aes(y = hh_nw_charge_wmean, color = "Households", shape="Housholds"), size=2) +
  geom_line(aes(y = hh_nw_charge_wmean, color = "Households", group = 1)) +
  geom_point(aes(y = busi_nw_charge_wmean, color = "Business", shape="Business"),size=2) +
  geom_line(aes(y = busi_nw_charge_wmean, color = "Business", group = 1)) +
  geom_point(aes(y = ind_nw_charge_wmean, color = "Industry", shape="Industry"),size=2) +
  geom_line(aes(y = ind_nw_charge_wmean, color = "Industry", group = 1)) +
  geom_vline(xintercept = "2018", linetype = "dashed", color = "black") +
  scale_color_grey(start = 0.2, end = 0.65, guide = "none") + 
  labs(x = NULL, y = "Network Charge ct/kWh", shape="consumer") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major.y = element_line(color = "gray90"),
        panel.grid.major.x = element_blank())

ggsave(filename = "export/4A1_Development_Charge.pdf", plot=developmentcharge, width = 29.7, height = 21, units = "cm")

## (a2) Entwicklung Netzentgelt in den letzten Jahren Real
developmentchargereal <-ggplot(df_years, aes(x = as.factor(year))) +
  geom_point(aes(y = hh_nw_charge_wmean_real, color = "Households", shape="Housholds"), size=2) +
  geom_line(aes(y = hh_nw_charge_wmean_real, color = "Households", group = 1)) +
  geom_point(aes(y = busi_nw_charge_wmean_real, color = "Business", shape="Business"),size=2) +
  geom_line(aes(y = busi_nw_charge_wmean_real, color = "Business", group = 1)) +
  geom_point(aes(y = ind_nw_charge_wmean_real, color = "Industry", shape="Industry"),size=2) +
  geom_line(aes(y = ind_nw_charge_wmean_real, color = "Industry", group = 1)) +
  geom_vline(xintercept = "2018", linetype = "dashed", color = "black") +
  scale_color_grey(start = 0.2, end = 0.65, guide = "none") + 
  labs(x = NULL, y = "Network Charge ct/kWh", shape="consumer") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major.y = element_line(color = "gray90"),
        panel.grid.major.x = element_blank())

ggsave(filename = "export/4A2_Development_Charge_Real.pdf", plot=developmentchargereal, width = 29.7, height = 21, units = "cm")

## (b) Varianzkoeffizinet Entwicklung
varianzen_years_HH <- municipal_network_charges %>%
  group_by(year) %>%
  filter (year>=2015) %>%
  summarize(Variationskoeffizient = (var(hh_nw_charge_wmean, na.rm = TRUE) / mean(hh_nw_charge_wmean)))%>%
  mutate(cat = "Households")
varianzen_years_Busi <- municipal_network_charges %>%
  group_by(year) %>%
  filter (year>=2015) %>%
  summarize(Variationskoeffizient = (var(busi_nw_charge_wmean, na.rm = TRUE) / mean(busi_nw_charge_wmean)))%>%
  mutate(cat = "Business")
varianzen_years_Ind <- municipal_network_charges %>%
  group_by(year) %>%
  filter (year>=2015) %>%
  summarize(Variationskoeffizient = (var(ind_nw_charge_wmean, na.rm = TRUE) / mean(ind_nw_charge_wmean, na.rm = TRUE)))%>%
  mutate(cat = "Industry")

varianzen_years <- bind_rows(varianzen_years_HH,varianzen_years_Ind)
rm(varianzen_years_Busi, varianzen_years_HH, varianzen_years_Ind)

scatter <- ggplot(varianzen_years, aes(x = as.factor(year), y = Variationskoeffizient, color = cat, shape = cat)) +
  geom_point(aes(group = cat), size=2) +
  geom_line(aes(group = cat)) +  # Add line plot
  geom_vline(xintercept = as.numeric("2019"), linetype = "dashed", color = "black") +
  labs(x = NULL, y = "coefficient of variation") +
  scale_color_manual(values = c("Households" = "gray", "Industry" = "black"), name = "Category") +
  scale_shape_manual(values = c("Households" = 16, "Industry" = 17)) +
  guides(color = guide_legend(title = "consumer", override.aes = list(shape = c(16, 17))), shape = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major.y = element_line(color = "gray90"),
        panel.grid.major.x = element_blank(),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))

ggsave(filename = "export/4B_Scatter.pdf", plot=scatter, width = 29.7, height = 21, units = "cm")
ggsave(filename = "export-png/4B_Scatter.png", plot = scatter, width = 7, height = 4.94, units = "in")

#c1) Preiskategorien Entwicklung Haushalte und Business Real
hh_basic <- df_years %>%
  mutate(cat = "Basic Charge")%>%
  mutate(charge=ns_o_lm_hh_gp_wmean_real)%>%
  dplyr::select(year,charge,cat)
hh_work <- df_years %>%
  mutate(cat = "Working Charge")%>%
  mutate(charge=ns_o_lm_hh_ap_wmean_real*10)%>%
  dplyr::select(year,charge,cat)
ind_work <- df_years %>%
  mutate(cat = "Working Charge")%>%
  mutate(charge=ms_2_ap_wmean_real*100)%>%
  dplyr::select(year,charge,cat)
ind_performance <- df_years %>%
  dplyr::select(year,ms_2_lp_wmean_real)%>%
  mutate(cat = "Performance Charge")%>%
  mutate(charge=ms_2_lp_wmean_real)%>%
  dplyr::select(year,charge,cat)
single_charges_hh <- bind_rows(hh_basic,hh_work)
single_charges_ind <- bind_rows(ind_work, ind_performance)

categorieshhreal <- ggplot(single_charges_hh, aes(x = as.factor(year), y = charge, color = cat, shape = cat)) +
  geom_point(size = 2) +
  geom_line(aes(group = cat))+
  geom_vline(xintercept = "2019", linetype = "dashed", color = "black") +
  labs(x = NULL, y = "Working Charge ct/kWh", shape = "Charge Category") +
  scale_color_manual(values = c("Basic Charge" = "black", "Working Charge" = "grey"), name = "Category") +
  scale_shape_manual(values = c("Basic Charge" = 16, "Working Charge" = 17)) +
  guides(color = guide_legend(title = "Charge Category", override.aes = list(shape = c(16, 17))), shape = "none") +
  scale_y_continuous(
    name = "Basic Charge EUR",
    sec.axis = sec_axis( trans=~./10, name="Working Charge ct/kWh")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(color = "gray20"),
        axis.title.y.right = element_text(color = "gray65", angle=90),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major.y = element_line(color = "gray90"),
        panel.grid.major.x = element_blank(),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))

ggsave(filename = "export/4C1_Categories_HH_Real.pdf", plot=categorieshhreal, width = 29.7, height = 21, units = "cm")
ggsave(filename = "export-png/4C1_Categories_HH_Real.png", plot = categorieshhreal, width = 7, height = 4.94, units = "in")


#d) Preiskategorien Entwicklung Industry
categoriesInd <- ggplot(df_years, aes(x = as.factor(year))) +
  geom_point(aes(y = ms_2_lp_wmean, color = "Performance Charge", shape="Performance Charge"), size=2) +
  geom_line(aes(y = ms_2_lp_wmean, color = "Performance Charge", group = 1)) +
  geom_point(aes(y = ms_2_ap_wmean*100, color = "Working Charge", shape="Working Charge"), size=2) +
  geom_line(aes(y = ms_2_ap_wmean*100, color = "Working Charge", group = 1)) +
  geom_vline(xintercept = "2018", linetype = "dashed", color = "black") +
  labs(x = NULL , y = "Working Charge ct/kWh", shape = "Charge Category") +
  scale_color_grey(start = 0.2, end = 0.65, guide = "none") + 
   scale_y_continuous(
    name = "Perfomance Charge EUR/kWh",
    sec.axis = sec_axis( trans=~./100, name="Working Charge ct/kWh")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(color = "gray20"),
        axis.title.y.right = element_text(color = "gray65"),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major.y = element_line(color = "gray90"),
        panel.grid.major.x = element_blank())

ggsave(filename = "export/4D1_Categories_Ind.pdf", plot=categoriesInd, width = 29.7, height = 21, units = "cm")

#d2) Preiskategorien Entwicklung Industry Real
categoriesIndReal <- ggplot(single_charges_ind, aes(x = as.factor(year), y = charge, color = cat, shape = cat)) +
  geom_point(size = 2) +
  geom_line(aes(group = cat))+
  geom_vline(xintercept = "2019", linetype = "dashed", color = "black") +
  labs(x = NULL, y = "network charge ct/kWh") +
  scale_color_manual(values = c("Performance Charge" = "gray", "Working Charge" = "black"), name = "Category") +
  scale_shape_manual(values = c("Performance Charge" = 16, "Working Charge" = 17)) +
  guides(color = guide_legend(title = "Charge Category", override.aes = list(shape = c(16, 17))), shape = "none") +
  labs(x = NULL , shape = "Charge Category") +
  scale_color_grey(start = 0.2, end = 0.65, guide = "none") + 
  scale_y_continuous(
    name = "Perfomance Charge EUR/kWh",
    sec.axis = sec_axis( trans=~./100, name="Working Charge ct/kWh")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(color = "gray20"),
        axis.title.y.right = element_text(color = "gray65"),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major.y = element_line(color = "gray90"),
        panel.grid.major.x = element_blank(),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))

ggsave(filename = "export/4D2_Categories_Ind_Real.pdf", plot=categoriesIndReal, width = 29.7, height = 21, units = "cm")
ggsave(filename = "export-png/4D2_Categories_Ind_Real.png", plot = categoriesIndReal, width = 7, height = 4.94, units = "in")

#e1) Übertragungsnetzbetreiber: Entwicklung Netzentgelte
transmission_charges <- read_dta("data/transmission_charges.dta")
transmission_charges <- as_tibble(transmission_charges)
transmission_net <- left_join(transmission_charges, inflation, by = "year")
transmission_net$trans_charge_real = transmission_net$trans_charge/transmission_net$vpi
transmission_net$höap2_real = transmission_net$höap2/transmission_net$vpi
transmission_net$hölp2_real = transmission_net$hölp2/transmission_net$vpi
transmission_net <- transmission_net %>%
  filter(year>=2015)%>%
  filter(year<2023)%>%
  mutate(
    regelgebiet_name = case_when(
      regelgebiet_name == "TenneT TSO GmbH" ~ "TenneT",
      regelgebiet_name == "Amprion GmbH" ~ "Amprion",
      regelgebiet_name == "50Hertz Transmission GmbH" ~ "50Hertz",
      regelgebiet_name == "TransnetBW GmbH" ~ "TransnetBW",
      TRUE ~ regelgebiet_name))

transcharge <- ggplot(transmission_net, aes(x = as.factor(year), y = trans_charge, color = regelgebiet_name, shape = regelgebiet_name)) +
  geom_point(aes(group = regelgebiet_name), size=2) +
  geom_line(aes(group = regelgebiet_name))+
  labs(x = NULL, y = "network charge ct/kWh") +
  scale_color_grey(start = 0.2, end = 0.65, name = "Category") +
  scale_shape_manual(values = c("50Hertz" = 15, "Amprion" = 16, "TenneT" = 17, "TransnetBW" = 18)) +
  guides(color = guide_legend(title = "TSO", override.aes = list(shape = c(15,16,17,18))), shape = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major.y = element_line(color = "gray90"),
        panel.grid.major.x = element_blank(),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))

ggsave(filename = "export/4E1_Transcharge.pdf", plot=transcharge, width = 29.7, height = 21, units = "cm")

#e2) Übertragungsnetzbetreiber: Entwicklung Netzentgelte Real

transcharge_real <- ggplot(transmission_net, aes(x = as.factor(year), y = trans_charge_real, color = regelgebiet_name, shape = regelgebiet_name)) +
  geom_point(aes(group = regelgebiet_name), size=2) +
  geom_line(aes(group = regelgebiet_name))+
  labs(x = NULL, y = "network charge ct/kWh") +
  scale_color_grey(start = 0.2, end = 0.65, name = "Category") +
  scale_shape_manual(values = c("50Hertz" = 15, "Amprion" = 16, "TenneT" = 17, "TransnetBW" = 18)) +
  guides(color = guide_legend(title = "TSO", override.aes = list(shape = c(15,16,17,18))), shape = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major.y = element_line(color = "gray90"),
        panel.grid.major.x = element_blank(),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))

ggsave(filename = "export/4E2_Transcharge_Real.pdf", plot=transcharge_real, width = 29.7, height = 21, units = "cm")
ggsave(filename = "export-png/4E2_Transcharge_Real.png", plot = transcharge_real, width = 7, height = 4.94, units = "in")

#f1) Übertragungsnetzbetreiber: Entwicklung Arbeitspreise
transmission_ap <- ggplot(transmission_net, aes(x = as.factor(year), y = höap2, color = regelgebiet_name, shape = regelgebiet_name)) +
  geom_point(aes(group = regelgebiet_name), size=2) +
  geom_line(aes(group = regelgebiet_name))+
  labs(x = NULL, y = "network charge ct/kWh") +
  scale_color_grey(start = 0.2, end = 0.65, name = "Category") +
  scale_shape_manual(values = c("50Hertz" = 15, "Amprion" = 16, "TenneT" = 17, "TransnetBW" = 18)) +
  guides(color = guide_legend(title = "TSO", override.aes = list(shape = c(15,16,17,18))), shape = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major.y = element_line(color = "gray90"),
        panel.grid.major.x = element_blank(),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))

ggsave(filename = "export/4F1_Transworking.pdf", plot=transmission_ap, width = 29.7, height = 21, units = "cm")


#f1) Übertragungsnetzbetreiber: Entwicklung Arbeitspreise
transmission_ap_real <- ggplot(transmission_net, aes(x = as.factor(year), y = höap2, color = regelgebiet_name, shape = regelgebiet_name)) +
  geom_point(aes(group = regelgebiet_name), size=2) +
  geom_line(aes(group = regelgebiet_name))+
  labs(x = NULL, y = "network charge ct/kWh") +
  scale_color_grey(start = 0.2, end = 0.65, name = "Category") +
  scale_shape_manual(values = c("50Hertz" = 15, "Amprion" = 16, "TenneT" = 17, "TransnetBW" = 18)) +
  guides(color = guide_legend(title = "TSO", override.aes = list(shape = c(15,16,17,18))), shape = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major.y = element_line(color = "gray90"),
        panel.grid.major.x = element_blank(),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))


ggsave(filename = "export/4F2_Transworking_Real.pdf", plot=transmission_ap_real, width = 29.7, height = 21, units = "cm")

#g1) Übertragungsnetzbetreiber: Entwicklung Leistungspreise
transmission_lp <- ggplot(transmission_net, aes(x = as.factor(year), y = hölp2, color = regelgebiet_name, shape = regelgebiet_name)) +
  geom_point(aes(group = regelgebiet_name), size=2) +
  geom_line(aes(group = regelgebiet_name))+
  labs(x = NULL, y = "network charge ct/kWh") +
  scale_color_grey(start = 0.2, end = 0.65, name = "Category") +
  scale_shape_manual(values = c("50Hertz" = 15, "Amprion" = 16, "TenneT" = 17, "TransnetBW" = 18)) +
  guides(color = guide_legend(title = "TSO", override.aes = list(shape = c(15,16,17,18))), shape = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major.y = element_line(color = "gray90"),
        panel.grid.major.x = element_blank(),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))

ggsave(filename = "export/4G1_Transperformance.pdf", plot=transmission_lp, width = 29.7, height = 21, units = "cm")

#g2) Übertragungsnetzbetreiber: Entwicklung Leistungspreise Real
transmission_lp_real <- ggplot(transmission_net, aes(x = as.factor(year), y = hölp2_real, color = regelgebiet_name, shape = regelgebiet_name)) +
  geom_point(aes(group = regelgebiet_name), size=2) +
  geom_line(aes(group = regelgebiet_name))+
  labs(x = NULL, y = "network charge ct/kWh") +
  scale_color_grey(start = 0.2, end = 0.65, name = "Category") +
  scale_shape_manual(values = c("50Hertz" = 15, "Amprion" = 16, "TenneT" = 17, "TransnetBW" = 18)) +
  guides(color = guide_legend(title = "TSO", override.aes = list(shape = c(15,16,17,18))), shape = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major.y = element_line(color = "gray90"),
        panel.grid.major.x = element_blank(),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))


ggsave(filename = "export/4G2_Transperformance.pdf", plot=transmission_lp_real, width = 29.7, height = 21, units = "cm")

#h1) Übertragungsnetzbetreiber: Entwicklung Leistungspreis und Arbeitspreis zusammen
transmission_both <- grid.arrange(transmission_ap, transmission_lp, ncol = 1)

ggsave(filename = "export/4H1_Transmission_both.pdf", plot=transmission_both, width = 29.7, height = 21, units = "cm")

#h2) Übertragungsnetzbetreiber: Entwicklung Leistungspreis und Arbeitspreis zusammen Real
transmission_both_Real <- grid.arrange(transmission_ap_real, transmission_lp_real, ncol = 1)

ggsave(filename = "export/4H2_Transmission_both_Real.pdf", plot=transmission_both_Real, width = 29.7, height = 21, units = "cm")
ggsave(filename = "export-png/4H2_Transmission_both_Real.png", plot = transmission_both_Real, width = 7, height = 4.94, units = "in")

#i1) Varianzzerlegung HH
groesse <- c (138780, 79187, 109712, 34600)
n <- nrow(datensatz_zerlegung)
datensatz_zerlegung$groesse <- rep(groesse, length.out = n)

varianzzerlegung_df <- datensatz_zerlegung %>%
  group_by(year) %>%
  summarize (invarhh = weighted.mean(Varianzhh,groesse), exvarhh = weighted.mean((MWhh-(weighted.mean(MWhh,groesse)))^2, groesse), Summehh = sum(invarhh+exvarhh), invarprohh = invarhh/Summehh, exvarprohh = exvarhh/Summehh, invarbusi = weighted.mean(Varianzbusi,groesse), exvarbusi = weighted.mean((MWbusi-(weighted.mean(MWbusi,groesse)))^2, groesse), Summebusi = sum(invarbusi+exvarbusi), invarprobusi = invarbusi/Summebusi, exvarprobusi = exvarbusi/Summebusi, invarind = weighted.mean(Varianzind,groesse), exvarind = weighted.mean((MWind-(weighted.mean(MWind,groesse)))^2, groesse), Summeind = sum(invarind+exvarind), invarproind = invarind/Summeind, exvarproind = exvarind/Summeind)%>%
  filter (year>=2013)
rm (groesse, n)


varianzzerlegunghh <- ggplot(varianzzerlegung_df, aes(x = as.factor(year))) +
  geom_bar(stat = "identity", aes(y = exvarprohh+invarprohh, fill="intern"))+
  geom_bar(stat = "identity", aes(y = exvarprohh, fill= "extern"))+
  geom_text(aes(y = exvarprohh, label = round(exvarprohh,3)), vjust = -0.5, color = "black", size = 3.5) +
  labs(x = NULL, y = "Percentage Variance", fill="Variance")+
  scale_fill_manual(values = c(intern = "gray65", extern = "gray30"), labels = c("External", "Internal"))+
  theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_blank(),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14)
  )

ggsave(filename = "export/4I1_varianzzerlegung_HH.pdf", plot=varianzzerlegunghh, width = 29.7, height = 21, units = "cm")
ggsave(filename = "export-png/4I1_varianzzerlegung_HH.png", plot = varianzzerlegunghh, width = 7, height = 4.94, units = "in")

#i2) Varianzzerlegung Busi
varianzzerlegungbusi <- ggplot(varianzzerlegung_df, aes(x = as.factor(year))) +
  geom_bar(stat = "identity", aes(y = exvarprobusi+invarprobusi, fill="intern"))+
  geom_bar(stat = "identity", aes(y = exvarprobusi, fill= "extern"))+
  geom_text(aes(y = exvarprobusi, label = round(exvarprobusi,3)), vjust = -0.5, color = "black", size = 3.5) +
  labs(x = NULL, y = "Percentage Variance", fill="Variance")+
  scale_fill_manual(values = c(intern = "gray65", extern = "gray30"), labels = c("External", "Internal"))+
  theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_blank())

ggsave(filename = "export/4I2_varianzzerlegung_Busi.pdf", plot=varianzzerlegungbusi, width = 29.7, height = 21, units = "cm")

#i3) Varianzzerlegung Ind
varianzzerlegungind <- ggplot(varianzzerlegung_df, aes(x = as.factor(year))) +
  geom_bar(stat = "identity", aes(y = exvarproind+invarproind, fill="intern"))+
  geom_bar(stat = "identity", aes(y = exvarproind, fill= "extern"))+
  geom_text(aes(y = exvarproind, label = round(exvarproind,3)), vjust = -0.5, color = "black", size = 3.5) +
  labs(x = NULL, y = "Percentage Variance", fill="Variance")+
  scale_fill_manual(values = c(intern = "gray65", extern = "gray30"), labels = c("External", "Internal"))+
  theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_blank(),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14)
  )

ggsave(filename = "export/4I3_varianzzerlegung_Ind.pdf", plot=varianzzerlegungind, width = 29.7, height = 21, units = "cm")
ggsave(filename = "export-png/4I3_varianzzerlegung_Ind.png", plot = varianzzerlegungind, width = 7, height = 4.94, units = "in")

#j1) Netzentgelt je Regelgebiet HH
charges_tennet_sum <- municipal_network_charges %>%
  filter (regelgebiet_num==1) %>%
  filter (year>=2015) %>%
  group_by (year) %>%
  summarize (charge_hh = mean (hh_nw_charge_wmean, na.rm = TRUE), charge_real_hh = mean (hh_nw_charge_wmean_real, na.rm = TRUE), var_hh = var (hh_nw_charge_wmean_real, na.rm = TRUE), charge_busi = mean (busi_nw_charge_wmean, na.rm = TRUE), charge_real_busi = mean (busi_nw_charge_wmean_real, na.rm = TRUE), var_busi = var (busi_nw_charge_wmean_real, na.rm = TRUE), charge_ind = mean (ind_nw_charge_wmean, na.rm = TRUE), charge_real_ind = mean (ind_nw_charge_wmean_real, na.rm = TRUE), var_ind = var (ind_nw_charge_wmean_real, na.rm = TRUE))%>%
  mutate(cat = "TenneT")

charges_amprion_sum <- municipal_network_charges%>%
  filter (regelgebiet_num==2) %>%
  filter (year>=2015)%>%
  group_by (year) %>%
  summarize (charge_hh = mean (hh_nw_charge_wmean, na.rm = TRUE), charge_real_hh = mean (hh_nw_charge_wmean_real, na.rm = TRUE), var_hh = var (hh_nw_charge_wmean_real, na.rm = TRUE), charge_busi = mean (busi_nw_charge_wmean, na.rm = TRUE), charge_real_busi = mean (busi_nw_charge_wmean_real, na.rm = TRUE), var_busi = var (busi_nw_charge_wmean_real, na.rm = TRUE), charge_ind = mean (ind_nw_charge_wmean, na.rm = TRUE), charge_real_ind = mean (ind_nw_charge_wmean_real, na.rm = TRUE), var_ind = var (ind_nw_charge_wmean_real, na.rm = TRUE))%>%
  mutate(cat = "Amprion")

charges_hertz_sum <- municipal_network_charges %>%
  filter (regelgebiet_num==3) %>%
  filter (year>=2015)%>%
  group_by (year) %>%
  summarize (charge_hh = mean (hh_nw_charge_wmean, na.rm = TRUE), charge_real_hh = mean (hh_nw_charge_wmean_real, na.rm = TRUE), var_hh = var (hh_nw_charge_wmean_real, na.rm = TRUE), charge_busi = mean (busi_nw_charge_wmean, na.rm = TRUE), charge_real_busi = mean (busi_nw_charge_wmean_real, na.rm = TRUE), var_busi = var (busi_nw_charge_wmean_real, na.rm = TRUE), charge_ind = mean (ind_nw_charge_wmean, na.rm = TRUE), charge_real_ind = mean (ind_nw_charge_wmean_real, na.rm = TRUE), var_ind = var (ind_nw_charge_wmean_real, na.rm = TRUE))%>%
  mutate(cat = "50Hertz")

charges_trans_sum <- municipal_network_charges %>%
  filter (regelgebiet_num==4) %>%
  filter (year>=2015)%>%
  group_by (year) %>%
  summarize (charge_hh = mean (hh_nw_charge_wmean, na.rm = TRUE), charge_real_hh = mean (hh_nw_charge_wmean_real, na.rm = TRUE), var_hh = var (hh_nw_charge_wmean_real, na.rm = TRUE), charge_busi = mean (busi_nw_charge_wmean, na.rm = TRUE), charge_real_busi = mean (busi_nw_charge_wmean_real, na.rm = TRUE), var_busi = var (busi_nw_charge_wmean_real, na.rm = TRUE), charge_ind = mean (ind_nw_charge_wmean, na.rm = TRUE), charge_real_ind = mean (ind_nw_charge_wmean_real, na.rm = TRUE), var_ind = var (ind_nw_charge_wmean_real, na.rm = TRUE))%>%
  mutate(cat = "TransnetBW")

charges_transmission_layer <- bind_rows(charges_tennet_sum,charges_amprion_sum,charges_hertz_sum,charges_trans_sum)


regionchargehh <- ggplot(charges_transmission_layer, aes(x = as.factor(year), y = charge_hh, color = cat, shape = cat)) +
  geom_point(size = 2) +
  geom_line(aes(group = cat))+
  geom_vline(xintercept = "2019", linetype = "dashed", color = "black") +
  labs(x = NULL, y = "network charge ct/kWh") +
  scale_color_grey(start = 0.2, end = 0.65, name = "Category") +
  scale_shape_manual(values = c("50Hertz" = 15, "Amprion" = 16, "TenneT" = 17, "TransnetBW" = 18)) +
  guides(color = guide_legend(title = "TSO zone", override.aes = list(shape = c(15,16,17,18))), shape = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major.y = element_line(color = "gray90"),
        panel.grid.major.x = element_blank(),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))

ggsave(filename = "export/4J1_Regionscharge_HH.pdf", plot=regionchargehh, width = 29.7, height = 21, units = "cm")

#j2) Netzentgelt je Regelgebiet HH Real
regionchargehhreal <- ggplot(charges_transmission_layer, aes(x = as.factor(year), y = charge_real_hh, color = cat, shape = cat)) +
  geom_point(size = 2) +
  geom_line(aes(group = cat))+
  geom_vline(xintercept = "2019", linetype = "dashed", color = "black") +
  labs(x = NULL, y = "network charge ct/kWh") +
  scale_color_grey(start = 0.2, end = 0.65, name = "Category") +
  scale_shape_manual(values = c("50Hertz" = 15, "Amprion" = 16, "TenneT" = 17, "TransnetBW" = 18)) +
  guides(color = guide_legend(title = "TSO zone", override.aes = list(shape = c(15,16,17,18))), shape = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major.y = element_line(color = "gray90"),
        panel.grid.major.x = element_blank(),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))

ggsave(filename = "export/4J2_Regionscharge_HH_Real.pdf", plot=regionchargehhreal, width = 29.7, height = 21, units = "cm")
ggsave(filename = "export-png/4J2_Regionscharge_HH_Real.png", plot = regionchargehhreal, width = 7, height = 4.94, units = "in")

#j3) Varianz je Regelgebiet HH Real
regionhhvarreal <- ggplot(charges_transmission_layer, aes(x = as.factor(year))) +
  geom_point(aes(y = var_tennet_hh, color = "TenneT TSO GmbH", shape="TenneT TSO GmbH"), size=2) +
  geom_line(aes(y = var_tennet_hh, color = "TenneT TSO GmbH", group = 1)) +
  geom_point(aes(y = var_amprion_hh, color = "Amprion GmbH", shape="Amprion GmbH"), size=2) +
  geom_line(aes(y = var_amprion_hh, color = "Amprion GmbH", group = 1)) +
  geom_point(aes(y = var_hertz_hh, color = "50Hertz Transmission GmbH", shape="50Hertz Transmission GmbH"), size=2) +
  geom_line(aes(y = var_hertz_hh, color = "50Hertz Transmission GmbH", group = 1)) +
  geom_point(aes(y = var_trans_hh, color = "TransnetBW GmbH", shape="TransnetBW GmbH"), size=2) +
  geom_line(aes(y = var_trans_hh, color = "TransnetBW GmbH", group = 1)) +
  geom_vline(xintercept = "2018", linetype = "dashed", color = "black") +
  scale_color_grey(start = 0.2, end = 0.65, guide = "none") + 
  labs(x = NULL, y = "Variance (ct/kWh)²", shape = "region of network operator") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major.y = element_line(color = "gray90"),
        panel.grid.major.x = element_blank())

ggsave(filename = "export/4J3_Regionsvariance_HH.pdf", plot=regionhhvarreal, width = 29.7, height = 21, units = "cm")

#j4) Netzentgelt Busi je Regelgebiet

regionchargebusi <- ggplot(charges_transmission_layer, aes(x = as.factor(year))) +
  geom_point(aes(y = charge_tennet_busi, color = "TenneT TSO GmbH", shape="TenneT TSO GmbH"), size=2) +
  geom_line(aes(y = charge_tennet_busi, color = "TenneT TSO GmbH", group = 1)) +
  geom_point(aes(y = charge_amprion_busi, color = "Amprion GmbH", shape="Amprion GmbH"), size=2) +
  geom_line(aes(y = charge_amprion_busi, color = "Amprion GmbH", group = 1)) +
  geom_point(aes(y = charge_hertz_busi, color = "50Hertz Transmission GmbH", shape="50Hertz Transmission GmbH"), size=2) +
  geom_line(aes(y = charge_hertz_busi, color = "50Hertz Transmission GmbH", group = 1)) +
  geom_point(aes(y = charge_trans_busi, color = "TransnetBW GmbH", shape="TransnetBW GmbH"), size=2) +
  geom_line(aes(y = charge_trans_busi, color = "TransnetBW GmbH", group = 1)) +
  geom_vline(xintercept = "2018", linetype = "dashed", color = "black") +
  scale_color_grey(start = 0.2, end = 0.65, guide = "none") + 
  labs(x = NULL, y = "network charge ct/kWh", shape = "region of network operator") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major.y = element_line(color = "gray90"),
        panel.grid.major.x = element_blank())

ggsave(filename = "export/4J4_Regionscharge_busi.pdf", plot=regionchargebusi, width = 29.7, height = 21, units = "cm")

#j5) Netzentgelt Busi je Regelgebiet Real
regionchargebusireal <- ggplot(charges_transmission_layer, aes(x = as.factor(year))) +
  geom_point(aes(y = charge_tennet_real_busi, color = "TenneT TSO GmbH", shape="TenneT TSO GmbH"), size=2) +
  geom_line(aes(y = charge_tennet_real_busi, color = "TenneT TSO GmbH", group = 1)) +
  geom_point(aes(y = charge_amprion_real_busi, color = "Amprion GmbH", shape="Amprion GmbH"), size=2) +
  geom_line(aes(y = charge_amprion_real_busi, color = "Amprion GmbH", group = 1)) +
  geom_point(aes(y = charge_hertz_real_busi, color = "50Hertz Transmission GmbH", shape="50Hertz Transmission GmbH"), size=2) +
  geom_line(aes(y = charge_hertz_real_busi, color = "50Hertz Transmission GmbH", group = 1)) +
  geom_point(aes(y = charge_trans_real_busi, color = "TransnetBW GmbH", shape="TransnetBW GmbH"), size=2) +
  geom_line(aes(y = charge_trans_real_busi, color = "TransnetBW GmbH", group = 1)) +
  geom_vline(xintercept = "2018", linetype = "dashed", color = "black") +
  scale_color_grey(start = 0.2, end = 0.65, guide = "none") + 
  labs(x = NULL, y = "network charge ct/kWh", shape = "region of network operator") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major.y = element_line(color = "gray90"),
        panel.grid.major.x = element_blank())

ggsave(filename = "export/4J5_Regionscharge_busi_Real.pdf", plot=regionchargebusireal, width = 29.7, height = 21, units = "cm")

#j6) Varianz Busi je Regelgebiet Real
regionbusivarreal <- ggplot(charges_transmission_layer, aes(x = as.factor(year))) +
  geom_point(aes(y = var_tennet_busi, color = "TenneT TSO GmbH", shape="TenneT TSO GmbH"), size=2) +
  geom_line(aes(y = var_tennet_busi, color = "TenneT TSO GmbH", group = 1)) +
  geom_point(aes(y = var_amprion_busi, color = "Amprion GmbH", shape="Amprion GmbH"), size=2) +
  geom_line(aes(y = var_amprion_busi, color = "Amprion GmbH", group = 1)) +
  geom_point(aes(y = var_hertz_busi, color = "50Hertz Transmission GmbH", shape="50Hertz Transmission GmbH"), size=2) +
  geom_line(aes(y = var_hertz_busi, color = "50Hertz Transmission GmbH", group = 1)) +
  geom_point(aes(y = var_trans_busi, color = "TransnetBW GmbH", shape="TransnetBW GmbH"), size=2) +
  geom_line(aes(y = var_trans_busi, color = "TransnetBW GmbH", group = 1)) +
  geom_vline(xintercept = "2018", linetype = "dashed", color = "black") +
  scale_color_grey(start = 0.2, end = 0.65, guide = "none") + 
  labs(x = NULL, y = "Variance (ct/kWh)²", shape = "region of network operator") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major.y = element_line(color = "gray90"),
        panel.grid.major.x = element_blank())

ggsave(filename = "export/4J6_Regionsvariance_Busi.pdf", plot=regionbusivarreal, width = 29.7, height = 21, units = "cm")

#j7) Netzentgelt Ind je Regelgebiet

regionchargeind <- ggplot(charges_transmission_layer, aes(x = as.factor(year))) +
  geom_point(aes(y = charge_tennet_ind, color = "TenneT TSO GmbH", shape="TenneT TSO GmbH"), size=2) +
  geom_line(aes(y = charge_tennet_ind, color = "TenneT TSO GmbH", group = 1)) +
  geom_point(aes(y = charge_amprion_ind, color = "Amprion GmbH", shape="Amprion GmbH"), size=2) +
  geom_line(aes(y = charge_amprion_ind, color = "Amprion GmbH", group = 1)) +
  geom_point(aes(y = charge_hertz_ind, color = "50Hertz Transmission GmbH", shape="50Hertz Transmission GmbH"), size=2) +
  geom_line(aes(y = charge_hertz_ind, color = "50Hertz Transmission GmbH", group = 1)) +
  geom_point(aes(y = charge_trans_ind, color = "TransnetBW GmbH", shape="TransnetBW GmbH"), size=2) +
  geom_line(aes(y = charge_trans_ind, color = "TransnetBW GmbH", group = 1)) +
  geom_vline(xintercept = "2018", linetype = "dashed", color = "black") +
  scale_color_grey(start = 0.2, end = 0.65, guide = "none") + 
  labs(x = NULL, y = "network charge ct/kWh", shape = "region of network operator") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major.y = element_line(color = "gray90"),
        panel.grid.major.x = element_blank())

ggsave(filename = "export/4J7_Regionscharge_Ind.pdf", plot=regionchargeind, width = 29.7, height = 21, units = "cm")

#j8) Netzentgelt Ind je Regelgebiet Real
regionchargeindreal <- ggplot(charges_transmission_layer, aes(x = as.factor(year))) +
  geom_point(aes(y = charge_tennet_real_ind, color = "TenneT TSO GmbH", shape="TenneT TSO GmbH"), size=2) +
  geom_line(aes(y = charge_tennet_real_ind, color = "TenneT TSO GmbH", group = 1)) +
  geom_point(aes(y = charge_amprion_real_ind, color = "Amprion GmbH", shape="Amprion GmbH"), size=2) +
  geom_line(aes(y = charge_amprion_real_ind, color = "Amprion GmbH", group = 1)) +
  geom_point(aes(y = charge_hertz_real_ind, color = "50Hertz Transmission GmbH", shape="50Hertz Transmission GmbH"), size=2) +
  geom_line(aes(y = charge_hertz_real_ind, color = "50Hertz Transmission GmbH", group = 1)) +
  geom_point(aes(y = charge_trans_real_ind, color = "TransnetBW GmbH", shape="TransnetBW GmbH"), size=2) +
  geom_line(aes(y = charge_trans_real_ind, color = "TransnetBW GmbH", group = 1)) +
  geom_vline(xintercept = "2018", linetype = "dashed", color = "black") +
  scale_color_grey(start = 0.2, end = 0.65, guide = "none") + 
  labs(x = NULL, y = "network charge ct/kWh", shape = "region of network operator") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major.y = element_line(color = "gray90"),
        panel.grid.major.x = element_blank())

ggsave(filename = "export/4J8_Regionscharge_Ind_Real.pdf", plot=regionchargeindreal, width = 29.7, height = 21, units = "cm")

#j9) Varianz Ind je Regelgebiet Real
regionindvarreal <- ggplot(charges_transmission_layer, aes(x = as.factor(year))) +
  geom_point(aes(y = var_tennet_ind, color = "TenneT TSO GmbH", shape="TenneT TSO GmbH"), size=2) +
  geom_line(aes(y = var_tennet_ind, color = "TenneT TSO GmbH", group = 1)) +
  geom_point(aes(y = var_amprion_ind, color = "Amprion GmbH", shape="Amprion GmbH"), size=2) +
  geom_line(aes(y = var_amprion_ind, color = "Amprion GmbH", group = 1)) +
  geom_point(aes(y = var_hertz_ind, color = "50Hertz Transmission GmbH", shape="50Hertz Transmission GmbH"), size=2) +
  geom_line(aes(y = var_hertz_ind, color = "50Hertz Transmission GmbH", group = 1)) +
  geom_point(aes(y = var_trans_ind, color = "TransnetBW GmbH", shape="TransnetBW GmbH"), size=2) +
  geom_line(aes(y = var_trans_ind, color = "TransnetBW GmbH", group = 1)) +
  geom_vline(xintercept = "2018", linetype = "dashed", color = "black") +
  scale_color_grey(start = 0.2, end = 0.65, guide = "none") + 
  labs(x = NULL, y = "Variance (ct/kWh)²", shape = "region of network operator") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major.y = element_line(color = "gray90"),
        panel.grid.major.x = element_blank())

ggsave(filename = "export/4J9_Regionsvariance_Ind.pdf", plot=regionindvarreal, width = 29.7, height = 21, units = "cm")

#j10) Netzentgelt je Regelgebiet HH Normiert
charges_transmission_layer2018 <- charges_transmission_layer %>%
  filter (year>=2015) %>%
  mutate (charge_hh_norm=charge_hh-charge_hh[year==2018]+1)%>%
  mutate (charge_hh_real_norm=charge_real_hh-charge_real_hh[year==2018]+1)%>%
  mutate (var_hh_norm=var_hh-var_hh[year==2018]+1)

regionchargehh_norm <- ggplot(charges_transmission_layer2018, aes(x = as.factor(year))) +
  geom_point(aes(y = charge_tennet_hh_norm, color = "TenneT TSO GmbH", shape="TenneT TSO GmbH"), size=2) +
  geom_line(aes(y = charge_tennet_hh_norm, color = "TenneT TSO GmbH", group = 1)) +
  geom_point(aes(y = charge_amprion_hh_norm, color = "Amprion GmbH", shape="Amprion GmbH"), size=2) +
  geom_line(aes(y = charge_amprion_hh_norm, color = "Amprion GmbH", group = 1)) +
  geom_point(aes(y = charge_hertz_hh_norm, color = "50Hertz Transmission GmbH", shape="50Hertz Transmission GmbH"), size=2) +
  geom_line(aes(y = charge_hertz_hh_norm, color = "50Hertz Transmission GmbH", group = 1)) +
  geom_point(aes(y = charge_trans_hh_norm, color = "TransnetBW GmbH", shape="TransnetBW GmbH"), size=2) +
  geom_line(aes(y = charge_trans_hh_norm, color = "TransnetBW GmbH", group = 1)) +
  scale_color_grey(start = 0.2, end = 0.65, guide = "none") + 
  labs(x = NULL, y = "network charge ct/kWh", shape = "region of network operator") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major.y = element_line(color = "gray90"),
        panel.grid.major.x = element_blank())

ggsave(filename = "export/4J10_Regionscharge_Norm_HH.pdf", plot=regionchargehh_norm, width = 29.7, height = 21, units = "cm")

#j11) Netzentgelt je Regelgebiet HH Real Normiert
regionchargehhreal_norm <- ggplot(charges_transmission_layer2018, aes(x = as.factor(year))) +
  geom_point(aes(y = charge_tennet_hh_real_norm, color = "TenneT TSO GmbH", shape="TenneT TSO GmbH"), size=2) +
  geom_line(aes(y = charge_tennet_hh_real_norm, color = "TenneT TSO GmbH", group = 1)) +
  geom_point(aes(y = charge_amprion_hh_real_norm, color = "Amprion GmbH", shape="Amprion GmbH"), size=2) +
  geom_line(aes(y = charge_amprion_hh_real_norm, color = "Amprion GmbH", group = 1)) +
  geom_point(aes(y = charge_hertz_hh_real_norm, color = "50Hertz Transmission GmbH", shape="50Hertz Transmission GmbH"), size=2) +
  geom_line(aes(y = charge_hertz_hh_real_norm, color = "50Hertz Transmission GmbH", group = 1)) +
  geom_point(aes(y = charge_trans_hh_real_norm, color = "TransnetBW GmbH", shape="TransnetBW GmbH"), size=2) +
  geom_line(aes(y = charge_trans_hh_real_norm, color = "TransnetBW GmbH", group = 1)) +
  scale_color_grey(start = 0.2, end = 0.65, guide = "none") + 
  labs(x = NULL, y = "network charge ct/kWh", shape = "region of network operator") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major.y = element_line(color = "gray90"),
        panel.grid.major.x = element_blank())

ggsave(filename = "export/4J11_Regionscharge_Norm_HH_Real.pdf", plot=regionchargehhreal_norm, width = 29.7, height = 21, units = "cm")

#j12) Varianz je Regelgebiet HH Real Normiert
regionvarhh_norm <- ggplot(charges_transmission_layer2018, aes(x = as.factor(year))) +
  geom_point(aes(y = var_tennet_hh_norm, color = "TenneT TSO GmbH", shape="TenneT TSO GmbH"), size=2) +
  geom_line(aes(y = var_tennet_hh_norm, color = "TenneT TSO GmbH", group = 1)) +
  geom_point(aes(y = var_amprion_hh_norm, color = "Amprion GmbH", shape="Amprion GmbH"), size=2) +
  geom_line(aes(y = var_amprion_hh_norm, color = "Amprion GmbH", group = 1)) +
  geom_point(aes(y = var_hertz_hh_norm, color = "50Hertz Transmission GmbH", shape="50Hertz Transmission GmbH"), size=2) +
  geom_line(aes(y = var_hertz_hh_norm, color = "50Hertz Transmission GmbH", group = 1)) +
  geom_point(aes(y = var_trans_hh_norm, color = "TransnetBW GmbH", shape="TransnetBW GmbH"), size=2) +
  geom_line(aes(y = var_trans_hh_norm, color = "TransnetBW GmbH", group = 1)) +
  scale_color_grey(start = 0.2, end = 0.65, guide = "none") + 
  labs(x = NULL, y = "network charge ct/kWh", shape = "region of network operator") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major.y = element_line(color = "gray90"),
        panel.grid.major.x = element_blank())

ggsave(filename = "export/4J12_Regionsvariance_Norm_HH.pdf", plot=regionvarhh_norm, width = 29.7, height = 21, units = "cm")

#j13) Varianz und Mean 4er
basic_regulatory <- ggplot(charges_transmission_layer, aes(x = as.factor(year))) +
  geom_vline(xintercept = "2018", linetype = "dashed", color = "black") +
  scale_color_grey(start = 0.2, end = 0.65, guide = NULL) + 
  labs(x = NULL, y = "network charge ct/kWh", shape = NULL) +
  scale_y_continuous(limits = c(6, 14)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major.y = element_line(color = "gray90"),
        panel.grid.major.x = element_blank(),
        legend.position = "none")

regulatory_tennet <- basic_regulatory + geom_line(aes(x = as.factor(year), y = charge_tennet_real_hh, color = "TenneT TSO GmbH", group = 1))+
  geom_ribbon(aes(x = 1:length(year), ymin = charge_tennet_real_hh-sqrt(var_tennet_hh), ymax = charge_tennet_real_hh+sqrt(var_tennet_hh)), fill = "gray40", alpha = 0.5)+ 
  geom_point(aes(x = as.factor(year), y = charge_tennet_real_hh, color = "TenneT TSO GmbH", shape="TenneT TSO GmbH"), size=2)

regulatory_amprion<- basic_regulatory + geom_line(aes(y = charge_amprion_real_hh, color = "Amprion GmbH", group = 1)) +
  geom_ribbon(aes(x = 1:length(year), ymin = charge_amprion_real_hh-sqrt(var_amprion_hh), ymax = charge_amprion_real_hh+sqrt(var_amprion_hh)), fill = "gray40", alpha = 0.5)+ 
  geom_point(aes(y = charge_amprion_real_hh, color = "Amprion GmbH", shape="Amprion GmbH"), size=2)

regulatory_hertz <- basic_regulatory +  geom_line(aes(y = charge_hertz_real_hh, color = "50Hertz Transmission GmbH", group = 1)) +
  geom_ribbon(aes(x = 1:length(year), ymin = charge_hertz_real_hh-sqrt(var_hertz_hh), ymax = charge_hertz_real_hh+sqrt(var_hertz_hh)), fill = "gray40", alpha = 0.5)+ 
  geom_point(aes(y = charge_hertz_real_hh, color = "50Hertz Transmission GmbH", shape="50Hertz Transmission GmbH"), size=2)

regulatory_transnet <- basic_regulatory +   geom_line(aes(y = charge_trans_real_hh, color = "TransnetBW GmbH", group = 1)) +
  geom_ribbon(aes(x = 1:length(year), ymin = charge_trans_real_hh-sqrt(var_trans_hh), ymax = charge_trans_real_hh+sqrt(var_trans_hh)), fill = "gray40", alpha = 0.5)+ 
  geom_point(aes(y = charge_trans_real_hh, color = "TransnetBW GmbH", shape="TransnetBW GmbH"), size=2)


regulatory <- grid.arrange(regulatory_amprion, regulatory_transnet, regulatory_hertz, regulatory_tennet, ncol = 2)
ggsave(filename = "export/4J13_Regionsvariance4.pdf", plot=regulatory, width = 29.7, height = 21, units = "cm")

#j14) Varianz und Mean 4er Norm
charges_normed_tennet <- charges_transmission_layer %>%
  filter (year>=2015) %>%
  filter(cat=="TenneT")%>%
  mutate (charge_hh_norm=charge_hh-charge_hh[year==2018]+1)%>%
  mutate (charge_hh_real_norm=charge_real_hh-charge_real_hh[year==2018]+1)%>%
  mutate (var_hh_norm=var_hh-var_hh[year==2018]+1)

charges_normed_trans <- charges_transmission_layer %>%
  filter (year>=2015) %>%
  filter(cat=="TransnetBW")%>%
  mutate (charge_hh_norm=charge_hh-charge_hh[year==2018]+1)%>%
  mutate (charge_hh_real_norm=charge_real_hh-charge_real_hh[year==2018]+1)%>%
  mutate (var_hh_norm=var_hh-var_hh[year==2018]+1)

charges_normed_hertz <- charges_transmission_layer %>%
  filter (year>=2015) %>%
  filter(cat=="50Hertz")%>%
  mutate (charge_hh_norm=charge_hh-charge_hh[year==2018]+1)%>%
  mutate (charge_hh_real_norm=charge_real_hh-charge_real_hh[year==2018]+1)%>%
  mutate (var_hh_norm=var_hh-var_hh[year==2018]+1)

charges_normed_amprion <- charges_transmission_layer %>%
  filter (year>=2015) %>%
  filter(cat=="Amprion")%>%
  mutate (charge_hh_norm=charge_hh-charge_hh[year==2018]+1)%>%
  mutate (charge_hh_real_norm=charge_real_hh-charge_real_hh[year==2018]+1)%>%
  mutate (var_hh_norm=var_hh-var_hh[year==2018]+1)


regulatory_tennet_norm <- ggplot(charges_normed_tennet, aes(x = as.factor(year))) +
  geom_vline(xintercept = "2019", linetype = "dashed", color = "black") +
  scale_color_grey(start = 0.2, end = 0.65, guide = NULL) + 
  labs(x = NULL, y = "network charge ct/kWh", shape = NULL) +
  scale_y_continuous(limits = c(-2, 5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, size = rel(0.8)),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major.y = element_line(color = "gray90"),
        panel.grid.major.x = element_blank(),
        legend.position = "none",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))+
  geom_line(aes(x = as.factor(year), y = charge_hh_real_norm, color = "TenneT", group = 1))+
  geom_ribbon(aes(x = 1:length(year), ymin = charge_hh_real_norm-sqrt(var_hh_norm), ymax = charge_hh_real_norm+sqrt(var_hh_norm)), fill = "gray40", alpha = 0.5)+ 
  geom_point(aes(x = as.factor(year), y = charge_hh_real_norm, color = "TenneT", shape="TenneT"), size=2)+
  ggtitle("TenneT")

regulatory_transnet_norm <- ggplot(charges_normed_trans, aes(x = as.factor(year))) +
  geom_vline(xintercept = "2019", linetype = "dashed", color = "black") +
  scale_color_grey(start = 0.2, end = 0.65, guide = NULL) + 
  labs(x = NULL, y = "network charge ct/kWh", shape = NULL) +
  scale_y_continuous(limits = c(-2, 5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, size = rel(0.8)),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major.y = element_line(color = "gray90"),
        panel.grid.major.x = element_blank(),
        legend.position = "none",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))+
  geom_line(aes(x = as.factor(year), y = charge_hh_real_norm, color = "TransnetBW", group = 1))+
  geom_ribbon(aes(x = 1:length(year), ymin = charge_hh_real_norm-sqrt(var_hh_norm), ymax = charge_hh_real_norm+sqrt(var_hh_norm)), fill = "gray40", alpha = 0.5)+ 
  geom_point(aes(x = as.factor(year), y = charge_hh_real_norm, color = "TransnetBW", shape="TransnetBW"), size=2)+
  ggtitle("TransnetBW")


regulatory_hertz_norm <- ggplot(charges_normed_hertz, aes(x = as.factor(year))) +
  geom_vline(xintercept = "2019", linetype = "dashed", color = "black") +
  scale_color_grey(start = 0.2, end = 0.65, guide = NULL) + 
  labs(x = NULL, y = "network charge ct/kWh", shape = NULL) +
  scale_y_continuous(limits = c(-2, 5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, size = rel(0.8)),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major.y = element_line(color = "gray90"),
        panel.grid.major.x = element_blank(),
        legend.position = "none",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))+
  geom_line(aes(x = as.factor(year), y = charge_hh_real_norm, color = "50Hertz", group = 1))+
  geom_ribbon(aes(x = 1:length(year), ymin = charge_hh_real_norm-sqrt(var_hh_norm), ymax = charge_hh_real_norm+sqrt(var_hh_norm)), fill = "gray40", alpha = 0.5)+ 
  geom_point(aes(x = as.factor(year), y = charge_hh_real_norm, color = "50Hertz", shape="50Hertz"), size=2)+
  ggtitle("50Hertz")



regulatory_amprion_norm <- ggplot(charges_normed_amprion, aes(x = as.factor(year))) +
  geom_vline(xintercept = "2019", linetype = "dashed", color = "black") +
  scale_color_grey(start = 0.2, end = 0.65, guide = NULL) + 
  labs(x = NULL, y = "network charge ct/kWh", shape = NULL) +
  scale_y_continuous(limits = c(-2, 5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, size = rel(0.8)),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major.y = element_line(color = "gray90"),
        panel.grid.major.x = element_blank(),
        legend.position = "none",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))+
  geom_line(aes(x = as.factor(year), y = charge_hh_real_norm, color = "Amprion", group = 1))+
  geom_ribbon(aes(x = 1:length(year), ymin = charge_hh_real_norm-sqrt(var_hh_norm), ymax = charge_hh_real_norm+sqrt(var_hh_norm)), fill = "gray40", alpha = 0.5)+ 
  geom_point(aes(x = as.factor(year), y = charge_hh_real_norm, color = "Amprion", shape="Amprion"), size=2)+
  ggtitle("Amprion")

regulatory_norm <- grid.arrange(regulatory_amprion_norm, regulatory_transnet_norm, regulatory_hertz_norm, regulatory_tennet_norm, ncol = 2)
ggsave(filename = "export/4J14_Regionsvariance4_Norm.pdf", plot=regulatory_norm, width = 29.7, height = 21, units = "cm")
ggsave(filename = "export-png/4J14_Regionsvariance4_Norm.png", plot = regulatory_norm, width = 7, height = 4.94, units = "in")

############################################################
# (5)
# a) Prüfen, ob signifikante Unterschiede zwischen den Regelgebieten bestehen
# z.B. Geodaten, Bevölkerung, Stromnutzung

# b) Prüfen, welchen Einfluss die Gesetzesänderung hatte
#b1) Durchführen im Gesamten Paneldatensatz

regression_data <- municipal_network_charges %>%
  filter (year >= 2015)%>%
  filter(regelgebiet_num<=4)%>%
  filter(!is.na(hh_nw_charge_wmean_real))%>%
  left_join(muni_data, c("year", "AGS"))%>%
  left_join(geodaten, c("year", "AGS"))%>%
  left_join (breitengrad, by="AGS")%>%
  mutate (treat=ifelse(year >= 2019, 1, 0))%>%
  mutate (untreat=ifelse(year <= 2018, 1, 0))%>%
  mutate (year2015=ifelse(year == 2015, 1, 0))%>%
  mutate (year2016=ifelse(year == 2016, 1, 0))%>%
  mutate (year2017=ifelse(year == 2017, 1, 0))%>%
  mutate (year2018=ifelse(year == 2018, 1, 0))%>%
  mutate (year2019=ifelse(year == 2019, 1, 0))%>%
  mutate (year2020=ifelse(year == 2020, 1, 0))%>%
  mutate (year2021=ifelse(year == 2021, 1, 0))%>%
  mutate (year2022=ifelse(year == 2022, 1, 0))%>%
  mutate(bundesanteil = case_when(
    year <= 2018 ~ 0,
    year == 2019 ~ 0.2,
    year == 2020 ~ 0.4,
    year == 2021 ~ 0.6,
    year == 2022 ~ 0.8)) %>%
  mutate(tennet = ifelse(regelgebiet_num == 1, 1, 0))%>%
  mutate(amprion = ifelse(regelgebiet_num == 2, 1, 0))%>%
  mutate(hertz = ifelse(regelgebiet_num == 3, 1, 0))%>%
  mutate(trans = ifelse(regelgebiet_num == 4, 1, 0))%>%
  mutate(year=year-min(year))

#b11) Transmission_charge
lm_model_hh_b11 <- lm (hh_nw_charge_wmean_real ~ trans_charge_wmean*year, data = regression_data)
summary(lm_model_hh_b11)

#b121) impact region household
regimpact1 <- lm (hh_nw_charge_wmean_real ~ tennet + hertz + trans, data = regression_data)
summary(regimpact1)
regimpact2 <- lm (hh_nw_charge_wmean_real ~ year + tennet + hertz + trans, data = regression_data)
summary(regimpact2)
regimpact3 <- lm (hh_nw_charge_wmean_real ~ treat, data = regression_data)
summary(regimpact3)
regimpact4 <- lm (hh_nw_charge_wmean_real ~  tennet*treat + hertz*treat + trans*treat + year, data = regression_data)
summary(regimpact4)

regression_impact <- stargazer(regimpact1, regimpact2, regimpact3, regimpact4, title="Development network charge with regard to treatment", align=TRUE, omit.stat=c("LL","ser","f"),no.space=TRUE, order=c("Constant"), covariate.labels = c("$Intercept$","$year$", "$Tennet \times treat$", "$50Hertz \times treat$", "$TrannetBW \times treat$", "$Tennet$", "$50Hertz$", "$TransnetBW$", "$treat$"), dep.var.labels = "$charge_{hh,real}$")
writeLines(regression_impact, "regressionstabellen/regimpact.txt")

#b122) impact region industrial
regimpact1ind <- lm (ind_nw_charge_wmean_real ~ tennet + hertz + trans, data = regression_data)
summary(regimpact1ind)
regimpact2ind <- lm (ind_nw_charge_wmean_real ~ year + tennet + hertz + trans, data = regression_data)
summary(regimpact2ind)
regimpact3ind <- lm (ind_nw_charge_wmean_real ~ treat, data = regression_data)
summary(regimpact3ind)
regimpact4ind <- lm (ind_nw_charge_wmean_real ~  tennet*treat + hertz*treat + trans*treat + year, data = regression_data)
summary(regimpact4ind)

regression_impact <- stargazer(regimpact1ind, regimpact2ind, regimpact3ind, regimpact4ind, title="Development network charge with regard to treatment", align=TRUE, omit.stat=c("LL","ser","f"),no.space=TRUE, order=c("Constant"), covariate.labels = c("$Intercept$","$year$", "$Tennet \times treat$", "$50Hertz \times treat$", "$TrannetBW \times treat$", "$Tennet$", "$50Hertz$", "$TransnetBW$", "$treat$"), dep.var.labels = "$charge_{ind,real}$")
writeLines(regression_impact, "regressionstabellen/regimpactind.txt")

#b13) development exvar years
exvar <- varianzzerlegung_df %>%
  filter(year>=2018)%>%
  dplyr::select(year, exvarhh, exvarprohh)%>%
  mutate(year = case_when(
    year == 2018 ~ 0,
    year == 2019 ~ 1,
    year == 2020 ~ 2,
    year == 2021 ~ 3,
    year == 2022 ~ 4))

exvar_data<-regression_data%>%
  left_join(exvar, by="year")
lm_model_b13 <- lm(exvarhh ~ year, data = exvar_data)
summary(lm_model_b13)

#b14) Abstand zum Mittelwert
abstand <- regression_data %>%
  group_by (year)%>%
  mutate(MW=mean(hh_nw_charge_wmean, na.rm = TRUE))%>%
  ungroup()%>%
  mutate(abstand=abs(MW-hh_nw_charge_wmean))

lm_model_b141 <- lm (abstand ~ tennet + hertz + trans, data = abstand)
summary(lm_model_b141)
lm_model_b142 <- lm (abstand ~ year + tennet + hertz + trans, data = abstand)
summary(lm_model_b142)
lm_model_b143 <- lm (abstand ~ treat, data = abstand)
summary(lm_model_b143)
lm_model_b144 <- lm (abstand ~  tennet*treat + hertz*treat + trans*treat + year, data = abstand)
summary(lm_model_b144)

regdeviation <- stargazer(lm_model_b141, lm_model_b142, lm_model_b143, lm_model_b144, title="Development deviation", align=TRUE, omit.stat=c("LL","ser","f") ,no.space=TRUE, order=c("Constant"), covariate.labels = c("$Intercept$","$year$", "$Tennet \times treat$", "$50Hertz \times treat$", "$TrannetBW \times treat$", "$Tennet$", "$50Hertz$", "$TransnetBW$", "$treat$"), dep.var.labels = "$deviation$")
writeLines(regdeviation, "regressionstabellen/regdeviation.txt")


#b15) Interne Varianz
invar <- municipal_network_charges %>%
  filter(!is.na(regelgebiet_num))%>%
  filter(year>=2015)%>%
  filter (regelgebiet_num<=4)%>%
  group_by(year, regelgebiet_num) %>%
  summarize(Varianzhh = var(hh_nw_charge_wmean_real, na.rm = TRUE))%>%
  mutate(year = case_when(
    year == 2015 ~ 0,
    year == 2016 ~ 1,
    year == 2017 ~ 2,
    year == 2018 ~ 3,
    year == 2019 ~ 4,
    year == 2020 ~ 5,
    year == 2021 ~ 6,
    year == 2020 ~ 7))

regression_data_invar <- regression_data %>%
  left_join(invar, by = c("year", "regelgebiet_num"))
lm_model_b15 <- lm(Varianzhh ~   year*treat, data = regression_data_invar)
summary(lm_model_b15)

#b16)Regression: Breitengrad
m1 <- lm (hh_nw_charge_wmean_real ~ LAT_DEZ*year, data = regression_data)
summary(m1)
m1f <- lm(hh_nw_charge_wmean_real ~ factor(regelgebiet_num) + LAT_DEZ * year, data = regression_data)
summary(m1)
m3 <- lm (hh_nw_charge_wmean_real ~  LAT_DEZ*treat + year, data = regression_data)
summary(m3)
m3f <- lm (hh_nw_charge_wmean_real ~  factor(regelgebiet_num) + LAT_DEZ*treat + year, data = regression_data)
summary(m3f)

regression_latitude <- stargazer(m1, m1f, m3, m3f, title="Results North-South Gradient", align=TRUE, omit.stat=c("LL","ser","f"), order=c("Constant", "LAT_DEZ"), dep.var.labels = "$charge_{hh,real}$", add.lines=list(c('Fixed effects','No', 'Yes','No', 'Yes')), omit = c("factor"),  covariate.labels = c("$Intercept$", "$latitude$", "$latitude \times year$", "$latitude \times treat$", "$treat$", "$year$"))
writeLines(regression_latitude, "regressionstabellen/regression_latitude.txt")

###########################
#b2) Durchführen in vier Subgruppen (je Regelgebiet)
regression_tennet <- regression_data %>%
  filter(regelgebiet_num == 1)
regression_trans <- regression_data %>%
  filter(regelgebiet_num == 4)
regression_amprion <- regression_data %>%
  filter(regelgebiet_num == 2)
regression_hertz <- regression_data %>%
  filter(regelgebiet_num == 3) 

hertz1 <- lm (hh_nw_charge_wmean_real ~ year, data = regression_hertz)
summary(hertz1)
hertz2 <- lm (hh_nw_charge_wmean_real ~ year + treat + year:treat, data = regression_hertz)
summary(hertz2)
hertz3 <- lm (hh_nw_charge_wmean_real ~ year + treat + year:treat + trans_charge_wmean_real*year, data = regression_hertz)
summary(hertz3)
hertz4 <- lm (hh_nw_charge_wmean_real ~ year + year2019 + year2020 + year2021 + year2022, data = regression_hertz)
summary(hertz4)
hertz <- stargazer(hertz1, hertz2, hertz4, no.space=TRUE, title="Results 50Hertz", align=TRUE, omit.stat=c("LL","ser","f"), order=c("Constant", "Year", "Treat", "Year:Treat", "Year2019", "Year2020", "Year2021", "Year2022"), covariate.labels = c("$Intercept$","$year$", "$treat$", "$year \times treat$", "$year_{2019}$", "$year_{2020}$", "$year_{2021}$", "$year_{2022}$"), dep.var.labels = "$charge_{hh,real}$")

                   
tennet1 <- lm (hh_nw_charge_wmean_real ~ year, data = regression_tennet)
summary(tennet1)
tennet2 <- lm (hh_nw_charge_wmean_real ~ year + treat + year:treat, data = regression_tennet)
summary(tennet2)
tennet3 <- lm (hh_nw_charge_wmean_real ~ year + treat + year:treat + trans_charge_wmean_real*year, data = regression_tennet)
summary(tennet3)
tennet4 <- lm (hh_nw_charge_wmean_real ~ year + year2019 + year2020 + year2021 + year2022, data = regression_tennet)
summary(tennet4)
tennet <- stargazer(tennet1, tennet2, tennet4, no.space=TRUE, title="Results Tennet", align=TRUE, omit.stat=c("LL","ser","f"), order=c("Constant", "Year", "Treat", "Year:Treat", "Year2019", "Year2020", "Year2021", "Year2022"), covariate.labels = c("$Intercept$","$year$", "$treat$", "$year \times treat$", "$year_{2019}$", "$year_{2020}$", "$year_{2021}$", "$year_{2022}$"), dep.var.labels = "$charge_{hh,real}$")

amprion1 <- lm (hh_nw_charge_wmean_real ~ year, data = regression_amprion)
summary(amprion1)
amprion2 <- lm (hh_nw_charge_wmean_real ~ year + treat + year:treat, data = regression_amprion)
summary(amprion2)
amprion3 <- lm (hh_nw_charge_wmean_real ~ year + treat + year:treat + trans_charge_wmean_real*year, data = regression_amprion)
summary(amprion3)
amprion4 <- lm (hh_nw_charge_wmean_real ~ year + year2019 + year2020 + year2021 + year2022, data = regression_amprion)
summary(amprion4)
amprion <- stargazer(amprion1, amprion2, amprion4, no.space=TRUE, omit.stat=c("LL","ser","f"), title="Results Amprion", align=TRUE, order=c("Constant", "Year", "Treat", "Year:Treat", "Year2019", "Year2020", "Year2021", "Year2022"), covariate.labels = c("$Intercept$","$year$", "$treat$", "$year \times treat$", "$year_{2019}$", "$year_{2020}$", "$year_{2021}$", "$year_{2022}$"), dep.var.labels = "$charge_{hh,real}$")

trans1 <- lm (hh_nw_charge_wmean_real ~ year, data = regression_trans)
summary(trans1)
trans2 <- lm (hh_nw_charge_wmean_real ~ year + treat + year:treat, data = regression_trans)
summary(trans2)
trans3 <- lm (hh_nw_charge_wmean_real ~ year + treat + year:treat + trans_charge_wmean_real*year, data = regression_trans)
summary(trans3)
trans4 <- lm (hh_nw_charge_wmean_real ~ year + year2019 + year2020 + year2021 + year2022, data = regression_trans)
summary(trans4)
trans <- stargazer(trans1, trans2, trans4, no.space=TRUE, omit.stat=c("LL","ser","f"), title="Results TransnetBW", align=TRUE, order=c("Constant", "Year", "Treat", "Year:Treat", "Year2019", "Year2020", "Year2021", "Year2022"), covariate.labels = c("$Intercept$","$year$", "$treat$", "$year \times treat$", "$year_{2019}$", "$year_{2020}$", "$year_{2021}$", "$year_{2022}$"), dep.var.labels = "$charge_{hh,real}$")

writeLines(c(tennet, hertz, trans, amprion), "regressionstabellen/subsamples.txt")

#2x2 Darstellung
sinkend <- stargazer(hertz1, hertz2, hertz4, tennet1, tennet2, tennet4, no.space=TRUE, omit.stat=c("LL","ser","f"), title="Results Subsamples 1", align=TRUE, order=c("Constant", "Year", "Treat", "Year:Treat", "Year2019", "Year2020", "Year2021", "Year2022"), covariate.labels = c("$Intercept$","$year$", "$treat$", "$year \times treat$", "$year_{2019}$", "$year_{2020}$", "$year_{2021}$", "$year_{2022}$"), dep.var.labels = "$charge_{hh,real}$",   column.labels   = c("50Hertz", "50Hertz", "50Hertz", "Tennet", "Tennet", "Tennet"),column.separate = c(1,1,1,1,1,1))
steigend <- stargazer(amprion1, amprion2, amprion4, trans1, trans2, trans4, no.space=TRUE, omit.stat=c("LL","ser","f"), title="Results Subsamples 2", align=TRUE, order=c("Constant", "Year", "Treat", "Year:Treat", "Year2019", "Year2020", "Year2021", "Year2022"), covariate.labels = c("$Intercept$","$year$", "$treat$", "$year \times treat$", "$year_{2019}$", "$year_{2020}$", "$year_{2021}$", "$year_{2022}$"), dep.var.labels = "$charge_{hh,real}$",   column.labels   = c("Amprion", "Amprion", "Amprion", "TransnetBW", "TransnetBW", "TransnetBW"),column.separate = c(1,1,1,1,1,1))
writeLines(c(sinkend, steigend), "regressionstabellen/Subsamples.tex")


#b3) Regression: Geteilte Datensätze Einwohner und Gewerbesteuer
regression_data <- regression_data[order(regression_data$population), ]
midpoint <- nrow(regression_data) / 2
first_half_population <- regression_data[1:midpoint, ]
second_half_population <- regression_data[(midpoint + 1):nrow(regression_data), ]
regression_data <- regression_data[order(regression_data$btax_multi), ]
midpoint <- nrow(regression_data) / 2
first_half_tax <- regression_data[1:midpoint, ]
second_half_tax <- regression_data[(midpoint + 1):nrow(regression_data), ]

lm_model_hh_population_1 <- lm (hh_nw_charge_wmean_real ~ tennet*year + hertz*year + trans*year, data = first_half)
summary(lm_model_hh_population_1)
lm_model_hh_population_2 <- lm (hh_nw_charge_wmean_real ~ tennet*year + hertz*year + trans*year, data = second_half)
summary(lm_model_hh_population_2)
lm_model_hh_tax_1 <- lm (hh_nw_charge_wmean_real ~ tennet*year + hertz*year + trans*year, data = first_half_tax)
summary(lm_model_hh_tax_1)
lm_model_hh_tax_2 <- lm (hh_nw_charge_wmean_real ~ tennet*year + hertz*year + trans*year, data = second_half_tax)
summary(lm_model_hh_tax_2)


#c) Test auf Korrelation
correlation_data <- na.omit(regression_data[, c("qkm_entnahmestellen_ns", "qkm_arbeit_ns", "qkm_stromkreislänge_ns", "population")])
cor(correlation_data$qkm_entnahmestellen_ns, correlation_data$qkm_arbeit_ns)
cor(correlation_data$qkm_entnahmestellen_ns, correlation_data$qkm_stromkreislänge_ns)
cor(correlation_data$qkm_arbeit_ns, correlation_data$qkm_stromkreislänge_ns)
cor(correlation_data$population, correlation_data$qkm_stromkreislänge_ns)
cor(correlation_data$population, correlation_data$qkm_arbeit_ns)
cor(correlation_data$population, correlation_data$qkm_entnahmestellen_ns)

#d) Test auf OLS Annahmen
##############################################################
#Test auf Heteroskedastie
# Step 1: Sch?tzen des ?konometrischen Modells
bp_regression  <- lm(hh_nw_charge_wmean ~ regelgebiet_num, data = regression_data)

# Step 2: Berechnung Rediduen und quadriete residuen
bp_residuals2  <- bp_regression$residuals^2
par(mfrow = c(1,2))
plot(regression_data$hh_nw_charge_wmean, bp_regression$residuals)

# Step 3: Sch?tzung der Hilfsregression
databp <- cbind(regression_data, bp_residuals2)
bp_results <- lm(bp_residuals2 ~ hh_nw_charge_wmean, data = databp)

# Step 4: F-Statistik
# H0 = Homoskedastie
# Alternativ-Hypothese: Heteroskedastie
summary(bp_results)
summary(bp_results)[10]
# H0 verwerfen --> keine Heteroskedastie
1/(1 - summary(lm_model_b3)$r.squared)
##############################################################
#Gibt es signifikante Unterschiede zwischen den vier Regionen?

#Test, ob Unterschiede beim Netzentgelt zwischen den vier Regionen bestehen (Anova Test)
summary(aov(regression_data$hh_nw_charge_wmean ~ regression_data$regelgebiet_name))

#Welchs Anova Test
oneway.test(hh_nw_charge_wmean ~ regelgebiet_name, data = regression_data, var.equal = FALSE)

#Levene Test auf Gleichheit der Varianz
regression_data$regelgebiet_name <- as.factor(regression_data$regelgebiet_name)
leveneTest(hh_nw_charge_wmean ~ regelgebiet_name, regression_data)

#Regressionsanalyse
summary (lm (population ~ year*tennet + year*amprion + year*hertz + year*trans, data = regression_data))
summary (lm (qkm_entnahmestellen_ns ~ year*tennet + year*amprion + year*hertz + year*trans, data = regression_data))
summary (lm (qkm_arbeit_ns ~ year*tennet + year*amprion + year*hertz + year*trans, data = regression_data))
summary (lm (qkm_stromkreislänge_ns ~ year*tennet + year*amprion + year*hertz + year*trans, data = regression_data))
summary (lm (qkm_entnahmestellen_ms ~ year*tennet + year*amprion + year*hertz + year*trans, data = regression_data))
summary (lm (qkm_arbeit_ms ~ year*tennet + year*amprion + year*hertz + year*trans, data = regression_data))
###################################

#6a) descritive statistics
descriptive_statistics <- regression_data %>%
  filter(!is.na(hh_nw_charge_wmean_real))%>%
  filter(!is.na(ind_nw_charge_wmean_real))%>%
  dplyr::select (year, tennet, amprion, hertz, trans, treat, LAT_DEZ, population, hh_nw_charge_wmean_real, hh_nw_charge_wmean, ind_nw_charge_wmean_real, ind_nw_charge_wmean, trans_charge_wmean_real, trans_charge_wmean, AGS)
                
table <- stargazer(as.data.frame(descriptive_statistics))
writeLines(table, "regressionstabellen/statistics.txt")

#6b) Characteristics
characteristics <- regression_data %>%
  filter(year==5)%>%
  group_by(regelgebiet_num)%>%
  filter (!traffic==2222222222.0)%>%
  dplyr::select(population, qkm_entnahmestellen_ns, qkm_arbeit_ns, qkm_stromkreislänge_ns, qkm_entnahmestellen_ms, qkm_arbeit_ms, qkm_stromkreislaenge_ms, traffic, agriculture)%>%
  summarise (population = mean(population, na.rm=TRUE), qkm_entnahmestellen_ns = mean(qkm_entnahmestellen_ns, na.rm=TRUE), qkm_arbeit_ns =mean(qkm_arbeit_ns, na.rm=TRUE), qkm_stromkreislänge_ns=mean(qkm_stromkreislänge_ns, na.rm=TRUE), qkm_entnahmestellen_ms = mean(qkm_entnahmestellen_ms, na.rm=TRUE), qkm_arbeit_ms = mean(qkm_arbeit_ms, na.rm=TRUE), qkm_stromkreislaenge_ms = mean(qkm_stromkreislaenge_ms, na.rm=TRUE), traffic = mean(traffic, na.rm=TRUE), agriculture = mean(agriculture, na.rm=TRUE))
characteristics <- t(characteristics)
print(xtable(characteristics, type = "latex"), file = "regressionstabellen/characteristics.tex")



################################################
#Energydata
library(readr)

combined_data <- data.frame()
file_names <- c("data/energy/Community_level_energy_(1).csv", "data/energy/Community_level_energy_(2).csv","data/energy/Community_level_energy_(3).csv","data/energy/Community_level_energy_(4).csv","data/energy/Community_level_energy_(5).csv","data/energy/Community_level_energy_(6).csv","data/energy/Community_level_energy_(7).csv","data/energy/Community_level_energy_(8).csv","data/energy/Community_level_energy_(9).csv","data/energy/Community_level_energy_(10).csv","data/energy/Community_level_energy_(11).csv","data/energy/Community_level_energy_(12).csv")
for (file in file_names) {
  # Lese die CSV-Datei ein und entferne doppelte Anführungszeichen
  data <- read_delim(file, delim = ";", escape_double = FALSE, trim_ws = TRUE)
  # Fügen Sie die importierten Daten zum kombinierten Datenrahmen hinzu
  combined_data <- bind_rows(combined_data, data)
}

energiedata <- municipal_network_charges %>%
  filter(year==2014)%>%
  rename(GEN=muni_name)%>%
  dplyr::select(AGS,GEN,hh_nw_charge_wmean,regelgebiet_num)%>%
  right_join(combined_data, by="GEN")%>%
  filter(!is.na(CEM))%>%
  filter(!is.na(hh_nw_charge_wmean))%>%
  filter(regelgebiet_num<=4)%>%
  mutate(tennet = ifelse(regelgebiet_num == 1, 1, 0))%>%
  mutate(amprion = ifelse(regelgebiet_num == 2, 1, 0))%>%
  mutate(hertz = ifelse(regelgebiet_num == 3, 1, 0))%>%
  mutate(trans = ifelse(regelgebiet_num == 4, 1, 0))

cor(energiedata$hh_nw_charge_wmean,energiedata$SIF)
cor(energiedata$hh_nw_charge_wmean,energiedata$CEM)

characteristics_energy <- energiedata %>%
  group_by(regelgebiet_num)%>%
  summarise (CEM = mean(CEM, na.rm=TRUE),SIF = mean(SIF, na.rm=TRUE))
characteristics_energy <- t(characteristics_energy)
print(xtable(characteristics_energy, type = "latex"), file = "characteristics_energy.tex")

