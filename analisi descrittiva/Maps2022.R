rm(list=ls())

library(rstudioapi)
library(tidyverse)
library(ggthemes)
library(sf)
library(giscoR)


# seleziono working directory
current_path<-getActiveDocumentContext()$path
setwd(dirname(current_path))
print(current_path)

# importo dataset 2022
elezioni_camera_2022 = read.csv("~/Università DSEI/ANNO 2/Tesi/analisi/dataset/elezioni_camera_2022.csv")

# aggiungo colonne con le proporzioni dei voti di ogni classe sul totale
elezioni_camera_2022$LEGA_pct = elezioni_camera_2022$LEGA / elezioni_camera_2022$TOTALE
elezioni_camera_2022$FDI_pct = elezioni_camera_2022$FDI / elezioni_camera_2022$TOTALE
elezioni_camera_2022$FI_pct = elezioni_camera_2022$FI / elezioni_camera_2022$TOTALE
elezioni_camera_2022$PD_pct = elezioni_camera_2022$PD / elezioni_camera_2022$TOTALE
elezioni_camera_2022$M5S_pct = elezioni_camera_2022$M5S / elezioni_camera_2022$TOTALE
elezioni_camera_2022$AZIV_pct = elezioni_camera_2022$AZIV / elezioni_camera_2022$TOTALE
elezioni_camera_2022$LOCALI_pct = elezioni_camera_2022$LOCALI / elezioni_camera_2022$TOTALE
elezioni_camera_2022$ALTRI_pct = elezioni_camera_2022$ALTRI / elezioni_camera_2022$TOTALE

# aggiungo colonna con nome della lista che ha ricevuto più voti
elezioni_camera_2022$VINCITORE = names(elezioni_camera_2022)[c(14:21)][max.col(elezioni_camera_2022[c(14:21)])]

# importo le coordinate geografiche per le mappe
ita = read_sf("~/Università DSEI/ANNO 2/Tesi/dati/Limiti01012022_g/Limiti01012022_g/Com01012022_g/Com01012022_g_WGS84.shp")

colnames(ita)[6] = "CODICE_ISTAT"
colnames(ita)[8] = "COMUNE"

# join per dataframe temporaneo con le coordinate geografiche
temp = left_join(elezioni_camera_2022, ita, by = "CODICE_ISTAT")

# importo i confini delle regioni italiane e trasformo le coordinate nel sistema di riferimento delle coordinate europee (crs = 3035)
nuts2 = gisco_get_nuts(year = 2021, resolution = 20, country = c("Italy"), nuts_level = 2) %>%
  select(NUTS_ID, NAME_LATN, geometry) 
nuts2_3035 = st_transform(nuts2, 3035)

# importo anche i paesi non italiani per il background della mappa
backg = gisco_get_countries(year = 2020,resolution = 20) %>%
  st_transform(3035)

# trasformo il dataset in sf e poi converto le coordinate originali in coordinate europee (crs = 3035)
data_sf = temp %>% st_as_sf()
data_map = st_transform(data_sf, 3035) 
data_map = data_map %>% mutate(VINCITORE = ifelse(REGIONE == "Valle d'Aosta/Valle d'Aoste", NA, VINCITORE))
data_map$VINCITORE = data_map$VINCITORE %>% as.factor()


# COLORI
FDIcol = '#0033cc'
FIcol = '#99cc00'
LEGAcol = '#339966'
PDcol = '#cc0099'
LOCALIcol = '#3399FF'
ALTRIcol = '#696969'
M5Scol = '#ff9900'
AZIVcol = '#990000'

# TEMA MAPPA
tema = theme(legend.position = "bottom",
             legend.title = element_text(size = 12, face = "bold"),
             legend.background = element_rect(fill = "white", colour = "white"),
             legend.key.size = unit(0.6, units = "cm"),
             legend.box.background = element_rect(colour = "black", size = 0.5),
             panel.border = element_rect(colour = "black", fill=NA),
             legend.key=element_rect(colour="black"),
             legend.text = element_text(size = 8),
             legend.spacing.y = unit(0.2, 'lines'))

tema_v2 = theme(panel.border = element_rect(colour = "black", fill=NA),
                legend.position="none")


# MAPPA PER FDI
ggplot() + 
  geom_sf(data = backg, fill = "#F5F5F5", color = NA)  +
  xlim(c(4050000, 5100000)) + ylim(c(1500000, 2650000)) +
  geom_sf(data = data_map, aes(fill = cut(FDI_pct, breaks = seq(0, 0.7, by = 0.10))), color = NA) +
  scale_fill_brewer(type = "qual", palette = "Purples", na.value = "#F5F5F5", labels = c("0 - 10%", "10 - 20%", "20 - 30%", "30 - 40%", "40 - 50%", "50 - 60%", "> 60%", "NA")) +
  #scale_fill_gradient(low = "#ffffff", high = FDIcol, na.value = "#ffffff", limits = c(0,100)) +
  geom_sf(data = nuts2_3035, fill = NA, colour = '#D3D3D3') +
  theme_void()  +
  #labs(title = "Proporzione Voti Fratelli d'Italia per comune (2022)",
  #     caption = "Fonte: Eligendo") +
       #fill = "Voti FDI (%)") +
  guides(fill = guide_legend(byrow = TRUE)) +
  theme(legend.title = element_blank(), legend.key.size = unit(1, units = "cm")) 
  tema_v2

# MAPPA PER FI
ggplot() + 
  geom_sf(data = backg, fill = "#F5F5F5", color = NA)  +
  xlim(c(4050000, 5100000)) + ylim(c(1500000, 2650000)) +
  geom_sf(data = data_map, aes(fill = cut(FI_pct, breaks = seq(0, 0.7, by = 0.10))), color = NA) +
  scale_fill_brewer(type = "qual", palette = "Purples", na.value = "#F5F5F5", labels = c("0 - 10%", "10 - 20%", "20 - 30%", "30 - 40%", "40 - 50%", "50 - 60%", "> 60%", "NA")) +
  #scale_fill_gradient(low = "#ffffff", high = FDIcol, na.value = "#ffffff", limits = c(0,100)) +
  geom_sf(data = nuts2_3035, fill = NA, colour = '#D3D3D3') +
  theme_void()  +
  #labs(title = "Proporzione Voti Forza Italia per comune (2022)",
  #     caption = "Fonte: Eligendo") +
  guides(fill = guide_legend(byrow = TRUE)) +
  theme(legend.title = element_blank(), legend.key.size = unit(1, units = "cm")) 
  tema_v2

# MAPPA PER LEGA
ggplot() + 
  geom_sf(data = backg, fill = "#F5F5F5", color = NA)  +
  xlim(c(4050000, 5100000)) + ylim(c(1500000, 2650000)) +
  geom_sf(data = data_map, aes(fill = cut(LEGA_pct, breaks = seq(0, 0.7, by = 0.10))), color = NA) +
  scale_fill_brewer(type = "qual", palette = "Purples", na.value = "#F5F5F5", labels = c("0 - 10%", "10 - 20%", "20 - 30%", "30 - 40%", "40 - 50%", "50 - 60%", "> 60%", "NA")) +
  #scale_fill_gradient(low = "#ffffff", high = FDIcol, na.value = "#ffffff", limits = c(0,100)) +
  geom_sf(data = nuts2_3035, fill = NA, colour = '#D3D3D3') +
  theme_void()  +
  #labs(title = "Proporzione Voti Lega per comune (2022)",
   #    caption = "Fonte: Eligendo") +
  guides(fill = guide_legend(byrow = TRUE)) +
  theme(legend.title = element_blank(), legend.key.size = unit(1, units = "cm")) 
  tema_v2

# MAPPA PER PD
ggplot() + 
  geom_sf(data = backg, fill = "#F5F5F5", color = NA)  +
  xlim(c(4050000, 5100000)) + ylim(c(1500000, 2650000)) +
  geom_sf(data = data_map, aes(fill = cut(PD_pct, breaks = seq(0, 0.7, by = 0.10))), color = NA) +
  scale_fill_brewer(type = "qual", palette = "Blues", na.value = "#F5F5F5", labels = c("0 - 10%", "10 - 20%", "20 - 30%", "30 - 40%", "40 - 50%", "50 - 60%", "> 60%", "NA")) +
  #scale_fill_gradient(low = "#ffffff", high = FDIcol, na.value = "#ffffff", limits = c(0,100)) +
  geom_sf(data = nuts2_3035, fill = NA, colour = '#D3D3D3') +
  theme_void()  +
  #labs(title = "Proporzione Voti PD per comune (2022)",
  #     caption = "Fonte: Eligendo") +
  guides(fill = guide_legend(byrow = TRUE)) +
  theme(legend.title = element_blank(), legend.key.size = unit(1, units = "cm")) 
  tema_v2

# MAPPA PER M5S
ggplot() + 
  geom_sf(data = backg, fill = "#F5F5F5", color = NA)  +
  xlim(c(4050000, 5100000)) + ylim(c(1500000, 2650000)) +
  geom_sf(data = data_map, aes(fill = cut(M5S_pct, breaks = seq(0, 0.7, by = 0.10))), color = NA) +
  scale_fill_brewer(type = "qual", palette = "Oranges", na.value = "#F5F5F5", labels = c("0 - 10%", "10 - 20%", "20 - 30%", "30 - 40%", "40 - 50%", "50 - 60%", "> 60%", "NA")) +
  #scale_fill_gradient(low = "#ffffff", high = FDIcol, na.value = "#ffffff", limits = c(0,100)) +
  geom_sf(data = nuts2_3035, fill = NA, colour = '#D3D3D3') +
  theme_void()  +
  #labs(title = "Proporzione Voti M5S per comune (2022)",
  #     caption = "Fonte: Eligendo") +
  guides(fill = guide_legend(byrow = TRUE)) +
  theme(legend.title = element_blank(), legend.key.size = unit(1, units = "cm")) 
  tema_v2

# MAPPA PER LOCALI
ggplot() + 
  geom_sf(data = backg, fill = "#F5F5F5", color = NA)  +
  xlim(c(4050000, 5100000)) + ylim(c(1500000, 2650000)) +
  geom_sf(data = data_map, aes(fill = cut(LOCALI_pct, breaks = seq(0, 0.7, by = 0.10))), color = NA) +
  scale_fill_brewer(type = "qual", palette = "Greens", na.value = "#F5F5F5", labels = c("0 - 10%", "10 - 20%", "20 - 30%", "30 - 40%", "40 - 50%", "50 - 60%", "> 60%", "NA")) +
  #scale_fill_gradient(low = "#ffffff", high = FDIcol, na.value = "#ffffff", limits = c(0,100)) +
  geom_sf(data = nuts2_3035, fill = NA, colour = '#D3D3D3') +
  theme_void()  +
  #labs(title = "Proporzione Voti Locali per comune (2022)",
  #     caption = "Fonte: Eligendo") +
  guides(fill = guide_legend(byrow = TRUE)) +
  theme(legend.title = element_blank(), legend.key.size = unit(1, units = "cm")) 
  tema_v2

# MAPPA PER ALTRI
ggplot() + 
  geom_sf(data = backg, fill = "#F5F5F5", color = NA)  +
  xlim(c(4050000, 5100000)) + ylim(c(1500000, 2650000)) +
  geom_sf(data = data_map, aes(fill = cut(ALTRI_pct, breaks = seq(0, 0.7, by = 0.10))), color = NA) +
  scale_fill_brewer(type = "qual", palette = "Greens", na.value = "#F5F5F5", labels = c("0 - 10%", "10 - 20%", "20 - 30%", "30 - 40%", "40 - 50%", "50 - 60%", "> 60%", "NA")) +
  #scale_fill_gradient(low = "#ffffff", high = FDIcol, na.value = "#ffffff", limits = c(0,100)) +
  geom_sf(data = nuts2_3035, fill = NA, colour = '#D3D3D3') +
  theme_void()  +
  #labs(title = "Proporzione Voti Altri partiti per comune (2022)",
  #     caption = "Fonte: Eligendo") +
  guides(fill = guide_legend(byrow = TRUE)) +
  theme(legend.title = element_blank(), legend.key.size = unit(1, units = "cm")) 
  tema_v2

# MAPPA PER AZIONE-IV
ggplot() + 
  geom_sf(data = backg, fill = "#F5F5F5", color = NA)  +
  xlim(c(4050000, 5100000)) + ylim(c(1500000, 2650000)) +
  geom_sf(data = data_map, aes(fill = cut(AZIV_pct, breaks = seq(0, 0.7, by = 0.10))), color = NA) +
  scale_fill_brewer(type = "qual", palette = "Reds", na.value = "#F5F5F5", labels = c("0 - 10%", "10 - 20%", "20 - 30%", "30 - 40%", "40 - 50%", "> 50%", "NA")) +
  #scale_fill_gradient(low = "#ffffff", high = FDIcol, na.value = "#ffffff", limits = c(0,100)) +
  geom_sf(data = nuts2_3035, fill = NA, colour = '#D3D3D3') +
  theme_void()  +
  #labs(title = "Proporzione Voti Azione - IV per comune (2022)",
  #     caption = "Fonte: Eligendo") +
  guides(fill = guide_legend(byrow = TRUE)) +
  theme(legend.title = element_blank(), legend.key.size = unit(1, units = "cm")) 
  tema_v2

# MAPPA PER VINCITORI
ggplot() + 
  geom_sf(data = backg, fill = "#F5F5F5", color = NA)  +
  xlim(c(4050000, 5100000)) + ylim(c(1500000, 2650000)) +
  geom_sf(data = data_map, aes(fill = VINCITORE), color = NA) +
  scale_fill_manual(name = "      Classi      ",
                    values = c("FDI" = FDIcol,
                                "FI" = FIcol,
                                "LEGA" = LEGAcol,
                                "PD" = PDcol,
                                "M5S" = M5Scol,
                                "AZIV" = AZIVcol,
                                "LOCALI" = LOCALIcol,
                                "ALTRI" = ALTRIcol),
                     na.value = '#F5F5F5') +
  geom_sf(data = nuts2_3035, fill = NA, colour = '#D3D3D3') +
  theme_void()  +
  #labs(title = "Partiti con più voti per comune (2022)",
  #     caption = "Fonte: Eligendo") +
  guides(fill = guide_legend(byrow = TRUE)) +
  tema_v2


