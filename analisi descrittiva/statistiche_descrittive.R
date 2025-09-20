rm(list=ls())
library(rstudioapi)
current_path<-getActiveDocumentContext()$path
setwd(dirname(current_path))

## 2018
elezioni_camera_2018 <- read.csv("~/Università DSEI/ANNO 2/Tesi/analisi/analisi descrittiva/elezioni_camera_2018.csv")

# Statistiche Descrittive per Zona Geografica
voti_zone2018 = aggregate.data.frame(elezioni_camera_2018[14:21], list(Zone = elezioni_camera_2018$ZONA_GEOGRAFICA), sum)
#write.csv(voti_zone2018, 'descrittive2018.csv')

## 2022
elezioni_camera_2022 <- read.csv("~/Università DSEI/ANNO 2/Tesi/analisi/analisi descrittiva/elezioni_camera_2022.csv")

# Statistiche Descrittive per Zona Geografica
voti_zone2022 = aggregate.data.frame(elezioni_camera_2022[14:22], list(Zone = elezioni_camera_2022$ZONA_GEOGRAFICA), sum)
#write.csv(voti_zone2022, 'descrittive2022.csv')

voti_zone2022 = aggregate.data.frame(elezioni_camera_2022[14:22], list(Zone = elezioni_camera_2022$REGIONE), sum)
votiSCN = aggregate.data.frame(df_full_ok2022[32], list(Zone = df_full_ok2022$CIRCOSCRIZIONE), sum)

# Come andiamo con gli astenuti? (2018)
elezioni_camera_2018$SCHEDE_NULLE = elezioni_camera_2018$VOTANTI - elezioni_camera_2018$TOTALE - elezioni_camera_2018$SCHEDE_BIANCHE
elezioni_camera_2018$NON_VOTO = elezioni_camera_2018$ELETTORI - elezioni_camera_2018$VOTANTI + elezioni_camera_2018$SCHEDE_BIANCHE + elezioni_camera_2018$SCHEDE_NULLE
non_voti_zone2018 = aggregate.data.frame(elezioni_camera_2018[c(9,24)], list(Zone = elezioni_camera_2018$ZONA_GEOGRAFICA), sum)

# Come andiamo con gli astenuti? (2022)
elezioni_camera_2022$SCHEDE_NULLE = elezioni_camera_2022$VOTANTI - elezioni_camera_2022$TOTALE - elezioni_camera_2022$SCHEDE_BIANCHE
elezioni_camera_2022$NON_VOTO = elezioni_camera_2022$ELETTORI - elezioni_camera_2022$VOTANTI + elezioni_camera_2022$SCHEDE_BIANCHE + elezioni_camera_2022$SCHEDE_NULLE
non_voti_zone2022 = aggregate.data.frame(elezioni_camera_2022[c(9,24)], list(Zone = elezioni_camera_2022$ZONA_GEOGRAFICA), sum)

