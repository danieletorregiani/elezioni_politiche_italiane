rm(list=ls())

library(rstudioapi)
library(tidyverse)
library(ggthemes)
library(sf)
library(giscoR)
#library(scales)
#library(ggcorrplot)
#library(GGally)
#library(statsr)
#library(Hmisc)
#library(stringi)
#library(Rmisc)
#library(ggpubr)


# seleziono working directory
current_path<-getActiveDocumentContext()$path
setwd(dirname(current_path))
print(current_path)

# importo dataset 2018
elezioni_camera_2018 = read.csv("~/Università DSEI/ANNO 2/Tesi/analisi/dataset/elezioni_camera_2018.csv")

# aggiungo colonne con le proporzioni dei voti di ogni classe sul totale
elezioni_camera_2018$LEGA_pct = elezioni_camera_2018$LEGA / elezioni_camera_2018$TOTALE
elezioni_camera_2018$FDI_pct = elezioni_camera_2018$FDI / elezioni_camera_2018$TOTALE
elezioni_camera_2018$FI_pct = elezioni_camera_2018$FI / elezioni_camera_2018$TOTALE
elezioni_camera_2018$PD_pct = elezioni_camera_2018$PD / elezioni_camera_2018$TOTALE
elezioni_camera_2018$M5S_pct = elezioni_camera_2018$M5S / elezioni_camera_2018$TOTALE
elezioni_camera_2018$LOCALI_pct = elezioni_camera_2018$LOCALI / elezioni_camera_2018$TOTALE
elezioni_camera_2018$ALTRI_pct = elezioni_camera_2018$ALTRI / elezioni_camera_2018$TOTALE

# aggiungo colonna con nome della lista che ha ricevuto più voti
elezioni_camera_2018$VINCITORE = names(elezioni_camera_2018)[c(14:20)][max.col(elezioni_camera_2018[c(14:20)])]
elezioni_camera_2018 = elezioni_camera_2018 %>% mutate(VINCITORE = ifelse(CIRCOSCRIZIONE == "AOSTA", NA, VINCITORE))
elezioni_camera_2018$VINCITORE = elezioni_camera_2018$VINCITORE %>% as.factor()
# transform variables in factors
#data$status = factor(data$status, labels = c("other", "deficit", "default"))
#data$gender = factor(data$gender, labels = c("F", "M"))
#data$degree = factor(data$degree, labels = c("no degree", "elementary", "middle", "high school", "university", "master"))

# importo le coordinate geografiche per le mappe
ita = read_sf("~/Università DSEI/ANNO 2/Tesi/dati/Limiti01012018_g/Limiti01012018_g/Com01012018_g/Com01012018_g_WGS84.shp")

colnames(ita)[7] = "CODICE_ISTAT"
colnames(ita)[9] = "COMUNE"

# join per dataframe temporaneo con le coordinate geografiche
temp = left_join(elezioni_camera_2018, ita, by = "CODICE_ISTAT")

# importo i confini delle regioni italiane e trasformo le coordinate nel sistema di riferimento delle coordinate europee (crs = 3035)
nuts2 = gisco_get_nuts(year = 2016, resolution = 20, country = c("Italy"), nuts_level = 2) %>%
  select(NUTS_ID, NAME_LATN, geometry) 
nuts2_3035 = st_transform(nuts2, 3035)

# importo anche i paesi non italiani per il background della mappa
backg = gisco_get_countries(year = 2016,resolution = 20) %>%
  st_transform(3035)

# trasformo il dataset in sf e poi converto le coordinate originali in coordinate europee (crs = 3035)
data_sf = temp %>% st_as_sf()
data_map = st_transform(data_sf, 3035) 

# COLORI
FDIcol = '#000066'
FIcol = '#99cc00'
LEGAcol = '#339966'
PDcol = '#cc0099'
LOCALIcol = '#9999ff'
ALTRIcol = '#696969'
M5Scol = '#ff9900'

# TEMA MAPPA
tema = theme(legend.position = c(0.85, 0.87),
             legend.title = element_text(size = 12, face = "bold"),
             legend.background = element_rect(fill = "white", colour = "white"),
             legend.key.size = unit(0.6, units = "cm"),
             legend.box.background = element_rect(colour = "black", size = 1),
             panel.border = element_rect(colour = "black", fill=NA),
             legend.key=element_rect(colour="black"),
             legend.text = element_text(size = 10),
             legend.spacing.y = unit(0.2, 'lines'))


# MAPPA PER FDI
ggplot() + 
  geom_sf(data = backg, fill = "#F5F5F5", color = NA)  +
  xlim(c(4050000, 5100000)) + ylim(c(1500000, 2650000)) +
  geom_sf(data = data_map, aes(fill = FDI_pct), color = NA) +
  scale_fill_gradient(low = "#ffffff", high = FDIcol, na.value = "#ffffff") +
  geom_sf(data = nuts2_3035, fill = NA, colour = '#D3D3D3') +
  theme_void()  +
  labs(title = "Voti Fratelli d'Italia (2018)",
       caption = "Fonte: Eligendo") +
  guides(fill = guide_legend(byrow = TRUE)) +
  tema

# MAPPA PER FI
ggplot() + 
  geom_sf(data = backg, fill = "#F5F5F5", color = NA)  +
  xlim(c(4050000, 5100000)) + ylim(c(1500000, 2650000)) +
  geom_sf(data = data_map, aes(fill = FI_pct), color = NA) +
  scale_fill_gradient(low = "#ffffff", high = FIcol, na.value = "#ffffff") +
  geom_sf(data = nuts2_3035, fill = NA, colour = '#D3D3D3') +
  theme_void()  +
  labs(title = "Voti Forza Italia (2018)",
       caption = "Fonte: Eligendo") +
  guides(fill = guide_legend(byrow = TRUE)) +
  tema

# MAPPA PER LEGA
ggplot() + 
  geom_sf(data = backg, fill = "#F5F5F5", color = NA)  +
  xlim(c(4050000, 5100000)) + ylim(c(1500000, 2650000)) +
  geom_sf(data = data_map, aes(fill = LEGA_pct), color = NA) +
  scale_fill_gradient(low = "#ffffff", high = LEGAcol, na.value = "#ffffff") +
  geom_sf(data = nuts2_3035, fill = NA, colour = '#D3D3D3') +
  theme_void()  +
  labs(title = "Voti Lega (2018)",
       caption = "Fonte: Eligendo") +
  guides(fill = guide_legend(byrow = TRUE)) +
  tema

# MAPPA PER PD
ggplot() + 
  geom_sf(data = backg, fill = "#F5F5F5", color = NA)  +
  xlim(c(4050000, 5100000)) + ylim(c(1500000, 2650000)) +
  geom_sf(data = data_map, aes(fill = PD_pct), color = NA) +
  scale_fill_gradient(low = "#ffffff", high = PDcol, na.value = "#ffffff") +
  geom_sf(data = nuts2_3035, fill = NA, colour = '#D3D3D3') +
  theme_void()  +
  labs(title = "Voti PD (2018)",
       caption = "Fonte: Eligendo") +
  guides(fill = guide_legend(byrow = TRUE)) +
  tema

# MAPPA PER M5S
ggplot() + 
  geom_sf(data = backg, fill = "#F5F5F5", color = NA)  +
  xlim(c(4050000, 5100000)) + ylim(c(1500000, 2650000)) +
  geom_sf(data = data_map, aes(fill = M5S_pct), color = NA) +
  scale_fill_gradient(low = "#ffffff", high = M5Scol, na.value = "#ffffff") +
  geom_sf(data = nuts2_3035, fill = NA, colour = '#D3D3D3') +
  theme_void()  +
  labs(title = "Voti M5S (2018)",
       caption = "Fonte: Eligendo") +
  guides(fill = guide_legend(byrow = TRUE)) +
  tema

# MAPPA PER LOCALI
ggplot() + 
  geom_sf(data = backg, fill = "#F5F5F5", color = NA)  +
  xlim(c(4050000, 5100000)) + ylim(c(1500000, 2650000)) +
  geom_sf(data = data_map, aes(fill = LOCALI_pct), color = NA) +
  scale_fill_gradient(low = "#ffffff", high = LOCALIcol, na.value = "#ffffff") +
  geom_sf(data = nuts2_3035, fill = NA, colour = '#D3D3D3') +
  theme_void()  +
  labs(title = "Voti Partiti Locali (2018)",
       caption = "Fonte: Eligendo") +
  guides(fill = guide_legend(byrow = TRUE)) +
  tema

# MAPPA PER ALTRI
ggplot() + 
  geom_sf(data = backg, fill = "#F5F5F5", color = NA)  +
  xlim(c(4050000, 5100000)) + ylim(c(1500000, 2650000)) +
  geom_sf(data = data_map, aes(fill = ALTRI_pct), color = NA) +
  scale_fill_gradient(low = "#ffffff", high = ALTRIcol, na.value = "#ffffff") +
  geom_sf(data = nuts2_3035, fill = NA, colour = '#D3D3D3') +
  theme_void()  +
  labs(title = "Voti Altri Partiti (2018)",
       caption = "Fonte: Eligendo") +
  guides(fill = guide_legend(byrow = TRUE)) +
  tema

# MAPPA PER VINCITORI
ggplot() + 
  geom_sf(data = backg, fill = "#F5F5F5", color = NA)  +
  xlim(c(4050000, 5100000)) + ylim(c(1500000, 2650000)) +
  geom_sf(data = data_map, aes(fill = VINCITORE), color = NA) +
  scale_fill_manual(name = "Partiti con più voti",
                    values = c("FDI" = FDIcol,
                                "FI" = FIcol,
                                "LEGA" = LEGAcol,
                                "PD" = PDcol,
                                "M5S" = M5Scol,
                                "LOCALI" = LOCALIcol,
                                "ALTRI" = ALTRIcol),
                     na.value = 'white') +
  geom_sf(data = nuts2_3035, fill = NA, colour = '#D3D3D3') +
  theme_void()  +
  labs(title = "Partiti con più voti per comune (2018)",
       caption = "Fonte: Eligendo") +
  guides(fill = guide_legend(byrow = TRUE)) +
  tema



# plot relative frequency about gender of the mayor and status of the town, excluding "other"
#gender_prop %>%
#  ggplot(aes(fill = gender)) +
#  geom_col(aes(x= status, y = Freq), position = "dodge") +
#  scale_y_continuous(labels = scales::percent) + 
#  labs(title = "Gender of the mayor and Status of the Municipality",
#       subtitle = "Given 100 male or female mayors, how much administer a default or deficit municipality (%)?",
#       caption = "Source: Ministero dell'Interno") + thm

# dataframe with proportion between degree and status, excluding "other" status to plot 
#degree_prop = data %>% select(degree, status) %>% drop_na(degree) %>% table() %>% prop.table(margin = 1) %>% data.frame() %>% filter(status != "other") %>% print()

# plot relative frequency about degree of the mayor and status of the town, excluding "other"
#degree_prop %>%
#  ggplot(aes(fill = status)) +
#  geom_col(aes(x= degree, y = Freq), position = "dodge") +
#  scale_y_continuous(labels = scales::percent) +
#  labs(title = "Degree of the mayor and Status of the Municipality",
#       subtitle = "Given 100 mayors with a specific degree, how much administer a default or deficit municipality (%)?",
#       caption = "Source: Ministero dell'Interno") + thm

###  MAKE SENSE?
#eta = data %>%
#  drop_na(eta_sindaco) %>%
#  ggplot(aes(x = status, y = eta_sindaco, fill = status)) + 
#  geom_boxplot(aes(fill = status)) +
#  theme(panel.background = element_rect(fill = '#eff4f4'),
#        plot.background = element_rect(fill = "#eff4f4"))
#eta

# conditional mean of citizens given status
#aggregate(data$citizen, list(data$status), FUN = mean, na.rm = TRUE)

# plot conditional mean 
#data %>%
#  ggplot(aes(x = status, y = citizen, fill=status)) +
#  geom_boxplot(outlier.shape = NA)  +
#  coord_cartesian(ylim = quantile(data$media, c(0, 0.97), na.rm = TRUE))+
#  labs(title = "Citizens per town, given its Status",
#       subtitle = "(Plot considers only smaller municipalities, excluding big cities)",
#       caption = "Source: Istat") +
#  thm

# conditional mean of individual income given status
#aggregate(data$individual_income, list(data$status), FUN = mean, na.rm = TRUE)

# compare 4 relevant financial indicators per status
#str_rig = data %>% drop_na(rigiditÃƒ..della.spesa) %>%
#  ggplot(aes(x=rigiditÃƒ..della.spesa, fill = status)) +
#  geom_density(alpha=0.7) +
#  labs(title = "Degree of Structural Rigidity per Status",
#       subtitle = "Degree of Structural Rigidity = (Personnel Expenses + Loan Repayments) / Current Revenue",
#       caption = "Source: Istat") + thm +
#  theme(plot.subtitle=element_text(size=11))
#str_rig

#dep_extfin = data %>% drop_na(grado.di.dipendenza.da.finanziamento.esterno) %>%
#  ggplot(aes(x=grado.di.dipendenza.da.finanziamento.esterno, fill = status)) +
#  geom_density(alpha=0.7) +
#  labs(title = "Degree of Dependence on External Financing per Status",
#       subtitle = "D. of D. on E. F. = Current Transfers / (Current Revenue + Current Transfers + Non Tributary Revenue)",
#       caption = "Source: Istat") + thm +
#  theme(plot.subtitle=element_text(size=11))
#dep_extfin

#cap_exp = data %>% drop_na(capacitÃƒ..di.spesa) %>%
#  ggplot(aes(x = capacitÃƒ..di.spesa , fill= status)) +
#  geom_density(alpha=0.7) +
#  labs(title = "Capacity of Expense per Status",
#       subtitle = "Capacity of Expense = Payments in Accrual Account / Commitments",
#       caption = "Source: Istat") + thm +
#  theme(plot.subtitle=element_text(size=11))
#cap_exp

#loan_rep = data %>% drop_na(spese.per.rimborso.prestiti.in.relazione.alle.entrate.correnti) %>%
#  ggplot(aes(x=spese.per.rimborso.prestiti.in.relazione.alle.entrate.correnti, fill = status)) +
#  geom_density(alpha=0.7) +
#  labs(title = "Loan Repayments on Current Revenue per Status",
#       subtitle = "L. R. on C. R. = Loan Repayments / (Current Revenue + Current Transfers + Non Tributary Revenue)",
#       caption = "Source: Istat") + thm +
#  theme(plot.subtitle=element_text(size=11))
#loan_rep

#ggarrange(str_rig, dep_extfin, cap_exp, loan_rep, ncol=2, nrow=2, common.legend = TRUE, legend="top")

