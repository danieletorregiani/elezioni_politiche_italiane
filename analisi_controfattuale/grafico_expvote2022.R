rm(list=ls())

library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggthemes)

# Carico dataset con voti attesi
ita2022_expvote <- read.csv("~/Università DSEI/ANNO 2/Tesi/analisi/analisi_controfattuale/ita2022_expvote.csv", sep=",")

# Selezionare e organizzare i dati per plottare gli istogrammi (in formato "lungo")
data_long <- ita2022_expvote %>%
  pivot_longer(everything(), names_to = "E_vote")  

# Calcolare le medie per ciascuna E_vote
media_per_E_vote <- ita2022_expvote %>%
  summarise_all(mean) %>%
  pivot_longer(everything(), names_to = "E_vote", values_to = "Media")

# Aggiungo voti reali
voti_reali <- c(0.026336, 0.027558, 0.17409,0.14135,0.084816,0.063179,0.091317,0.39136)
media_per_E_vote$voti_reali = voti_reali


# Creare il istogramma con ggplot e facet_wrap
#ggplot(data_long, aes(x = value, fill = E_vote)) +
#  geom_histogram(binwidth = 0.02, alpha = 0.7, position = "identity") +  # Istogrammi sovrapposti
#  geom_vline(data = media_per_E_vote, aes(xintercept = Media, color = E_vote), linetype = "dashed", size = 1) +
#  theme_clean() +
#  facet_wrap(~ E_vote, scales = "free") +
#  scale_x_continuous(limits = c(0, 1)) +
#  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = "none")

colors <- c("#696969", "#990000", "#0033cc","#99cc00", "#339966", "#ff9900", "black", "#cc0099")

# Creare il grafico della densità con ggplot e facet_wrap
ggplot(data_long, aes(x = value, fill = E_vote)) +
  geom_density(alpha = 0.4, position = "identity") +  # Istogrammi sovrapposti
  geom_vline(data = media_per_E_vote, aes(xintercept = Media), linetype = "dotted", color = "black", size = 1) +
  geom_vline(data = media_per_E_vote, aes(xintercept = voti_reali), color = "black", size = 1, alpha = 0.5) +
  theme_clean() +
  facet_wrap(~ E_vote, scales = "free") + # ~ E_vote crea un grafico per ogni valore nella colonna data_long$E_vote
  scale_x_continuous(limits = c(0, 1)) +
  scale_fill_manual(values = colors) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = "none", 
        strip.text = element_text(margin = margin(b = 10)), panel.spacing = unit(2, "lines"))

# ora calcolo le statistiche campionsrie

# Calcolo della media per ciascuna colonna
media_colonne <- colMeans(ita2022_expvote)

# Visualizzazione delle medie per ciascuna colonna
media_colonne

# Calcolo della deviazione standard per ciascuna colonna utilizzando apply
dev_std_colonne <- apply(ita2022_expvote, 2, sd)

# Visualizzazione delle deviazioni standard per ciascuna colonna
dev_std_colonne

# Funzione per calcolare l'intervallo di confidenza al 95%
calcola_intervallo_confidenza <- function(x) {
  t.test(x)$conf.int
}

# Calcolo degli intervalli di confidenza per ciascuna colonna utilizzando apply
intervalli_confidenza <- apply(ita2022_expvote, 2, calcola_intervallo_confidenza)

# Visualizzazione degli intervalli di confidenza per ciascuna colonna
intervalli_confidenza
