# ctrl + enter ajaa aina sen rivin millä olet tai maalatun alueen

# asenna paketit
install.packages("tidyverse")
install.packages("viridis")
install.packages("cowplot")
install.packages("haven")


# avaa paketit
library(tidyverse)
library(viridis)
library(cowplot)
library(haven)

# vaihda tähän poolku omaan kansioon
setwd("~/data_science/practice_and_teach_R/Hanna") 

# lataa data (oma data!) joko samaan kansioon tai sitten koko polku tuohon
data <- read_spss("fakedata.sav")
# varmista että sun kategoriset muuttujat on kategorisia (ottaa arvot spss labeleistä)
data$y1 <- haven::as_factor(data$y1, levels = "labels")
data$y2 <- haven::as_factor(data$y2, levels = "labels")

head(data) # katso data
str(data) # tarkista muuttujatyypit

# tässä teen kuusi kuvaa sukupuolijaolla ja tallennan ne muuttujiksi "kuva 1a" jne
kuva1a <- data %>% ggplot() + 
  geom_density(aes(x1a, fill = y1)) + # tekee density kuvan x1a muuttujasta, jonka jakaa ja värjää y1 muuttujan mukaan
  geom_vline(xintercept = 8) + # lisää cutoff -viivan haluttuun paikkaan (vaihda numero!)
  geom_text(x = 8, y = 0.1, label = "cut off", angle = 90, vjust = 1.1) + # lisää tekstin 90 asteen kulmaan cutoff viivan viereen, säädä y-korkeus oman datan mukaan per kuva tai poista teksti jos se ei sovi
  labs(fill = "Sex", y = "Density", x = "Variable 1") + # otsikoiden nimet voi säätää täsä
  # facet_wrap(~y2) + # jos haluaa jakaa vielä toisen kategorisen mukaan niin tällä esim. voi, nyt ei päällä
  scale_fill_viridis_d(alpha = 0.8, option = "F") + # hakee värit viridis-paleteista, paletista F, voit vaihtaa
  theme_classic() # voit kokeilla myös muita teemoja, kuten theme_minimal yms.

kuva1b <- data %>% ggplot() + 
  geom_density(aes(x1b, fill = y1)) +
  geom_vline(xintercept = 8) +
  geom_text(x = 8, y = 0.1, label = "cut off", angle = 90, vjust = 1.1) +
  labs(fill = "Sex", y = "Density", x = "Variable 1") +
  #facet_wrap(~y2) +
  scale_fill_viridis_d(alpha = 0.8, option = "F") +
  theme_classic()

kuva2a <- data %>% ggplot() + 
  geom_density(aes(x2a, fill = y1)) +
  geom_vline(xintercept = 8) +
  geom_text(x = 8, y = 0.1, label = "cut off", angle = 90, vjust = 1.1) +
  labs(fill = "Sex", y = "Density", x = "Variable 1") +
  #facet_wrap(~y2) +
  scale_fill_viridis_d(alpha = 0.8, option = "F") +
  theme_classic()

kuva2b <- data %>% ggplot() + 
  geom_density(aes(x2b, fill = y1)) +
  geom_vline(xintercept = 8) +
  geom_text(x = 8, y = 0.1, label = "cut off", angle = 90, vjust = 1.1) +
  labs(fill = "Sex", y = "Density", x = "Variable 1") +
  #facet_wrap(~y2) +
  scale_fill_viridis_d(alpha = 0.8, option = "F") +
  theme_classic()

kuva3a <- data %>% ggplot() + 
  geom_density(aes(x3a, fill = y1)) +
  geom_vline(xintercept = 8) +
  geom_text(x = 8, y = 0.1, label = "cut off", angle = 90, vjust = 1.1) +
  labs(fill = "Sex", y = "Density", x = "Variable 1") +
  #facet_wrap(~y2) +
  scale_fill_viridis_d(alpha = 0.8, option = "F") +
  theme_classic()

kuva3b <- data %>% ggplot() + 
  geom_density(aes(x3b, fill = y1)) +
  geom_vline(xintercept = 8) +
  geom_text(x = 8, y = 0.1, label = "cut off", angle = 90, vjust = 1.1) +
  labs(fill = "Sex", y = "Density", x = "Variable 1") +
  #facet_wrap(~y2) +
  scale_fill_viridis_d(alpha = 0.8, option = "F") +
  theme_classic()


# cowplotilla voi yhdistellä nämä, kikkaile vapaasti
yhdistelmä1 <- cowplot::plot_grid(kuva1a, kuva1b)
yhdistelmä1


yhdistelmä2 <- cowplot::plot_grid(kuva1a, kuva1b,
                                  kuva2a, kuva2b,
                                  kuva3a, kuva3b,
                                  nrow = 3,
                                  labels = c("1A", "1B",
                                             "2A", "2B",
                                             "3A", "3B"))
yhdistelmä2

# tallenna korkelaatuinen versio lopullisesta, kikkaile säädöillä vapaasti
ggsave(filename = "kuvio.png", plot = yhdistelmä2,  device = "png", dpi = 300, height = 10, width = 12)
