library(car)
library(dplyr)
library(lme4)
library(stringr)
library(tidyr)
library(ggplot2)
library(ggmosaic)


daten <- read.csv("/Users/uetzelsche/Desktop/data_komparation_2022-07-19_15-36.csv", sep=";", row.names=,1, na.strings="")

# Daten bereinigen:
# * Nicht-Muttersprachler aussortieren
# * Datensatz Phonotaktik-Studie auswählen
# * ungerade Nummern: Adjektiv-Bedingung
# * gerade Nummern: Substantiv-Bedingung

daten_adj <- daten %>%
  filter(DE01 == '1') %>%
  select(CASE, matches("A[AOUD]0[13579]")) %>%
  select(CASE, !matches("_03a")) %>%
  select(CASE, matches("_"))

###############################################################
# Erster Teil: Adjektive
###############################################################

# In Long-Format konvertieren

daten_adj_mod <- pivot_longer(daten_adj, starts_with("A"), names_to = "Bedingung", values_to = "Bewertung")


# Umlaut-Bedingungen rekodieren

# Info zur Variablenkodierung bei sociosurvey: https://www.soscisurvey.de/help/doku.php/de:results:values

# Mehrfachauswahl
# Äquivalent zu einer dichotomen Skala („trifft nicht zu“/„trifft zu“) sind hier für jedes Item die Werte

# 1 Item nicht angekreuzt („trifft nicht zu“)
# 2 Item angekreuzt („trifft zu“)

daten_adj_mod$Stammvokal <- substr(daten_adj_mod$Bedingung, 2, 2) 

daten_adj_mod$Bewertung <- ifelse(daten_adj_mod$Bewertung == '2', "ja", NA)

daten_adj_mod <- 
daten_adj_mod %>%
mutate(Phonotaktik = (as.factor(ifelse(grepl("A[AOUD]01", daten_adj_mod$Bedingung), "rk", ifelse(grepl("A[AOUD]03", daten_adj_mod$Bedingung), "st", ifelse(grepl("A[AOUD]05", daten_adj_mod$Bedingung),  "t", ifelse(grepl("A[AOUD]07", daten_adj_mod$Bedingung), "ng", "V:")))))))


daten_adj_mod$Bedingung <- ifelse(grepl("_01", daten_adj_mod$Bedingung), "kUL", ifelse(grepl("_02", daten_adj_mod$Bedingung), "UL", "eigene Variante"))

daten_adj_final <-
  daten_adj_mod %>%
  select(CASE, Bedingung, Stammvokal, Phonotaktik, Bewertung) %>%
  unite(Bedingung_Bewertung, c(Bedingung, Bewertung)) %>%
  mutate(Bewertung = str_replace_all(Bedingung_Bewertung, c("UL_ja" = "UL", "kUL_ja" = "kUL", "eigene Variante_ja" = "eigene Variante"))) %>%
  filter(Bewertung == 'UL' | Bewertung == 'kUL' | Bewertung == 'eigene Variante')

daten_adj_analyse <-
  daten_adj_final %>% 
  subset(Bewertung != "eigene Variante") %>%
  select(CASE, Stammvokal, , Phonotaktik, Bewertung) 

daten_adj_analyse$Bewertung <- factor(daten_adj_analyse$Bewertung)

# Tabellen zur Übersicht als .csv exportieren

frequenztabelle_adj_count <- xtabs(~ Bewertung + Stammvokal + Phonotaktik, daten_adj_analyse)

prozente <- ftable(frequenztabelle_adj_count)

prozente <- prop.table(prozente, margin = 2) * 100

frequenztabelle_adj_prozent <- stats:::format.ftable(prozente, quote = FALSE)


write.csv2(frequenztabelle_adj_count, file = "/Users/uetzelsche/Desktop/Frequenztabelle_Phonotaktik.csv", row.names = TRUE)

write.table(frequenztabelle_adj_prozent, sep = ";", file = "/Users/uetzelsche/Desktop/Prozentwerte_Phonotaktik.csv")


# Gemischste Modelle berechnen (logistische Regression)

# Modell 1: Informant (CASE) als zufälliger Effekt, Stammvokal + Phonotaktik als fixee Effekte

modell1 <- glmer(Bewertung ~ Stammvokal + Phonotaktik + (1|CASE), data=daten_adj_analyse, family = binomial())

# Referenzkategorie ändern: Die Diphthong-Kategorie ist am sinnvollsten, da bei ihr am wenigsten Umlaut zu erwarten ist. Bei der Phonotaktik nehmen wir die ng-Kategorie, da sie im ursprünglichen Modell nicht signifikant war.

daten_adj_analyse$Stammvokal <- factor(daten_adj_analyse$Stammvokal)  

daten_adj_analyse$Bewertung <- factor(daten_adj_analyse$Phonotaktik)
      
Stammvokal_neu <- relevel(daten_adj_analyse$Stammvokal, ref = "D")
Phonotaktik_neu <- relevel(daten_adj_analyse$Phonotaktik, ref = "ng")

# Neues Modell mit geänderten Referenzkategorien

modell2 <- glmer(Bewertung ~ Stammvokal_neu + Phonotaktik_neu + (1|CASE), data=daten_adj_analyse, family = binomial())


# Komplexere Modelle führen zu Konvergenzfehlern, das müsste man sich mal genauer anschauen.

# Zusammenfassung des Modells und Anova (mittels car-Paket)

summary(modell2)
Anova(modell2) # aus dem car-Paket


# Mosaikplots zur Visualisierung

a <-
ggplot(data = subset(daten_adj_final, Stammvokal == 'A')) +
  geom_mosaic(aes(x = product(Phonotaktik, Stammvokal), fill=Bewertung)) + theme_mosaic() + labs(x = 'Anteil Bewertungen', y= 'Phonotaktik', title = 'Steigerungsumlaut', subtitle = 'Stammvokal: a') + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())

o <-
  ggplot(data = subset(daten_adj_final, Stammvokal == 'O')) +
  geom_mosaic(aes(x = product(Phonotaktik, Stammvokal), fill=Bewertung)) + theme_mosaic() + labs(x = 'Anteil Bewertungen', y= 'Phonotaktik', title = 'Steigerungsumlaut', subtitle = 'Stammvokal: o') + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())


u <-
  ggplot(data = subset(daten_adj_final, Stammvokal == 'U')) +
  geom_mosaic(aes(x = product(Phonotaktik, Stammvokal), fill=Bewertung)) + theme_mosaic() + labs(x = 'Anteil Bewertungen', y= 'Phonotaktik', title = 'Steigerungsumlaut', subtitle = 'Stammvokal: u') + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())
         
            
au <-
  ggplot(data = subset(daten_adj_final, Stammvokal == 'D')) +
  geom_mosaic(aes(x = product(Phonotaktik, Stammvokal), fill=Bewertung)) + theme_mosaic() + labs(x = 'Anteil Bewertungen', y= 'Phonotaktik', title = 'Steigerungsumlaut', subtitle = 'Stammvokal: au') + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())


# Plots speichern
  
setwd("/Users/uetzelsche/Desktop")

a
quartz.save("Stammvokal_a.png","png", dpi = 300) 

o
quartz.save("Stammvokal_o.png","png", dpi = 300) 

u
quartz.save("Stammvokal_u.png","png", dpi = 300)

au
quartz.save("Stammvokal_au.png","png", dpi = 300) 



###############################################################
# Zweiter Teil: Substantive
###############################################################

daten_sub <- daten %>%
  filter(DE01 == '1') %>%
  select(CASE, matches("A[AOUD][01][02468]")) %>%
  select(CASE, !matches("_03a")) %>%
  select(CASE, matches("_"))

# Das muss ich mir mal genauer anschauen; hab im Moment leider keine Zeit dazu.
