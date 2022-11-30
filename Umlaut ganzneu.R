library(car)
library(dplyr)
library(lme4)
library(stringr)
library(tidyr)
library(ggplot2)
library(ggmosaic)
library(tigerstats)

daten <- read.csv("/Users/uetzelsche/Desktop/data_komparation_2022-07-19_15-36.csv", sep=";", row.names=,1, na.strings="")


# Daten bereinigen:
# * Nicht-Muttersprachler aussortieren
# * Datensatz Augst-Studie auswählen

daten_mod <- daten %>%
  filter(DE01 == '1') %>%
  select(CASE, matches("S[AODUM]0")) %>%
  select(CASE, !matches("_03a")) %>%
  select(CASE, matches("_"))


# In Long-Format konvertieren

daten_mod <- pivot_longer(daten_mod, starts_with("S"), names_to = "Bedingung", values_to = "Bewertung")


# Umlaut-Bedingung rekodieren

# Info zur Variablenkodierung bei sociosurvey: https://www.soscisurvey.de/help/doku.php/de:results:values

# Mehrfachauswahl
# Äquivalent zu einer dichotomen Skala („trifft nicht zu“/„trifft zu“) sind hier für jedes Item die Werte

# 1 Item nicht angekreuzt („trifft nicht zu“)
# 2 Item angekreuzt („trifft zu“)

daten_mod$Stammvokal <- substr(daten_mod$Bedingung, 2, 2) 
  
daten_mod$Bewertung <- ifelse(daten_mod$Bewertung == '2', "ja", NA)

daten_mod$Bedingung <- ifelse(grepl("_01", daten_mod$Bedingung), "kUL", ifelse(grepl("_02", daten_mod$Bedingung), "UL", "eigene Variante"))

daten_final <-
daten_mod %>%
  select(CASE, Bedingung, Stammvokal, Bewertung) %>%
  unite(Bedingung_Bewertung, c(Bedingung, Bewertung)) %>%
  mutate(Bewertung = str_replace_all(Bedingung_Bewertung, c("UL_ja" = "UL", "kUL_ja" = "kUL", "eigene Variante_ja" = "eigene Variante"))) %>%
  filter(Bewertung == 'UL' | Bewertung == 'kUL' | Bewertung == 'eigene Variante')

daten_analyse <-
  daten_final %>% 
  subset(Bewertung != "eigene Variante") %>%
  select(CASE, Stammvokal, Bewertung) 

daten_analyse$Bewertung <- factor(daten_analyse$Bewertung)

# Tabellen zur Übersicht als .csv exportieren

frequenztabelle_count <- xtabs(~ Bewertung + Stammvokal, daten_analyse)

frequenztabelle_prozent <- colPerc(frequenztabelle_count)

write.csv2(frequenztabelle_count, file = "/Users/uetzelsche/Desktop/Steigerungsumlaut_Bewertung_Zahlen.csv", row.names = TRUE)

write.csv2(frequenztabelle_prozent, file = "/Users/uetzelsche/Desktop/Steigerungsumlaut_Bewertung_Prozent.csv", row.names = TRUE)

# Gemischste Modelle berechnen (logistische Regression)

# Modell 1 (einfach): Informant (CASE) als zufälliger Effekt, Stammvokal als fixer Effekt

modell1 <- glmer(Bewertung ~ Stammvokal + (1|CASE), data=daten_analyse, family = binomial())

# Modell 2 (komplex): Informant (CASE) und Stammvokal als zufällige Effekte, Stammvokal als fixer Effekt

modell2 <- glmer(Bewertung ~ Stammvokal + (1|CASE) + (1|Stammvokal), data=daten_analyse, family = binomial())

# lme4 weist darauf hin, dass es keinen systematischer Effekt von Stammvokal als zufälliger Faktor gibt; siehe dazu die folgende Diskussion: https://stackoverflow.com/questions/60028673/lme4-error-boundary-singular-fit-see-issingular

# Für alle Fälle können wir aber beide Modelle vergleichen:

modellvergleich <- anova(modell1, modell2)

modellvergleich

# Zusammenfassung von Modell 1 und Anova (mittels car-Paket)
# Stammfaktor ist hochsignifikant

summary(modell1)
Anova(modell1) # aus dem car Paket


# Mosaikplot zur Visualisierung

ggplot(data = daten_final) +
  geom_mosaic(aes(x = product(Stammvokal), fill=Bewertung)) + theme_mosaic() + labs(x= 'Stammvokal', y= 'Anteil Varianten', title = 'Steigerungsumlaut', subtitle = 'Akzeptabilitätsdaten') 

 