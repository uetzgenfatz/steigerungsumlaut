## ---------------------------------------------
## Wenkerauswertungen Steigerungsumlaut 
## Author: Oliver Schallert
## Date Created: 2025-09-07
## Date Modified: 2025-09-16 
## ---------------------------------------------

library(tidyverse)
library(dplyr)
library(here)
#library(rcompanion)    # Berechnung von Cramérs V
#library(TH.data)       # Abhängigkeit zu rcompanion
# Obacht! Bei diesen Paketen gibt es einen Konflikt mit dplyr

# Load and modify files

setwd("/Users/uetzelsche/Library/CloudStorage/Dropbox/Kühlbox/Untersuchung Steigerungsumlaut/Wenkerauswertungen Komparativumlaut neu (September 2025)")

ws16 <- here("WS_16_ausgewertet.csv")

ws29 <- here("WS_29_ausgewertet.csv")

ws22_31 <- here("WS_22_31_ausgewertet.csv")

ws16_rohdaten <- read.csv(ws16, sep=";", stringsAsFactors = FALSE, na.strings="", header = TRUE)

ws29_rohdaten <- read.csv(ws29, sep=";", stringsAsFactors = FALSE, na.strings="", header = TRUE)

ws22_31_rohdaten <- read.csv(ws22_31, sep=";", stringsAsFactors = FALSE, na.strings="", header = TRUE)


ws16_mod <- 
  ws16_rohdaten %>%
  filter(!is.na(WS16..Stammvokal.Positiv), !is.na(WS16..Stammvokal.Komparativ))

ws29_mod <- 
  ws29_rohdaten %>%
  filter(!is.na(WS29..Stammvokal.Positiv), !is.na(WS29..Stammvokal.Komparativ))

ws29_mod_auslaut <- 
  ws29_rohdaten %>%
  filter(!is.na(WS29..Stammauslaut.Positiv), !is.na(WS29..Stammauslaut.Komparativ))

# Ich habe alle Fälle aussortiert, wo "hart/härter" als lexikalische Variante angegeben ist

ws22_31_mod <- 
  ws22_31_rohdaten %>%
  filter(!is.na(WS22..Stammvokal), !is.na(WS31..Stammvokal)) %>%
  filter(!grepl("\\h", WS22..Adjektivform)) %>%
  filter(!grepl("\\h", WS31..Adjektivform))



# WS 16 [groß/größer]: Typfrequenzen und weitere Analysen

ws16_typen_frequenz <- ws16_mod %>%
  group_by(
    `WS16..Stammvokal.Positiv`,     
    `WS16..Stammvokal.Komparativ`,
  ) %>%
  summarise(
    Anzahl = n(),
    .groups = "drop"
  ) %>%
  mutate(Prozent = round(Anzahl / sum(Anzahl) * 100, 1)) %>%  # neue Spalte
  arrange(desc(Anzahl)) 


ws16_typen_frequenz_mod <- ws16_typen_frequenz %>%
  mutate(
    Typ = str_c(`WS16..Stammvokal.Positiv`, " | ", `WS16..Stammvokal.Komparativ`)
  ) %>%
  filter(Anzahl >= 5)


ws16_export <- ws16_mod %>%
  left_join(ws16_typen_frequenz_mod, by = c(
    "WS16..Stammvokal.Positiv",
    "WS16..Stammvokal.Komparativ"
  )) %>%
  arrange(desc(Anzahl))

ws16_export$Ort..Longitude <- gsub(",", "\\.", ws16_export$Ort..Longitude)
ws16_export$Ort..Latitude <- gsub(",", "\\.", ws16_export$Ort..Latitude)


ws16_export_final <- 
  ws16_export %>%
  select(Bogennummer, Ort..Name, Ort..Longitude, Ort..Latitude, Typ)



# WS 29 [hoch/höher]: Typfrequenzen und weitere Analysen

ws29_typen_frequenz <- ws29_mod %>%
  group_by(
    `WS29..Stammvokal.Positiv`,     
    `WS29..Stammvokal.Komparativ`,
  ) %>%
  summarise(
    Anzahl = n(),
    .groups = "drop"
  ) %>%
  mutate(Prozent = round(Anzahl / sum(Anzahl) * 100, 1)) %>%  # neue Spalte
  arrange(desc(Anzahl)) 

ws29_typen_frequenz_mod <- ws29_typen_frequenz %>%
  mutate(
    Typ = str_c(`WS29..Stammvokal.Positiv`, " | ", `WS29..Stammvokal.Komparativ`)
  ) %>%
  filter(Anzahl >= 5)

ws29_export <- ws29_mod %>%
  left_join(ws29_typen_frequenz_mod, by = c(
    "WS29..Stammvokal.Positiv",
    "WS29..Stammvokal.Komparativ"
  )) %>%
  arrange(desc(Anzahl))

ws29_export$Ort..Longitude <- gsub(",", "\\.", ws29_export$Ort..Longitude)
ws29_export$Ort..Latitude <- gsub(",", "\\.", ws29_export$Ort..Latitude)


ws29_export_final <- 
  ws29_export %>%
  select(Bogennummer, Ort..Name, Ort..Longitude, Ort..Latitude, Typ)



ws29_stammauslaut_frequenz <- ws29_mod_auslaut %>%
  group_by(
    `WS29..Stammauslaut.Positiv`,     
    `WS29..Stammauslaut.Komparativ`,
  ) %>%
  summarise(
    Anzahl = n(),
    .groups = "drop"
  ) %>%
  mutate(Prozent = round(Anzahl / sum(Anzahl) * 100, 1)) %>%  # neue Spalte
  arrange(desc(Anzahl)) 


ws29_stammauslaut_frequenz_mod <- ws29_stammauslaut_frequenz %>%
  mutate(
    Typ = str_c(`WS29..Stammauslaut.Positiv`, " | ", `WS29..Stammauslaut.Komparativ`)
  ) %>%
  filter(Anzahl >= 5)


ws29_stammauslaut_export <- ws29_mod %>%
  left_join(ws29_stammauslaut_frequenz_mod, by = c(
    "WS29..Stammauslaut.Positiv",
    "WS29..Stammauslaut.Komparativ"
  )) %>%
  arrange(desc(Anzahl))

ws29_stammauslaut_export$Ort..Longitude <- gsub(",", "\\.", ws29_stammauslaut_export$Ort..Longitude)

ws29_stammauslaut_export$Ort..Latitude <- gsub(",", "\\.", ws29_stammauslaut_export$Ort..Latitude)


ws29_stammauslaut_export_final <- 
  ws29_stammauslaut_export %>%
  select(Bogennummer, Ort..Name, Ort..Longitude, Ort..Latitude, Typ)


# WS 22/31 [laut/lauter]: Typfrequenzen und weitere Analysen

ws22_31_typen_frequenz <- ws22_31_mod %>%
  group_by(
    `WS22..Stammvokal`,     
    `WS31..Stammvokal`,
  ) %>%
  summarise(
    Anzahl = n(),
    .groups = "drop"
  ) %>%
  mutate(Prozent = round(Anzahl / sum(Anzahl) * 100, 1)) %>%  # neue Spalte
  arrange(desc(Anzahl)) 

ws22_31_typen_frequenz_mod <- ws22_31_typen_frequenz %>%
  mutate(
    Typ = str_c(`WS22..Stammvokal`, " | ", `WS31..Stammvokal`)
  ) %>%
  filter(Anzahl >= 5)

ws22_31_export <- ws22_31_mod %>%
  left_join(ws22_31_typen_frequenz_mod, by = c(
    "WS22..Stammvokal",
    "WS31..Stammvokal"
  )) %>%
  arrange(desc(Anzahl))

ws22_31_export$Longitude <- gsub(",", "\\.", ws22_31_export$Longitude)

ws22_31_export$Latitude <- gsub(",", "\\.", ws22_31_export$Latitude)

names(ws22_31_export)[names(ws22_31_export) == "REDE_Namen"] <- "Ort"

ws22_31_export_final <- 
  ws22_31_export %>%
  select(Bogennummer, Ort, Longitude, Latitude, Typ) %>%
  filter(!is.na(Typ))



tabelle_ws22_31 <- table(as.factor(ws22_31_mod$WS22..Stammvokal), as.factor(ws22_31_mod$WS31..Stammvokal))


# Normaler Chi2-Test macht Probleme; benutze Paramterschätzung via Bootstrapping stattdessen

chi_ws22_31 <- chisq.test(tabelle_ws22_31, simulate.p.value = TRUE)
cramer_ws22_31 <- cramerV(tabelle_ws22_31, bias.correct = TRUE)  # mit Bias-Korrektur

# Interpretation von Cramérs V:
## ~0.1 = kleiner Effekt
## ~0.3 = mittlerer Effekt
## ~0.5 = großer Effekt

# Vergleich mit größ/größer-Alternation

tabelle_ws16 <- table(as.factor(ws16_mod$WS16..Stammvokal.Positiv), as.factor(ws16_mod$WS16..Stammvokal.Komparativ))

chi_ws16 <- chisq.test(tabelle_ws16, simulate.p.value = TRUE)
cramer_ws16 <- cramerV(tabelle_ws16, bias.correct = TRUE)

# Alle Samples exportieren

write.csv2(ws16_export_final, file = "ws16_typen_kartierung.csv", row.names = TRUE)

write.csv2(ws29_export_final, file = "ws29_typen_kartierung.csv", row.names = TRUE)

write.csv2(ws29_stammauslaut_export_final, file = "ws29_stammauslaut_kartierung.csv", row.names = TRUE)

write.csv2(ws22_31_export_final, file = "ws22_31_typen_kartierung.csv", row.names = TRUE)





