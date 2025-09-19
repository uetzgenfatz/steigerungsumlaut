library(tidyverse)
library(dplyr)
library(here)
library(psych) # Für Cohens Kappa
library(ggplot2)

# erstellt am 9.9.25; muss noch in das Haupt-Skript integriert werden
# Neue Daten und Features:
## * Prozentwerte
## * Cohens Kappa für (a) laut/lauter vs. Haus/Häuser und (b) groß/größer vs. Vögelchen


setwd("/Users/uetzelsche/Library/CloudStorage/Dropbox/Kühlbox/Untersuchung Steigerungsumlaut/Wenkerauswertungen Komparativumlaut neu (September 2025)")


# laut/lauter

ws22_31 <- here("WS_22_31_ausgewertet.csv")

ws22_31_rohdaten <- read.csv(ws22_31, sep=";", stringsAsFactors = FALSE, na.strings="", header = TRUE)

# Ich habe alle Fälle aussortiert, wo "hart/härter" als lexikalische Variante angegeben ist

ws22_31_mod <- 
  ws22_31_rohdaten %>%
  filter(!is.na(WS22..Stammvokal), !is.na(WS31..Stammvokal)) %>%
  filter(!grepl("\\h", WS22..Adjektivform)) %>%
  filter(!grepl("\\h", WS31..Adjektivform))

# Haus(e)/Häuser

ws26_33 <- here("WS_26_33_ausgewertet.csv")


ws26_33_rohdaten <- read.csv(ws26_33, sep=";", stringsAsFactors = FALSE, na.strings="", header = TRUE)


ws26_33_mod <- 
  ws26_33_rohdaten %>%
  filter(!is.na(WS26.Stammvokal), !is.na(WS33.Stammvokal))


# WS 26/33 [Hause/Häuser]: Typfrequenzen und weitere Analysen


ws26_33_typen_frequenz <- ws26_33_mod %>%
  group_by(
    `WS26.Stammvokal`,     
    `WS33.Stammvokal`,
  ) %>%
  summarise(
    Anzahl = n(),
    .groups = "drop"
  ) %>%
  mutate(Prozent = round(Anzahl / sum(Anzahl) * 100, 1)) %>%  # neue Spalte
  arrange(desc(Anzahl))

ws26_33_typen_frequenz_mod <- ws26_33_typen_frequenz %>%
  mutate(
    Typ = str_c(`WS26.Stammvokal`, " | ", `WS33.Stammvokal`)
  ) %>%
  filter(Anzahl >= 5)

ws26_33_export <- ws26_33_mod %>%
  left_join(ws26_33_typen_frequenz_mod, by = c(
    "WS26.Stammvokal",
    "WS33.Stammvokal"
  )) %>%
  arrange(desc(Anzahl))

ws26_33_export$Longitude <- gsub(",", "\\.", ws26_33_export$Longitude)

ws26_33_export$Latitude <- gsub(",", "\\.", ws26_33_export$Latitude)

names(ws26_33_export)[names(ws26_33_export) == "REDE_Namen"] <- "Ort"

ws26_33_export_final <- 
  ws26_33_export %>%
  select(Bogennummer, Ort, Longitude, Latitude, Typ)

write.csv2(ws26_33_export_final, file = "ws26_33_typen_kartierung.csv", row.names = TRUE)


# groß/größer

ws16 <- here("WS_16_ausgewertet.csv")

ws16_rohdaten <- read.csv(ws16, sep=";", stringsAsFactors = FALSE, na.strings="", header = TRUE)

ws16_mod <- 
  ws16_rohdaten %>%
  filter(!is.na(WS16..Stammvokal.Positiv), !is.na(WS16..Stammvokal.Komparativ))



# Vögelchen (Datensatz muss noch konsolidiert werden)

ws36 <- here("ws36.csv")

ws36_rohdaten <- read.csv(ws36, sep=";", stringsAsFactors = FALSE, na.strings="", header = TRUE)

ws36_mod <- 
  ws36_rohdaten %>%
  filter(!is.na(Stammvokal.WS.36))



# Cohens Kappa für (a) laut/lauter vs. Haus/Häuser und (b) groß/größer vs. Vögelchen

## Daten verknüpfen

umlaut_vergleich_a <- ws22_31_mod %>%
  left_join(ws26_33_mod, by = "Bogennummer") %>%
  select(
    Bogennummer,
    REDE_Namen.x,
    Longitude.x,
    Latitude.x,
    WS22..Adjektivform,
    WS22..Stammvokal,
    WS31..Adjektivform,
    WS31..Stammvokal,
    WS26.Wortform,
    WS26.Stammvokal,
    WS33.Wortform,
    WS33.Stammvokal
  ) %>%
  rename(
    Ortspunkt = REDE_Namen.x,
    Longitude = Longitude.x,
    Latitude = Latitude.x
  )


umlaut_vergleich_b <- ws16_mod %>%
  left_join(ws36_mod, by = "Bogennummer") %>%
  select(
    Ort..Name.x,
    Ort..Longitude.x,
    Ort..Latitude.x,
    WS16..Adjektivform.Positiv,
    WS16..Stammvokal.Positiv,
    WS16..Adjektivform.Komparativ,
    WS16..Stammvokal.Komparativ,
    Wortform.WS.36,
    Stammvokal.WS.36
  ) %>%
  rename(
    Ortspunkt = Ort..Name.x,
    Longitude = Ort..Longitude.x,
    Latitude = Ort..Latitude.x
  )

# Als Faktor umkodieren (im Moment haben wir chr-Format)

## Umlautvergleich (a)

umlaut_vergleich_a$WS22..Stammvokal <- as.factor(umlaut_vergleich_a$WS22..Stammvokal)

umlaut_vergleich_a$WS31..Stammvokal <- as.factor(umlaut_vergleich_a$WS31..Stammvokal)

umlaut_vergleich_a$WS26.Stammvokal <- as.factor(umlaut_vergleich_a$WS26.Stammvokal)

umlaut_vergleich_a$WS33.Stammvokal <- as.factor(umlaut_vergleich_a$WS33.Stammvokal)

## Umlautvergleich (b)

umlaut_vergleich_b$WS16..Stammvokal.Positiv <- as.factor(umlaut_vergleich_b$WS16..Stammvokal.Positiv)

umlaut_vergleich_b$WS16..Stammvokal.Komparativl <- as.factor(umlaut_vergleich_b$WS16..Stammvokal.Komparativ)

umlaut_vergleich_b$Stammvokal.WS.36 <- as.factor(umlaut_vergleich_b$Stammvokal.WS.36)


# Vergleichsvektor bilden

## Umlautvergleich (a)

umlaut_vergleich_a$laut_vergleich <- paste(umlaut_vergleich_a$WS22..Stammvokal, umlaut_vergleich_a$WS31..Stammvokal, sep = "_")

umlaut_vergleich_a$haus_vergleich <- paste(umlaut_vergleich_a$WS26.Stammvokal, umlaut_vergleich_a$WS33.Stammvokal, sep = "_")

umlaut_vergleich_a$laut_vergleich <- as.factor(umlaut_vergleich_a$laut_vergleich)
umlaut_vergleich_a$haus_vergleich <- as.factor(umlaut_vergleich_a$haus_vergleich)


## Umlautvergleich (b)

umlaut_vergleich_b$groß_voegelchen_vergleich <- paste(umlaut_vergleich_b$WS16..Stammvokal.Positiv, umlaut_vergleich_b$Stammvokal.WS.36, sep = "_")

umlaut_vergleich_b$groß_groeßer_vergleich <- paste(umlaut_vergleich_b$WS16..Stammvokal.Positiv, umlaut_vergleich_b$WS16..Stammvokal.Komparativ, sep = "_")

umlaut_vergleich_b$groß_voegelchen_vergleich <- as.factor(umlaut_vergleich_b$groß_voegelchen_vergleich)

umlaut_vergleich_b$groß_groeßer_vergleich <- as.factor(umlaut_vergleich_b$groß_groeßer_vergleich)


# Cohens Kappa berechnen (inkl. Konfidenzintervall) und Plots erstellen

## Umlautvergleich (a)

kappa_umlaut_vergleich_a <- cohen.kappa(cbind(umlaut_vergleich_a$laut_vergleich, umlaut_vergleich_a$haus_vergleich))

## Umlautvergleich (b)

kappa_umlaut_vergleich_b <- cohen.kappa(cbind(umlaut_vergleich_b$groß_voegelchen_vergleich, umlaut_vergleich_b$groß_groeßer_vergleich))


## Werte extrahieren und Plots erstellen

## Umlautvergleich (a)

kappa_lower_a <- kappa_umlaut_vergleich_a$confid["weighted kappa", "lower"]
kappa_estimate_a <- kappa_umlaut_vergleich_a$confid["weighted kappa", "estimate"]
kappa_upper_a <- kappa_umlaut_vergleich_a$confid["weighted kappa", "upper"]


kappa_plot_a <- data.frame(
  Kappa = kappa_estimate_a,
  Lower = kappa_lower_a,
  Upper = kappa_upper_a,
  Kategorie = "laut/lauter vs. Hause/Häuser"
)


## Umlautvergleich (b)

kappa_lower_b <- kappa_umlaut_vergleich_b$confid["weighted kappa", "lower"]
kappa_estimate_b <- kappa_umlaut_vergleich_b$confid["weighted kappa", "estimate"]
kappa_upper_b <- kappa_umlaut_vergleich_b$confid["weighted kappa", "upper"]


kappa_plot_b <- data.frame(
  Kappa = kappa_estimate_b,
  Lower = kappa_lower_b,
  Upper = kappa_upper_b,
  Kategorie = "groß/Vögelchen vs. groß/größer"
)

## Plotten

kappa_df <- rbind(kappa_plot_a, kappa_plot_b)

ggplot(kappa_df, aes(x = Kategorie, y = Kappa)) +
  geom_point(size = 4, color = "steelblue") +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.15, color = "steelblue") +
  ylim(0.7, 0.9) +
  labs(
    title = "Vergleich der gewichteten Kappa-Werte",
    y = "Kappa",
    x = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

