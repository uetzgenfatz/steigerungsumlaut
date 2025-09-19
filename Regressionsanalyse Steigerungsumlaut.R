## ---------------------------------------------
## Regressionsanalyse Steigerungsumlaut 
## Author: Oliver Schallert
## Date Created: 2025-09-12
## Date Modified: 2025-09-16
## ---------------------------------------------

library(tidyverse)
library(dplyr)
library(here)
library(rcompanion) # Berechnung von Cramérs V
library(logistf) # Firth-Regression


# -----------------------------
# Dateipfade und Rohdaten
# -----------------------------

setwd("/Users/uetzelsche/Library/CloudStorage/Dropbox/Kühlbox/Untersuchung Steigerungsumlaut/Regressionsanalyse Steigerungsumlaut")

permalink <- here("Adjektivauswertung final.csv")

daten <- read.csv(permalink, sep=";", stringsAsFactors = FALSE, na.strings="", header = TRUE)


# Daten zum (a) Standarddeutschen und (b) zum Alemannischen auswählen; Adjektive mit nicht-umlautfähigem Stammvokal werden jeweils aussortiert.

daten_adj_standard <- daten %>%
  select(Adjektiv, Stammvokal.Standard, Silbenzahl.Standard, Auslaut.Standard, Auslaut.Standard.vereinfacht, Frequenz, Umlaut.Standard) %>%
  filter(Umlaut.Standard == 'ja' | Umlaut.Standard == 'nein') %>%
  filter(!is.na(Frequenz)) %>%
  filter(Stammvokal.Standard != 'ei' & Stammvokal.Standard != 'au' & Stammvokal.Standard != 'i' & Stammvokal.Standard != 'ä' ) %>%
  mutate(Umlaut.Standard = as.factor(Umlaut.Standard))

daten_adj_VA <- daten %>%
  select(Adjektiv, Stammvokal.VA, Silbenzahl.VA, Auslaut.VA, Auslaut.VA.vereinfacht, Frequenz, Umlaut.VA) %>%
  filter(Umlaut.VA == 'ja' | Umlaut.VA == 'nein') %>%
  filter(!is.na(Frequenz)) %>%
  filter(Stammvokal.VA != 'ei' & Stammvokal.VA != 'üe' & Stammvokal.VA != 'i' & Stammvokal.VA != 'ü') %>%
  mutate(Umlaut.VA = as.factor(Umlaut.VA)) %>%
  mutate(Stammvokal.VA = as.factor(Stammvokal.VA))


# -----------------------------
# Frequenzdaten
# -----------------------------

# Wir nutzen übersichlichkeitshalber die xtabs-Funktion. Das Ganze ist alternativ auch mit dplyr umgesetzt.

# Daten zum Standarddeutschen:

auslaut_standard_normal_tab <- xtabs(~ Umlaut.Standard + Auslaut.Standard, daten_adj_standard)

auslaut_standard_vereinfacht_tab <- xtabs(~ Umlaut.Standard + Auslaut.Standard, daten_adj_standard)

silbenzahl_standard_tab <- xtabs(~ Umlaut.Standard + Silbenzahl.Standard, daten_adj_standard)

stammvokal_standard_tab <- xtabs(~ Umlaut.Standard + Stammvokal.Standard, daten_adj_standard)

frequenz_standard_tab <- xtabs(~ Umlaut.Standard + Frequenz, daten_adj_standard)


auslaut_standard_normal <-
  summarise(group_by(daten_adj_standard, Auslaut.Standard, Umlaut.Standard), .groups = 'drop', count = n()) %>%
  mutate(freq = count / sum(count) * 100)

auslaut_standard_vereinfacht <-
  summarise(group_by(daten_adj_standard, Auslaut.Standard.vereinfacht, Umlaut.Standard), .groups = 'drop', count = n()) %>%
  mutate(freq = count / sum(count) * 100)

silbenzahl_standard <-
  summarise(group_by(daten_adj_standard, Silbenzahl.Standard, Umlaut.Standard), .groups = 'drop', count = n()) %>%
  mutate(freq = count / sum(count) * 100)

stammvokal_standard <-
  summarise(group_by(daten_adj_standard, Stammvokal.Standard, Umlaut.Standard), .groups = 'drop', count = n()) %>%
  mutate(freq = count / sum(count) * 100)

frequenz_standard <-
  summarise(group_by(daten_adj_standard, Frequenz, Umlaut.Standard), .groups = 'drop', count = n()) %>%
  mutate(freq = count / sum(count) * 100)


# Daten zum VA

auslaut_VA_normal_tab <- xtabs(~ Umlaut.VA + Auslaut.VA, daten_adj_VA)

auslaut_VA_vereinfacht_tab <- xtabs(~ Umlaut.VA + Auslaut.VA.vereinfacht, daten_adj_VA)

silbenzahl_VA <- xtabs(~ Umlaut.VA + Silbenzahl.VA, daten_adj_VA)

stammvokal_VA <- xtabs(~ Umlaut.VA + Stammvokal.VA, daten_adj_VA)

frequenz_VA <- xtabs(~ Umlaut.VA + Frequenz, daten_adj_VA)


auslaut_VA_normal <-
  summarise(group_by(daten_adj_VA, Auslaut.VA, Umlaut.VA), .groups = 'drop', count = n()) %>%
  mutate(freq = count / sum(count) * 100)

auslaut_VA_alternativ <-
  summarise(group_by(daten_adj_VA, Auslaut.VA.vereinfacht, Umlaut.VA), .groups = 'drop', count = n()) %>%
  mutate(freq = count / sum(count) * 100)

silbenzahl_VA <-
  summarise(group_by(daten_adj_VA, Silbenzahl.VA, Umlaut.VA), .groups = 'drop', count = n()) %>%
  mutate(freq = count / sum(count) * 100)

stammvokal_VA <-
  summarise(group_by(daten_adj_VA, Stammvokal.VA, Umlaut.VA), .groups = 'drop', count = n()) %>%
  mutate(freq = count / sum(count) * 100)

frequenz_VA <-
  summarise(group_by(daten_adj_VA, Frequenz, Umlaut.VA), .groups = 'drop', count = n()) %>%
  mutate(freq = count / sum(count) * 100)

# Vergleich zwischen umlautenden Adjektiv im Standarddeutschen vs. Alemannischen. Wir benutzen einen einfachen Hypothesentest und berechnen zusätzlich die Effektstärke:

UL_prop_VA <- daten %>%
  filter(!is.na(Umlaut.VA)) %>%
  count(Umlaut.VA)

UL_prop_Standard <- daten %>%
  filter(!is.na(Umlaut.Standard)) %>%
  count(Umlaut.Standard)

UL_vergleich <- bind_cols(
  Standarddeutsch = UL_prop_Standard %>% pull(2),
  Alemannisch     = UL_prop_VA %>% pull(2)
) %>%
  as.data.frame() %>%
  `rownames<-`(c("Umlaut", "kein Umlaut", "variabel"))

# Ein normaler Hypothesentest liefert eine ungenaue Schätzung des p-Werts (zu kleine Zeilensummen bei der Ausprägung „variabel/teilweise“). 

chisq.test(UL_vergleich)

# Wir führen alternativ eine Parameterschätzung via Bootstrapping durch:

chisq.test(UL_vergleich, simulate.p.value=TRUE)

# Alternativ können wir auch die Ausprägung „variabel/teilweise“ verwerfen:

UL_vergleich_alternativ <- tibble(
  Kategorie       = c("Umlaut", "kein Umlaut"),
  Standarddeutsch = UL_prop_Standard[1:2, 2],
  Alemannisch     = UL_prop_VA[1:2, 2]
)

UL_vergleich_alternativ_df <- UL_vergleich_alternativ %>%
  column_to_rownames("Kategorie")

# Ein Hypothesentest liefert hier korrekte Werte:

chisq.test(UL_vergleich_alternativ_df) # p < 0.001, also hochsignifikant

# Effektstärke (Cramérs V)

cramerV(as.matrix(UL_vergleich_alternativ_df), bias.correct = TRUE) # 0.39, also mittlerer Zusammenhang

# Interpretation (Daumenregel nach Cohen)
## 0.1 ≈ kleiner Zusammenhang
## 0.3 ≈ mittlerer Zusammenhang
## 0.5 ≈ starker Zusammenhang


# -----------------------------
# Logistische Regression
# -----------------------------

# Standarddeutsch-Daten

## Da sich Probleme mit quasi-Separation ergeben, werden bei `Auslaut` einige Varianten zusammengefasst.

daten_adj_standard$Auslaut.Standard_korr <- forcats::fct_lump_min(daten_adj_standard$Auslaut.Standard, min = 5)

daten_adj_standard$Auslaut.Standard_korr <- fct_recode(daten_adj_standard$Auslaut.Standard_korr,"Other" = "M")

formel_Standard <- as.formula("Umlaut.Standard ~ Frequenz + Stammvokal.Standard + Silbenzahl.Standard + Auslaut.Standard_korr")

logit_modell_Standard <- glm(formel_Standard, family = binomial, data = daten_adj_standard)

summary(logit_modell_Standard)


# Firth-Regression (Paket `logistf`) mit ursprünglichem Datensatz; teilweise noch immer Probleme

logit_modell_Standard_korr <- logistf(Umlaut.Standard ~ Frequenz + Stammvokal.Standard + Silbenzahl.Standard + Auslaut.Standard, data = daten_adj_standard)

summary(logit_modell_Standard_korr)

# Kleinster p-Wert ist `Auslaut.StandardSS` bei 0.064, d.h. tendenziell relevant, aber noch nicht signifikant.

# Wir testen jeden Prädiktor gegen das Modell ohne diesen Prädiktor, und zwar mittels Likelihood-Ratio-Test.

anova(logit_modell_Standard, test="LRT")

## Schrittweise Hinzufügung: Die Reihenfolge beeinflusst die Prädiktoren!
## Wäre beispielsweise `Frequenz` signifikant, wenn es nach allen anderen Variablen hinzugefügt wird, evtl. nicht mehr.
## Signifikanz: Nur `Silbenzahl.Standard` ist hier ein starker Prädiktor.
## Je größer die Reduktion der Devianz, desto stärker trägt die Variable zur Modellanpassung bei.

# Sequenzieller Modellvergleich (mittels ANOVA)
logit_modell_Standard_red <- update(logit_modell_Standard, . ~ . - Auslaut_korr)

anova(logit_modell_Standard_red, test = "Chisq")

# Backward selection liefert ähnliche Resultate
fit_step <- step(logit_modell_Standard, direction = "backward", trace = TRUE)


# Alemannisch-Daten

## Da sich auch hier Probleme mit quasi-Separation ergeben, werden bei `Auslaut` einige Varianten zusammengefasst. Bei `Stammvokal` wird ua ausgeschlossen.


daten_adj_VA$Stammvokal.VA_korr <- forcats::fct_lump_min(daten_adj_VA$Stammvokal.VA, min = 5)

daten_adj_VA$Auslaut.VA_korr <- forcats::fct_lump_min(daten_adj_VA$Auslaut.VA, min = 15)


formel_VA <- as.formula("Umlaut.VA ~ Frequenz + Stammvokal.VA_korr + Silbenzahl.VA + Auslaut.VA_korr")

logit_modell_VA <- glm(formel_VA, family = binomial, data = daten_adj_VA)

summary(logit_modell_VA)

# Firth-Regression ist vernünftig interpretierbar

logit_modell_VA_korr <- logistf(Umlaut.VA ~ Frequenz + Stammvokal.VA_korr + Auslaut.VA_korr, data = daten_adj_VA)

summary(logit_modell_VA_korr)

## Wenn das 95%-CI 0 enthält, ist der Effekt nicht signifikant.
## Beispiel: Frequenz CI = [-0.0715, -0.00936]; enthält nicht 0, also signifikanter negativer Effekt.
## Beispiel: Auslaut.VA_korrM CI = [-4.11, 2.25]; enthält 0, also nicht signifikant.

# Signifikante Prädiktoren: Frequenz, Silbenzahl.VA, einige Ausprägungen von `Stammvokal`

# Rückwärtsselektion; muss mittels eigener Funktion umgesetzt werden, da wir kein glm-Objekt haben

backward_lrtest <- function(response, predictors, data, alpha = 0.05) {
  vars <- predictors
  
  lr_pval <- function(full, reduced) {
    lr <- tryCatch(anova(full, reduced), error = function(e) NULL)
    # Prüfen, ob lr ein data.frame mit Spalte 'Pr(>Chisq)' ist und mindestens 2 Zeilen hat
    if (!is.data.frame(lr) || !"Pr(>Chisq)" %in% colnames(lr) || nrow(lr) < 2) return(NA)
    return(lr$`Pr(>Chisq)`[2])
  }
  
  repeat {
    if (length(vars) == 0) break
    
    form <- as.formula(paste(response, "~", paste(vars, collapse = " + ")))
    voll <- logistf(form, data = data)
    
    pvals <- sapply(vars, function(var) {
      reduced_vars <- setdiff(vars, var)
      if (length(reduced_vars) == 0) return(NA)
      reduced_form <- as.formula(paste(response, "~", paste(reduced_vars, collapse = " + ")))
      reduced_model <- logistf(reduced_form, data = data)
      lr_pval(voll, reduced_model)
    })
    
    max_p <- max(pvals, na.rm = TRUE)
    if (max_p <= alpha) break
    
    remove_var <- names(pvals)[which.max(pvals)]
    vars <- setdiff(vars, remove_var)
  }
  
  final_model <- logistf(as.formula(paste(response, "~", paste(vars, collapse = " + "))), data = data)
  return(final_model)
}

modell_VA_final <- backward_lrtest(
  response = "Umlaut.VA",
  predictors = c("Frequenz", "Stammvokal.VA_korr", "Silbenzahl.VA", "Auslaut.VA_korr"),
  data = daten_adj_VA
)

summary(modell_VA_final)

# -----------------------------
# Odds Ratios + 95%-CI
# -----------------------------

coefs <- modell_VA_final$coefficients
OR <- exp(coefs)
CI <- confint(modell_VA_final)
OR_CI <- exp(CI)

OR_table <- data.frame(
  Prädiktor = names(coefs),
  OR        = OR,
  CI_lower  = OR_CI[,1],
  CI_upper  = OR_CI[,2]
)

print(OR_table)


# -----------------------------
# Visualisierungen
# -----------------------------

OR_table <- OR_table %>%
  arrange(OR) %>%
  mutate(Prädiktor = factor(Prädiktor, levels = Prädiktor))  # Reihenfolge fixieren

ggplot(OR_table, aes(x = Prädiktor, y = OR)) +
  geom_point(size = 3, color = "steelblue") +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), width = 0.2, color = "steelblue") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  coord_flip() +
  ylab("Odds Ratio (95% CI)") +
  xlab("") +
  ggtitle("Odds Ratios der Prädiktoren (Firth-Regression)") +
  theme_minimal(base_size = 14)



heatmap_data <- expand.grid(
  Stammvokal.VA_korr = levels(daten_adj_VA$Stammvokal.VA_korr),
  Auslaut.VA_korr    = levels(daten_adj_VA$Auslaut.VA_korr),
  Silbenzahl.VA       = round(mean(daten_adj_VA$Silbenzahl.VA, na.rm = TRUE)),
  Frequenz            = mean(daten_adj_VA$Frequenz, na.rm = TRUE)
)

heatmap_data$Stammvokal.VA_korr <- factor(heatmap_data$Stammvokal.VA_korr, 
                                          levels = levels(daten_adj_VA$Stammvokal.VA_korr))
heatmap_data$Auslaut.VA_korr <- factor(heatmap_data$Auslaut.VA_korr, 
                                       levels = levels(daten_adj_VA$Auslaut.VA_korr))

heatmap_data$pred_prob <- predict(modell_VA_final, newdata = heatmap_data, type = "response")

ggplot(heatmap_data, aes(x = Auslaut.VA_korr, y = Stammvokal.VA_korr, fill = pred_prob)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "steelblue", name = "P(Umlaut)") +
  geom_text(aes(label = round(pred_prob, 2)), color = "black", size = 4) +
  xlab("Auslaut") +
  ylab("Stammvokal") +
  ggtitle("Vorhergesagte Wahrscheinlichkeit für Umlaut") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


daten_adj_VA$pred <- predict(modell_VA_final, type = "response")

ggplot(daten_adj_VA, aes(x = pred, y = as.numeric(Umlaut.VA)-1)) +
  geom_jitter(height = 0.05, width = 0, alpha = 0.4, color = "purple") +
  geom_smooth(method = "loess", color = "black", size = 1) +
  ylab("Beobachtetes Umlaut-Outcome (0/1)") +
  xlab("Vorhergesagte Wahrscheinlichkeit") +
  ggtitle("Observed vs. Predicted") +
  theme_minimal(base_size = 14)


# -----------------------------
# In Arbeit
# -----------------------------

########## vereinfachter Auslaut (nur C bzw. CC): Standarddeutsch
daten_adj_standard$Auslaut.Standard_neu <- forcats::fct_lump_min(daten_adj_standard$Auslaut.Standard.vereinfacht, min = 5)

daten_adj_standard$Auslaut.Standard_neu <- fct_recode(daten_adj_standard$Auslaut.Standard_neu,"Other" = "M")

formel_Standard_vereinfacht <- as.formula("Umlaut.Standard ~ Frequenz + Stammvokal.Standard + Silbenzahl.Standard + Auslaut.Standard_neu")

logit_modell_Standard_vereinfacht <- glm(formel_Standard_vereinfacht, family = binomial, data = daten_adj_standard)

summary(logit_modell_Standard_vereinfacht)

anova(logit_modell_Standard_vereinfacht, test = "LRT")

library(detectseparation)

detect_separation(
  Umlaut.Standard ~ Frequenz + Stammvokal.Standard + Silbenzahl.Standard + Auslaut.Standard_neu,
  data = daten_adj_standard,
  family = binomial()
)

detect_separation(logit_modell_Standard_vereinfacht)


# mit neuer Version von detectseparation nicht nötig
y <- daten_adj_standard$Umlaut.Standard
x <- model.matrix(~ Frequenz + Stammvokal.Standard + Silbenzahl.Standard + Auslaut.Standard_neu,
                  data = daten_adj_standard)

detect_separation(x = x, y = y, family = binomial())
##########


########## vereinfachter Auslaut (nur C bzw. CC): Alemannisch
formel_VA_vereinfacht <- as.formula("Umlaut.VA ~ Frequenz + Stammvokal.VA_korr + Silbenzahl.VA + Auslaut.VA_neu")

daten_adj_VA$Auslaut.VA_neu <- forcats::fct_lump_min(daten_adj_VA$Auslaut.VA.vereinfacht, min = 5)

daten_adj_VA$Auslaut.VA_neu <- fct_recode(daten_adj_VA$Auslaut.VA_neu,"Other" = "M")

logit_modell_VA_vereinfacht <- glm(formel_VA_vereinfacht, family = binomial, data = daten_adj_VA)

summary(logit_modell_VA_vereinfacht)

anova(logit_modell_VA_vereinfacht, test = "LRT")

coefs.VA <- logit_modell_VA_vereinfacht$coefficients
OR.VA <- exp(coefs.VA)
CI.VA <- confint(logit_modell_VA_vereinfacht)
OR_CI.VA <- exp(CI.VA)

OR_table.VA <- data.frame(
  Prädiktor = names(coefs.VA),
  OR        = OR.VA,
  CI_lower  = OR_CI.VA[,1],
  CI_upper  = OR_CI.VA[,2]
)

print(OR_table.VA)


##########

