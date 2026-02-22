# ========================================
# Importation des données
# ========================================
setwd('/Users/evamontford/Library/CloudStorage/OneDrive-InstitutCatholiquedeLille/ESPOL/L3/S6/Sociologie')
library(readxl)
library(dplyr)
library(tidyr)
library(psych)
filePath <- 'Présidentielles_2012-2022.xlsx'
# PS
vote_PS <- read_xlsx(filePath, sheet = "Elections_PS")
## Renommer les variables
vote_PS <- vote_PS %>%
  rename(
  	code_depart = "Code du département",
    libelle = "Libellé du département",
    code_circo = "Code de la circonscription",
    coor1 = "Coor. 1",
    coor2 = "Coor.2",
    inscrits2012 = "Inscrits 2012",
    exp2012 = "Exprimés 2012",
    voix_inscrits_2012 = "Elections de 2012 (1er tour) Voix/Inscrits",
    voix_expr_2012 = "Elections de 2012 (1er tour) Voix/Exp",
    inscrits2017 = "Inscrits 2017",
    exp2017 = "Exprimés 2017",
    voix_inscrits_2017 = "Elections de 2017 (1er tour) Voix/Inscrits",
    voix_expr_2017 = "Elections de 2017 (1er tour) Voix/Exp",
    inscrits2022 = "Inscrits 2022",
    exp2022 = "Exprimés 2022",
    voix_inscrits_2022 = "Elections de 2022 (1er tour) Voix/Inscrits",
    voix_expr_2022 = "Elections de 2022(1er tour) Voix/Exp",
    voix2012 = "Elections de 2012 (1er tour)",
    voix2017 = "Elections de 2017 (1er tour)",
    voix2022 = "Elections de 2022 (1er tour)"
  )
### Enlever les NA
vote_PS <- vote_PS %>% drop_na()
## Statistiques descriptives
### General par colonnes
vote_PS_desc <- vote_PS %>%
  select(-code_depart, -libelle, -code_circo, -coor1, -coor2) 
  describe()
write.csv(vote_PS_desc, "elections_PS_general.csv", row.names = TRUE)
### General par rangees 
vote_PS_rangees <- vote_PS %>%
  rowwise() %>%
  mutate(
    moyenne = mean(c_across(c(voix_expr_2012, voix_expr_2017, voix_expr_2022)), na.rm = TRUE),
    mediane = median(c_across(c(voix_expr_2012, voix_expr_2017, voix_expr_2022)), na.rm = TRUE),
    ecart_type = sd(c_across(c(voix_expr_2012, voix_expr_2017, voix_expr_2022)), na.rm = TRUE),
    variance = var(c_across(c(voix_expr_2012, voix_expr_2017, voix_expr_2022)), na.rm = TRUE),
    min = min(c_across(c(voix_expr_2012, voix_expr_2017, voix_expr_2022)), na.rm = TRUE),
    max = max(c_across(c(voix_expr_2012, voix_expr_2017, voix_expr_2022)), na.rm = TRUE)
  ) %>%
  ungroup()
vote_PS_rangees <- vote_PS_rangees %>%
  select(moyenne, mediane, ecart_type, variance, min, max) 
write.csv(vote_PS_rangees, "elections_PS.csv", row.names = TRUE)
# LR
vote_LR <- read_xlsx(filePath, sheet = "Elections_LR")
## Renommer les variables
vote_LR <- vote_LR %>%
  rename(
  	code_depart = "Code du département",
    libelle = "Libellé du département",
    code_circo = "Code de la circonscription",
    coor1 = "Coor. 1",
    coor2 = "Coor.2",
    inscrits2012 = "Inscrits 2012",
    exp2012 = "Exprimés 2012",
    voix_inscrits_2012 = "Elections de 2012 (1er tour) Voix/Inscrits",
    voix_expr_2012 = "Elections de 2012 (1er tour) Voix/Exp",
    inscrits2017 = "Inscrits 2017",
    exp2017 = "Exprimés 2017",
    voix_inscrits_2017 = "Elections de 2017 (1er tour) Voix/Inscrits",
    voix_expr_2017 = "Elections de 2017 (1er tour) Voix/Exp",
    inscrits2022 = "Inscrits 2022",
    exp2022 = "Exprimés 2022",
    voix_inscrits_2022 = "Elections de 2022 (1er tour) Voix/Inscrits",
    voix_expr_2022 = "Elections de 2022(1er tour) Voix/Exp",
    voix2012 = "Elections de 2012 (1er tour)",
    voix2017 = "Elections de 2017 (1er tour)",
    voix2022 = "Elections de 2022 (1er tour)"
  )
### Enlever les NA
vote_LR <- vote_LR %>% drop_na()
## Statistiques descriptives
### General par colonnes
vote_LR_desc <- vote_LR %>%
  select(-code_depart, -libelle, -code_circo, -coor1, -coor2) 
write.csv(vote_LR_desc, "elections_LR_general.csv", row.names = TRUE)
### General par rangees 
vote_LR_rangees <- vote_LR %>%
  rowwise() %>%
  mutate(
    moyenne = mean(c_across(c(voix_expr_2012, voix_expr_2017, voix_expr_2022)), na.rm = TRUE),
    mediane = median(c_across(c(voix_expr_2012, voix_expr_2017, voix_expr_2022)), na.rm = TRUE),
    ecart_type = sd(c_across(c(voix_expr_2012, voix_expr_2017, voix_expr_2022)), na.rm = TRUE),
    variance = var(c_across(c(voix_expr_2012, voix_expr_2017, voix_expr_2022)), na.rm = TRUE),
    min = min(c_across(c(voix_expr_2012, voix_expr_2017, voix_expr_2022)), na.rm = TRUE),
    max = max(c_across(c(voix_expr_2012, voix_expr_2017, voix_expr_2022)), na.rm = TRUE)
  ) %>%
  ungroup()
vote_LR_rangees <- vote_LR_rangees %>%
  select(moyenne, mediane, ecart_type, variance, min, max) 
write.csv(vote_LR_rangees, "elections_LR.csv", row.names = TRUE)

#Lier résultats PS et LR
## Votes exprimés
### PS
voix_expr_PS <- vote_PS %>%
	select(voix_expr_2012, voix_expr_2017, voix_expr_2022)
voix_expr_PS <- voix_expr_PS %>%
	rename(	
	voix_expr_2012_PS = voix_expr_2012,
	voix_expr_2017_PS = voix_expr_2017,
	voix_expr_2022_PS = voix_expr_2022)

### LR 
voix_expr_LR <- vote_LR %>%
	select(voix_expr_2012, voix_expr_2017, voix_expr_2022)
voix_expr_LR <- voix_expr_LR %>%
	rename(	
	voix_expr_2012_LR = voix_expr_2012,
	voix_expr_2017_LR = voix_expr_2017,
	voix_expr_2022_LR = voix_expr_2022)
	
### Lier les deux 
voix_expr <- bind_cols(voix_expr_PS, voix_expr_LR)
## Pearson
cor.test(
	voix_expr_gouv$voix_expr_2012_PS,
	voix_expr_gouv$voix_expr_2012_LR,
	method = "pearson",
	use = "complete.obs"
)
## Spearson
cor.test(
	voix_expr_gouv$voix_expr_2012_PS,
	voix_expr_gouv$voix_expr_2012_LR,
	method = "spearman",
	use = "complete.obs"
	