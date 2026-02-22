# ========================================
# Packages
# ========================================
library(readxl)
library(dplyr)
library(tidyr)
library(psych)

setwd("/Users/evamontford/Library/CloudStorage/OneDrive-InstitutCatholiquedeLille/ESPOL/L3/S6/Sociologie")
filePath <- "Présidentielles_2012-2022.xlsx"

# ========================================
# Fonction de nettoyage + stats
# ========================================
traitement_parti <- function(sheet_name, parti) {
  
  data <- read_xlsx(filePath, sheet = sheet_name) %>%
    
    rename(
      code_depart = "Code du département",
      libelle = "Libellé du département",
      code_circo = "Code de la circonscription",
      coor1 = "Coor. 1",
      coor2 = "Coor.2",
      inscrits2012 = "Inscrits 2012",
      exp2012 = "Exprimés 2012",
      voix_expr_2012 = "Elections de 2012 (1er tour) Voix/Exp",
      voix_expr_2017 = "Elections de 2017 (1er tour) Voix/Exp",
      voix_expr_2022 = "Elections de 2022(1er tour) Voix/Exp"
    ) %>%
    
    drop_na()
  
  # =========================
  # Stats colonnes
  # =========================
  
  desc <- data %>%
    select(starts_with("voix_expr")) %>%
    describe()
  
  write.csv(desc, paste0("elections_", parti, "_general.csv"))
  
  # =========================
  # Stats lignes
  # =========================
  
  data_rangees <- data %>%
    mutate(
      moyenne = rowMeans(select(., starts_with("voix_expr")), na.rm = TRUE),
      ecart_type = apply(select(., starts_with("voix_expr")), 1, sd, na.rm = TRUE)
    ) %>%
    select(moyenne, ecart_type)
  
  write.csv(data_rangees, paste0("elections_", parti, ".csv"))
  
  return(data)
}

# ========================================
# Application PS / LR
# ========================================

vote_PS <- traitement_parti("Elections_PS", "PS")
vote_LR <- traitement_parti("Elections_LR", "LR")

# ========================================
# Corrélations
# ========================================

voix_expr <- vote_PS %>%
  select(voix_expr_2012, voix_expr_2017, voix_expr_2022) %>%
  rename_with(~ paste0(., "_PS")) %>%
  bind_cols(
    vote_LR %>%
      select(voix_expr_2012, voix_expr_2017, voix_expr_2022) %>%
      rename_with(~ paste0(., "_LR"))
  )

# Pearson 2012
cor.test(
  voix_expr$voix_expr_2012_PS,
  voix_expr$voix_expr_2012_LR,
  method = "pearson"
)

# Spearman 2012
cor.test(
  voix_expr$voix_expr_2012_PS,
  voix_expr$voix_expr_2012_LR,
  method = "spearman"
)
