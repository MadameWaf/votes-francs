# ========================================
# 1. PACKAGES
# ========================================

library(readxl)
library(dplyr)
library(tidyr)
library(psych)
library(sf)
library(ggplot2)

# ========================================
# 2. IMPORTATION DES DONNÉES
# ========================================

filePath <- "Présidentielles_2012-2022.xlsx"

vote_PS <- read_xlsx(filePath, sheet = "Elections_PS")

# ========================================
# 3. RENOMMER LES VARIABLES
# ========================================

vote_PS <- vote_PS %>%
  rename(
    code_depart = "Code du département",
    libelle = "Libellé du département",
    id_circo = "Code de la circonscription",
    voix_expr_2012 = "Elections de 2012 (1er tour) Voix/Exp",
    voix_expr_2017 = "Elections de 2017 (1er tour) Voix/Exp",
    voix_expr_2022 = "Elections de 2022 (1er tour) Voix/Exp"
  )

# ⚠️ IMPORTANT : ne PAS utiliser drop_na()

# ========================================
# 4. VÉRIFIER LES TYPES
# ========================================

# La clé doit être en caractère
vote_PS <- vote_PS %>%
  mutate(id_circo = as.character(id_circo))

# Les voix doivent rester numériques
vote_PS <- vote_PS %>%
  mutate(
    voix_expr_2012 = as.numeric(voix_expr_2012),
    voix_expr_2017 = as.numeric(voix_expr_2017),
    voix_expr_2022 = as.numeric(voix_expr_2022)
  )

# ==============================
# Correction du format id_circo
# ==============================

# On met tout en caractère
vote_PS <- vote_PS %>%
  mutate(id_circo = as.character(id_circo))

# On supprime le premier caractère si longueur = 5
vote_PS <- vote_PS %>%
  mutate(id_circo = ifelse(nchar(id_circo) == 5,
                           substr(id_circo, 2, 5),
                           id_circo))
                           
# ========================================
# 5. IMPORTER LA CARTE
# ========================================

carte <- st_read("circonscriptions_legislatives_030522.json")

# Vérifier le nom exact de la colonne d'identifiant
names(carte)

# Supposons qu’elle s’appelle "id_circo"
carte <- carte %>%
  mutate(id_circo = as.character(id_circo))

# Nettoyage espaces invisibles
carte$id_circo <- trimws(carte$id_circo)
vote_PS$id_circo <- trimws(vote_PS$id_circo)

# ========================================
# 6. DIAGNOSTIC DES PROBLÈMES DE JOINTURE
# ========================================

problemes <- vote_PS %>%
  anti_join(st_drop_geometry(carte), by = "id_circo")

nrow(problemes)      # doit être 0
head(problemes)

# ========================================
# 7. JOINTURE
# ========================================

carte_vote <- carte %>%
  left_join(vote_PS, by = "id_circo")

# Vérifier les NA
sum(is.na(carte_vote$voix_expr_2022))

# ========================================
# 8. CARTOGRAPHIE (exemple 2022)
# ========================================

ggplot(carte_vote) +
  geom_sf(aes(fill = voix_expr_2022), color = NA) +
  scale_fill_viridis_c(option = "plasma", na.value = "grey90") +
  labs(
    title = "Vote PS - Présidentielle 2022 (1er tour)",
    fill = "Voix exprimées"
  ) +
  theme_minimal()
