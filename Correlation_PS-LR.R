# =====================================================
# ANALYSE DES CORRELATIONS PS / LR (2012–2022)
# =====================================================

# =========================
# 1. Packages
# =========================
library(readxl)
library(dplyr)
library(tidyr)
library(psych)
library(corrplot)

# =========================
# 2. Importation
# =========================

filePath <- "Présidentielles_2012-2022.xlsx"

vote_PS <- read_xlsx(filePath, sheet = "Elections_PS")
vote_LR <- read_xlsx(filePath, sheet = "Elections_LR")

# =========================
# 3. Renommage des variables utiles
# =========================

renommage <- c(
  voix_expr_2012 = "Elections de 2012 (1er tour) Voix/Exp",
  voix_expr_2017 = "Elections de 2017 (1er tour) Voix/Exp",
  voix_expr_2022 = "Elections de 2022(1er tour) Voix/Exp"
)

vote_PS <- vote_PS %>%
  rename(!!!renommage) %>%
  drop_na(voix_expr_2012, voix_expr_2017, voix_expr_2022)

vote_LR <- vote_LR %>%
  rename(!!!renommage) %>%
  drop_na(voix_expr_2012, voix_expr_2017, voix_expr_2022)

# =========================
# 4. Construction base commune
# =========================

voix_expr <- vote_PS %>%
  select(voix_expr_2012, voix_expr_2017, voix_expr_2022) %>%
  rename_with(~ paste0(., "_PS")) %>%
  bind_cols(
    vote_LR %>%
      select(voix_expr_2012, voix_expr_2017, voix_expr_2022) %>%
      rename_with(~ paste0(., "_LR"))
  )

# =========================
# 5. MATRICE DE CORRELATION
# =========================

# --- Pearson
matrice_pearson <- cor(
  voix_expr,
  method = "pearson",
  use = "complete.obs"
)

# --- Spearman
matrice_spearman <- cor(
  voix_expr,
  method = "spearman",
  use = "complete.obs"
)

# =========================
# 6. Matrice complète avec p-values
# =========================

cor_complete <- corr.test(
  voix_expr,
  method = "pearson",
  use = "complete"
)

matrice_correlations <- cor_complete$r
matrice_pvalues <- cor_complete$p

# =========================
# 7. Export CSV
# =========================

write.csv(matrice_pearson, "matrice_correlation_pearson.csv")
write.csv(matrice_spearman, "matrice_correlation_spearman.csv")
write.csv(matrice_pvalues, "matrice_pvalues.csv")

# =========================
# 8. Visualisation graphique
# =========================

corrplot(
  matrice_pearson,
  method = "color",
  type = "upper",
  addCoef.col = "black",
  tl.col = "black",
  tl.srt = 45
)

# =========================
# 9. Résumé console
# =========================

print("=== MATRICE PEARSON ===")
print(round(matrice_pearson, 3))

print("=== MATRICE P-VALUES ===")
print(round(matrice_pvalues, 5))
