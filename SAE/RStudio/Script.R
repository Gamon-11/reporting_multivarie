################################################################################
# Auteurs     : Maxime GAMONDELE, Salif SAMAKE ,Rasmata SAWADOGO
# Projet      : SAE 4.02 - Reporting d'une analyse multivariée
# Thématique  : Cohorte Agrican
# Source      : GPCA
# Nom fichier : Script.R
################################################################################


################################################################################
# --------------------------  Chargement des librairies  --------------------- #
################################################################################

library(dplyr)
library(ggplot2)
library(ggpubr)
library(forcats)
library(GGally)
library(patchwork)
library(ggAVplots)
library(cowplot)
library(car)
library(xtable)
library(rsq)
library(FactoMineR)


################################################################################
# --------------------------  Importation des données  ----------------------- #
################################################################################
data = readRDS("../Data/data1975Agrican")


################################################################################
# --------------------------  Préparation des données  ----------------------- #
################################################################################



data$date_deb_tot <- apply(
  data[, c("BovDeb2", "MouDeb2", "CocDeb2", "CheDeb2", "VolDeb2",
           "PraiDeb2", "VigneDeb2", "MaisFin2", "BleDeb2", "PoisDeb2", 
           "BetDeb2", "TouDeb2", "ColDeb2", "TabacDeb2", "ArbDeb2",
           "PdTDeb2", "LegChampDeb2", "SerresDeb2")], 
  1, 
  function(x) {
    x <- x[x > 0]  # Exclure les 0
    if (length(x) == 0) return(NA)  # Si toutes les valeurs sont 0 ou NA, on retourne NA
    min(x, na.rm = TRUE)  # Calcul du min sans 0
  }
)


data$date_fin_tot = apply(data[, c("BovFin2", "MouFin2", "CocFin2", "CheFin2", "VolFin2",
                                "PraiFin2","VigneFin2", "MaisFin2", "BleFin2", "PoisFin2", 
                                "BetFin2", "TouFin2", "ColFin2", "TabacFin2", "ArbFin2",
                                "PdTFin2", "LegChampFin2", "SerresFin2")], 
                       1,
                       max,
                       na.rm = TRUE)  

data$duree_tot = data$date_fin_tot - data$date_deb_tot



# Liste des noms des variables de début (Deb2)
deb_cols <- c("BovDeb2", "MouDeb2", "CocDeb2", "CheDeb2", "VolDeb2",
              "PraiDeb2", "VigneDeb2", "MaisDeb2", "BleDeb2", "PoisDeb2", 
              "BetDeb2", "TouDeb2", "ColDeb2", "TabacDeb2", "ArbDeb2",
              "PdTDeb2", "LegChampDeb2", "SerresDeb2")

# Générer les noms des colonnes Fin2 en remplaçant "Deb2" par "Fin2"
fin_cols <- gsub("Deb2", "Fin2", deb_cols)


# Calcul de la durée pour chaque variable et création des nouvelles colonnes
for (i in seq_along(deb_cols)) {
  duree_col <- paste0("duree_", gsub("Deb2", "", deb_cols[i]))  # Nom de la nouvelle colonne
  data[[duree_col]] <- data[[fin_cols[i]]] - data[[deb_cols[i]]]  # Calcul de la durée
}





# Trouver toutes les colonnes de durée générées précédemment
duree_cols <- grep("^duree_", colnames(data), value = TRUE)

# Boucle pour créer les colonnes de ratio
for (duree_col in duree_cols) {
  ratio_col <- gsub("duree_", "ratio_", duree_col)  # Créer le nom du ratio
  
  # Calcul du ratio : durée individuelle / durée totale
  data[[ratio_col]] <- ifelse(data$duree_tot > 0, data[[duree_col]] / data$duree_tot, NA)
}

data <- subset(data, select = -ratio_tot)


# Trouver toutes les colonnes de ratio générées précédemment
ratio_cols <- grep("^ratio_", colnames(data), value = TRUE)

# Créer une nouvelle table contenant uniquement les ratios
table_ratio <- data[, c("id", ratio_cols), drop = FALSE]

str(data)


# Trouver toutes les colonnes qui commencent par "Cult"
cult_cols <- grep("^Cult", colnames(data), value = TRUE)

# Calculer la fréquence d'apparition des valeurs non nulles pour chaque colonne "Cult"
cult_frequencies <- colSums(data[, cult_cols] != 0, na.rm = TRUE)

# Trier les colonnes par fréquence décroissante et garder les 8 premières
top_8_cult <- names(sort(cult_frequencies, decreasing = TRUE))[1:8]
elevage  <- grep("^El", colnames(data), value = TRUE)

# Renommer top_8_cult
top_8_cult <- gsub("^Cult(.*)Fin3$", "ratio_\\1", top_8_cult)
# Renommer elevage
elevage <- gsub("^El(.*)Fin3$", "ratio_\\1", elevage)

cols_to_keep <- c("id", top_8_cult, elevage)
ratios <- table_ratio[, cols_to_keep]

rownames(ratios) <- ratios$id
ratios <- ratios[, -1]

################################################################################
# -------------------------  Analyse en Composante Principale ---------------- #
################################################################################

