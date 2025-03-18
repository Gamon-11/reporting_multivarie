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
library(factoextra)
library(magrittr)
library(kableExtra)
library(knitr)
library(dplyr)
library(gt)


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

# Renommer les colonnes
colnames(ratios) <- c(
  "Prairies",          # ratio_Prai -> Prairies
  "Blé ou orge",       # ratio_Ble -> Blé ou orge
  "Mais",              # ratio_Mais -> Mais
  "Vignes",            # ratio_Vigne -> Vignes
  "Colza",             # ratio_Col -> Colza
  "Tournesol",         # ratio_Tou -> Tournesol
  "Bettraves",         # ratio_Bet -> Bettraves
  "Pois fourragers",   # ratio_Pois -> Pois fourragers
  "Bovins",            # ratio_Bov -> Bovins
  "Moutons/Chèvres",   # ratio_Mou -> Moutons/chèvres
  "Cochons",           # ratio_Coc -> Cochons
  "Chevaux",           # ratio_Che -> Chevaux
  "Volailles"          # ratio_Vol -> Volailles
)


################################################################################
#                         Tableau des valeurs propres                          #
################################################################################
res.pca = PCA(ratios, scale.unit = TRUE, graph = FALSE)
df <- res.pca$eig %>%
  as.data.frame() %>%
  select(-1) %>%  # Suppression de la première colonne (Eigen values)
  setNames(c("% d'inertie", "% d'inertie cumulé")) %>%
  mutate(
    `% d'inertie` = round(as.numeric(`% d'inertie`), 2),  # Arrondi à 2 décimales
    `% d'inertie cumulé` = round(as.numeric(`% d'inertie cumulé`), 2)  # Arrondi à 2 décimales
  )

# Sélectionner les lignes à surligner (comp 1 à comp 8)
color.me <- 8  # Indices des lignes à colorer

# Générer le tableau avec kableExtra
df %>%
  kbl(escape = FALSE, align = "c") %>%
  kable_styling(full_width = FALSE, font_size = 14) %>%
  row_spec(color.me, bold = TRUE, color = "white", background = "purple")


################################################################################
# -------------------------  Analyse en Composante Principale ---------------- #
################################################################################


res.pca = PCA(ratios, scale.unit = TRUE, graph = FALSE,ncp = 8)
# Extraire le pourcentage d'inertie cumulée

fviz_pca_var(res.pca, 
             col.var = "springgreen4",       
             alpha.var = 8,             
             repel = TRUE,                
             ggtheme = theme_minimal()) + 
  theme(
    text = element_text(size = 14),       
    plot.title = element_text(face = "bold", hjust = 0.5), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "bottom"           
  ) + 
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  labs(title = "Nuage des Variables - Analyse en Composantes Principales")


inertie_cum = res.pca$eig
# En utilisant le critère de 80% de l'inertie restituée, on serait amener à séléctionner les 8 premières composantes.
# Nous allons si après détaillé les 4 premières composantes principales :

#res.pca$var








################################################################################
#  Matrice des corélation entre les variables et les composantes principales   #
################################################################################

cor_matrix <- as.data.frame(round(res.pca$var$cor[, 1:4], 2))

# Appliquer le surlignage colonne par colonne
cor_matrix_html <- cor_matrix %>%
  mutate(across(everything(), ~ cell_spec(.x, "html",
                                          color = ifelse(.x > 0.6, "white",  # Blanc si fond rouge (>0.6)
                                                         ifelse(.x > 0.4, "white",   # Blanc si fond orange (>0.4)
                                                                ifelse(.x > 0.2, "black", "black"))),  # Noir si fond jaune (0.2<x≤0.4), Noir sinon
                                          background = ifelse(.x > 0.6, "red",   # Rouge si > 0.6
                                                              ifelse(.x > 0.4, "orange",   # Orange si > 0.4
                                                                     ifelse(.x > 0.2, "yellow", "white"))),  # Jaune si > 0.2, sinon blanc
                                          align = "center")))

# Générer le tableau HTML avec kableExtra
cor_matrix_html %>%
  kbl(escape = FALSE, align = "c") %>%
  kable_styling(full_width = FALSE, font_size = 14)





################################################################################
#                           Classiffication automatique
#                               A l'aide des K-Means
################################################################################

################################################################################
# ------------------------- Coude pour le nombre de cluster     -------------- #
################################################################################
#ici on voit une cassure à 6 donc on choisi 6 class d'apres la methode du coude

fviz_nbclust(res.pca$var$coord,
             hcut,
             k.max = 12,
             method = "wss") +
  geom_vline(xintercept = 8,
             linetype = "dashed",
             color = "red")

clust_data <- res.pca$ind$coord[, 1:8]



################################################################################
# -------------------------  Tableau des effectifs par cluster     -------------#
################################################################################

# 1. Appliquer K-Means
km.res <- kmeans(clust_data, centers = 8)

# 2. Créer un tableau des fréquences des clusters
cluster_counts <- as.data.frame(table(km.res$cluster))
colnames(cluster_counts) <- c("Cluster", "Effectif")  # Renommer les colonnes


########################################## POURQUOI #####
# 3. Calculer les moyennes des variables par cluster 
cluster_means <- aggregate(clust_data, by = list(cluster = km.res$cluster), FUN = mean)
print(cluster_means)

top_4_effectifs <- cluster_counts %>% 
  arrange(desc(Effectif)) %>% 
  head(4) %>% 
  pull(Effectif)

# Appliquer la mise en forme pour les 4 plus grands Effectif
cluster_counts_html <- cluster_counts %>% 
  mutate(
    Effectif = cell_spec(Effectif, "html", 
                         color = ifelse(Effectif %in% top_4_effectifs, "white", "black"),  # Blanc si top 4, sinon noir
                         background = ifelse(Effectif %in% top_4_effectifs, "red", "white"),  # Rouge si top 4, sinon blanc
                         align = "center")
  )

# Générer le tableau HTML avec kableExtra
cluster_counts_html %>%
  kbl(escape = FALSE, align = "c") %>%
  kable_styling(full_width = FALSE, font_size = 14)


################################################################################
# -------------------  Tableau des cultures les plus fréquentes    ------------#
################################################################################

# Extraire les 8 cultures les plus fréquentes
cult_frequencies <- colSums(data[, cult_cols] != 0, na.rm = TRUE)

# Trier les cultures par ordre décroissant de fréquence et sélectionner les 8 premières
top_8_cult <- names(sort(cult_frequencies, decreasing = TRUE))[1:8]

# Créer un tableau avec les fréquences des 8 cultures les plus fréquentes
top_8_cult_table <- data.frame(
  Culture = top_8_cult,
  Fréquence = cult_frequencies[top_8_cult]
)

top_8_cult_table %>% 
  select(-Culture)->top_8_cult_table

rownames(top_8_cult_table) <- c(
  "Prairies",
  "Blé ou orge",
  "Maïs",
  "Vigne",
  "Colza",
  "Tournesol",
  "Betteraves",
  "Pois fourragers"
)


top_3_freq <- top_8_cult_table %>% 
  arrange(desc(Fréquence)) %>% 
  head(3) %>% 
  pull(Fréquence)

# Appliquer la mise en forme pour les 3 plus grands Effectif
culture_matrix_html <- top_8_cult_table %>% 
  mutate(
    Fréquence = cell_spec(Fréquence, "html", 
                         color = ifelse(Fréquence %in% top_3_freq, "white", "black"),  # Blanc si top 4, sinon noir
                         background = ifelse(Fréquence %in% top_3_freq, "red", "white"),  # Rouge si top 4, sinon blanc
                         align = "center")
  )


# Générer le tableau HTML avec kableExtra
culture_matrix_html %>%
  kbl(escape = FALSE, align = "c") %>%
  kable_styling(full_width = FALSE, font_size = 14)

data$NbPaquetAnneeF

# duree tabac

colnames(data)





# Ajouter les clusters aux données
data$Cluster <- as.factor(km.res$cluster)

# Renommer les clusters avec des étiquettes "Cluster1", "Cluster2", etc.
levels(data$Cluster) <- paste0("Cluster", 1:length(levels(data$Cluster)))

# Compter les effectifs par cluster et sélectionner les 4 plus grands
top_clusters <- names(sort(table(data$Cluster), decreasing = TRUE)[1:4])

# Filtrer les données pour ne garder que les 4 clusters dominants
data_filtered <- data %>% filter(Cluster %in% top_clusters)

# Création du tableau de synthèse avec les pourcentages des fumeurs et non-fumeurs
tableau_synthese <- data_filtered %>%
  group_by(Cluster) %>%
  summarise(
    "Effectif" = n(),
    "Âge moyen au début de la carrière (écart-type)" = paste0(round(mean(date_deb_tot - A_Nais, na.rm = TRUE), 2), 
                                                              " (", round(sd(date_deb_tot - A_Nais, na.rm = TRUE), 2), ")"),
    "Proportion de fumeurs" = round((sum(TabagismeF == 1, na.rm = TRUE) / n()) * 100, 2),
    "Nombre moyen de paquets de tabac consommés annuellement" = round(mean(NbPaquetAnneeF, na.rm = TRUE), 2),
    
    "Durée moyenne d'activité (écart-type)" = paste0(round(mean(duree_tot, na.rm = TRUE), 2), 
                                                     " (", round(sd(duree_tot, na.rm = TRUE), 2), ")")
  ) %>%
  mutate(across(everything(), as.character)) %>%  # Convertir toutes les colonnes en caractères
  pivot_longer(-Cluster, names_to = "Variable", values_to = "Valeur") %>%
  pivot_wider(names_from = "Cluster", values_from = "Valeur")

tableau_synthese %>% 
  column_to_rownames(var = "Variable") -> tableau_synthese

# Affichage du tableau
print(tableau_synthese)



# Option pour un affichage formaté en RMarkdown
# knitr::kable(tableau_synthese, caption = "Tableau de synthèse des 4 clusters dominants")


# Convertir les noms de lignes en une colonne temporaire, les traiter, puis les remettre en rownames
tableau_synthese_html <- tableau_synthese %>%
  rownames_to_column(var = "Variable") %>%
  rowwise() %>%
  mutate(across(-Variable,  # Exclure la colonne des noms
                ~ ifelse(.x == max(c_across(everything()), na.rm = TRUE), 
                         cell_spec(.x, "html", align = "center", bold = TRUE,  
                                   background = "red", color = "white"),  
                         cell_spec(.x, "html", align = "center")))) %>%
  column_to_rownames(var = "Variable")  # Remettre les noms de lignes

# Générer le tableau HTML avec kableExtra
tableau_synthese_html %>%
  kbl(escape = FALSE, align = "c") %>%
  kable_styling(full_width = FALSE, font_size = 14)

