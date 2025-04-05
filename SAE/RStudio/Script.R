################################################################################
# Auteurs     : Maxime GAMONDELE, Salif SAMAKE, Rasmata SAWADOGO
# Projet      : SAE 4.02 - Reporting d'une analyse multivariée
# Thématique  : Cohorte Agrican
# Source      : GPCA
# Nom fichier : Script.R
################################################################################


################################################################################
#                        CHARGEMENT DES LIBRAIRIES                             #
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
library(gt)


################################################################################
#                         IMPORTATION DES DONNÉES                              #
################################################################################
data = readRDS("../Data/data1975Agrican")


################################################################################
#                         PRÉPARATION DES DONNÉES                              #
################################################################################
# 1. Calcul des dates de début et fin d'activité totale
################################################################################
# Calcul de la date de début (minimum des dates de début par activité, excluant les valeurs nulles)
data$date_deb_tot <- apply(
  data[, c("BovDeb2", "MouDeb2", "CocDeb2", "CheDeb2", "VolDeb2",
           "PraiDeb2", "VigneDeb2", "MaisFin2", "BleDeb2", "PoisDeb2", 
           "BetDeb2", "TouDeb2", "ColDeb2", "TabacDeb2", "ArbDeb2",
           "PdTDeb2", "LegChampDeb2", "SerresDeb2")], 
  1, 
  function(x) {
    x <- x[x > 0]  
    if (length(x) == 0) return(NA)  
    min(x, na.rm = TRUE)  
  }
)

# Calcul de la date de fin (maximum des dates de fin par activité)
data$date_fin_tot = apply(data[, c("BovFin2", "MouFin2", "CocFin2", "CheFin2", "VolFin2",
                                   "PraiFin2","VigneFin2", "MaisFin2", "BleFin2", "PoisFin2", 
                                   "BetFin2", "TouFin2", "ColFin2", "TabacFin2", "ArbFin2",
                                   "PdTFin2", "LegChampFin2", "SerresFin2")], 
                          1,
                          max,
                          na.rm = TRUE)  

# Calcul de la durée totale d'activité
data$duree_tot = data$date_fin_tot - data$date_deb_tot


################################################################################
# 2. Calcul des durées individuelles par type d'activité                       #
################################################################################
# Liste des noms des variables de début (Deb2)
deb_cols <- c("BovDeb2", "MouDeb2", "CocDeb2", "CheDeb2", "VolDeb2",
              "PraiDeb2", "VigneDeb2", "MaisDeb2", "BleDeb2", "PoisDeb2", 
              "BetDeb2", "TouDeb2", "ColDeb2", "TabacDeb2", "ArbDeb2",
              "PdTDeb2", "LegChampDeb2", "SerresDeb2")

# Générer les noms des colonnes Fin2 en remplaçant "Deb2" par "Fin2"
fin_cols <- gsub("Deb2", "Fin2", deb_cols)

# Calcul de la durée pour chaque variable et création des nouvelles colonnes
for (i in seq_along(deb_cols)) {
  duree_col <- paste0("duree_", gsub("Deb2", "", deb_cols[i])) 
  data[[duree_col]] <- data[[fin_cols[i]]] - data[[deb_cols[i]]]  
}


################################################################################
# 3. Calcul des ratios de durée par rapport à la durée totale                  #
################################################################################
# Trouver toutes les colonnes de durée générées précédemment
duree_cols <- grep("^duree_", colnames(data), value = TRUE)

# Boucle pour créer les colonnes de ratio
for (duree_col in duree_cols) {
  ratio_col <- gsub("duree_", "ratio_", duree_col)  
  
  # Calcul du ratio : durée individuelle / durée totale
  data[[ratio_col]] <- ifelse(data$duree_tot > 0, data[[duree_col]] / data$duree_tot, NA)
}

data <- subset(data, select = -ratio_tot)


################################################################################
# 4. Création d'une table de ratios                                            #
################################################################################
# Trouver toutes les colonnes de ratio générées précédemment
ratio_cols <- grep("^ratio_", colnames(data), value = TRUE)

# Créer une nouvelle table contenant uniquement les ratios
table_ratio <- data[, c("id", ratio_cols), drop = FALSE]

str(data)


################################################################################
# 5. Sélection des cultures et élevages les plus fréquents                     #
################################################################################
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

# Renommer les colonnes pour plus de clarté
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
# 6. Calcul des pourcentages de temps par tâche agricole                       #
################################################################################
# Liste des tâches pour chaque activité
taches_agricoles <- list(
  Prairies = c("PraiHerDebFinale2", "PraiFoinDebFinale2"),
  Vigne = c("VignePfaconDebFinale2", "VignePestDebFinale2", "VigneVendDebFinale2","VigneChaiDebFinale2","VigneEntDebFinale2"),
  Mais = c("MaisSemenDebFinale2", "MaisSemisDebFinale2", "MaisPestDebFinale2","MaisRecDebFinale2"),
  Blé = c("BleSemenDebFinale2", "BleSemisDebFinale2", "BlePestDebFinale2","BleRecDebFinale2"),
  Pois = c("PoisSemenDebFinale2", "PoisSemisDebFinale2", "PoisPestDebFinale2","PoisRecDebFinale2"),
  Betteraves = c("BetSemenDebFinale2", "BetSemisDebFinale2", "BetPestDebFinale2","BetRecDebFinale2"),
  Tournesol = c("TouSemenDebFinale2", "TouSemisDebFinale2", "TouPestDebFinale2","TouRecDebFinale2"),
  Colza = c("ColSemenDebFinale2", "ColSemisDebFinale2", "ColPestDebFinale2","ColRecDebFinale2"),
  Tabac = c("TabacSemisDebFinale2", "TabacPestDebFinale2","TabacRecDebFinale2"),
  Arboriculture = c("ArbTailleDebFinale2", "ArbPestDebFinale2","ArbRecDebFinale2")
)

# Calculer la durée de chaque tâche (différence entre dates de début et de fin)
for (tache in unlist(taches_agricoles)) {
  fin_tache <- gsub("DebFinale2", "FinFinale2", tache)  # Trouver la colonne de fin correspondante
  data[[paste0("duree_", tache)]] <- data[[fin_tache]] - data[[tache]]  # Calcul de la durée
}

# Calculer le pourcentage de chaque tâche par rapport à la durée totale
for (tache in unlist(taches_agricoles)) {
  # Calculer la durée de la tâche
  fin_tache <- gsub("DebFinale2", "FinFinale2", tache)
  duree_tache <- data[[fin_tache]] - data[[tache]]
  
  # Calculer le pourcentage
  data[[paste0("pourcentage_", tache)]] <- 
    ifelse(data$duree_tot > 0, 
           (duree_tache / data$duree_tot) * 100, 
           NA)
}

# Créer ratio2 avec les pourcentages
ratio2 <- data %>%
  select(id, starts_with("pourcentage_"))

# Ajuster les noms de colonnes
colnames(ratio2) <- gsub("pourcentage_", "ratio_", colnames(ratio2))
colnames(ratio2) <- gsub("DebFinale2", "", colnames(ratio2))

# Aperçu des résultats
head(ratio2)


################################################################################
#                         ANALYSE EN COMPOSANTES PRINCIPALES                    #
################################################################################

################################################################################
# 1. Tableau des valeurs propres                                               #
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
# 2. Exécution de l'ACP et visualisation                                       #
################################################################################
res.pca = PCA(ratios, scale.unit = TRUE, graph = FALSE, ncp = 8)
# Extraire le pourcentage d'inertie cumulée

# Graphique du cercle des corrélations pour visualiser les variables
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



################################################################################
# 3. Matrice des corrélations variables-composantes principales                #
################################################################################
cor_matrix <- as.data.frame(round(res.pca$var$cor[, 1:4], 2))

# Appliquer le surlignage colonne par colonne
cor_matrix_html <- cor_matrix %>%
  mutate(across(everything(), ~ cell_spec(.x, "html",
                                          color = ifelse(abs(.x) > 0.6, "white",  # Blanc si fond rouge (>0.6)
                                                         ifelse(abs(.x) > 0.4, "white",   # Blanc si fond orange (>0.4)
                                                                ifelse(abs(.x) > 0.2, "black", "black"))),  # Noir si fond jaune (0.2<x≤0.4), Noir sinon
                                          background = ifelse(abs(.x) > 0.6, "red",   # Rouge si |x| > 0.6
                                                              ifelse(abs(.x) > 0.4, "orange",   # Orange si |x| > 0.4
                                                                     ifelse(abs(.x) > 0.2, "yellow", "transparent"))),  # Jaune si |x| > 0.2, sinon blanc
                                          align = "center")))


# Générer le tableau HTML avec kableExtra
cor_matrix_html %>%
  kbl(escape = FALSE, align = "c") %>%
  kable_styling(full_width = FALSE, font_size = 14)


################################################################################
#                         CLASSIFICATION AUTOMATIQUE (K-MEANS)                  #
################################################################################

################################################################################
# 1. Détermination du nombre optimal de clusters                               #
################################################################################
# La méthode du coude est utilisée pour déterminer le nombre optimal de clusters
fviz_nbclust(res.pca$var$coord,
             hcut,
             k.max = 12,
             method = "wss") +
  geom_vline(xintercept = 8,
             linetype = "dashed",
             color = "red")

clust_data <- res.pca$ind$coord[, 1:8]


################################################################################
# 2. Effectifs par cluster                                                     #
################################################################################
set.seed(123) 
# 1. Appliquer K-Means
km.res <- kmeans(clust_data, centers = 8, nstart = 50)

# 2. Créer un tableau des fréquences des clusters
cluster_counts <- as.data.frame(table(km.res$cluster))
colnames(cluster_counts) <- c("Cluster", "Effectif") 

# Top 4 des clusters avec le plus grands effectif
top_4_effectifs <- cluster_counts %>% 
  arrange(desc(Effectif)) %>% 
  head(4) %>% 
  pull(Effectif)

# Appliquer la mise en forme pour les 4 plus grands Effectif
cluster_counts_html <- cluster_counts %>% 
  mutate(
    Effectif = cell_spec(Effectif, "html", 
                         color = ifelse(Effectif %in% top_4_effectifs, "white", "black"),  
                         background = ifelse(Effectif %in% top_4_effectifs, "red", "white"), 
                         align = "center")
  )

# Générer le tableau HTML avec kableExtra
cluster_counts_html %>%
  kbl(escape = FALSE, align = "c") %>%
  kable_styling(full_width = FALSE, font_size = 14)


################################################################################
# 3. Cultures les plus fréquentes                                              #
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
  select(-Culture) -> top_8_cult_table

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
                          color = ifelse(Fréquence %in% top_3_freq, "white", "black"),  
                          background = ifelse(Fréquence %in% top_3_freq, "red", "white"),  
                          align = "center")
  )


# Générer le tableau HTML avec kableExtra
culture_matrix_html %>%
  kbl(escape = FALSE, align = "c") %>%
  kable_styling(full_width = FALSE, font_size = 14)


################################################################################
#                     ANALYSE DES CLUSTERS ET INTERPRÉTATION                    #
################################################################################

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


tableau_synthese_html <- tableau_synthese %>%
  rownames_to_column(var = "Cluster") %>%
  rowwise() %>%
  mutate(across(-Cluster,  
                ~ ifelse(as.numeric(gsub("\\s*\\(.*\\)", "", .x)) == 
                           max(as.numeric(gsub("\\s*\\(.*\\)", "", c_across(everything()))), na.rm = TRUE),  
                         cell_spec(.x, "html", align = "center", bold = TRUE,  
                                   background = "red", color = "white"),  
                         cell_spec(.x, "html", align = "center")))) %>%
  ungroup()  


# Générer le tableau HTML avec kableExtra 
tableau_synthese_html %>%
  kbl(escape = FALSE, align = "c") %>%
  kable_styling(full_width = FALSE, font_size = 14)


################################################################################
#              ANALYSE DÉTAILLÉE DES CARACTÉRISTIQUES DES CLUSTERS             #
################################################################################

km_res_df <- data.frame(ID = names(km.res$cluster), cluster = km.res$cluster)

merged_df <- merge(ratios, km_res_df, by.x = 0, by.y = "ID")
names(merged_df)[1] <- "ID" 

cluster_sizes <- table(merged_df$cluster)

top_clusters <- sort(cluster_sizes, decreasing = TRUE)[1:4]
top_cluster_ids <- as.integer(names(top_clusters))  

filtered_df <- merged_df[merged_df$cluster %in% top_cluster_ids, ]
filtered_df$cluster <- as.factor(filtered_df$cluster)  

# Remplacer les valeurs manquantes par la moyenne
filtered_df <- filtered_df %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

names(filtered_df)[which(names(filtered_df) == "ID")[2]] <- "ID_duplicated"

numeric_cols <- sapply(filtered_df, is.numeric)
numeric_cols["cluster"] <- FALSE 
var_names <- names(filtered_df)[numeric_cols]

result_df <- data.frame(Variable = var_names)

# Calculer les moyennes par cluster
for (cl in top_cluster_ids) {
  cluster_data <- filtered_df[filtered_df$cluster == cl, ]
  means <- sapply(var_names, function(var) mean(cluster_data[[var]], na.rm = TRUE))
  col_name <- paste0("Moyenne_C", cl)
  result_df[[col_name]] <- means
}

global_means <- sapply(var_names, function(var) mean(merged_df[[var]], na.rm = TRUE))
result_df$Moyenne_Globale <- global_means  

# Analyse des variables caractéristiques par cluster
res.catdes <- catdes(filtered_df, num.var = which(names(filtered_df) == "cluster"), proba = 1)


bon_cluster <- as.numeric(names(res.catdes$quanti))

final_df <- data.frame(Variable = rownames(res.catdes$quanti[[bon_cluster[1]]]))

# Extraction des moyennes et v-test par cluster
for (cl in bon_cluster) {
  mean_values <- res.catdes$quanti[[as.character(cl)]][, "Mean in category"]
  v_test_values <- res.catdes$quanti[[as.character(cl)]][, "v.test"]
  
  final_df[[paste0("Moy c", cl)]] <- mean_values[match(final_df$Variable, rownames(res.catdes$quanti[[as.character(cl)]]))]
  final_df[[paste0("v-test c", cl)]] <- v_test_values[match(final_df$Variable, rownames(res.catdes$quanti[[as.character(cl)]]))]
}

final_df$Moyenne <- rowMeans(sapply(bon_cluster, function(cl) {
  res.catdes$quanti[[as.character(cl)]][, "Mean in category"]
}), na.rm = TRUE)

final_df[,-1] <- round(final_df[,-1], 3)  

# Réorganiser les colonnes
ordered_cols <- c("Variable")
for (cl in bon_cluster) {
  ordered_cols <- c(ordered_cols, paste0("Moy c", cl), paste0("v-test c", cl))
}

ordered_cols <- c(ordered_cols, "Moyenne")

final_df <- final_df[, ordered_cols]

vtest_cols <- grep("^v-test", names(final_df))
for (col in vtest_cols) {
  final_df[[col]] <- as.numeric(as.character(final_df[[col]]))
}
formatted_df <- final_df

# Mise en forme conditionnelle du tableau final
moy_cols <- grep("^Moy", names(formatted_df))
vtest_cols <- grep("^v-test", names(formatted_df))

for (i in 1:length(moy_cols)) {
  moy_col <- moy_cols[i]
  vtest_col <- vtest_cols[i]
  
  # Mettre en évidence les valeurs significatives
  for (row in 1:nrow(formatted_df)) {
    if (!is.na(formatted_df[row, vtest_col]) && 
        is.numeric(formatted_df[row, vtest_col]) && 
        abs(formatted_df[row, vtest_col]) > 10) {
      formatted_df[row, moy_col] <- cell_spec(
        formatted_df[row, moy_col], 
        "html", 
        color = "white", 
        background = "red"
      )
    }
  }
}


# Génération du tableau HTML final
kbl(formatted_df, escape = FALSE, align = "c") %>%
  kable_styling(full_width = FALSE, font_size = 14)
