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

################################################################################
# --------------------------  Importation des données  ----------------------- #
################################################################################
data = readRDS("../Data/data1975Agrican")
