
##  Étude des déterminants de la performance environnementale des entreprises de EUROSTOXX 600
### Partie ACP:

# --- 1. Installation et chargement des bibliothèques nécessaires ---

install.packages("readxl")
install.packages("FactoMineR")
install.packages("factoextra")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("ggcorrplot")

library(readxl)
library(FactoMineR)
library(factoextra)
library(dplyr)
library(ggplot2)

# --- 2. Chargement des données ---
#Définir un répertoire de travail et charger la base de données:
setwd("C:/Users/Visiteur/Desktop/ACP DEVOIR")
EurStox <- read_excel("variables_EuroStoxx600.xlsx")

# Vérification initiale des données
cat("\nAperçu des données :\n")
head(EurStox)
str(EurStox)
summary(EurStox)
colnames(EurStox)

# Nettoyage des noms des colonnes :
colnames(EurStox) <- make.names(colnames(EurStox))

# --- 3. Préparation des données pour l'ACP ---
# Sélection des colonnes nécessaires pour l'ACP:
price_vars <- EurStox[, c(
  "X1.day.Price.PCT.Change...Σ.Avg.", 
  "X5.day.Price.PCT.Change...Σ.Avg.", 
  "X4.week.Price.PCT.Change...Σ.Avg.", 
  "X13.week.Price.PCT.Change...Σ.Avg.", 
  "X26.week.Price.PCT.Change...Σ.Avg.", 
  "YTD.Price.PCT.Change...Σ.Avg.", 
  "X52.week.Price.PCT.Change...Σ.Avg.",
  "Price.52.Week.High...Σ.Avg."
)]

# Renommer les colonnes :
colnames(price_vars) <- c(
  "1d_PCT", "5d_PCT", "4w_PCT", "13w_PCT", 
  "26w_PCT", "YTD_PCT", "52w_PCT", "Price_52w_High"
)
str (price_vars)
# Vérification des valeurs manquantes et Suppression des lignes contenant des valeurs manquantes
cat("\nValeurs manquantes par colonne :\n")
print(colSums(is.na(price_vars)))
price_vars <- na.omit(price_vars)

# --- 4. Vérification de la matrice de corrélation ---

cat("\nMatrice de corrélation :\n")
cor_matrix <- cor(price_vars)
print(cor_matrix)
library(ggcorrplot)
ggcorrplot(cor_matrix, lab = TRUE, lab_size = 3, colors = c("blue", "white", "red"), title = "Matrice de corrélation")

# --- 5. Analyse en Composantes Principales ---
# --- Représentation des individus dans l'ACP ---
# Lancer l'ACP sur les données normalisées
acp <- PCA(price_vars, scale.unit = TRUE, ncp = ncol(price_vars), graph = FALSE)

# Représentation graphique des individus dans l'espace des deux premières composantes principales
fviz_pca_ind(acp, 
             geom = "point",         
             pointsize = 1,          
             col.ind = "cos2",       
             gradient.cols = c("blue", "yellow", "red"), 
             repel = TRUE           
) + 
  ggtitle("Représentation des individus dans l'ACP") +
  theme_minimal()

# Visualisation de l'éboulis des valeurs propres
fviz_eig(acp, addlabels = TRUE, ylim = c(0, 50), 
         main = "Éboulis des valeurs propres (Scree Plot)")

# --- Contributions et qualités de représentation des variables ---
# Contributions des variables aux deux premières composantes principales
fviz_pca_var(acp, col.var = "contrib", 
             gradient.cols = c("blue", "yellow", "red"), 
             repel = TRUE) + 
  ggtitle("Contributions des variables aux composantes principales")

# Résumé des résultats de l'ACP
cat("\nValeurs propres et pourcentage d'inertie expliqué :\n")
print(acp$eig)

cat("\nContributions des variables aux premières composantes principales :\n")
print(acp$var$contrib)


# les contributions (ctr) et cos² des variables sur Dim.1 et Dim.2
contributions <- acp$var$contrib
cos2 <- acp$var$cos2

# Combiner les données dans un DataFrame
contrib_cos2_df <- data.frame(
  Variable = rownames(contributions),
  Dim1_Ctr = contributions[, 1],  # Contributions sur Dim.1
  Dim1_Cos2 = cos2[, 1],          # Cos² sur Dim.1
  Dim2_Ctr = contributions[, 2],  # Contributions sur Dim.2
  Dim2_Cos2 = cos2[, 2]           # Cos² sur Dim.2
)

# Afficher le tableau final
print(contrib_cos2_df)

# Sauvegarder les résultats dans un fichier CSV si nécessaire
write.csv(contrib_cos2_df, "Contributions_Cos2_Dim1_Dim2.csv", row.names = FALSE)
cat("Les contributions et cos² ont été sauvegardés dans 'Contributions_Cos2_Dim1_Dim2.csv'.\n")


# --- Extraction des composantes principales F1 et F2 ---
# Limiter le DataFrame à la taille des coordonnées (si ACP a éliminé des lignes)
EurStox <- EurStox[1:nrow(acp$ind$coord), ]

# Ajouter F1 (Dim.1) et F2 (Dim.2) au jeu de données
EurStox$F1 <- acp$ind$coord[, 1]  # Première composante principale
EurStox$F2 <- acp$ind$coord[, 2]  # Deuxième composante principale

# Vérification des distributions de F1 et F2
cat("\nDistribution des composantes principales (F1 et F2) :\n")
summary(EurStox$F1)
summary(EurStox$F2)

# --- Sauvegarder le dataframe avec les nouvelles variables F1 et F2 ---
write.csv(EurStox, "EurStox_with_F1_F2.csv", row.names = FALSE)
cat("Les résultats de l'ACP, y compris F1 et F2, ont été sauvegardés dans 'EurStox_with_F1_F2.csv'.\n")

# --- Régression linéaire avec F1 et F2 ---
# Charger les données mises à jour
EurStox <- read.csv("EurStox_with_F1_F2.csv")

# Renommer les colonnes pour simplifier leur utilisation
colnames(EurStox) <- make.names(colnames(EurStox))

# Préparation des données pour la régression
EurStox_regr <- EurStox[, c(
  "Environment.Pillar.Score",                      
  "COUNTRY.OF.DOMICIL",                             
  "EMPLOYEES",                                     
  "OPERATING.PROFIT.MARGIN",                        
  "NET.SALES.OR.REVENUES",                         
  "Market.Cap...Σ.Avg.",                            
  "RETURN.ON.INVESTED.CAPITAL",                     
  "CSR.Sustainability.Committee",                   
  "Value...Board.Structure.Independent.Board.Members", 
  "F1",                                             
  "F2"                                             
)]

# Renommer les colonnes pour les rendre plus lisibles
colnames(EurStox_regr) <- c(
  "EPS", "Country", "Employees", "Margin", 
  "Sales", "MarketCap", "ROIC", "CSR", "Board", "F1", "F2"
)

# Conversion des variables qualitatives en facteurs
EurStox_regr$CSR <- as.factor(EurStox_regr$CSR)
EurStox_regr$Country <- as.factor(EurStox_regr$Country)

# Conversion des variables quantitatives en numériques
convert_to_numeric <- function(column_name) {
  # Identifier les valeurs non numériques
  non_numeric_values <- column_name[is.na(as.numeric(column_name))]
  cat("Valeurs non numériques dans la colonne :", unique(non_numeric_values), "\n")
  
  # Nettoyer la colonne en supprimant les caractères non numériques
  column_name <- gsub("[^0-9.-]", "", column_name)  # Conserve chiffres, point et tiret
  column_name <- gsub("^\\s+|\\s+$", "", column_name) # Supprime les espaces
  column_name <- as.numeric(column_name)  # Convertit en numérique
  
  return(column_name)
}

# Appliquer la conversion aux colonnes pertinentes
numeric_columns <- c("EPS", "Employees", "Margin", "Sales", "MarketCap", "ROIC", "F1", "F2")
for (col in numeric_columns) {
  EurStox_regr[[col]] <- convert_to_numeric(EurStox_regr[[col]])
}

# Vérification des valeurs manquantes
valeurs_manquantes <- colSums(is.na(EurStox_regr))
print(valeurs_manquantes)

# Suppression des lignes avec des valeurs manquantes
EurStox_regr <- na.omit(EurStox_regr)

# --- Régression linéaire ---
# Création du modèle avec F1 et F2 comme variables explicatives
model <- lm(EPS ~ Country + Employees + Margin + Sales + MarketCap + ROIC + CSR + Board + F1 + F2, data = EurStox_regr)

# Résumé du modèle
summary(model)

# Création du modèle avec F1 (première composante) 
model2 <- lm(EPS ~ Country + Employees + Margin + Sales + MarketCap + ROIC + CSR + Board + F1 , data = EurStox_regr)

# Résumé du modèle
summary(model2)

# Création du modèle Sans variable financière:  
model3 <- lm(EPS ~ Country + Employees + Margin + Sales + MarketCap + ROIC + CSR + Board, data = EurStox_regr)

# Résumé du modèle
summary(model3)


# Création du modèle Avec F2:  
model4 <- lm(EPS ~ Country + Employees + Margin + Sales + MarketCap + ROIC + CSR + Board + F2, data = EurStox_regr)

# Résumé du modèle
summary(model4)


##Fin#
