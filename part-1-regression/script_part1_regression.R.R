## Étude des déterminants de la performance environnementale des entreprises de EUROSTOXX 600:

#Définir un répertoire de travail:
setwd("C:/Users/Visiteur/Desktop/projet R")

#Charger les données:
install.packages("readxl")
library(readxl)
EurStox<-read_excel("variables_EuroStoxx600.xlsx")

#Vérifier les premières lignes pour confirmer l'importation de la base de données: 
head(EurStox)

## Structure des données / Résumé statistique sur notre Base de Données / Affichage de la base de donnée:
str(EurStox)
summary(EurStox)
View(EurStox)

## Q1: # Justificatif de l'exclusion de la variable "Carbon Intensity per Energy Produced"
str(EurStox$`Carbon Intensity per Energy Produced`)
unique_values <- unique(EurStox$`Carbon Intensity per Energy Produced`)
print(unique_values)


##Partie C: Dataviz, statistiques descriptives et régression linéaire :
# Créer un nouveau dataframe avec les variables spécifiées à l'étude:
colnames(EurStox)
new_EurStox <- EurStox[, c(
  "Environment Pillar Score", 
  "COUNTRY OF DOMICIL", 
  "EMPLOYEES", 
  "OPERATING PROFIT MARGIN", 
  "NET SALES OR REVENUES", 
  "Market Cap\r\n(Σ=Avg)", 
  "RETURN ON INVESTED CAPITAL", 
  "CSR Sustainability Committee", 
  "Value - Board Structure/Independent Board Members",
   "YTD Price PCT Change\r\n(Σ=Avg)"
)]

# Afficher les premières lignes du nouveau dataframe:
head(new_EurStox)

# Renommer les colonnes spécifiques à l'étude:
colnames(new_EurStox) <- c(
  "EPS",  # Environment Pillar Score
  "COUNTRY OF DOMICIL", 
  "EMP",  # EMPLOYEES
  "MARGIN",  # OPERATING PROFIT MARGIN
  "SALES",  # NET SALES OR REVENUES
  "MCAP",  # Market Cap
  "RCAP",  # RETURN ON INVESTED CAPITAL
  "CSR",  # CSR Sustainability Committee
  "Board",  # Value - Board Structure/Independent Board Members
  "YTD" #YTD Price PCT Change\r\n(Σ=Avg)
)
# Vérifier les noms des colonnes après le renommage:
print(colnames(new_EurStox))

## Structure des données / Résumé statistique / Affichage de la nouvelle base de donnée:

head(new_EurStox)
View(new_EurStox)
str(new_EurStox)
summary(new_EurStox)

#Nettoyage de la base de données: 
# Vérifier les valeurs manquantes
valeurs_manquantes <- colSums(is.na(new_EurStox))
print(valeurs_manquantes)

#conversion des variables qualitatives 
new_EurStox$CSR <- as.factor(new_EurStox$CSR)  # Conversion en facteur
new_EurStox$`COUNTRY OF DOMICIL` <- as.factor(new_EurStox$`COUNTRY OF DOMICIL`)

#conversion des variables quantitatives : 
convert_to_numeric <- function(column_name) {
  # Identifier les valeurs non numériques
  non_numeric_values <- column_name[is.na(as.numeric(column_name))]
  cat("Valeurs non numériques dans la colonne :", unique(non_numeric_values), "\n")
  
  # Nettoyer la colonne en supprimant les caractères non numériques
  # Remplacez les valeurs non numériques:
  column_name <- gsub("[^0-9.-]", "", column_name)  # Conserve seulement chiffres, point, et tiret
  column_name <- gsub("^\\s+|\\s+$", "", column_name) # Enlever les espaces au début et à la fin
  
  # Convertir la colonne en numérique
  column_name <- as.numeric(column_name)
  
  return(column_name)
}

# Appliquer la fonction de conversion à chaque colonne
new_EurStox$EPS <- convert_to_numeric(new_EurStox$EPS)
new_EurStox$EMP <- convert_to_numeric(new_EurStox$EMP)
new_EurStox$MARGIN <- convert_to_numeric(new_EurStox$MARGIN)
new_EurStox$SALES <- convert_to_numeric(new_EurStox$SALES)
new_EurStox$RCAP <- convert_to_numeric(new_EurStox$RCAP)
new_EurStox$Board <- convert_to_numeric(new_EurStox$Board)
new_EurStox$YTD <- convert_to_numeric(new_EurStox$YTD)

# Vérifier les valeurs manquantes après conversion
valeurs_manquantes <- colSums(is.na(new_EurStox))
print(valeurs_manquantes)

str(new_EurStox)
dim(new_EurStox)

## Supprimer les valeurs manquantes: 
new_EurStox <- na.omit(new_EurStox)
dim(new_EurStox) 

summary(new_EurStox)

#Statistiques descriptives pour les variables quantitatives détaillées: 
install.packages("psych")  
library(psych)  
describe(new_EurStox[, c("EPS", "EMP", "MARGIN", "SALES", "MCAP", "RCAP", "Board", "YTD")])



#Statistiques descriptives pour les variables qualitatives détaillées:
# Fréquence et pourcentage des modalités pour COUNTRY OF DOMICIL
freq_country <- table(new_EurStox$`COUNTRY OF DOMICIL`)
percent_country <- prop.table(freq_country) * 100  # Conversion des proportions en pourcentages

cat("Fréquence des modalités pour COUNTRY OF DOMICIL:\n")
print(freq_country)

cat("\nPourcentages des modalités pour COUNTRY OF DOMICIL:\n")
print(round(percent_country, 2))  # Arrondir à deux décimales

# Fréquence et pourcentage des modalités pour CSR
freq_csr <- table(new_EurStox$CSR)
percent_csr <- prop.table(freq_csr) * 100  # Conversion des proportions en pourcentages

cat("\nFréquence des modalités pour CSR:\n")
print(freq_csr)

cat("\nPourcentages des modalités pour CSR:\n")
print(round(percent_csr, 2))  # Arrondir à deux décimales

## Data visualisation :
##visualisation des données quantitatives: 
# Création d'histogrammes pour les variables quantitatives
par(mfrow = c(2, 4))  # Affichage multiple de 2 lignes x 4 colonnes pour optimiser l'espace

# Histogramme pour chaque variable
hist(new_EurStox$EPS, main = "Distribution de EPS", xlab = "EPS", col = "skyblue", breaks = 30)
hist(new_EurStox$EMP, main = "Distribution de EMP", xlab = "EMP", col = "lightgreen", breaks = 30)
hist(new_EurStox$MARGIN, main = "Distribution de MARGIN", xlab = "MARGIN", col = "lightcoral", breaks = 30)
hist(new_EurStox$SALES, main = "Distribution de SALES", xlab = "SALES", col = "lightpink", breaks = 30)
hist(new_EurStox$MCAP, main = "Distribution de MCAP", xlab = "MCAP", col = "lightyellow", breaks = 30)
hist(new_EurStox$RCAP, main = "Distribution de RCAP", xlab = "RCAP", col = "lightblue", breaks = 30)
hist(new_EurStox$Board, main = "Distribution de Board", xlab = "Board", col = "lavender", breaks = 30)
hist(new_EurStox$YTD, main = "Distribution YTD ", xlab = "PRICE", col = "orange", breaks = 30)

par(mfrow = c(1, 1))  # Réinitialiser l'affichage multiple

# Création de boxplots pour chaque variable
par(mfrow = c(2, 4))  # Affichage multiple de 2 lignes x 4 colonnes

boxplot(new_EurStox$EPS, main = "Boxplot de EPS", ylab = "EPS", col = "skyblue")
boxplot(new_EurStox$EMP, main = "Boxplot de EMP", ylab = "EMP", col = "lightgreen")
boxplot(new_EurStox$MARGIN, main = "Boxplot de MARGIN", ylab = "MARGIN", col = "lightcoral")
boxplot(new_EurStox$SALES, main = "Boxplot de SALES", ylab = "SALES", col = "lightpink")
boxplot(new_EurStox$MCAP, main = "Boxplot de MCAP", ylab = "MCAP", col = "lightyellow")
boxplot(new_EurStox$RCAP, main = "Boxplot de RCAP", ylab = "RCAP", col = "lightblue")
boxplot(new_EurStox$Board, main = "Boxplot de Board", ylab = "Board", col = "lavender")
boxplot(new_EurStox$YTD, main = "Boxplot de IF", ylab = "YTD", col = "red")

par(mfrow = c(1, 1))  # Réinitialiser l'affichage multiple


#visualisation des variables qualitatives:
# Charger ggplot2 pour la visualisation
install.packages("ggplot2")
library(ggplot2)
 #  graphique à barres pour la variable COUNTRY OF DOMICIL:
ggplot(data = new_EurStox, aes(x = `COUNTRY OF DOMICIL`)) +
  geom_bar(fill = "purple", color = "black") +
  labs(title = "Distribution des Pays de Domiciliation", x = "Pays", y = "Nombre d'Observations") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotation pour une meilleure lisibilité


# Graphique à barres pour CSR (présence d'un comité de durabilité)
ggplot(data = new_EurStox, aes(x = CSR)) +
  geom_bar(fill = "orange", color = "black") +
  labs(title = "Présence d'un Comité de Durabilité (CSR)", x = "Présence CSR", y = "Nombre d'Observations") +
  theme_minimal()

# Calculer la fréquence des pays pour construire un diagramme en camembert
country_freq <- table(new_EurStox$`COUNTRY OF DOMICIL`)
country_df <- as.data.frame(country_freq)
colnames(country_df) <- c("Pays", "Fréquence")

# Diagramme en camembert pour COUNTRY OF DOMICIL
ggplot(country_df, aes(x = "", y = Fréquence, fill = Pays)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Répartition des Pays de Domiciliation") +
  theme_void() +
  theme(legend.position = "right")
###
# Calculer la fréquence de CSR pour construire un diagramme en camembert
csr_freq <- table(new_EurStox$CSR)
csr_df <- as.data.frame(csr_freq)
colnames(csr_df) <- c("CSR", "Fréquence")

# Diagramme en camembert pour CSR
ggplot(csr_df, aes(x = "", y = Fréquence, fill = CSR)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Répartition de la Présence CSR") +
  theme_void() +
  theme(legend.position = "right")


## Modèle de régression linéaire multiple
# Sélectionner les variables numériques
variables_numeriques <- new_EurStox[, c("EMP", "MARGIN", "SALES", "MCAP", "RCAP", "Board", "YTD", "EPS")]

# Calculer la matrice de corrélation
matrice_corr <- cor(variables_numeriques)

# Afficher la matrice de corrélation
print(matrice_corr)
names(new_EurStox)
#Rééchelonner la variable pour spécifier la référence :
new_EurStox$`COUNTRY OF DOMICIL` <- relevel(new_EurStox$`COUNTRY OF DOMICIL`, ref = "FR")
# Modèle de regression linéaire sans YTD
modele_eps <- lm(EPS ~ `COUNTRY OF DOMICIL` + EMP + MARGIN + SALES + MCAP + RCAP + CSR + Board, data = new_EurStox)

# Afficher le résumé du modèle
summary(modele_eps)
# Modèle de regréssion linéaire en ajoutant la  variable financière YTD:
modele_eps_avec_YTD <- lm(EPS ~ `COUNTRY OF DOMICIL` + EMP + MARGIN + SALES + MCAP + RCAP + CSR + Board+YTD, data = new_EurStox)
# Afficher le résumé du modèle:
summary(modele_eps_avec_YTD)
# Installer et charger le package car si nécessaire
install.packages("car")
library(car)

# Calculer les VIF pour vérifier la multicolinéarité
vif( modele_eps_avec_YTD )


 