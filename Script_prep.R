##############################################################################
##############################################################################
# Auteur : Boukola GBAYE


# Chargement des librairies
library(readxl) #pour lire le fichier Excel
library(tidyr) #pour dépivoter
library(purrr) #pour automatiser


# Chargement des données


# Récupérer les noms de nos feuilles
sheet_names = excel_sheets("Energy statistical country datasheets 2025-08 for web.xlsx")
print(sheet_names)
print(length(sheet_names))

#Fonction de traitement d'une feuille par son indice
donnees_feuille = function(i){
  # Charger la feuille du pays i
  data = read_excel("Energy statistical country datasheets 2025-08 for web.xlsx", 
                    sheet = i,
                    col_names = FALSE )
  
  # Récupération du nom du pays (colonne D, ligne 5)
  nom_pays = data[[5, 4]]
  #Quand on fait nom_pays = data[5,4], R extrait un sous-data frame (ou tibble) 
  # de la seule cellule, mais pas un vecteur simple, alors que nous on veut le nom même 
  
  # On coupe selon les données qui nous intéressent, ici, ce sont celles de Installed Electricity 
  # Capacity [MW] ligne 286 jusqu'à ligne 294, colonnes C à AK (3 à 37)
  data_interet = data[286 : 294, 3 : 37]
  
  # Il nous faut mettre les noms de colonnes pour notre df d'intérêt
  noms_colonnes = c("Type", as.character(1990:2023))
  
  # Assignation des noms de colonnes
  colnames(data_interet) = noms_colonnes
  
  # Dépivotage du df avec pivot_longer de tidyr
  data_dep = pivot_longer(data_interet,
                          cols = -"Type", #toutes les colonnes sont à dépivoter sauf Tupe
                          names_to = "Year", #les anciennes colonnes seront rangées sous Year
                          values_to = "Power" #les anciennes valeurs dans les cases seront sous Power
                          )
  
  # Ajout d'une colonne avec le nom du pays
  data_dep$Country = nom_pays
  
  # On renvoie le df final
  return(data_dep)
}


# Application de la fonction à toutes les feuilles du classeur Excel et compilation des résultats
# avec le map_dfr de purrr
# map_dfr va permettre d'appliquer la fonction à chaque feuille, mais aussi de directement compiler
# le résultat dans un dataframe

dataset = map_dfr(.x = 3:(length(sheet_names)-3), # les feuilles des pays vont de 3 au nbre de feuilles-1
                  .f = donnees_feuille)

# On vérifie la structure du dataframe
str(dataset)

# Correction des types
within(dataset,
       {
         Year = as.numeric(Year);
         Country = as.factor(Country);
         Type = as.factor(Type);
         Power = as.numeric(Power);
       })->dataset_tp
 
str(dataset_tp)


# Enregistrement du jeu de données produit
saveRDS(object = dataset_tp,
        file = "Electricity_Power_UE.RDS")

write.csv(dataset_tp, file = "Electricity_Power_UE.csv", row.names = FALSE)
