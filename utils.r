#chargons le repertoire de travail 
setwd("~/Documents/Data_science/Data_Mining/Exercices_et_TPs/TP_final_datamining") 

pretraitement <- function (df){
library(dplyr)#fonction pour normaliser les colonnes numériques
# library("dplyr")


  # Rénommons PAY_0 en PAY_1,comme ça on ne quitte plus de PAY_0 subitement à PAY_2
  df[1,][df[1,]=='PAY_0'] = 'PAY_1'
  #enlevons la ligne 1 qui contient la description de chaque colonne
  df = df[-1,-1]

  #tranformations les colonones qualitatives en facteur
  df$X2 = as.factor(df$X2)#sexe
  df$X3 = as.factor(df$X3)#education
  df$X4 = as.factor(df$X4)#mariage


  df$X6 = as.factor(df$X6)
  df$X7 = as.factor(df$X7)
  df$X8 = as.factor(df$X8)
  df$X9 = as.factor(df$X9)
  df$X10 = as.factor(df$X10)
  df$X11 = as.factor(df$X11)

  df$Y = as.factor(df$Y)#class

  # Fin tranformations des colonones qualitatives en facteur

  #transformons les colonnes montant en nombre
  df$X1 = as.numeric(as.character(df$X1))#limitBalance
  df$X5 = as.numeric(as.character(df$X5))#age
  cols_to_numeric = 12:23
  df[ ,cols_to_numeric] <- apply(df[ , cols_to_numeric], 2, 
                      function(x) as.numeric(as.character(x)))
  # Fin transformation les colonnes montant en nombre

  #Normalisation des colonnes montant X12-X23 ET X1
  df = df %>% mutate(across(where(is.numeric), scale)) #z-score
  # df$X1 = (df$X1 - min(df$X1)) / (max(df$X1) - min(df$X1))
  # df$X5 = (df$X5 - min(df$X5)) / (max(df$X5) - min(df$X5))
  # df[ ,cols_to_numeric] = apply(df[ , cols_to_numeric], 2, 
  #                     function(x)min_max_norm(x))
  # Fin Normalisation des colonnes montant X12-X23 ET X1
  
  
  return (df)
}

#Analyse statistique descriptive des donnees et commentaires
Analyse_statistique <-function(df){
  summary(df)
}

# min_max_norm <- function(x) {
#     return (x - min(x)) / (max(x) - min(x))
# }
 extract_rules <- function(df) {
    library(OneR)
   rules = OneR(Y ~ . , data = df)
   return(rules)
 }
 
#Construction du model a partie de l'arbre de decision 
 
decision_tree <-function(df){
  library(rpart)
  Ad = rpart(Y~. , df)
  return(Ad)
}

# matrice de confusion de l'arbre de decision
matrice_conf <- function(df){
  nt = sample(1:nrow(df), 0.7*nrow(df))
  train = df[nt ,]
  test = df[-nt ,]
  dt_train = decision_tree(train)
  prediction = predict(dt_train , test[,-24] , type = c("class"))
  matrice = table(test[,-24] ,prediction)

  return(matrice)
}

 
 