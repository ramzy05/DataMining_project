
main <- function() {

#chargons le repertoire de travail 
setwd("~/Documents/Data_science/Data_Mining/Exercices_et_TPs/TP_final_datamining/DataMining_project-ram") 
  
  
#stockons le dataset dans la variable df

# df = read.csv(file = 'dataset_credit_card.csv',sep=',',header = T,na.strings = '?')
df = read.csv(file = 'dataset_credit_card.csv',sep=',',header = T,na.strings = '?')

#D'après la commande suivante il n'y a pas de valeurs manquantes
print(paste('Number of missing values =',sum(is.na(df))))

#pretaitemente
df = pretraitement(df)
#Analyse statistique descriptive des donnees et commentaires

Anal_statistique = Analyse_statistique(df)
Anal_statistique

#extraction des regles de classifications
classification_rules = extract_rules(df)
classification_rules
summary(classification_rules)

#Arbre de decision 
library(ggplot2)
arbre_de_decision = decision_tree(df)
arbre_de_decision
ggplot2(arbre_de_decision)
text(arbre_de_decision)

#matrice de confusion
matrice_de_confusion = matrice_conf(df) 
matrice_de_confusion

return(Anal_statistique , classification_rules ,arbre_de_decision , matrice_de_confusion )
}

