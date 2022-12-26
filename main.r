main <- function() {

#stockons le dataset dans la variable df

# df = read.csv(file = 'dataset_credit_card.csv',sep=',',header = T,na.strings = '?')
df = read.csv(file = 'dataset_credit_card.csv',sep=',',header = T,na.strings = '?')

#D'après la commande suivante il n'y a pas de valeurs manquantes
#print(paste('Number of missing values =',sum(is.na(df))))



#pretaitement
  df = pretaitement(df)
    #summary(df)
  set.seed(1) #assure que train et test donne la même valeur a chaque éxecution du code, on peut passer un nbr =! de 1 a set.seed

  #u 70% des données pour training set et 30% pour test set
  sample <- sample(c(TRUE, FALSE), nrow(df), replace=TRUE, prob=c(0.7,0.3))
  train  <- df[sample, ]
  test   <- df[!sample, ]

#extraction des règles
  # rules = extract_rules(df) #uncomment to test
  # rules #decommenter pour voir les prmières règles

#tree model
  # confusion_matrix = supervisedLearnModelGenerator(train, test, model_type ='tree') #uncomment to test
  # get_model_details(confusion_matrix)

#nnet model
  # confusion_matrix = supervisedLearnModelGenerator(train, test,model_type ='neuralnet') #uncomment to test
  # get_model_details(confusion_matrix)

#knn model
  # confusion_matrix = supervisedLearnModelGenerator(train, test,model_type ='knn') #uncomment to test
  # get_model_details(confusion_matrix)

#svm model
  # confusion_matrix = supervisedLearnModelGenerator(train, test,model_type ='svm') #uncomment to test
  # get_model_details(confusion_matrix)

#hierachical clust
  unsupervisedLearnModelGenerator(df,model_type ='hierachical clust') #uncomment to test

}
