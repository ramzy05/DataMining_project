main <- function() {

#stockons le dataset dans la variable df

# df = read.csv(file = 'dataset_credit_card.csv',sep=',',header = T,na.strings = '?')
df = read.csv(file = 'dataset_credit_card.csv',sep=',',header = T,na.strings = '?')

#D'après la commande suivante il n'y a pas de valeurs manquantes
#print(paste('Number of missing values =',sum(is.na(df))))

.

#pretaitement
  df = pretaitement(df)
    #summary(df)
  set.seed(1)

  #u 70% des données pour training set et 30% pour test set
  sample <- sample(c(TRUE, FALSE), nrow(df), replace=TRUE, prob=c(0.7,0.3))
  train  <- df[sample, ]
  test   <- df[!sample, ]

#extraction des règles
  rules = extract_rules(df)
  # rules #decommenter pour voir les prmières règles
}

