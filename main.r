main <- function() {

#stockons le dataset dans la variable df

# df = read.csv(file = 'dataset_credit_card.csv',sep=',',header = T,na.strings = '?')
df = read.csv(file = 'dataset_credit_card.csv',sep=',',header = T,na.strings = '?')

#D'après la commande suivante il n'y a pas de valeurs manquantes
#print(paste('Number of missing values =',sum(is.na(df))))



#pretaitement
df = pretaitement(df)
    #summary(df)
  df
}

