main <- function() {
   
#taper les commandes suivantes dans le terminal pour executer le projet

#  source('utils.r')
#  source('main.r')

#stockons le dataset dans la variable df

df = read.csv(file = 'dataset_credit_card.csv',sep=',',header = T,na.strings = '?')

#D'après la commande suivante il n'y a pas de valeurs manquantes
#print(paste('Number of missing values =',sum(is.na(df))))



#pretaitement
df = pretaitement(df)
    print(df[1,])
}
