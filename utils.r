

pretaitement <- function (df){
  # install.packages("dplyr")
  library(dplyr)#fonction pour normaliser les colonnes numériques


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

  # Fin tranformations les colonones qualitatives en facteur

  #transformation les colonnes montant en nombre
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

# min_max_norm <- function(x) {
#     return (x - min(x)) / (max(x) - min(x))
# }
 extract_rules <- function(df) {
    # install.packages("arules")
    library(Matrix)
    library(arules)
    df_disc = discretizeDF(df)
    trans = transactions(df_disc)
    rules <- apriori(trans, parameter=list(supp = 0.4, conf = 0.9, target = "rules",maxtime=0))
    df_rules <- as(rules, "data.frame") 
    # return (df_disc)
    return (inspect(head(rules, n = 10, by = "lift")))
 }

 supervisedLearnModelGenerator <- function(train, test, model_type) {
    predict_y = test$Y #
    if(model_type == 'tree'){
      # install.packages("rpart")
      # install.packages("rpart.plot")
      library(rpart)
      library(rpart.plot)
      fitted_model <- rpart(Y~., data = train, method = 'class')
      rpart.plot(fitted_model, extra = 106)
    }
    else if(model_type == 'neuralnet'){
      # install.packages('nnet')
      library(nnet)
      fitted_model <- nnet(Y~., data = train, size = 10)

    }else if(model_type == 'knn'){
      library(lattice)
      library(grid)
      library(DMwR)#ou library(DMwR2)
      fitted_model <- kNN(Y~., train, test, norm=FALSE, k = 5)
    }

    else if(model_type == 'svm'){
      # install.packages("e1071")
      library(e1071)
      
      fitted_model = svm(Y ~ .,train)
    }


    if(model_type =='knn'){#ce test est fait pour eviter une erreur lors de l'appel de la fonction predict
      predict_y = fitted_model#
    }else{
    predict_y = predict(fitted_model, test, type = 'class')
    }
    print(paste(model_type,'model'))
    confusion_matrix = as.data.frame.matrix(table(test$Y, predict_y))
    rownames(confusion_matrix) = c('F','V')
    colnames(confusion_matrix) = c('F','V')
    get_precision_details(confusion_matrix)
 }

 get_precision_details <- function(conf_matrix){
    print(conf_matrix)
    precision = mean(c(
        conf_matrix['V','V']/(conf_matrix['V','V'] + conf_matrix['F','V']),
        conf_matrix['F','F']/(conf_matrix['F','F'] + conf_matrix['V','F'])
      )) 

    rappel = mean(c(
        conf_matrix['V','V']/(conf_matrix['V','V'] + conf_matrix['V','F']),
        conf_matrix['F','F']/(conf_matrix['F','F'] + conf_matrix['F','V'])
      )) 
    mesure = (2 * rappel * precision)/(rappel + precision)
   
    precision = c(paste(round(precision*100, digits=2),'%'))
    rappel = c(paste(round(rappel*100, digits=2),'%'))
    # mesure = c(((conf_matrix['V','V'] * 2) / (2*conf_matrix['V','V'] + conf_matrix['F','V'] + conf_matrix['V','F']))=
    mesure = c(paste(round(mesure*100, digits=2),'%'))
    return(
      data.frame(
        precision,
        rappel,
        mesure
      )
    )
 }