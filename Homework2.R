
 library(readr)
 library(mice)
 library(hydroGOF)
 library(Amelia)
 library(mi)
 library(missForest)
 
 #load the input dataset
 Iris <- read_csv("~/Downloads/Iris.csv")
 View(Iris)
 #ignore the serial numbers and create a new dataset
 original_iris_dataset = Iris[-1]
 
 #function to insert NA at random
 insert_na_values = function(df,x,y){
        df[ sample(c(TRUE, NA), prob = c(x, y), size = length(df), replace = TRUE) ]
 }
 
#dataframe after inserting 2% na values
 df_with_na = as.data.frame(apply(original_iris_dataset[-5],MARGIN =2,FUN = function(sample_df) insert_na_values(sample_df,x = 0.98,y = 0.02)))
 summary(df_with_na)
 #Method 1 - Mice
 #impute NA values using mice package
 imputation_using_mice =  mice(df_with_na,m=1,maxit=50,meth='pmm',seed=500)
 densityplot(imputation_using_mice)
 #build a new dataframe with replacing NA with imputed values
 df_with_imputed_values_mice = mice::complete(imputation_using_mice,1)
#compute RMSE fr original iris dataset VS the dataset with imputed values using mice
 rmse(df_with_imputed_values_mice,original_iris_dataset[-5])
 #use knn to predict the class
 # we use k= 1 in this case - since the accuracy is high 
 df_with_imputed_values_mice$species = knn(train = original_iris_dataset[-5], test = df_with_imputed_values_mice, cl= original_iris_dataset$Species,k = 1)
 confusionMatrix(original_iris_dataset$Species,df_with_imputed_values_mice$species)
 #Method 2 -Amelia
 #impute NA values using Amelia package
 imputation_using_amelia = amelia(df_with_na, m=1, parallel = "multicore")
 summary(imputation_using_amelia)
 df_with_imputed_values_amelia = imputation_using_amelia$imputations[[1]]
 rmse(df_with_imputed_values_amelia,original_iris_dataset[-5])
 #use knn to predict the class
 # we use k= 1 in this case - since the accuracy is high 
 library(class)
 df_with_imputed_values_amelia$species = knn(train = original_iris_dataset[-5], test = df_with_imputed_values_amelia, cl= original_iris_dataset$Species,k = 1)
 confusionMatrix(original_iris_dataset$Species,df_with_imputed_values_amelia$species)
#save(a.out, file = "imputations.RData")
 #Method 3 - missForest
 imputation_using_missforest = missForest(df_with_na)
 df_with_imputed_values_missforest = imputation_using_missforest$ximp
 rmse(df_with_imputed_values_missforest,original_iris_dataset[-5])
 #use knn to predict the class
 # we use k= 1 in this case - since the accuracy is high 
 library(class)
 df_with_imputed_values_missforest$species = knn(train = original_iris_dataset[-5], test = df_with_imputed_values_missforest, cl= original_iris_dataset$Species,k = 1)
 confusionMatrix(original_iris_dataset$Species,df_with_imputed_values_missforest$species)
 #Method 4 - mi
 imputation_using_mi = mi(df_with_na,n.iter = 2)
 plot(imputation_using_mi)
 df_with_imputed_values_mi = mi::complete(imputation_using_mi,m= 1)
 summary(df_with_imputed_values_mi)
 rmse(df_with_imputed_values_mi[1:4],original_iris_dataset[-5])
 #use knn to predict the class
 # we use k= 1 in this case - since the accuracy is high 
 library(class)
 df_with_imputed_values_mi$species = knn(train = original_iris_dataset[-5], test = df_with_imputed_values_mi[1:4], cl= original_iris_dataset$Species,k = 1)
 confusionMatrix(original_iris_dataset$Species,df_with_imputed_values_mi$species)
 
 #dataframe after inserting 5% na values
 df_with_na = as.data.frame(apply(original_iris_dataset[-5],MARGIN =2,FUN = function(sample_df) insert_na_values(sample_df,x = 0.95,y = 0.05)))
 summary(df_with_na)
 #Method 1 - Mice
 #impute NA values using mice package
 imputation_using_mice =  mice(df_with_na,m=1,maxit=50,meth='pmm',seed=500)
 densityplot(imputation_using_mice)
 #build a new dataframe with replacing NA with imputed values
 df_with_imputed_values_mice = mice::complete(imputation_using_mice,1)
 #compute RMSE fr original iris dataset VS the dataset with imputed values using mice
 rmse(df_with_imputed_values_mice,original_iris_dataset[-5])
 #use knn to predict the class
 # we use k= 1 in this case - since the accuracy is high 
 df_with_imputed_values_mice$species = knn(train = original_iris_dataset[-5], test = df_with_imputed_values_mice, cl= original_iris_dataset$Species,k = 1)
 confusionMatrix(original_iris_dataset$Species,df_with_imputed_values_mice$species)
 #Method 2 -Amelia
 #impute NA values using Amelia package
 imputation_using_amelia = amelia(df_with_na, m=1, parallel = "multicore")
 summary(imputation_using_amelia)
 df_with_imputed_values_amelia = imputation_using_amelia$imputations[[1]]
 rmse(df_with_imputed_values_amelia,original_iris_dataset[-5])
 #use knn to predict the class
 # we use k= 1 in this case - since the accuracy is high 
 library(class)
 df_with_imputed_values_amelia$species = knn(train = original_iris_dataset[-5], test = df_with_imputed_values_amelia, cl= original_iris_dataset$Species,k = 1)
 confusionMatrix(original_iris_dataset$Species,df_with_imputed_values_amelia$species)
 #save(a.out, file = "imputations.RData")
 #Method 3 - missForest
 imputation_using_missforest = missForest(df_with_na)
 df_with_imputed_values_missforest = imputation_using_missforest$ximp
 rmse(df_with_imputed_values_missforest,original_iris_dataset[-5])
 #use knn to predict the class
 # we use k= 1 in this case - since the accuracy is high 
 library(class)
 df_with_imputed_values_missforest$species = knn(train = original_iris_dataset[-5], test = df_with_imputed_values_missforest, cl= original_iris_dataset$Species,k = 1)
 confusionMatrix(original_iris_dataset$Species,df_with_imputed_values_missforest$species)
 #Method 4 - mi
 imputation_using_mi = mi(df_with_na,n.iter = 2)
 plot(imputation_using_mi)
 df_with_imputed_values_mi = mi::complete(imputation_using_mi,m= 1)
 summary(df_with_imputed_values_mi)
 rmse(df_with_imputed_values_mi[1:4],original_iris_dataset[-5])
 #use knn to predict the class
 # we use k= 1 in this case - since the accuracy is high 
 library(class)
 df_with_imputed_values_mi$species = knn(train = original_iris_dataset[-5], test = df_with_imputed_values_mi[1:4], cl= original_iris_dataset$Species,k = 1)
 confusionMatrix(original_iris_dataset$Species,df_with_imputed_values_mi$species)
 
 #dataframe after inserting 10% na values
 df_with_na = as.data.frame(apply(original_iris_dataset[-5],MARGIN =2,FUN = function(sample_df) insert_na_values(sample_df,x = 0.90,y = 0.1)))
 summary(df_with_na)
 #Method 1 - Mice
 #impute NA values using mice package
 imputation_using_mice =  mice(df_with_na,m=1,maxit=50,meth='pmm',seed=500)
 densityplot(imputation_using_mice)
 #build a new dataframe with replacing NA with imputed values
 df_with_imputed_values_mice = mice::complete(imputation_using_mice,1)
 #compute RMSE fr original iris dataset VS the dataset with imputed values using mice
 rmse(df_with_imputed_values_mice,original_iris_dataset[-5])
 #use knn to predict the class
 # we use k= 1 in this case - since the accuracy is high 
 df_with_imputed_values_mice$species = knn(train = original_iris_dataset[-5], test = df_with_imputed_values_mice, cl= original_iris_dataset$Species,k = 1)
 confusionMatrix(original_iris_dataset$Species,df_with_imputed_values_mice$species)
 #Method 2 -Amelia
 #impute NA values using Amelia package
 imputation_using_amelia = amelia(df_with_na, m=1, parallel = "multicore")
 summary(imputation_using_amelia)
 df_with_imputed_values_amelia = imputation_using_amelia$imputations[[1]]
 rmse(df_with_imputed_values_amelia,original_iris_dataset[-5])
 #use knn to predict the class
 # we use k= 1 in this case - since the accuracy is high 
 library(class)
 df_with_imputed_values_amelia$species = knn(train = original_iris_dataset[-5], test = df_with_imputed_values_amelia, cl= original_iris_dataset$Species,k = 1)
 confusionMatrix(original_iris_dataset$Species,df_with_imputed_values_amelia$species)
 #save(a.out, file = "imputations.RData")
 #Method 3 - missForest
 imputation_using_missforest = missForest(df_with_na)
 df_with_imputed_values_missforest = imputation_using_missforest$ximp
 rmse(df_with_imputed_values_missforest,original_iris_dataset[-5])
 #use knn to predict the class
 # we use k= 1 in this case - since the accuracy is high 
 library(class)
 df_with_imputed_values_missforest$species = knn(train = original_iris_dataset[-5], test = df_with_imputed_values_missforest, cl= original_iris_dataset$Species,k = 1)
 confusionMatrix(original_iris_dataset$Species,df_with_imputed_values_missforest$species)
 #Method 4 - mi
 imputation_using_mi = mi(df_with_na,n.iter = 2)
 plot(imputation_using_mi)
 df_with_imputed_values_mi = mi::complete(imputation_using_mi,m= 1)
 summary(df_with_imputed_values_mi)
 rmse(df_with_imputed_values_mi[1:4],original_iris_dataset[-5])
 #use knn to predict the class
 # we use k= 1 in this case - since the accuracy is high 
 library(class)
 df_with_imputed_values_mi$species = knn(train = original_iris_dataset[-5], test = df_with_imputed_values_mi[1:4], cl= original_iris_dataset$Species,k = 1)
 confusionMatrix(original_iris_dataset$Species,df_with_imputed_values_mi$species)
 
 #dataframe after inserting 15% na values
 df_with_na = as.data.frame(apply(original_iris_dataset[-5],MARGIN =2,FUN = function(sample_df) insert_na_values(sample_df,x = 0.85,y = 0.15)))
 summary(df_with_na)
 #Method 1 - Mice
 #impute NA values using mice package
 imputation_using_mice =  mice(df_with_na,m=1,maxit=50,meth='pmm',seed=500)
 densityplot(imputation_using_mice)
 #build a new dataframe with replacing NA with imputed values
 df_with_imputed_values_mice = mice::complete(imputation_using_mice,1)
 #compute RMSE fr original iris dataset VS the dataset with imputed values using mice
 rmse(df_with_imputed_values_mice,original_iris_dataset[-5])
 #use knn to predict the class
 # we use k= 1 in this case - since the accuracy is high 
 df_with_imputed_values_mice$species = knn(train = original_iris_dataset[-5], test = df_with_imputed_values_mice, cl= original_iris_dataset$Species,k = 1)
 confusionMatrix(original_iris_dataset$Species,df_with_imputed_values_mice$species)
 #Method 2 -Amelia
 #impute NA values using Amelia package
 imputation_using_amelia = amelia(df_with_na, m=1, parallel = "multicore")
 summary(imputation_using_amelia)
 df_with_imputed_values_amelia = imputation_using_amelia$imputations[[1]]
 rmse(df_with_imputed_values_amelia,original_iris_dataset[-5])
 #use knn to predict the class
 # we use k= 1 in this case - since the accuracy is high 
 library(class)
 df_with_imputed_values_amelia$species = knn(train = original_iris_dataset[-5], test = df_with_imputed_values_amelia, cl= original_iris_dataset$Species,k = 1)
 confusionMatrix(original_iris_dataset$Species,df_with_imputed_values_amelia$species)
 #save(a.out, file = "imputations.RData")
 #Method 3 - missForest
 imputation_using_missforest = missForest(df_with_na)
 df_with_imputed_values_missforest = imputation_using_missforest$ximp
 rmse(df_with_imputed_values_missforest,original_iris_dataset[-5])
 #use knn to predict the class
 # we use k= 1 in this case - since the accuracy is high 
 library(class)
 df_with_imputed_values_missforest$species = knn(train = original_iris_dataset[-5], test = df_with_imputed_values_missforest, cl= original_iris_dataset$Species,k = 1)
 confusionMatrix(original_iris_dataset$Species,df_with_imputed_values_missforest$species)
 #Method 4 - mi
 imputation_using_mi = mi(df_with_na,n.iter = 2)
 plot(imputation_using_mi)
 df_with_imputed_values_mi = mi::complete(imputation_using_mi,m= 1)
 summary(df_with_imputed_values_mi)
 rmse(df_with_imputed_values_mi[1:4],original_iris_dataset[-5])
 #use knn to predict the class
 # we use k= 1 in this case - since the accuracy is high 
 library(class)
 df_with_imputed_values_mi$species = knn(train = original_iris_dataset[-5], test = df_with_imputed_values_mi[1:4], cl= original_iris_dataset$Species,k = 1)
 confusionMatrix(original_iris_dataset$Species,df_with_imputed_values_mi$species)
 
 #dataframe after inserting 20% na values
 df_with_na = as.data.frame(apply(original_iris_dataset[-5],MARGIN =2,FUN = function(sample_df) insert_na_values(sample_df,x = 0.8,y = 0.2)))
 summary(df_with_na)
 #Method 1 - Mice
 #impute NA values using mice package
 imputation_using_mice =  mice(df_with_na,m=1,maxit=50,meth='pmm',seed=500)
 densityplot(imputation_using_mice)
 #build a new dataframe with replacing NA with imputed values
 df_with_imputed_values_mice = mice::complete(imputation_using_mice,1)
 #compute RMSE fr original iris dataset VS the dataset with imputed values using mice
 rmse(df_with_imputed_values_mice,original_iris_dataset[-5])
 #use knn to predict the class
 # we use k= 1 in this case - since the accuracy is high 
 df_with_imputed_values_mice$species = knn(train = original_iris_dataset[-5], test = df_with_imputed_values_mice, cl= original_iris_dataset$Species,k = 1)
 confusionMatrix(original_iris_dataset$Species,df_with_imputed_values_mice$species)
 #Method 2 -Amelia
 #impute NA values using Amelia package
 imputation_using_amelia = amelia(df_with_na, m=1, parallel = "multicore")
 summary(imputation_using_amelia)
 df_with_imputed_values_amelia = imputation_using_amelia$imputations[[1]]
 rmse(df_with_imputed_values_amelia,original_iris_dataset[-5])
 #use knn to predict the class
 # we use k= 1 in this case - since the accuracy is high 
 library(class)
 df_with_imputed_values_amelia$species = knn(train = original_iris_dataset[-5], test = df_with_imputed_values_amelia, cl= original_iris_dataset$Species,k = 1)
 confusionMatrix(original_iris_dataset$Species,df_with_imputed_values_amelia$species)
 #save(a.out, file = "imputations.RData")
 #Method 3 - missForest
 imputation_using_missforest = missForest(df_with_na)
 df_with_imputed_values_missforest = imputation_using_missforest$ximp
 rmse(df_with_imputed_values_missforest,original_iris_dataset[-5])
 #use knn to predict the class
 # we use k= 1 in this case - since the accuracy is high 
 library(class)
 df_with_imputed_values_missforest$species = knn(train = original_iris_dataset[-5], test = df_with_imputed_values_missforest, cl= original_iris_dataset$Species,k = 1)
 confusionMatrix(original_iris_dataset$Species,df_with_imputed_values_missforest$species)
 #Method 4 - mi
 imputation_using_mi = mi(df_with_na,n.iter = 2)
 plot(imputation_using_mi)
 df_with_imputed_values_mi = mi::complete(imputation_using_mi,m= 1)
 summary(df_with_imputed_values_mi)
 rmse(df_with_imputed_values_mi[1:4],original_iris_dataset[-5])
 #use knn to predict the class
 # we use k= 1 in this case - since the accuracy is high 
 library(class)
 df_with_imputed_values_mi$species = knn(train = original_iris_dataset[-5], test = df_with_imputed_values_mi[1:4], cl= original_iris_dataset$Species,k = 1)
 confusionMatrix(original_iris_dataset$Species,df_with_imputed_values_mi$species)
 
 #dataframe after inserting 25% na values
 df_with_na = as.data.frame(apply(original_iris_dataset[-5],MARGIN =2,FUN = function(sample_df) insert_na_values(sample_df,x = 0.75,y = 0.25)))
 summary(df_with_na)
 #Method 1 - Mice
 #impute NA values using mice package
 imputation_using_mice =  mice(df_with_na,m=1,maxit=50,meth='pmm',seed=500)
 densityplot(imputation_using_mice)
 #build a new dataframe with replacing NA with imputed values
 df_with_imputed_values_mice = mice::complete(imputation_using_mice,1)
 #compute RMSE fr original iris dataset VS the dataset with imputed values using mice
 rmse(df_with_imputed_values_mice,original_iris_dataset[-5])
 #use knn to predict the class
 # we use k= 1 in this case - since the accuracy is high 
 df_with_imputed_values_mice$species = knn(train = original_iris_dataset[-5], test = df_with_imputed_values_mice, cl= original_iris_dataset$Species,k = 1)
 confusionMatrix(original_iris_dataset$Species,df_with_imputed_values_mice$species)
 #Method 2 -Amelia
 #impute NA values using Amelia package
 imputation_using_amelia = amelia(df_with_na, m=1, parallel = "multicore")
 summary(imputation_using_amelia)
 df_with_imputed_values_amelia = imputation_using_amelia$imputations[[1]]
 rmse(df_with_imputed_values_amelia,original_iris_dataset[-5])
 #use knn to predict the class
 # we use k= 1 in this case - since the accuracy is high 
 library(class)
 df_with_imputed_values_amelia$species = knn(train = original_iris_dataset[-5], test = df_with_imputed_values_amelia, cl= original_iris_dataset$Species,k = 1)
 confusionMatrix(original_iris_dataset$Species,df_with_imputed_values_amelia$species)
 #save(a.out, file = "imputations.RData")
 #Method 3 - missForest
 imputation_using_missforest = missForest(df_with_na)
 df_with_imputed_values_missforest = imputation_using_missforest$ximp
 rmse(df_with_imputed_values_missforest,original_iris_dataset[-5])
 #use knn to predict the class
 # we use k= 1 in this case - since the accuracy is high 
 library(class)
 df_with_imputed_values_missforest$species = knn(train = original_iris_dataset[-5], test = df_with_imputed_values_missforest, cl= original_iris_dataset$Species,k = 1)
 confusionMatrix(original_iris_dataset$Species,df_with_imputed_values_missforest$species)
 #Method 4 - mi
 imputation_using_mi = mi(df_with_na,n.iter = 2)
 plot(imputation_using_mi)
 df_with_imputed_values_mi = mi::complete(imputation_using_mi,m= 1)
 summary(df_with_imputed_values_mi)
 rmse(df_with_imputed_values_mi[1:4],original_iris_dataset[-5])
 #use knn to predict the class
 # we use k= 1 in this case - since the accuracy is high 
 library(class)
 df_with_imputed_values_mi$species = knn(train = original_iris_dataset[-5], test = df_with_imputed_values_mi[1:4], cl= original_iris_dataset$Species,k = 1)
 confusionMatrix(original_iris_dataset$Species,df_with_imputed_values_mi$species)
 
 
#Part II
 iris_dataset = original_iris_dataset[-5]
 iris_dataset$SepalWidthCm[iris_dataset$SepalLengthCm > 6 ] = NA
 summary(iris_dataset)
 #Method1 - mice
 imputation_using_mice =  mice(iris_dataset,m=1,maxit=50,meth='pmm',seed=500)
 densityplot(imputation_using_mice)
 #build a new dataframe with replacing NA with imputed values
 df_with_imputed_values_mice = mice::complete(imputation_using_mice,1)
 #compute RMSE fr original iris dataset VS the dataset with imputed values using mice
 rmse(df_with_imputed_values_mice,original_iris_dataset[-5])
 #use knn to predict the class
 # we use k= 1 in this case - since the accuracy is high 
 df_with_imputed_values_mice$species = knn(train = original_iris_dataset[-5], test = df_with_imputed_values_mice, cl= original_iris_dataset$Species,k = 1)
 confusionMatrix(original_iris_dataset$Species,df_with_imputed_values_mice$species)
 #Method 2 -Amelia
 #impute NA values using Amelia package
 imputation_using_amelia = amelia(iris_dataset, m=1, parallel = "multicore")
 summary(imputation_using_amelia)
 df_with_imputed_values_amelia = imputation_using_amelia$imputations[[1]]
 rmse(df_with_imputed_values_amelia,original_iris_dataset[-5])
 #use knn to predict the class
 # we use k= 1 in this case - since the accuracy is high 
 library(class)
 df_with_imputed_values_amelia$species = knn(train = original_iris_dataset[-5], test = df_with_imputed_values_amelia, cl= original_iris_dataset$Species,k = 1)
 confusionMatrix(original_iris_dataset$Species,df_with_imputed_values_amelia$species)
 #save(a.out, file = "imputations.RData")
 #Method 3 - missForest
 class(iris_dataset) = class(df_with_na)
 imputation_using_missforest = missForest(iris_dataset)
 df_with_imputed_values_missforest = imputation_using_missforest$ximp
 rmse(df_with_imputed_values_missforest,original_iris_dataset[-5])
 #use knn to predict the class
 # we use k= 1 in this case - since the accuracy is high 
 library(class)
 df_with_imputed_values_missforest$species = knn(train = original_iris_dataset[-5], test = df_with_imputed_values_missforest, cl= original_iris_dataset$Species,k = 1)
 confusionMatrix(original_iris_dataset$Species,df_with_imputed_values_missforest$species)
 #Method 4 - mi
 imputation_using_mi = mi(iris_dataset,n.iter = 2)
 plot(imputation_using_mi)
 df_with_imputed_values_mi = mi::complete(imputation_using_mi,m= 1)
 summary(df_with_imputed_values_mi)
 rmse(df_with_imputed_values_mi[2],original_iris_dataset[2])
 i#use knn to predict the class
 # we use k= 1 in this case - since the accuracy is high 
 library(class)
 df_with_imputed_values_mi$species = knn(train = original_iris_dataset[-5], test = df_with_imputed_values_mi[1:4], cl= original_iris_dataset$Species,k = 1)
 confusionMatrix(original_iris_dataset$Species,df_with_imputed_values_mi$species)
 
 
 
 
 