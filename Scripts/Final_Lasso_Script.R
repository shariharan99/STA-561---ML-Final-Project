library(glmnet)

#Read in 50 imputed data set and store it in list
data.files <- list.files(path = 'data/Complete_Data_wdiver_Nov19', full.names = TRUE)
list.of.data <- lapply(data.files, read.table)

# Vector to store cv1se_lasso1 values
cv1se_lasso1 = 0

#Create matrix to store values on feature "diver" from linear regression
lm_diver_lasso1 = as.data.frame(matrix(0, ncol = 4, nrow = 50))
colnames(lm_diver_lasso1) = c("est", "se","tval", "pval")

#This function runs lm for each of the 50 imputed data sets, using variables from lasso
for(i in 1:length(list.of.data)){
  #Take first dataset
  temp = list.of.data[[i]]
  #remove "leiad11" col
  temp = temp[,-1]
  #extract reponse var "ALL_RATE_1112"
  y = temp[,2]
  #extract predictor matrix
  x = as.matrix(temp[,-2])
  #Run cv.lasso
  lasso_cv_obj = cv.glmnet(x, y)
  #Store lambda.1se value
  cv1se_lasso1[i] = lasso_cv_obj$lambda.1se
  #run lasso using cv1se_lasso1
  lasso.obj = glmnet(x, y, lambda = cv1se_lasso1[i])
  #Get significant indices of significant predictors from lasso, drop intercept and store in lm_data
  lm_data = x[,which(coef(lasso.obj)!=0)[-1]-1]
  #Combine predictors and response
  all_data = as.data.frame(cbind(y, lm_data))
  #Run linear regression
  lm_obj = lm(y~. ,data = all_data)
  #Store results for feature "diver"
  lm_diver_lasso1[i,] = summary(lm_obj)$coefficients["diver",]
}

lm_diver_lasso1 = cbind(lm_diver_lasso1, cv1se_lasso1)
write.table(lm_diver_lasso1, file = "Lasso1")

# Vector to store cv1se_lasso2 values
cv1se_lasso2 = 0

#Create matrix to store values on feature "diver" from linear regression
lm_diver_lasso2 = as.data.frame(matrix(0, ncol = 4, nrow = 50))
colnames(lm_diver_lasso2) = c("est", "se","tval", "pval")

#This function runs lm for each of the 50 imputed data sets, using variables from lasso and their interaction terms
for(i in 1:length(list.of.data)){
  #Take first dataset
  temp = list.of.data[[i]]
  #remove "leiad11" col
  temp = temp[,-1]
  #extract reponse var "ALL_RATE_1112"
  y = temp[,2]
  #extract predictor matrix
  x = as.matrix(temp[,-2])
  #Run cv.lasso
  lasso_cv_obj = cv.glmnet(x, y)
  #Store lambda.1se value
  cv1se_lasso2[i] = lasso_cv_obj$lambda.1se
  #run lasso using cv1se_lasso2
  lasso.obj = glmnet(x, y, lambda = cv1se_lasso2[i])
  #Get significant indices of significant predictors from lasso, drop intercept and store in lm_data
  lm_data = x[,which(coef(lasso.obj)!=0)[-1]-1]
  #Get matrix with interaction terms of lm_data
  lm_data_int = model.matrix(y~.^2, data = as.data.frame(cbind(y, lm_data)))
  #Combine interaction terms and squared preditors and response
  all_data = as.data.frame(cbind(y, lm_data_int))
  #Run linear regression
  lm_obj = lm(y~. ,data = all_data)
  #Store results for feature "diver"
  lm_diver_lasso2[i,] = summary(lm_obj)$coefficients["diver",]
}

lm_diver_lasso2 = cbind(lm_diver_lasso2, cv1se_lasso2)
write.table(lm_diver_lasso2, file = "lasso_lm_diver_val_wosq")

# Vector to store cv1se_lasso3 values
cv1se_lasso3 = 0

#Create matrix to store values on feature "diver" from linear regression
lm_diver_lasso3 = as.data.frame(matrix(0, ncol = 4, nrow = 50))
colnames(lm_diver_lasso3) = c("est", "se","tval", "pval")

#This function runs lm for each of the 50 imputed data sets, using variables from lasso, their squared terms and their interaction terms
for(i in 1:length(list.of.data)){
  #Take first dataset
  temp = list.of.data[[i]]
  #remove "leiad11" col
  temp = temp[,-1]
  #extract reponse var "ALL_RATE_1112"
  y = temp[,2]
  #extract predictor matrix
  x = as.matrix(temp[,-2])
  #Run cv.lasso
  lasso_cv_obj = cv.glmnet(x, y)
  #Store lambda.1se value
  cv1se_lasso3[i] = lasso_cv_obj$lambda.1se
  #run lasso using cv1se_lasso3
  lasso.obj = glmnet(x, y, lambda = cv1se_lasso3[i])
  #Get significant indices of significant predictors from lasso, drop intercept and store in lm_data
  lm_data = x[,which(coef(lasso.obj)!=0)[-1]-1]
  #Get matrix with squared predictors from lm_data
  lm_data_sq = lm_data^2
  colnames(lm_data_sq) = paste("sq", 1:ncol(lm_data_sq))
  #Get matrix with interaction terms of lm_data
  lm_data_int = model.matrix(y~.^2, data = as.data.frame(cbind(y, lm_data)))
  #Combine interaction terms and squared preditors and response
  all_data = as.data.frame(cbind(y, lm_data_int,lm_data_sq))
  #Run linear regression
  lm_obj = lm(y~. ,data = all_data)
  #Store results for feature "diver"
  lm_diver_lasso3[i,] = summary(lm_obj)$coefficients["diver",]
}

lm_diver_lasso3 = cbind(lm_diver_lasso3, cv1se_lasso1)
write.table(lm_diver_lasso3, file = "Lasso3")
