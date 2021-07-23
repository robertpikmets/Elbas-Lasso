library(tidyverse); library(lubridate); library(zoo); library(glmnet)

####### merging all data frames #######

vwap_op_merge <- left_join(op_merge, vwap_data, by = c("delivery_day", "delivery_hour"))

#missing value imputation
vwap_op_merge <- vwap_op_merge %>% missing_val()

merge_all <- left_join(vwap_op_merge, tidy_spot, by = c("delivery_day", "weekday", "delivery_hour"))
merge_all2 <- left_join(merge_all, regu_data, by = c("delivery_day", "weekday", "delivery_hour"))
merge_all3 <- left_join(merge_all2, join_latest, by = c("delivery_day", "delivery_hour"))
merge_all3 <- merge_all3 %>% 
  missing_val()

#merge_all3 <- readRDS("merge_all3.rds")

variables3 <- merge_all3 %>% 
  ungroup() %>% 
  select(-delivery_day)

#define initial training set, validation and test set
calibration_window <- 364
validation_window <- 91
test_window <- nrow(variables3)/24-(calibration_window+validation_window)


###### Baseline models ######

calc_errors <- function(errors){
  mae <- sum(abs(errors))/length(errors)
  rmse <- (sum(errors**2)/length(errors))**(1/2)
  return(cbind(mae, rmse))
}

y_var <- merge_all3$near_vwap
val_idx <- (24*calibration_window+1):(24*(calibration_window+validation_window))
test_idx <- (length(y_var)-24*test_window+1):length(y_var)

#baseline 1 - far-VWAP 
baseline1_errors_val <- variables3$far_vwap[val_idx] - y_var[val_idx]
baseline1_errors_test <- variables3$far_vwap[test_idx] - y_var[test_idx]
baseline1_val <- calc_errors(baseline1_errors_val)
baseline1_test <- calc_errors(baseline1_errors_test)

#baseline 2 latest-VWAP
baseline2_errors_val <- variables3$latest_vwap[val_idx] - y_var[val_idx]
baseline2_errors_test <- variables3$latest_vwap[test_idx] - y_var[test_idx]
baseline2_val <- calc_errors(baseline2_errors_val)
baseline2_test <- calc_errors(baseline2_errors_test)

#baseline 3 SE3 price
baseline3_errors_val <- variables3$SE3_spot[val_idx] - y_var[val_idx]
baseline3_errors_test <- variables3$SE3_spot[test_idx] - y_var[test_idx]
baseline3_val <- calc_errors(baseline3_errors_val)
baseline3_test <- calc_errors(baseline3_errors_test)

#baseline 4 h-5 near-vwap
baseline4_errors_val <- variables3$h5_near_vwap[val_idx] - y_var[val_idx]
baseline4_errors_test <- variables3$h5_near_vwap[test_idx] - y_var[test_idx]
baseline4_val <- calc_errors(baseline4_errors_val)
baseline4_test <- calc_errors(baseline4_errors_test)

#baseline 5 d-1 near-vwap
baseline5_errors_val <- variables3$d1_near_vwap[val_idx] - y_var[val_idx]
baseline5_errors_test <- variables3$d1_near_vwap[test_idx] - y_var[test_idx]
baseline5_val <- calc_errors(baseline5_errors_val)
baseline5_test <- calc_errors(baseline5_errors_test)


###### LASSO models ######

freq <- 24 #univariate model window rolled forward by 24 hours
lambda_vec_longer <- 10**(-(31-c(1:31))/6) #from 0.00001 to 1, for univariate
lambda_vec_mv3 <- c(10**(-(10-c(4:19))/6)) #for initial multivariate framework
lambda_vec <- 10**(-(19-c(1:15))/6) # from 0.001 to 0.22, for adjusted multivariate framework


##### UNIVARIATE #####

#this function builds a univariate model, given a index for lambda vector
build_validation_model <- function(x_vars, y_var, calibration_window, validation_window, freq, lambda, lambda_vec){
  
  errors <- rep(NA, validation_window*freq)
  
  #roll the window by 24 (freq) hours each model calibration
  for (i in 0:(validation_window-1)) {
    #set train and validation indexes, which will be used to index x_vars and y_var
    train <- (1+freq*i):(freq*(calibration_window+i))
    validation <- (freq*(calibration_window+i)+1):(freq*(calibration_window+i+1))
    
    #train a Lasso model on a 364-day calibration window aka training data
    model <- glmnet(x_vars[train,], y_var[train], alpha = 1, lambda = lambda_vec[lambda])
    
    pred <- predict(model, s = lambda_vec[lambda], newx = x_vars[validation,])
    errors[(1+i*freq):(i*freq+freq)] <- pred-y_var[validation]
  }
  return(errors)
}

#this function iterates over a given lambda vector and calls build_validation_model
validate_lambda <- function(df, calibration_window, validation_window, freq, lambda_vec) {
  
  start.time <- Sys.time()
  
  y_var <- df$near_vwap
  x_vars <- model.matrix(near_vwap~. , df)[,-1]
  
  mae <- rep(NA,length(lambda_vec))
  rmse <- rep(NA,length(lambda_vec))
  error_matrix <- matrix(NA, nrow = validation_window*freq, ncol = length(lambda_vec))
  
  for (lambda in 1:length(lambda_vec)) {
    errors <- build_validation_model(x_vars, y_var, calibration_window, validation_window, freq, lambda, lambda_vec)
    
    error_matrix[,lambda] <- errors
    mae[lambda] <- sum(abs(errors))/length(errors)
    rmse[lambda] <- (sum(errors**2)/length(errors))**(1/2)
  }
  
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
  
  return(list(error_matrix, as.data.frame(cbind(lambda_vec, mae, rmse))))
}

#this model takes in a given lambda value and calculates prediction errors on the test set
evaluate_test_set <- function(df, lambda, calibration_window, validation_window, freq) {
  
  start.time <- Sys.time()
  
  test_window <- nrow(df)/24-(calibration_window+validation_window)
  y_var <- df$near_vwap
  x_vars <- model.matrix(near_vwap~. , df)[,-1] #drops the intercept, added by glmnet automatically
  
  predictions <- rep(NA, test_window*freq)
  model_coefs <- matrix(nrow = test_window, ncol = ncol(x_vars)+1) #+1 for intercept
  
  #roll the window by 24 (freq) hours each model calibration
  for (i in 0:(test_window-1)) {
    #shift forward by validation window, assume validation is done
    train <- (1+freq*(validation_window+i)):(freq*(calibration_window+validation_window+i))
    #first test day is 24 hours of 2017-03-31
    test <- (freq*(calibration_window+validation_window+i)+1):(freq*(calibration_window+validation_window+i+1))
    
    #train a Lasso model on a 364-day calibration window aka training data
    model <- glmnet(x_vars[train,], y_var[train], alpha = 1, lambda = lambda)
    model_coefs[(i+1),] <- matrix(coef(model))[,1]
    
    pred <- predict(model, s = lambda, newx = x_vars[test,])
    predictions[(1+i*freq):(i*freq+freq)] <- pred
  }
  errors <- predictions - y_var[(length(y_var)-24*test_window+1):length(y_var)]
  coef_names <- dimnames(coef(model))[[1]] #save coefficient names once
  
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
  
  return(list(cbind(predictions, errors), model_coefs, coef_names))
}


###### MULTIVARIATE ######

#given an index for the lambda vector, builds the model
build_validation_model_mv <- function(x_vars, y_var, calibration_window, validation_window, freq, lambda, lambda_vec){
  
  errors <- matrix(NA, nrow = validation_window, ncol = freq)
  
  for (i in 0:(validation_window-1)) {
    train <- (1+1*i):(1*(calibration_window+i))
    validation <- (1*(calibration_window+i)+1):(1*(calibration_window+i+1))
    
    y_train <- y_var[train,]
    
    for (j in c(1:24)) {
      x_train <- x_vars[x_vars$delivery_hour == j,][train,] %>% select(-delivery_hour)
      x_train_matrix <- model.matrix(near_vwap~. , x_train)[,-1]
      
      x_val <- x_vars[x_vars$delivery_hour == j,][validation,] %>% select(-delivery_hour)
      x_val_matrix <- t(as.matrix(model.matrix(near_vwap~. , x_val)[,-1]))
      
      y_var_train <- pull(y_train[,j]) #pull gets the vector from a tibble/dataframe
      
      #model is trained on the training set and prediction made on validation set
      #lambda here is an index for the vector of lambda values (lambda_vec)
      model <- glmnet(x_train_matrix, y_var_train, alpha = 1, lambda = lambda_vec[lambda])
      pred <- predict(model, s = lambda_vec[lambda], newx = x_val_matrix)
      errors[i+1,j] <- pred-pull(y_var[i+validation,j])
    }
  }
  return(errors)
}

#iterates over a vector of lambda values, calls build_validation_model_mv each time
validate_lambda_mv <- function(df, calibration_window, validation_window, freq, lambda_vec) {
  
  start.time <- Sys.time()
  
  y_var <- df %>% select(c("delivery_hour", "near_vwap"))
  
  y_var_mv <- y_var %>% 
    group_by(delivery_hour) %>% 
    mutate(row = row_number()) %>% #has to be done for unique distinguishing of each data point
    pivot_wider(names_from = delivery_hour, values_from = near_vwap, names_prefix = "hour") %>% 
    select(-row)
  
  mae_matrix <- matrix(NA, nrow=length(lambda_vec), ncol=24)
  rmse_matrix <- matrix(NA, nrow=length(lambda_vec), ncol=24)
  
  #three-dimensional error array
  error_array <- array(NA, c(validation_window, freq, length(lambda_vec)))
  
  for (lambda in 1:length(lambda_vec)) {
    errors <- build_validation_model_mv(df, y_var_mv, calibration_window, validation_window, freq, lambda, lambda_vec)
    
    error_array[,,lambda] <- errors
    mae_matrix[lambda,] <- colSums(abs(errors))/nrow(errors)
    rmse_matrix[lambda,] <- (colSums(errors**2)/nrow(errors))**(1/2)
    print(lambda) #for keeping track when code is running
    
  }
  
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
  
  return(list(mae_matrix, rmse_matrix, error_array))
}

#function parameter lambda has to be a vector of 24 indexes for lambda_vec
#assumes validation is done, calculates model errors on the test set
evaluate_test_set_mv <- function(df, lambda, calibration_window, validation_window, freq, lambda_vec) {
  
  start.time <- Sys.time()
  
  test_window <- nrow(df)/24-(calibration_window+validation_window)
  
  y_var <- df %>% select(c("delivery_hour", "near_vwap"))
  
  y_var_mv <- y_var %>% 
    group_by(delivery_hour) %>% 
    mutate(row = row_number()) %>% #has to be done for unique distinguishing of each data point
    pivot_wider(names_from = delivery_hour, values_from = near_vwap, names_prefix = "hour") %>% 
    select(-row)
  
  errors <- matrix(NA, nrow = test_window, ncol = freq)
  model_coefs <- array(NA, c(test_window, 24, 
                             dim(model.matrix(near_vwap~. , df %>% select(-delivery_hour)))[2]))
  
  for (i in 0:(test_window-1)) {
    #shift forward by validation window, assume validation is done
    train <- (1+1*(validation_window+i)):(1*(calibration_window+validation_window+i))
    test <- (1*(calibration_window+validation_window+i)+1):(1*(calibration_window+validation_window+i+1))
    
    y_train <- y_var_mv[train,]
    
    for (j in c(1:24)) {
      #filter x variables to only contain data for given hour j
      x_train <- df[df$delivery_hour == j,][train,] %>% select(-delivery_hour)
      x_train_matrix <- model.matrix(near_vwap~. , x_train)[,-1]
      
      #test set
      x_val <- df[df$delivery_hour == j,][test,] %>% select(-delivery_hour)
      #transposing model matrix is necessary because R treats a single row dataframe
      #as a vector and a bug arises, which is fixed with t() command
      x_val_matrix <- t(as.matrix(model.matrix(near_vwap~. , x_val)[,-1]))
      
      y_var_train <- pull(y_train[,j])
      
      model <- glmnet(x_train_matrix, y_var_train, alpha = 1, lambda = lambda_vec[lambda[j]])
      pred <- predict(model, s = lambda_vec[lambda[j]], newx = x_val_matrix)
      
      errors[(i+1),j] <- pred-pull(y_var_mv[test,j])
      model_coefs[(i+1),j,] <- matrix(coef(model))[,1]
    }
    
    coef_names <- dimnames(coef(model))[[1]] #coefficient names saved once
    print(i) #keeping track
  }
  
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
  
  return(list(errors, model_coefs, coef_names))
}

##### Univariate results #####

#validation results
uni_val_results <- validate_lambda(variables3, calibration_window, validation_window, freq, lambda_vec_longer)
#best lambda based on RMSE criteria
uni_lambda <- lambda_vec_longer[which.min(uni_val_results[[2]]$rmse)]
#test set results with previously found optimal lambda
uni_test_results <- evaluate_test_set(variables3, uni_lambda, calibration_window, validation_window, freq)
#uni_test_results <- readRDS("uni_test_results.rds")

#validation set accuracy
uni_val_mae <- uni_val_results[[2]]$mae[which.min(uni_val_results[[2]]$rmse)]
uni_val_rmse <- uni_val_results[[2]]$rmse[which.min(uni_val_results[[2]]$rmse)]

#test set accuracy
uni_test_set_errors <- uni_test_results[[1]][,2]
test_set_accuracy <- calc_errors(uni_test_set_errors)


#new part for defence:
#Can/could one observe differences in forecast performance for 
#the period April 2017-June 2018 and for July 2018-December 2020?

nrow(merge_all3[test_idx,][merge_all3[test_idx,]$delivery_day<"2018-07-01",])
#10968
#April 2017-June 2018
uni_test_set_errors[1:10968]
#July 2018-December 2020
uni_test_set_errors[(10968+1):length(uni_test_set_errors)]

calc_errors(uni_test_set_errors[1:10968])
calc_errors(uni_test_set_errors[(10968+1):length(uni_test_set_errors)])



###### Multivariate results ######

#validation set, initial lambda vector
mv_val_results <- validate_lambda_mv(variables3, calibration_window, validation_window, freq, lambda_vec_mv3)

calc_mv_val_error <- function(mv_val_results) {
  mv_val_rmse <- sum(apply(mv_val_results[[2]], 2, min))/24
  mv_val_mae <- sum(apply(mv_val_results[[1]], 2, min))/24
  return(cbind(mv_val_mae,mv_val_rmse))
}

calc_mv_val_error(mv_val_results)

#a vector of 24 indexes, each specifying the optimal lambda for all 24 delivery hours of the day
mv_lambda <- apply(mv_val_results[[2]], 2, which.min)
#test set results
mv_test_results <- evaluate_test_set_mv(variables3, mv_lambda, calibration_window, validation_window, freq, lambda_vec_mv3)

#RMSE of multivariate test set
(sum(abs(mv_test_results[[1]])**2)/(24*nrow(mv_test_results[[1]])))**(1/2)
#MAE of multivariate test set
sum(abs(mv_test_results[[1]]))/(24*nrow(mv_test_results[[1]]))


##### Adjusted multivariate framework #####
#less variables and smaller lambda values

#occurrence of predictors in univariate framework is counted on all test set days
coef_names <- uni_test_results[[3]]
model_coefs <- uni_test_results[[2]]
coef_count <- colSums(model_coefs != 0) #non zero coefficients
coef_percent <- coef_count/nrow(model_coefs)

coef_data <- as.data.frame(cbind(coef_names, coef_count, coef_percent))
coef_data$coef_count <- as.numeric(as.character(coef_data$coef_count))
coef_data$coef_percent <- as.numeric(as.character(coef_data$coef_percent))

#new part for defence:
#how many explanatory variables are fitted on average?
counts <- coef_data$coef_count[2:length(coef_data$coef_count)]
sum(counts)/1372

#get top 20 variables, excluding intercept, which is the first element
top_sorted_coef <- coef_data[order(-coef_count),][2:21,]
#bottom 10 variables
bot_sorted_coef <- coef_data[order(coef_count),][1:10,]

top_sorted_coef_names <- as.character(top_sorted_coef$coef_names)
#manually rename factor variables, e.g. DD_FI1 to DD_FI
#because entire variable has to be chosen
top_sorted_coef_names[top_sorted_coef_names=="DD_FI1"] <- "DD_FI"
#last 3 are delivery_hour9, delivery_hour12, weekday7
#d7_near_vwap as 21st most frequent substitutes one of the delivery_hour variables
top_sorted_coef_names <- top_sorted_coef_names[1:(length(top_sorted_coef_names)-3)]
top_20_coef_names <- c(top_sorted_coef_names, "delivery_hour", "weekday", "d7_near_vwap")

#subset the main dataframe of x variables
variables3_small <- variables3[,top_20_coef_names]
variables3_small$near_vwap <- variables3$near_vwap #add near-vwap (output) as well

#smaller lambda values, from 0.001 to 0.215
mv_val_results_small <- validate_lambda_mv(variables3_small, calibration_window, validation_window, freq, lambda_vec)
#validation set errors
calc_mv_val_error(mv_val_results_small)

#determine best lambdas
mv_lambda_small <- apply(mv_val_results_small[[2]], 2, which.min)

#test set results
mv_test_results_small <- evaluate_test_set_mv(variables3_small, mv_lambda_small, calibration_window, validation_window, freq, lambda_vec)
#test set RMSE
(sum(abs(mv_test_results_small[[1]])**2)/(24*nrow(mv_test_results_small[[1]])))**(1/2)
#test set MAE
sum(abs(mv_test_results_small[[1]]))/(24*nrow(mv_test_results_small[[1]]))

