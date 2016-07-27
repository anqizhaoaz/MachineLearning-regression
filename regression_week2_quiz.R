
#### Regression Week 2: Multiple Linear Regression Assignment 1

## In this notebook you will use data on house sales in King County to predict prices using multiple regression. 
## The first assignment will be about exploring multiple regression in particular exploring the impact of adding features to a regression 
## and measuring error. In the second assignment you will implement a gradient descent algorithm.


## The algorithm
## In each step of the gradient descent we will do the following:
  
## 1. Compute the predicted values given the current slope and intercept
## 2. Compute the prediction errors (prediction - Y)
## 3. Update the intercept:
##    compute the derivative: sum(errors)
##    compute the adjustment as step_size times the derivative
##    decrease the intercept by the adjustment
## 4. Update the slope:
##    compute the derivative: sum(errors*input)
##    compute the adjustment as step_size times the derivative
##    decrease the slope by the adjustment
## 5. Compute the magnitude of the gradient
## 6. Check for convergence


## load train and test data
kc_train <- read.csv("C:/Users/apple/Desktop/regression/kc_house_train_data.csv", header=TRUE)
str(kc_train)
kc_test <- read.csv("C:/Users/apple/Desktop/regression/kc_house_test_data.csv", header=TRUE)
str(kc_test)

## creat enew variables
## 3. Although we often think of multiple regression as including multiple different features (e.g. # of bedrooms, square feet, and # of bathrooms) but we can also consider transformations of existing variables 
#  e.g. the log of the square feet or even "interaction" variables such as the product of bedrooms and bathrooms. Add 4 new variables in both your train_data and test_data.
kc_train$bedrooms_squared = kc_train$bedrooms * kc_train$bedrooms
kc_train$bed_bath_rooms = kc_train$bedrooms * kc_train$bathrooms
kc_train$log_sqft_living = log(kc_train$sqft_living)
kc_train$lat_plus_long = kc_train$lat + kc_train$long

kc_test$bedrooms_squared = kc_test$bedrooms * kc_test$bedrooms
kc_test$bed_bath_rooms = kc_test$bedrooms * kc_test$bathrooms
kc_test$log_sqft_living = log(kc_test$sqft_living)
kc_test$lat_plus_long = kc_test$lat + kc_test$long

## 4. Quiz Question: what are the mean (arithmetic average) values of your 4 new variables on TEST data? (round to 2 digits)
summary(kc_test)

## 5. estimate the regression coefficients/weights for predicting ‘price’ for the following three models
# Model 1: ‘sqft_living’, ‘bedrooms’, ‘bathrooms’, ‘lat’, and ‘long’
# Model 2: ‘sqft_living’, ‘bedrooms’, ‘bathrooms’, ‘lat’,‘long’, and ‘bed_bath_rooms’
# Model 3: ‘sqft_living’, ‘bedrooms’, ‘bathrooms’, ‘lat’,‘long’, ‘bed_bath_rooms’, ‘bedrooms_squared’, ‘log_sqft_living’, and ‘lat_plus_long’
model1 = lm(price ~ sqft_living + bedrooms + bathrooms + lat + long, data=kc_train)
model2 = lm(price ~ sqft_living + bedrooms + bathrooms + lat + long + bed_bath_rooms, data=kc_train)
model3 = lm(price ~ sqft_living + bedrooms + bathrooms + lat + long + bed_bath_rooms + bedrooms_squared + log_sqft_living + lat_plus_long, data=kc_train)

## 6. Quiz Question: What is the sign (positive or negative) for the coefficient/weight for ‘bathrooms’ in Model 1?
summary(model1)

## 7. Quiz Question: What is the sign (positive or negative) for the coefficient/weight for ‘bathrooms’ in Model 2?
summary(model2)

## 8. Is the sign for the coefficient the same in both models? Think about why this might be the case.
summary(model3)

## 9. Now using your three estimated models compute the RSS (Residual Sum of Squares) on the Training data
## 10. Quiz Question: Which model (1, 2 or 3) had the lowest RSS on TRAINING data?
sum((model1$residuals)^2)
sum((model2$residuals)^2)
sum((model3$residuals)^2)

## 11. Now using your three estimated models compute the RSS on the Testing data
## 12. Quiz Question: Which model (1, 2, or 3) had the lowest RSS on TESTING data?
rss_model1 = sum((kc_test$price - predict(model1, newdata=kc_test))^2)
rss_model1
rss_model2 = sum((kc_test$price - predict(model2, newdata=kc_test))^2)
rss_model2
rss_model3 = sum((kc_test$price - predict(model3, newdata=kc_test))^2)
rss_model3


#### Regression Week 2: Multiple Linear Regression Quiz 2
## Estimating Multiple Regression Coefficients (Gradient Descent)

#### 3.Next write a function that takes a data set, a list of features (e.g. [‘sqft_living’, ‘bedrooms’]), to be used as inputs, and a name of the output (e.g. ‘price’). 
##   This function should return a features_matrix (2D array) consisting of first a column of ones followed by columns containing the values of the input features in the data set in the same order as the input list. 
##   It should also return an output_array which is an array of the values of the output in the data set (e.g. ‘price’).
get_matrix = function(dataset, feature_name, output_name){
  dataset['constant'] = 1 # add a constant column
  feature_matrix = as.matrix(dataset[,c('constant', feature_name)])
  output_array = as.matrix(dataset[, c(output_name)])
  return(list(feature_matrix=feature_matrix, output=output_array))
}

## test get_matrix function
dataset=kc_train
feature_name = c('sqft_living','bedrooms')
output_name = c('price')

feature_matrix = (get_matrix(kc_train, feature_name, output_name))$feature_matrix
output = (get_matrix(kc_train, feature_name, output_name))$output


#### 4. Write a function ‘predict_output’ which accepts a 2D array ‘feature_matrix’ and a 1D array ‘weights’ and returns a 1D array ‘predictions’
predict_output = function(feature_matrix, weights){
  predictions = feature_matrix %*% weights
  return(predictions)
}


#### 5. If we have a the values of a single input feature in an array ‘feature’ and the prediction ‘errors’ (predictions - output) 
## then the derivative of the regression cost function with respect to the weight of ‘feature’ is just twice the dot product between ‘feature’ and ‘errors’. 

## Write a function that accepts a ‘feature’ array and ‘error’ array and returns the ‘derivative’ (a single number). 
## The derivative of the cost for the intercept is the sum of the errors
## The derivative of functio the cost for the slope is the sum of the product of the errors and the input

feature_derivative = function(errors, features){
  derivative = -2 * (t(features) %*% errors) #t(0 matrix transporse)
  return(derivative)
}


#### 6. Now we will use our predict_output and feature_derivative to write a gradient descent function.
regression_gradient_descent = function(feature_matrix, output, initial_weights, step_size, tolerance){
  converged = FALSE
  weights = initial_weights
  while(!converged){
    # compute the predictions based on feature_matrix and weights:
    # compute the errors as predictions - output:
    predictions = predict_output(feature_matrix, weights)
    errors = predictions - output
    gradient = feature_derivative(errors, feature_matrix)
    gradient_magnitude = sqrt(sum(gradient^2))
    weights = weights + step_size * gradient
    if(gradient_magnitude < tolerance){
      converged = TRUE
    }
  }
  return(weights)
}


#### 8. Now we will run the regression_gradient_descent function on some actual data. 
## In particular we will use the gradient descent to estimate the model from Week 1 using just an intercept and slope. Use the following parameters:

dataset=kc_train
feature_name = c('sqft_living')
output_name = c('price')

feature_matrix = (get_matrix(kc_train, feature_name, output_name))$feature_matrix
output = (get_matrix(kc_train, feature_name, output_name))$output
initial_weights = c(-47000, 1)
step_size = 7e-12
tolerance = 2.5e7

## 9. Quiz Question: What is the value of the weight for sqft_living -- the second element of ‘simple_weights’ (rounded to 1 decimal place)?
simple_weights = regression_gradient_descent(feature_matrix, output, initial_weights, step_size, tolerance)
simple_weights


## 10. Now build a corresponding ‘test_simple_feature_matrix’ and ‘test_output’ using test_data. 
## Using ‘test_simple_feature_matrix’ and ‘simple_weights’ compute the predicted house prices on all the test data.
feature_name = c('sqft_living')
output_name = c('price')
test_feature_matrix = (get_matrix(kc_test, feature_name, output_name))$feature_matrix
predicted_price_test = predict_output(test_feature_matrix, simple_weights)


## 11. Quiz Question: What is the predicted price for the 1st house in the Test data set for model 1 (round to nearest dollar)?
predicted_price_test[1] #356134

## 12. Now compute RSS on all test data for this model. Record the value and store it for later
rss_test = sum((predicted_price_test - kc_test$price)^2)
rss_test #2.754e+14

## 13. Now we will use the gradient descent to fit a model with more than 1 predictor variable (and an intercept). Use the following parameters:
#  model features = ‘sqft_living’, ‘sqft_living_15’
#  output = ‘price’

dataset=kc_train
feature_name = c('sqft_living', 'sqft_living15')
output_name = c('price')

feature_matrix = (get_matrix(dataset, feature_name, output_name))$feature_matrix
output = (get_matrix(dataset, feature_name, output_name))$output
initial_weights = c(-100000, 1, 1)
step_size = 4e-12
tolerance = 1e9

simple_weights = regression_gradient_descent(feature_matrix, output, initial_weights, step_size, tolerance)
simple_weights

## 14. Use the regression weights from this second model (using sqft_living and sqft_living_15) and predict the outcome of all the house prices on the TEST data.
feature_name = c('sqft_living', 'sqft_living15')
output_name = c('price')
test_feature_matrix = (get_matrix(kc_test, feature_name, output_name))$feature_matrix
predicted_price_test = predict_output(test_feature_matrix, simple_weights)

## 15. Quiz Question: What is the predicted price for the 1st house in the TEST data set for model 2 (round to nearest dollar)?
predicted_price_test[1] #366651

## 16. What is the actual price for the 1st house in the Test data set?
kc_test$price[1] #310000

## 17. Quiz Question: Which estimate was closer to the true price for the 1st house on the TEST data set, model 1 or model 2?
# model 1

## 18. Now compute RSS on all test data for the second model. Record the value and store it for later.
rss_test = sum((predicted_price_test - kc_test$price)^2)
rss_test # 2.702634e+14


## 19. Quiz Question: Which model (1 or 2) has lowest RSS on all of the TEST data?
# Model 2



