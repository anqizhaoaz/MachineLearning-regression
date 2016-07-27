

kc_house <- read.csv("C:/Users/apple/Desktop/regression/kc_house_data.csv")
str(kc_house)

## 2. Split data into 80% training and 20% test data.
test <- read.csv("C:/Users/apple/Desktop/regression/kc_house_test_data.csv")
train <- read.csv("C:/Users/apple/Desktop/regression/kc_house_train_data.csv")

## 3. Write a generic function that accepts a column of data (e.g, an SArray) ‘input_feature’ and another column ‘output’ 
# and returns the Simple Linear Regression parameters ‘intercept’ and ‘slope’. 
simple_linear_regression = function(input_features, output){
  ysum = sum(output)
  xsum = sum(input_features)
  xy_sum = sum(output*input_features)
  xx_sum = sum(input_features**2)
  n = length(output)
  slope = (xy_sum - ((ysum*xsum)/n))/(xx_sum - ((xsum**2)/n))
  intercept = (ysum/n) - slope*(xsum/n)
  return(list(slope=slope, intercept=intercept))
}


## 4. Use your function to calculate the estimated slope and intercept on the training data to predict ‘price’ given ‘sqft_living’
# simple_linear_regression(train$sqft_living, train$price)
sqft = simple_linear_regression(train$sqft_living, train$price)
sqft

## 5. Write a function that accepts a column of data ‘input_feature’, the ‘slope’, and the ‘intercept’ you learned, 
# and returns an a column of predictions ‘predicted_output’ for each entry in the input column. 

predicted_value = function(slope, intercept, input_features) {
  estimated = input_features*slope + intercept
  return (list(estimated.value=estimated))
}

predicted=predicted_value(sqft$slope, sqft$intercept, train$sqft_living)

## 6. Quiz Question: Using your Slope and Intercept from (4), What is the predicted price for a house with 2650 sqft?
predicted_value(sqft$slope, sqft$intercept, 2650)

## 7. Write a function that accepts column of data: ‘input_feature’, and ‘output’ and the regression parameters ‘slope’ 
# and ‘intercept’ and outputs the Residual Sum of Squares (RSS)
sum_of_squares = function (input_feature, output, slope, intercept) {
  residuals = (input_feature*slope + intercept) - output
  rss = sum(residuals^2)
  return (list(RSS=rss))
}

## 8. Quiz Question: According to this function and the slope and intercept from (4) 
# What is the RSS for the simple linear regression using squarefeet to predict prices on TRAINING data?
sum_of_squares(train$sqft_living, train$price, sqft$slope, sqft$intercept)


## 9. invert: Write a function that accept a column of data:‘output’ and the regression parameters 
# ‘slope’ and ‘intercept’ and outputs the column of data: ‘estimated_input’.
invert = function(output, slope, intercept) {
  estimated_input_feature = (output-intercept)/slope
  return (list(estimated_input_feature=estimated_input_feature))
}

## 10. Quiz Question: According to this function and the regression slope and intercept from (3) 
# what is the estimated square-feet for a house costing $800,000?

invert(800000, sqft$slope, sqft$intercept)

## 11. Instead of using ‘sqft_living’ to estimate prices we could use ‘bedrooms’ (a count of the number of bedrooms in the house) to estimate prices. Using your function from (3) calculate the Simple Linear Regression slope and intercept for estimating price based on bedrooms. 
## Save this slope and intercept for later (you might want to call them e.g. bedroom_slope, bedroom_intercept).
bedroom = simple_linear_regression(train$bedrooms, train$price)
bedroom

## 13. Quiz Question: Which model (square feet or bedrooms) has lowest RSS on TEST data? 
#  Think about why this might be the case.
sum_of_squares(test$sqft_living, test$price, sqft$slope, sqft$intercept)
sum_of_squares(test$bedroom, test$price, bedroom$slope, bedroom$intercept)



