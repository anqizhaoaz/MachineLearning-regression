
## Regression Week 3: Polynomial Regression Quiz
## May 2, 2016

## 1. write a function that adds powers of a feature to columns of a data frame
polynomial = function(feature, power) {
  return(data.frame(sapply(1:power, function(x){(feature)**x})))
}
# sapply: Applies a function to elements in a list and returns the results in a vector, matrix or a list.
# more information about sapply: http://www.ats.ucla.edu/stat/r/library/advanced_function_r.htm


## 2. load sales data
sales <- read.csv("C:/Users/apple/Desktop/regression/kc_house_data.csv")
str(sales)

require(ggplot2)
xyplot = ggplot(sales, aes(sqft_living,price)) + geom_point(alpha=0.25)
xyplot

## sort sales data by 'sqft_living'
sales <- sales[order(sales$sqft_living),] 
head(sales)

## 3. Make a 1 degree polynomial SFrame with sales['sqft_living'] as the feature. Call it poly1_data
poly1_data = data.frame(sales$sqft_living)
head(poly1_data)

## 4. Add sales['price'] to poly1_data as this will be our output variable.
poly1_data$price = sales$price
head(poly1_data)

## 5. Compute the regression weights for predicting sales['price'] based on the 1 degree polynomial feature sqft_living. The result should be an intercept and slope
model1 = lm(price~., data=poly1_data)
summary(model1)

## 6. compute the regression weights for predicting sales['price'] based on the 1 degree polynomial feature 'sqft_living'
model1_fitted = data.frame(sqft_living = sales$sqft_living)
model1_fitted$price = model1$fitted.values
head(model1_fitted)

## 7. Next use the produce a scatter plot of the training data (just square feet vs price) and add the fitted model. 
xyplot + geom_line(data=model1_fitted, color="red")

## 8. Now that you have plotted the results using a 1st degree polynomial, try it again using a 2nd degree and 3rd degree polynomial
poly2_data = polynomial(sales$sqft_living, 2)
head(poly2_data)
poly3_data = polynomial(sales$sqft_living, 3)
head(poly3_data)

poly2_data$price = sales$price
poly3_data$price = sales$price

model2 = lm(price ~ ., data=poly2_data)
summary(model2)
model2_fitted = data.frame(sqft_living = sales$sqft_living)
model2_fitted$price = model2$fitted.values

model3 = lm(price ~ ., data=poly3_data)
summary(model3)
model3_fitted = data.frame(sqft_living = sales$sqft_living)
model3_fitted$price = model3$fitted.values

## Look at the fitted lines, do they appear as you would expect
xyplot + geom_line(data=model1_fitted, color=c("#E69F00"), size=1.2) + geom_line(data=model2_fitted, color=c("#0072B2"), size=1.2) +
  geom_line(data=model3_fitted, color=c("#009E73"), size=1.2)

## 9. Now try a 15th degree polynomial. Print out the coefficients and look at the resulted fitted line.
#  Print out the coefficients and look at the resulted fitted line
#  Do you think this degree is appropriate for these data? If we were to use a different subset of the data do you think we would get pretty much the same curve?
poly15_data = polynomial(sales$sqft_living, 15)
head(poly15_data)
poly15_data$price = sales$price

model15 = lm(price ~ ., data=poly15_data)
summary(model15)
model15_fitted = data.frame(sqft_living = sales$sqft_living)
model15_fitted$price = model15$fitted.values

xyplot + geom_line(data=model1_fitted, color=c("#F0E442"), size=1.2) + 
  geom_line(data=model2_fitted, color=c("#0072B2"), size=1.2) +
  geom_line(data=model3_fitted, color=c("#009E73"), size=1.2) +
  geom_line(data=model15_fitted, color=c("#D55E00"), size=1.2)


## 10.four subset
wk3_set1 <- read.csv("C:/Users/apple/Desktop/regression/wk3_kc_house_set_1_data.csv")
wk3_set2 <- read.csv("C:/Users/apple/Desktop/regression/wk3_kc_house_set_2_data.csv")
wk3_set3 <- read.csv("C:/Users/apple/Desktop/regression/wk3_kc_house_set_3_data.csv")
wk3_set4 <- read.csv("C:/Users/apple/Desktop/regression/wk3_kc_house_set_4_data.csv")

p1_dat = polynomial(wk3_set1$sqft_living, 15)
p2_dat = polynomial(wk3_set2$sqft_living, 15)
p3_dat = polynomial(wk3_set3$sqft_living, 15)
p4_dat = polynomial(wk3_set4$sqft_living, 15)

p1_dat$price = wk3_set1$price
p2_dat$price = wk3_set2$price
p3_dat$price = wk3_set3$price
p4_dat$price = wk3_set4$price

m1 = lm(price ~ ., data = p1_dat)
m2 = lm(price ~ ., data = p2_dat)
m3 = lm(price ~ ., data = p3_dat)
m4 = lm(price ~ ., data = p4_dat)


#11. Estimate a 15th degree polynomial on all 4 sets, plot the results and view the coefficients for all four models.
sub1_fitted = data.frame(sqft_living=wk3_set1$sqft_living)
sub1_fitted$price = wk3_set1$price
sub1_fitted$predprice = m1$fitted.values
sub1_fitted$group = 1

sub2_fitted = data.frame(sqft_living=wk3_set2$sqft_living)
sub2_fitted$price = wk3_set2$price
sub2_fitted$predprice = m2$fitted.values
sub2_fitted$group = 2

sub3_fitted = data.frame(sqft_living=wk3_set3$sqft_living)
sub3_fitted$price = wk3_set3$price
sub3_fitted$predprice = m3$fitted.values
sub3_fitted$group = 3

sub4_fitted = data.frame(sqft_living=wk3_set4$sqft_living)
sub4_fitted$price = wk3_set4$price
sub4_fitted$predprice = m4$fitted.values
sub4_fitted$group = 4

newdata <- rbind(sub1_fitted, sub2_fitted, sub3_fitted, sub4_fitted)

ggplot(newdata, aes(x=sqft_living, y=price, color=group)) +
  geom_point(alpha=0.25) + 
  geom_line(aes(x=sqft_living, y=predprice)) +
  facet_wrap(~group)

## 12. Quiz Question: Is the sign (positive or negative) for power_15 the same in all four models?
summary(m1)
summary(m2)
summary(m3)
summary(m4)

## 13. Quiz Question: True/False the plotted fitted lines look the same in all four plots

## 14. Since the "best" polynomial degree is unknown to us we will use cross validation to select the best degree. 
#  If you're using SFrames then create a training, validation and testing subsets as follows:
#  If you're not using SFrames then please download the provided csv files for training, validation and test data.
test <- read.csv("C:/Users/apple/Desktop/regression/wk3_kc_house_test_data.csv")
train <- read.csv("C:/Users/apple/Desktop/regression/wk3_kc_house_train_data.csv")
valid <- read.csv("C:/Users/apple/Desktop/regression/wk3_kc_house_valid_data.csv")

## 15. Now for each degree from 1 to 15:
# Build an polynomial data set using training_data['sqft_living'] as the feature and the current degree
# Add training_data['price'] as a column to your polynomial data set
# Learn a model on TRAINING data to predict 'price' based on your polynomial data set at the current degree
# Compute the RSS on VALIDATION for the current model (print or save the RSS)

for (i in 1:15) {
  # Train model using training data
  train_data = polynomial(train$sqft_living, i)
  train_data$price = train$price
  model = lm(price ~ ., data=train_data)
  
  # Compute RSS using validation set
  valid_data = polynomial(valid$sqft_living, i)
  valid_data$price = valid$price
  valid_predicted = predict(model, newdata=valid_data)
  rss = sum((valid$price - valid_predicted)^2)/(10^10)
  print(paste(c("Degree: ", i, " RSS: ", rss), collapse=""))
  
}


## 16. Quiz Question: Which degree (1, 2, ., 15) had the lowest RSS on Validation data?

## 17. Now that you have selected a degree compute the RSS on TEST data for the model with the best degree from the Validation data.
train_poly5 = polynomial(train$sqft_living, 6)
train_poly5$price = train$price ## add price to the data since it's the target
model_poly5 = lm(price ~ ., data=train_poly5)

test_data5=polynomial(test$sqft_living, 6)
predicted_test5 = predict(model_poly5, newdata=test_data5)

rss = sum((test$price - predicted_test5)^2)
rss

## 18. Quiz Question: what is the RSS on TEST data for the model with the degree selected from Validation data? 
#  (Make sure you got the correct degree from the previous question)
