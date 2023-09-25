rm(list=ls())
cat("\014")
library(ggplot2)

# Data Loading and Preparation  -- Feature Engineering
house_price <- read.csv(file='data.csv', stringsAsFactors = FALSE)

# structure of R object house_price
str(house_price) 
head(house_price,5)

# Summary statistics of house_price
summary(house_price)


# removing columns that is not helpful in developing model for predicting house prices
nrow(house_price[house_price$waterfront == 0,])
# since 90 percent of the values 
# out of 4600 rows for waterfront column, 4567 rows value is 0 so we are removing 
# and columns like date, street, city, statezip, country will not contribute for model so
# we are removing these columns
df <- house_price[,-c(1,8,15,16,17,18)]
nrow(df[df$view == 0,])
cor(df)
# and also for view column, 4140 rows value is 0 almost 90 percent of values and also
# there is no strongly correlated with other predictor variables and response variable.
# so it will not contribute for model and we are removing view column.
df <- df[,-7]


# Data cleaning and Transformation
# created new column age_of_house from yr_built column
current_year <- as.numeric(format(Sys.time(), "%Y"))
df$age_of_house <- current_year - df$yr_built
df <- df[,-10]

# changed name of column yr_renovated to year_renovated
df$year_renovated <- df[,"yr_renovated"]
df <- df[,-10]

# Since most of the rows for year_renovated are 0 we can convert it into binary variable
# 1 - renovated and 0 - not renovated
nrow(df[df$year_renovated ==0,])
df$year_renovated <- ifelse(df$year_renovated > 0, 1, 0)

# for sqft_basement also, most of the rows are 0 so we can convert it into binary variable
# 1 - have basement and 0 - do not have basement
nrow(df[df$sqft_basement ==0,])
df$sqft_basement<- ifelse(df$sqft_basement > 0, 1, 0)

# Data Cleaning -- Handling Missing and Zero values
colMeans(is.na(df))
summary(df$price)
table(df$price==0)
df <- df[df$price!=0,]



# Descriptive Statistics
# Exploratory Data Analysis
# Checking outliers -- using box plots for key features
boxplot(df$price, main="Price")
boxplot(df$bedrooms, main="Bedrooms")
boxplot(df$bathrooms, main="Bathrooms")
boxplot(df$sqft_living, main="Sqft_Living")
boxplot(df$sqft_above, main="Sqft_Above")
boxplot(df$age_of_house, main="Age of house")
boxplot(df$sqft_lot, main="Area of the plot")
# clearly from the plots there are outliers.

# first we need to remove outliers to prepare data for modelling
# 1st Method:
outlier_treat1 <- function(x){
  UC = quantile(x, p=0.99,na.rm=T)
  LC = quantile(x, p=0.01,na.rm=T)
  x=ifelse(x>UC,UC, x)
  x=ifelse(x<LC,LC, x)
  return(x)
}
df1 = data.frame(apply(df, 2, FUN=outlier_treat1))
min(df1$price)
max(df1$price)
cor_matrix1 <- cor(df1)
cor_matrix1
model1 <- lm(price~.,data=df1)
summary(model1)
#Residual standard error: 218700 on 4540 degrees of freedom
#Multiple R-squared:  0.5481,	Adjusted R-squared:  0.5471 
#F-statistic: 550.7 on 10 and 4540 DF,  p-value: < 2.2e-16

# 2 Method:
SD <- function(v){
  sqrt(sum((v-mean(v))^2)/length(v))
}
outlier_treat2 <- function(x){
  mean_val = mean(x)
  sd_val = SD(x)
  UC = mean_val + 3 * sd_val
  LC = mean_val - 3 * sd_val
  x = ifelse(x > UC, UC, x)
  x = ifelse(x < LC, LC, x)
  return(x)
}
df2 <- data.frame(lapply(df, FUN = outlier_treat2))
min(df2$price)
max(df2$price)
summary(df2)
cor_matrix2 <- cor(df2)
cor_matrix2
model2 <- lm(price~.,data=df2)
summary(model2)
#Residual standard error: 227600 on 4540 degrees of freedom
#Multiple R-squared:  0.5399,	Adjusted R-squared:  0.5389 
#F-statistic: 532.8 on 10 and 4540 DF,  p-value: < 2.2e-16


# 3 Method:
outlier_treat3 <- function(x) {
  q1 <- quantile(x, probs = 0.25)
  q3 <- quantile(x, probs = 0.75)
  iqr <- q3 - q1
  UC <- q3 + 1.5 * iqr
  LC <- q1 - 1.5 * iqr
  x <- ifelse(x > UC, UC, x)
  x <- ifelse(x < LC, LC, x)
  return(x)
}
df3 <- data.frame(lapply(df, FUN = outlier_treat3))
min(df3$price)
max(df3$price)
cor_matrix3 <- cor(df3)
cor_matrix3
model3 <- lm(price~.,data=df3)
summary(model3)
#Residual standard error: 170500 on 4540 degrees of freedom
#Multiple R-squared:  0.5624,	Adjusted R-squared:  0.5614 
#F-statistic: 583.5 on 10 and 4540 DF,  p-value: < 2.2e-16

# we have tried 3 different methods for removing outliers
# comparing summary statistics of the three methods,model 3 has high R-squared value
# model 3 has low Residual standard error
# and also F-statistic is higher for model 3 and p-values are also on comparable scale
# High R-squared value explains more of the variation in the response variable 
# is explained by the predictors.
# High F-statistic indicates more significant relationship between predictors and response variable.
# low Residual Error value makes 3 method model with more precision.
# So we are moving forward with method 3 for removing outliers and we will be using df3 for model.

# Checking boxplots whether outliers are removed
boxplot(df3$price, main="Price")
boxplot(df3$bedrooms, main="Bedrooms")
boxplot(df3$bathrooms, main="Bathrooms")
boxplot(df3$sqft_living, main="Sqft_Living")
boxplot(df3$sqft_above, main="Sqft_Above")
boxplot(df3$age_of_house, main="Age of house")
boxplot(df3$sqft_lot, main="Area of the plot")
# outliers have been removed


# Histograms
hist(df3$bedrooms, breaks = 5, col = "violet", main = "Histogram for no. of bathrooms", xlab = "Bedrooms")
hist(df3$bathrooms, breaks = 10, col = "green", main = "Histogram for no. of bathrooms", xlab = "Bathrooms")
hist(df3$price, breaks = 10, col = "red", main = "Histogram for price", xlab = "Price")
hist(df$sqft_living, breaks = 10, col = "blue", main = "Histogram for area of living", xlab = "Sqft_living",xlim = c(0,8000))
hist(df$age_of_house, breaks = 10, col = "brown", main = "Histogram for Age of house", xlab = "Age")
hist(df$floors, breaks = 6, col = "orange", main = "Histogram for no. Floors", xlab = "Floors")

# scatter plot between price and sqft_living
ggplot(data=df3,aes(x=sqft_living,y=price))+geom_point()+geom_smooth(method="lm",se=F)
# clearly from the plot, we can observe that there is linear relationship between price and sqft_living 
# it is positive. so it means more area of plot used for living(sqft_living) more the price.

# scatter plot between price and sqft_lot
ggplot(data=df3,aes(x=sqft_lot,y=price))+geom_point()+geom_smooth(method="lm",se=F)
# clearly from the plot, we can observe that there is linear relationship between price and sqft_lot
# it is positive. so it means more total area of plot(sqft_lot) more the price.
# but the linear relationship is less strong compared to sqft_living


# scatter plot between price and sqft_above
ggplot(data=df3,aes(x=sqft_above,y=price))+geom_point()+geom_smooth(method="lm",se=F)
# clearly from the plot, we can observe that there is linear relationship between price and sqft_above
# it is positive. so it means more total area above ground(sqft_above) more the price.
# but the linear relationship is almost as strong as compared to sqft_living

# scatter plot between price of house and number of bedrooms
g <- ggplot(df3,aes(x=sqft_living,y=price,col=factor(bedrooms))) 
g+geom_point() +geom_smooth(method="lm",se=F)+ labs(col="Bedrooms")  + scale_color_discrete(labels = unique(df3$bedrooms))
# From the scatterplot, we can infer that price of the house depends on number of bedrooms


# scatter plot between price of house and number of bathrooms
h <- ggplot(df3,aes(x=sqft_living,y=price,col=factor(bathrooms))) 
h+geom_point() +geom_smooth(method="lm",se=F)+ labs(col="Bathrooms") + scale_color_discrete(labels = unique(df3$bathrooms))
# From the scatterplot, we can infer that price of the house depends on number of bathrooms


# scatter plot between price of house and condition of house
i <- ggplot(df3,aes(x=sqft_living,y=price,col=factor(condition))) 
i+geom_point() +geom_smooth(method="lm",se=F)+ labs(col="Condition")
# From the scatterplot, we can infer that there ia relationship between price of the house 
# and condition of house -- Better the condition of house better the price.


# Since we have 0 and 1 values for sqft_basement indicating no basement and having basement
# we can use boxplot to check
basement<-ifelse(df$sqft_basement > 0, "Yes", "No")
ggplot(data=df3,aes(y=price,x=basement, fill=basement))+geom_boxplot()
# From boxplot, we can infer that having basement has slightly higher price.



library(caret)
set.seed(123)
trainIndex <- createDataPartition(df3$price, p = 0.6, list = FALSE)
training_set <- df3[trainIndex, ]
test_set <- df3[-trainIndex, ]


cat("No. of rows for training:", nrow(training_set), "\n")
cat("No. of rows for testing:", nrow(test_set), "\n")

mod1 <- lm(price~.,data=training_set)
summary(mod1)
#Residual standard error: 171800 on 2721 degrees of freedom
#Multiple R-squared:  0.5552,	Adjusted R-squared:  0.5536 
#F-statistic: 339.6 on 10 and 2721 DF,  p-value: < 2.2e-16

cor(df3)
mod2 <- lm(price~. -year_renovated,data=training_set)
summary(mod2)
#Residual standard error: 171900 on 2722 degrees of freedom
#Multiple R-squared:  0.5547,	Adjusted R-squared:  0.5532 
#F-statistic: 376.8 on 9 and 2722 DF,  p-value: < 2.2e-16
# As correlation coefficient between year_renovated and price is very low 
# so year_renovated column was removed and there is almost same model performance.

mod3 <- lm(price~. -year_renovated -age_of_house,data=training_set)
summary(mod3)
#Residual standard error: 178800 on 2723 degrees of freedom
#Multiple R-squared:  0.5179,	Adjusted R-squared:  0.5165 
#F-statistic: 365.6 on 8 and 2723 DF,  p-value: < 2.2e-16
# As correlation coefficient between age_of_house and price is very low 
# But If we remove age_of_house column there is difference in model performance.
# so we are not removing age_of_house column from model

# So our final model will be on all predictors except year_renovated and condition
# because by removing condition column also condition there is almost same model performance.
mod_final <- lm(price~. -year_renovated -condition,data=training_set)
summary(mod_final)
test <- predict(mod_final,test_set)
#Residual standard error: 172600 on 2723 degrees of freedom
#Multiple R-squared:  0.551,	Adjusted R-squared:  0.5497 
#F-statistic: 417.7 on 8 and 2723 DF,  p-value: < 2.2e-16
# From the summary of the model we can see 
# all the t-values are greater than 2 and their corresponding p-values 
# are very small (less than 0.05), indicating that all the coefficients 
# are statistically significant at the 5% level of significance. 
# So, we can conclude that all the predictor variables have a
# statistically significant relationship with the response variable.


# creating data frame with columns actual value,predicted value and error value
test <- predict(mod_final,test_set)
result_diff <- cbind(actual=test_set$price,predicted=test)
result_diff <- as.data.frame(result_diff)
error <- result_diff$actual-result_diff$predicted
error <- as.data.frame(error)
final_result <- cbind(result_diff,error)
final_result
# We have created final_result data frame with columns actual,predicted and error columns
# That we can use for scatter plot between actual and predicted values.

# creating scatter point between 
k <- ggplot(data=test_set,aes(y=test,x=price))
k+geom_point(colour='blue')+labs( y='Predicted Values',x='Actual Values') + geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed",size=1.2)
# From the scatterplot, we can observe that points are tightly clustered around the diagonal line
# As per that, We can infer that predicted values are very close to actual values
# From this, we can say the model has high accuracy.
# And also there is clearly linear relationship between variables.

# Now creating residual plot for final model
plot(mod_final,which=1)
# From the residual plot, we can observe that all points clustered around 0 line.
# We can infer that model is the model is making accurate predictions on average,
# as the residuals (i.e., the differences between the observed values and the predicted values) 
# are close to zero for most of the data points.
# Clearly we can see there is no correlation between residuals and fitted values and
# also there is no specific pattern.
# and curve is relatively flat indicating model is working fine.


# Coefficients of model -- Evaluating the model
coef_matrix <- summary(mod_final)$coefficients
model_coefficients <- coef_matrix[ , 1]                   
as.data.frame(model_coefficients)
# We can infer from the coefficients:

# Holding all other features fixed, an increase of one bedroom is associated with 
# an decrease of $42924.95 in price.

# Holding all other features fixed, an increase of one bathroom is associated with 
# an increase of $41672.69 in price.

# Holding all other features fixed, an increase of one sq feet area for living is associated with 
# an increase of $169.88 in price.

# Holding all other features fixed, an increase of one sq feet area for lot is associated with 
# an decrease of $3.96 in price.

# Holding all other features fixed, an increase of one floor is associated with 
# an increase of $43087.09 in price.

# Holding all other features fixed, an increase of one sq feet area above ground is 
# associated with an increase of $68.35 in price.

# Holding all other features fixed, a house having a basement is associated with 
# an increase of $47120.43 in price.

# Holding all other features fixed, an increase in age of house by 1 year is associated with 
# an increase of $2419.31 in price.

# out of all the features,  the feature that has the highest impact 
# on the price of a house is house having basement, so basement feature. 

# So final Multiple Linear regression equation is:
# House_Price_Prediction = -79001.01 - (42924.95)X1+ (41672.69)X2+ 
# (169.88)X3- (3.96)X4+ (43087.09)X5+ (68.35)X6+ (47120.43)X7+ (2419.31)*X8

# X1 = Number of Bedrooms
# X2 = Number of Bathrooms
# X3 = Square feet area of living
# X4 = Square feet area of lot
# X5 = Number of floors
# X6 = Square feet are above ground
# X7 = A house having basement
# X8 = Age of the House

# Now predicting house price for the sample data:

# No. of bedrooms = 3
# No. of bathrooms = 2
# Area of living(sq feet) = 1250
# Area of lot(sq feet)= 2400
# No. of floors = 2
# Area above ground(sq feet) = 1200
# Basement = Yes
# Age of the house = 35 years old

predicted_price = -79001.01 - (42924.95)*(3)+ (41672.69)*(2)+ (169.88)*(1250)- (3.96)*(2400)+ (43087.09)*(2)+ 
  (68.35)*(1200)+ (47120.43)*(1)+ (2419.31)*(35)
cat("Predicted price of the house: $", formatC(predicted_price, digits = 0, format = "f", big.mark = ","), sep = "")
# The Predicted house price is $378406

