library(MASS)

setwd("D:/Years/Senior/Fall/ST541 (R)/Project/R Directory")

final_data <- read.delim('marketing_campaign.csv', header=TRUE)
attach(final_data)

## Cleaning, Manipulating and Creating Data 
delete.na <- function(DF, n=0) {
  DF[rowSums(is.na(DF)) <= n,]
}


final_data <- delete.na(final_data, 0)

#resetting the index of the observations
row.names(final_data) <- NULL 

Total_Purchases <- sum(final_data$NumWebPurchases + final_data$NumStorePurchases + final_data$NumCatalogPurchases)


final_data$MntMeatProducts <- final_data$MntMeatProducts+0.00001

# making the date value numeric instead of categorical
final_data$Dt_Customer <- as.Date(final_data$Dt_Customer, format="%d-%m-%Y") 



# In reviewing the Education and Marital_Status, we found some data that needed cleaning. In Marital_Status, we found 'Absurd','Alone','Married','Single','Together','Widowed', and 'YOLO'.
# A few of these variables ('Absurd','Alone','YOLO') can be condensed in to single to lower the amount of categories and have more meaningful categories.
# Education has '2n Cycle', 'Basic', 'Graduation', 'Master', and 'PhD'. 2n Cycle is another way to say Masters degree, so those two categories can be combined as well.

final_data["Marital_Status"][final_data["Marital_Status"] == "Absurd"] <- "Single"
final_data["Marital_Status"][final_data["Marital_Status"] == "Alone"] <- "Single"
final_data["Marital_Status"][final_data["Marital_Status"] == "YOLO"] <- "Single"
final_data["Education"][final_data["Education"] == "2n Cycle"] <- "Master"
attach(final_data)


## Descriptive Analysis
NumWeb <- sum(final_data$NumWebPurchases)
NumStore <- sum(final_data$NumStorePurchases)
NumCatalog <- sum(final_data$NumCatalogPurchases)

web_per = NumWeb / Total_Purchases
store_per  = NumStore / Total_Purchases
cat_per  = NumCatalog / Total_Purchases

web_per
store_per
cat_per

count <- c(web_per, store_per, cat_per)

pie(count, labels = c("Web Purchases/32.53%" , "Store Purchases/46.19%", "Catalog Purchases/21.27%"))

par(mfrow=c(2,3))

boxplot(Income,main='Income');boxplot(Recency,main='Recency');boxplot(MntWines,main='MntWine');boxplot(MntFruits,main='MntFruits');boxplot(MntMeatProducts,main='MntMeatProducts');boxplot(MntFishProducts,main='MntFishProducts');boxplot(MntSweetProducts,main='MntSweetProducts');boxplot(MntGoldProds,main='MntGoldProds');
hist(Year_Birth,main='Year_Birth');hist(Kidhome,main='Kidhome');hist(Teenhome,main='Teenhome');hist(NumDealsPurchases,main='NumDealsPurchases');hist(NumWebPurchases,main='NumWebPurchases');hist(NumCatalogPurchases,main='NumCatalogPurchases');hist(NumStorePurchases,main='NumStorePurchases');hist(NumWebVisitsMonth,main='NumWebVisitsMonth');
hist(AcceptedCmp1,main='AcceptedCmp1');hist(AcceptedCmp2,main='AcceptedCmp2');hist(AcceptedCmp3,main='AcceptedCmp3');hist(AcceptedCmp4,main='AcceptedCmp4');hist(AcceptedCmp5,main='AcceptedCmp5');hist(Complain,main='Complain');hist(Response,main='Response');
barplot(table(Education),main='Education');barplot(table(Marital_Status),main='Marital_Status')

hist(final_data$Complain)   ##Checking the frequency that a customer Complains


####### BE SURE TO INCLUDE THE FOLLOWING 2 LINES ###########
cor(final_data[, c('MntWines','MntFishProducts','MntFruits', 'MntGoldProds', 'MntSweetProducts', 'MntMeatProducts')])
cor(final_data[, c("NumStorePurchases","NumWebPurchases","NumCatalogPurchases")])

# We can see that Income has an extreme outlier, and upon further investigation we believe it to be bad data.
# The income was put at 666666, which is more than 500,000 above the next highest. We removed that data point.
# We also noticed in the histogram that there were 2 observations of birth years in the 1800s and one that was from 1900. We decided to remove those, as the next closest birth year was 1940.

final_data <- final_data[-c(2210, 229, 327, 182),]
row.names(final_data) <- NULL #resetting the index of the observations
attach(final_data)

final_data$NumTotalPurchases <- final_data$NumWebPurchases+ final_data$NumStorePurchases+ final_data$NumCatalogPurchases # creating a total amount of purchases column to use as another possible prediction


#---- Creating Models to Predict which profile of user will use which Purchasing Method
#
## Predicting Number of Store Purchases 

### The base model that I was creating which I will perform a stepwise function on to determine which predictors are the most significant.
nsp_model <- lm(final_data$NumStorePurchases~ final_data$Year_Birth + final_data$Marital_Status + final_data$NumCatalogPurchases +
                final_data$Income + final_data$Dt_Customer + final_data$Recency + final_data$Response + final_data$NumTotalPurchases + final_data$NumWebVisitsMonth,
               data = final_data)


### Performing the stepwise function
nsp_step <- stepAIC(nsp_model, direction = "both")
summary(nsp_step) ##summarizing the model -- R-squared value of 0.8334, with 7/8 predictors being significant and the model being deemed extremely significant with an F-statistic of 1383.

## I then decided to remove the insignificant predictor from the model to reduce complexity.
nsp_step <- lm(final_data$NumStorePurchases~ final_data$Year_Birth + final_data$NumCatalogPurchases + final_data$Income + final_data$Dt_Customer
               + final_data$Response + final_data$NumTotalPurchases + final_data$NumWebVisitsMonth, data = final_data)
summary(nsp_step)

par(mfrow=c(2,2))
plot(nsp_step) ##plotting its summary statistics: 
## Residuals vs Fitted: The plot has a normal scatter, with a small cluster existing underneath the rest of the points. 
## Normal Q-Q: A majority of the points are within margin to the line defining normal distribution, except for a few at the bottom. 
            ## You can note that they are the same points as the ones found in the Residuals vs Fitted Model.
## Scale-Location: A majority of the points are close to the line, with a few (also a part of the original cluster found in Residuals vs Fitted)
                ## leveraging the model's line and range up.
## Residuals vs Leverage: This final plot also shows the same 3 points from the original 3 model fit statistic plots being deemed outliers, as they are 
                          ## far from the normal cluster of points and pull the line for cooks distance down. 

## I found from plotting the summary statistics of this model that there were 3 observations that could be labeled as outliers, as they 
## had appeared on every one of the charts away from the normal spreads and fits that R had deemed to be the best. These 3 observations were:
## 1787, 1956 and 1879. 
nsp_data <- final_data[-c(1787, 1956, 1879), ]
attach(nsp_data)
row.names(nsp_data) <- NULL #resetting the index of the observations


nsp_step <- lm(nsp_data$NumStorePurchases~ nsp_data$Year_Birth + nsp_data$NumCatalogPurchases + nsp_data$Income + nsp_data$Dt_Customer
               + nsp_data$Response + nsp_data$NumTotalPurchases + nsp_data$NumWebVisitsMonth, data = nsp_data)

summary(nsp_step)  ##summarizing the model -- R-squared value of 0.8634, which increased from 0.8334, maintained the 7 significant predictors, and increased the models F-statistic from 1383 to 2000. 
plot(nsp_step)



# Plotting significant predictors
par(mfrow=c(2,3))
plot(NumStorePurchases~Year_Birth+NumCatalogPurchases+Income+Dt_Customer+NumTotalPurchases+NumWebVisitsMonth, data = nsp_data)

################## Poisson Model #######################
library(faraway)
poisson_model <- glm(NumStorePurchases~ Year_Birth + Marital_Status + NumCatalogPurchases + 
                    Dt_Customer + Recency + Response + NumTotalPurchases + NumWebVisitsMonth, 
                    data = nsp_data, family=poisson(link=log))
poisson_model <- step(poisson_model, direction="both")
summary(poisson_model)
plot(poisson_model)

y <- NumStorePurchases
library(ResourceSelection)
hoslem.test(y, fitted(poisson_model), g=10)

# We also used a poisson regression to attempt to predict the number of in-store purchases. There was not over-dispersion, so a quasi-poisson model was not needed.
# We can see that the poisson model is a good fit due to the high p-value when running the goodness-of-fit test


## From reviewing the final summary of the model, you can see that younger individuals with a lower income, less purchases from the catalog, more total purchases
## less web visits per month and the longer they are a customer they will have a higher number of purchases from the store.


### Logistic Regression ##########
log.reg <- glm(final_data$Complain~., data=final_data, family=binomial)
complaints <- step(log.reg, direction='both', data=final_data)
summary(complaints)

pred <- predict(complaints, type="response")
y <- final_data$Complain
cbind(y, pred)

table(round(pred))
sum(pred>=0.5)

class.complaint <- data.frame(response = y, predicted = round(pred,0))
xtabs(~ predicted + response, data = class.complaint)


### Linear Regressions ##########
par(mfrow=c(2,2))
wine_data <- final_data

m2.1 <- lm((wine_data$MntWines)~., data=wine_data)
wine_model1 <- step(m2.1,direction="both", data=wine_data)
summary(wine_model1)
plot(wine_model1)

m2.2 <- lm(log(wine_data$MntWines)~., data=wine_data)
wine_model2 <- step(m2.2,direction="both", data=wine_data)
summary(wine_model2)
plot(wine_model2)


### WINE REGRESSION

final_data$MntWines <- final_data$MntWines+0.00001 # used for transformation
wine_data <- final_data

m2.3 <- lm((wine_data$MntWines)^(1/2)~., data=wine_data)
wine_model3 <- step(m2.3,direction="both", data=wine_data)
summary(wine_model3)
plot(wine_model3)

# Plotting significant predictors
par(mfrow=c(3,3))
plot((MntWines)^(1/2)~Year_Birth+Income+Dt_Customer+MntMeatProducts+MntSweetProducts+MntGoldProds+
       NumDealsPurchases+NumStorePurchases+NumWebVisitsMonth)
# There are only 7 observations in MntWines, so we did not feel the need to remove the observations. Transforming
# the data solved the issues of potentially getting a negative number predicted

# After checking these plots and expanding upon our transformed variables in correlation to the predictors used in our final model, we can see that 

## AMOUNT OF MEAT PRODUCTS REGRESSION
m7.1 <- lm(meat_data$MntMeatProducts~., data=meat_data)
meat_model1 <- step(m7.1, direction="both", data = meat_data)
summary(meat_model1)
plot(meat_model1)

final_data$MntMeatProducts <- final_data$MntMeatProducts+0.00001
par(mfrow=c(2,2))
meat_data <- final_data[-c(1879,1842,640),]

m7.2 <- lm(log(meat_data$MntMeatProducts)~., data=meat_data)
meat_model2 <- step(m7.2, direction="both", data = meat_data)
summary(meat_model2)
plot(meat_model2)

# Plotting significant predictors
par(mfrow=c(2,3))
plot(log(MntMeatProducts)~Income+Dt_Customer+Recency+MntWines+MntFishProducts+MntSweetProducts+MntGoldProds+
       NumDealsPurchases+NumWebPurchases+NumCatalogPurchases+NumStorePurchases+NumWebVisitsMonth, data=meat_data)
# There was only one observation that had no meat purchases in our data, which allowed the transfomation
# to solve the issues that the observation would have caused the model.