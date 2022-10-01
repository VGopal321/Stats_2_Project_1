library(tidyverse)
library(glmnet)
library(caret)
library(ggplot2)
library(FNN)
library(jtools)
library(interactions)
setwd("C:\\Users\\Michael\\OneDrive\\Documents\\College\\SMU\\Applied Statistics")
cars <- data.frame(read.csv('data1.csv'))
cars$combined.mpg <- (cars$highway.MPG + cars$city.mpg) / 2
cars %>% ggplot(aes(x = highway.MPG, y = city.mpg, color = as.factor(Engine.Cylinders))) + ggtitle(("Engine Cylinders and MPG")) + geom_point()
cars$Engine.Cylinders <- as.factor(cars$Engine.Cylinders)
table(cars$Driven_Wheels)
cars$Driven_Wheels <- as.factor(cars$Driven_Wheels)
str(cars)
cars$Number.of.Doors <- as.factor(cars$Number.of.Doors)
cars$Transmission.Type <- as.factor(cars$Transmission.Type)
cars$Vehicle.Size <- as.factor(cars$Vehicle.Size)
cars$Make <- as.factor(cars$Make)
cars$Model <- as.factor(cars$Model)
cars$Engine.Fuel.Type <- as.factor(cars$Engine.Fuel.Type)
cars$Vehicle.Style <- as.factor(cars$Vehicle.Style)
cars$Market.Category <- as.factor(cars$Market.Category)
hist(cars$Popularity)
colSums(is.na(cars))
which(is.na(cars$Engine.HP))
for(i in 1:nrow(cars)){
  if(cars$Engine.Fuel.Type[i] == 'electric' & is.na(cars$Engine.HP[i])){
    cars$Engine.HP[i] <- 0
  }
}
for(i in 1:nrow(cars)){
  if((cars$Make[i] == 'Tesla') & is.na(cars$Number.of.Doors[i])){
    cars$Number.of.Doors[i] <- 4
  }
  if(cars$Make[i] == 'Ferrari' & is.na(cars$Number.of.Doors[i])){
    cars$Number.of.Doors[i] <- 2
  }
  if(is.na(cars$Engine.HP[i]) & cars$Engine.Fuel.Type[i] == 'electric'){
    cars$Engine.HP[i] <- 0
  }
}

# Engine.HP = 25 (0.209%)
# Engine.Cylinders = 30 (0.252%)

cars_no_na <- data.frame(cars %>% na.omit())

cars_no_na %>% ggplot(aes(x = Transmission.Type, y = Popularity)) + geom_boxplot() + ggtitle('Popularity vs Transmission Type')
cars_no_na %>% ggplot(aes(x = combined.mpg, y = Popularity)) + geom_point() + ggtitle('Popularity vs Combined MPG')
cars[which.max(cars$city.mpg),]
cars[which.max(cars$highway.MPG),]
no_electric <- cars_no_na %>% filter(Engine.Fuel.Type != 'electric')
no_electric[which.max(no_electric$city.mpg),]
no_electric[which.max(no_electric$highway.MPG),]
cars_no_na[which.max(cars_no_na$highway.MPG),]
cars_no_na <- cars_no_na[-c(1120),] # Wrong observation
cars_no_na[which.max(cars_no_na$MSRP),]
common_consumer <- cars_no_na %>% filter(MSRP < 100000)
weird <- cars_no_na %>% filter(Engine.Fuel.Type == '')
cars_no_na$BeforeAfter2000 <- NA
cars_no_na$BeforeAfter2000=ifelse(cars_no_na$Year<=2000,'Before','After')
cars_no_na$BeforeAfter2000 <- as.factor(cars_no_na$BeforeAfter2000)
# for(i in 1:nrow(cars_no_na)){
#   if(cars_no_na$Year <= '2000'){
#     cars_no_na$BeforeAfter2000[i] <- 0
#   } else{
#     cars_no_na$BeforeAfter2000[i] <- 1
#   }
# }
write.csv(cars_no_na,'cars_no_na.csv', row.names = TRUE)


########################### Plots ##############################
cars_no_na %>% ggplot(aes(y = MSRP, x = Popularity)) + geom_point() + ggtitle('Popularity vs MSRP')
cars_no_na %>% ggplot(aes(y = MSRP, x = Popularity)) + geom_point() + ggtitle('Popularity vs MSRP')
cars_no_na %>% ggplot(aes(x = Year, y = MSRP, color = Popularity)) + geom_jitter() + ggtitle('Change in MSRP and Popularity Over Time')
common_consumer %>% ggplot(aes(x = Year, y = MSRP, color = Popularity)) + geom_point() + ggtitle('Change in MSRP and Popularity Over Time')
common_consumer %>% ggplot(aes(x = combined.mpg, y = MSRP, color = Popularity)) + geom_point() + ggtitle('Change in MSRP and Combined MPG')
cars_no_na %>% ggplot(aes(x = combined.mpg, y = MSRP, color = Popularity)) + geom_point() + ggtitle('Change in MSRP and Combined MPG')
common_consumer %>% ggplot(aes(x = combined.mpg, y = MSRP, color = Popularity)) + geom_point() + ggtitle('Change in MSRP and Combined MPG')
common_consumer %>% ggplot(aes(y = MSRP, fill = Engine.Cylinders, x = Popularity)) + geom_boxplot() + ggtitle('Boxplot of Engine Cylinders and Popularity')
cars_no_na %>% ggplot(aes(y = MSRP, fill = Engine.Cylinders, x = Popularity)) + geom_boxplot() + ggtitle('Boxplot of Engine Cylinders and Popularity')
common_consumer %>% ggplot(aes(y = MSRP, x = Popularity, fill = Engine.Fuel.Type)) + geom_boxplot()
cars_no_na %>% ggplot(aes(y= Engine.HP, x = Market.Category, fill = Market.Category)) + geom_boxplot() + ggtitle('Engine.HP vs Market.Category')
cars_no_na %>% ggplot(aes(y = city.mpg, x = Transmission.Type, fill = Transmission.Type)) + geom_boxplot()

# Transform data


# Split Data
set.seed(777)
splitPerc <- 0.80
trainIndices <- sample(1:dim(cars_no_na)[1], round(splitPerc * dim(cars_no_na)[1]))
train <- cars_no_na[trainIndices,]
test <- cars_no_na[-trainIndices,]
splitPerc <- 0.50
trainIndices2 <- sample(1:dim(test)[1], round(splitPerc * dim(test)[1]))
test_final <- test[trainIndices2,]
validation <- test[-trainIndices2,]

######################### Models #############################
# LASSO
x <- model.matrix(MSRP~., train)[,-1]
y <- train$Popularity

xtest <- model.matrix(MSRP~., validation)[,-1]
ytest <- validation$Popularity

grid <- 10^seq(10, -2, length=100)
lasso.mod <- glmnet(x, y, alpha=1, lambda = grid)
cv.out <- cv.glmnet(x, y, alpha = 1) #alpha=1 performs LASSO
plot(cv.out)
bestlambda <- cv.out$lambda.min
lasso.pred <- predict(lasso.mod, s=bestlambda, newx=xtest)
testMSE_LASSO <- mean((ytest-lasso.pred)^2)
testMSE_LASSO
coef(lasso.mod, s=bestlambda)

fit2 <- lm(MSRP ~ Make + Market.Category + Engine.HP + Engine.Cylinders + BeforeAfter2000 + Engine.Fuel.Type, data = train)
summary(fit2)
prediction <- (predict(fit2,newdata=validation))
validation_MSE <- mean((validation$MSRP-prediction)^2)
# row.names(prediction) <- NULL

# Confusion matrix
confusionMatrix(table(prediction, validation$MSRP))
createConfusionMatrix <- function(act, pred) {
  # You've mentioned that neither actual nor predicted may give a complete
  # picture of the available classes, hence:
  numClasses <- max(act, pred)
  # Sort predicted and actual as it simplifies what's next. You can make this
  # faster by storing `order(act)` in a temporary variable.
  pred <- pred[order(act)]
  act  <- act[order(act)]
  sapply(split(pred, act), tabulate, nbins=numClasses)
}
createConfusionMatrix(prediction$`predict(fit2, newdata = validation)`, validation$MSRP)
# gmodels::CrossTable(prediction, validation$MSRP)

# R2
r2 <- function(prediction){
  resids <- prediction-validation$MSRP
  SSR <- sum(resids^2)
  SSE <- sum((prediction-mean(validation$MSRP))^2)
  R2 <- 1-(SSR/SSE)
  R2
}

# Plots 2
plot(fit2)
ols_plot_cooksd_bar(fit2)
ols_plot_resid_lev(fit2)

# LM Model 2
fit3 <- lm(MSRP ~ Engine.HP * Engine.Cylinders, data = train)
interact_plot(fit3, pred = Engine.HP, modx = Engine.Cylinders, plot.points = TRUE)

fit4 <- lm(MSRP ~ Make + Market.Category + Engine.HP + Engine.Cylinders + BeforeAfter2000 + Engine.Fuel.Type + (Engine.Cylinders * Engine.HP) + (Engine.HP * Market.Category), data = train)
summary(fit4)
prediction4 <- (predict(fit4,newdata=validation))
validation_MSE_4 <- mean((validation$MSRP-prediction4)^2)
r2(prediction4)
plot(fit4)

########## KNN Regression #############
X_train <- train
X_train <- sapply(X_train, unclass)
Y_train <- as.data.frame(X_train) %>% select(c(MSRP))
X_train <- as.data.frame(X_train) %>% select(-c(MSRP))
validation_knn <- sapply(validation, unclass)
validation_knn_Y <- as.data.frame(validation_knn) %>% select(MSRP)
validation_knn <- as.data.frame(validation_knn) %>% select(-c(MSRP))
fit5 <- FNN::knn.reg(train = X_train, test = validation_knn, y = Y_train, k = 6)
rmse = function(actual, predicted) {
  sqrt(mean((actual - predicted) ^ 2))
}
make_knn_pred = function(k = 1) {
  pred = FNN::knn.reg(train = X_train, 
                      test = validation_knn, 
                      y = Y_train$MSRP, k = k)$pred
  act  = validation_knn_Y$MSRP
  paste0('RMSE: ', rmse(predicted = pred, actual = act))
  resids <- pred-validation_knn_Y$MSRP
  SSR <- sum(resids^2)
  SSE <- sum((pred-mean(validation_knn_Y$MSRP))^2)
  R2 <- 1-(SSR/SSE)
  data.frame('R2' = R2, 'RMSE' = rmse(predicted = pred, actual = act), 'MSE' = (rmse(predicted = pred, actual = act))^2)
}
thing <- make_knn_pred(k=1)
RMSE_1 <- thing$RMSE
n <- 1
for(i in 2:250){
  print(i)
  values <- make_knn_pred(k=i)
  if(values$RMSE < RMSE_1){
    n <- i
    RMSE_1 <- values$RMSE
  }
}
# k = 1