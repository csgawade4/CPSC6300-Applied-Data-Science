# <--------------------------LOADING THE DATASET----------------------->

library(readxl)
library(tree)
library(data.table)

acceleration_data <- read_excel("~/Downloads/PROJECT-DATA.xlsx")

# <-------------------------DATA- CLEANING PROCEDURE-------------------->

acceleration_data <- acceleration_data[,-1]

acceleration_data <- na.omit(acceleration_data)

library(boot)

# <-------------------------CHANGED COLUMN-NAMES------------------------>

colnames(acceleration_data)[1] <- "ACCELERATION_TIME"
colnames(acceleration_data)[2] <- "PEAK_POWER"
colnames(acceleration_data)[3] <- "RPM_FOR_PEAK_POWER"
colnames(acceleration_data)[4] <- "PEAK_TORQUE"

colnames(acceleration_data)[5] <- "RPM_FOR_PEAK_TORQUE"
colnames(acceleration_data)[6] <- "WEIGHT"
colnames(acceleration_data)[7] <- "GEAR_RATIO_1"

colnames(acceleration_data)[8] <- "GEAR_RATIO_2"
colnames(acceleration_data)[9] <- "GEAR_RATIO_3"
colnames(acceleration_data)[10] <- "FINAL_DRIVE_AXLE_RATIO"
colnames(acceleration_data)[11] <- "DRIVE_TRAIN_CATEGORY"

set.seed(1)
N <- nrow(acceleration_data)
train <- sample(1:N, 0.9*N)
test <- seq(1:N)[-train]

acceleration.test <- acceleration_data[test, "ACCELERATION_TIME"]

prediction_data <- read_excel("~/Downloads/prediction.xlsx")

# <-------------------------- REGRESSION TREE ----------------------->


tree.acceleration <- tree(ACCELERATION_TIME~., data = acceleration_data,subset = train)

yhat.tree <- predict(tree.acceleration, acceleration_data[test,])

sum((yhat.tree-acceleration.test)^2)/12 # 0.804

prediction_result_tree <- predict(tree.acceleration,prediction_data) # 6.485714 4.069565 4.069565 


#plot(yhat.tree, transpose(acceleration.test))

#abline(0,1)


# <-------------------------- PRUNED_REGRESSION TREE ---------------------->

prune.acceleration <- prune.tree(tree.acceleration,best = 4)

yhat.tree_prune <- predict(prune.acceleration,newdata =  acceleration_data[test,])

sum((yhat.tree_prune-acceleration.test)^2)/12 # 0.4931461

prediction_result_tree_prune <- predict(prune.acceleration,prediction_data) #  5.372414 3.985106 3.985106



# <-------------------------- SIMPLE_LINEAR_REGRESSION ----------------------->


lm.acceleration <- glm(ACCELERATION_TIME ~ .,data = acceleration_data, family = gaussian)

yhat.lm <- predict(lm.acceleration, acceleration_data[test,])

cv = cv.glm(acceleration_data,lm.acceleration , K= nrow(acceleration_data)) #1.340683 

prediction_result_lm <- predict(lm.acceleration,prediction_data) # 6.450791 5.409444 4.339393

# <-------------------------- BOOSTED REGRESSION TREE ----------------------->

boosts_acceleration <- gbm(ACCELERATION_TIME ~ . , data=acceleration_data[train,], distribution="gaussian", n.trees=5000,cv.folds = 119,interaction.depth = 4)

prediction = predict(boosts_acceleration,newdata = acceleration_data[test,],n.trees=5000)

test_data_for_MSE <- acceleration_data[test,"ACCELERATION_TIME"]

sum((prediction - test_data_for_MSE)^2)/12 #0.8005862

prediction_result_boosted <- predict(boosts_acceleration,prediction_data,n.trees = 5000) # 6.143885 4.156689 4.800802

# <-------------------------- RANDOM_FOREST ----------------------->
# BAGGED_REGRESSION_TREE

library(randomForest)

P <- ncol(acceleration_data)-1

bagged.acceleration <-  randomForest(ACCELERATION_TIME~., data=acceleration_data, subset=train, mtry = P, importance=TRUE)

yhat.bag <- predict(bagged.acceleration, acceleration_data[test,])

sum((yhat.bag-acceleration.test)^2)/12 #0.7354396

prediction_result_bagged <- predict(bagged.acceleration,prediction_data) # 5.961757 4.668007 4.278187 


# WITHOUT BAGGING (mtry = 6)

random_forest_acceleration <-  randomForest(ACCELERATION_TIME~., data=acceleration_data, subset=train, mtry = 6, importance=TRUE)

yhat.random_forest <- predict(random_forest_acceleration, acceleration_data[test,])

sum((yhat.random_forest-acceleration.test)^2)/12 #0.6227446

prediction_result_without_bagging <- predict(random_forest_acceleration,prediction_data) # 6.015023 4.608453 4.317817
