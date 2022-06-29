# <--------------------------LOADING THE DATASET----------------------->

library(readxl)

library(mgcv)

acceleration_data <- read_excel("~/Desktop/PROJECT_DATA.xlsx")

# <-------------------------DATA- CLEANING PROCEDURE-------------------->

acceleration_data <- acceleration_data[,-1]

acceleration_data <- na.omit(acceleration_data)

names(acceleration_data)

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

# <-------------------------CHANGED COLUMN-NAMES------------------------>
library(splines)

# <-------------------------MODEL_SELECTION------------------------>


generalized_additive_model <- gam(ACCELERATION_TIME ~  s(PEAK_POWER) + s(RPM_FOR_PEAK_TORQUE)+ s(RPM_FOR_PEAK_POWER) + s(GEAR_RATIO_3) + s(PEAK_TORQUE)+ s(GEAR_RATIO_1) + s(GEAR_RATIO_2) + s(WEIGHT) + s(FINAL_DRIVE_AXLE_RATIO) + DRIVE_TRAIN_CATEGORY ,data=acceleration_data)
generalized_additive_model

# <-------------------------CALCULATION OF TEST ERROR RATE------------------------>

loocv.error <- cv.glm(acceleration_data, generalized_additive_model, K = nrow(acceleration_data))

loocv.error$delta[1] #0.7851676

# <-------------------------Prediction using GAM------------------------>

prediction_data <- read_excel("~/prediction.xlsx")

#7.190430 5.397135 4.262188 

prediction_result <- predict(generalized_additive_model,prediction_data)
prediction_result