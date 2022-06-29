# <--------------------------LOADING THE DATASET----------------------->

library(readxl)

acceleration_data <- read_excel("~/Desktop/PROJECT_DATA.xlsx")

# <-------------------------DATA- CLEANING PROCEDURE-------------------->

acceleration_data <- acceleration_data[,-1]

acceleration_data <- na.omit(acceleration_data)

names(acceleration_data)

library(ggplot2)

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

attach(acceleration_data)

# <-----------------VISUALIZATION OF RESPONSE VARIABLE------------------->

index_of_acceleration_time_variable <- as.numeric(row.names(acceleration_data))

ggplot(data=acceleration_data)+geom_point(mapping = aes(x=`index_of_acceleration_time_variable`,y=ACCELERATION_TIME))


# <-----------------VISUALIZATION OF RESPONSE VARIABLE WITH PREDICTORS---------->

ggplot(data=acceleration_data)+geom_point(mapping = aes(x=`PEAK_POWER`,y=ACCELERATION_TIME))+ ggtitle("Peak Power vs Acceleration_time")
ggplot(data=acceleration_data)+geom_point(mapping = aes(x=`RPM_FOR_PEAK_POWER`,y=ACCELERATION_TIME))+ggtitle("RPM for Peak Power vs Acceleration_time")
ggplot(data=acceleration_data)+geom_point(mapping = aes(x=`PEAK_TORQUE`,y=ACCELERATION_TIME))+ggtitle("Peak Torque vs Acceleration_time")

ggplot(data=acceleration_data)+geom_point(mapping = aes(x=`RPM_FOR_PEAK_TORQUE`,y=ACCELERATION_TIME)) + ggtitle("Rpm_For_Peak_Torque vs Acceleration_time")
ggplot(data=acceleration_data)+geom_point(mapping = aes(x=`WEIGHT`,y=ACCELERATION_TIME)) + ggtitle("Weight vs Acceleration_time")
ggplot(data=acceleration_data)+geom_point(mapping = aes(x=`GEAR_RATIO_1`,y=ACCELERATION_TIME)) + ggtitle("Gear_Ratio_1 vs Acceleration_time")

ggplot(data=acceleration_data)+geom_point(mapping = aes(x=`GEAR_RATIO_2`,y=ACCELERATION_TIME)) + 
    ggtitle("2nd gear ratio vs Acceleration_time")
ggplot(data=acceleration_data)+geom_point(mapping = aes(x=`GEAR_RATIO_3`,y=ACCELERATION_TIME))+ 
    ggtitle("3rd gear ratio vs Acceleration_time")
ggplot(data=acceleration_data)+geom_point(mapping = aes(x=`FINAL_DRIVE_AXLE_RATIO`,y=ACCELERATION_TIME))+ 
    ggtitle("Final drive ratio vs Acceleration_time")

boxplot(ACCELERATION_TIME~DRIVE_TRAIN_CATEGORY,dataset = acceleration_data,main = "BOXPLOT_w.r.t._DRIVE_TRAIN_CATEGORY") 

