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
cars_no_na <- cars_no_na[-c(1120),]
cars_no_na[which.max(cars_no_na$MSRP),]
common_consumer <- cars_no_na %>% filter(MSRP < 100000)
weird <- cars_no_na %>% filter(Engine.Fuel.Type == '')

# Plots
cars_no_na %>% ggplot(aes(y = MSRP, x = Popularity)) + geom_point() + ggtitle('Popularity vs MSRP')
cars_no_na %>% ggplot(aes(x = Year, y = MSRP, color = Popularity)) + geom_jitter() + ggtitle('Change in MSRP and Popularity Over Time')
common_consumer %>% ggplot(aes(x = Year, y = MSRP, color = Popularity)) + geom_point() + ggtitle('Change in MSRP and Popularity Over Time')
common_consumer %>% ggplot(aes(x = combined.mpg, y = MSRP, color = Popularity)) + geom_point() + ggtitle('Change in MSRP and Combined MPG')
cars_no_na %>% ggplot(aes(x = combined.mpg, y = MSRP, color = Popularity)) + geom_point() + ggtitle('Change in MSRP and Combined MPG')
common_consumer %>% ggplot(aes(y = MSRP, fill = Engine.Cylinders, x = Popularity)) + geom_boxplot() + ggtitle('Boxplot of Engine Cylinders and Popularity')
cars_no_na %>% ggplot(aes(y = MSRP, fill = Engine.Cylinders, x = Popularity)) + geom_boxplot() + ggtitle('Boxplot of Engine Cylinders and Popularity')
common_consumer %>% ggplot(aes(y = MSRP, x = Popularity, fill = Engine.Fuel.Type)) + geom_boxplot()

