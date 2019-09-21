#Remove previous data to start new analysis
rm(list = ls())

#set working directory
setwd("I:/DATA Scientist Assignments/Bike rental project")

x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees')

lapply(x, require, character.only = TRUE)
rm(x)

#Check weather directory has been update or not
getwd()

#Load the data for analysis
dataBike = read.csv("day.csv", header = T)[-1]

#Extract sample top 10 data
head(dataBike, 10)

#-----------------------------------Exploratory Analysis----------------------------------#
#Structure of data
str(dataBike)
length(colnames(dataBike))
names(dataBike)

#remove variables those are not contains significant information
dataBike = subset(dataBike, select = -c(dteday,casual,registered))

names(dataBike)

#Rename the variable
names(dataBike)[names(dataBike) == "yr"] = "year"
names(dataBike)[names(dataBike) == "mnth"] = "month"
names(dataBike)[names(dataBike) == "hum"] = "humidity"
names(dataBike)[names(dataBike) == "cnt"] = "count"
names(dataBike)[names(dataBike) == "temp"] = "temperature"   

#convert into factor data type
dataBike$season = as.factor(dataBike$season)
dataBike$year = as.factor(dataBike$year)
dataBike$holiday = as.factor(dataBike$holiday)
dataBike$weathersit = as.factor(dataBike$weathersit)
dataBike$workingday = as.factor(dataBike$workingday)
dataBike$month = as.factor(dataBike$month)
dataBike$weekday = as.factor(dataBike$weekday)

#---------------------Data understanding--------------------#
#load library ggplot2 form apply graphical views
library(ggplot2)
#plot b/w count and season
ggplot(dataBike, aes(x = temperature, y = count)) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.2))+
  geom_point(aes(color = humidity))+
  labs( title = "Bike count with respect to normalised temperature  and humidity")

ggplot(dataBike, aes(x = temperature, y = count, color = windspeed, size = humidity)) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.2))+
  geom_point()+
  labs( title = "Bike count with respect to normalised temperature  and windspeed")

ggplot(dataBike, aes(x = temperature, y = count, color = season, size = count, shape = weathersit)) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.2))+
  geom_point()+
  labs( title = "Bike count with respect to normalised temperature  and weathersit")

ggplot(dataBike, aes(x = windspeed)) + 
  geom_histogram(aes(y =..density..),
                 breaks = seq(0, 0.5, by = 0.1), 
                 fill="blue",
                 alpha = .4,position="dodge") + 
  geom_density(col=4) +
  labs(title="Distribution of windspeed") +
  labs(x="windspeed", y="Density of windspeed") +
  theme(legend.position="top")

#---------------------------------Data Pre Processing---------------------------#
#-----------------Missing value analysis------------------#
missingValue = data.frame(apply(dataBike,2, function(x){sum(is.na(x))}))
rm(missingValue)

#---------------Outlier Analysis-----------------------#
#data manupulation: convert string categories into factor numeric
for(i in 1:ncol(dataBike)){
  if(class(dataBike[,i]) == 'factor'){
    dataBike[,i] = factor(dataBike[,i], labels = (1:length(levels(factor(dataBike[,i])))))
  }
}  
rm(i)

#Boxplot to find outlier 
library(ggplot2)
number_index = sapply(dataBike, is.numeric)
numeric_data = dataBike[, number_index]
cnames = colnames(numeric_data)
for(i in 1:length(cnames)){
  assign(paste0("DB", i), ggplot(aes_string(y = (cnames[i]), x = "count"), data = subset(dataBike))+ 
                       stat_boxplot(geom = "errorbar", width = 0.5) +
                       geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                                    outlier.size=1, notch=FALSE) +
                       theme(legend.position="bottom")+
                       labs(y=cnames[i],x="count")+
                       ggtitle(paste("Box plot for",cnames[i])))
}
rm(i)

## Plotting plots together
gridExtra::grid.arrange(DB1, DB2,DB3, ncol=3)
gridExtra::grid.arrange(DB4,DB5,ncol=2)

#Remove outlier using boxplot
temp = dataBike
dataBike = temp

for (i in cnames){
  val = dataBike[,i][dataBike[,i] %in% boxplot.stats(dataBike[,i])$out]
  print(val)
  dataBike[,i][dataBike[,i] %in% val] = NA
}

#Actual value = 0.318333
#mean  0.6297861
#median  0.6294766
#knn 0.4374127
missingValue = data.frame(apply(dataBike,2,function(x){ sum(is.na(x))}))
dataBike$humidity[62]
dataBike$humidity[is.na(dataBike$humidity)] = mean(dataBike$humidity, na.rm = T)
dataBike$humidity[is.na(dataBike$humidity)] = median(dataBike$humidity, na.rm = T)
dataBike = knnImputation(dataBike, k=3)

#----------------feature selection-----------------#
#Correltion analysis for continous variales
corrgram(dataBike[,number_index], order = F ,upper.panel = panel.pie,
         text.panel = panel.txt, main ="correlation plot for numeric variables")
#variable temperature and atemp are highly correlated with each other  

#ANOVA test for categorical variables
factorVal = sapply(dataBike, is.factor)
factorVariables = dataBike[, factorVal]
cat_variables = names(factorVariables)
 for(i in cat_variables){
   print(i)
   anovaresult = summary(aov(formula = count~dataBike[,i],dataBike))
   print(anovaresult)
 }

#remove variable after apply correlation and ANOVA
dataBike = subset(dataBike, select = -c(atemp,holiday,weekday,workingday))

#-------------feature scaling----------------------#
cat_del_ind = sapply(dataBike, is.numeric)
cat_del = dataBike[, cat_del_ind]
cnames_del = names(cat_del)
#skewness test
library(propagate)
for(i in cnames_del){
  print(i)
  skew = skewness(dataBike[,i])
  print(skew)
}
hist(dataBike$temperature, col = "blue", xlab = "temperature", ylab = "Frequency",
                      main = "histogram of teperature")
hist(dataBike$humidity, col = "blue", xlab = "Humidity", ylab = "Frequency",
                      main = "histogram of humidity")
hist(dataBike$windspeed, col = "blue", xlab = "windspeed", ylab = "Frequency",
                      main = "histogram of windspeed")
                      
#summary
for(i in cnames_del){
  print(summary(dataBike[,i]))
}
#as summary the data is in normalised form so no need to scaling

#write the pre processed data to drive
write.csv(dataBike, "dataBike_count.csv", row.names = FALSE)


#----------------------Model Developemnt---------------------#
#Clean the environment
rmExcept("dataBike")

#MAPE
#calculate MAPE
MAPE = function(y, y1){
  mean(abs((y - y1)/y))
}

#R square
rsquare = function(y,y1){
  cor(y,y1)^2
}

#Divide data into train and test using stratified sampling method
set.seed(1234)
train.index = sample(1:nrow(dataBike), .80 * nrow(dataBike))
train = dataBike[ train.index,]
test  = dataBike[-train.index,]

#---------------------Decision tree---------------#
#Load Libraries
library(rpart)
library(MASS)

# ##rpart for regression
DT_model = rpart(count ~ ., data = train, method = "anova")

#Predict for train cases
train_DT = predict(DT_model, train[-8])

#predict for test cases
test_DT = predict(DT_model, test[-8])

#MAPE 
MAPE_DT_train = (MAPE(train[,8], train_DT))*100
#MAPE_DT_train = 53.17%
MAPE_DT_test = (MAPE(test[,8], test_DT))*100
#MAPE_DT_test = 23.52%

#Rsquare
rquare_train_DT = rsquare(train[,8], train_DT)
#rquare_train = 0.8181892
rquare_test_DT = rsquare(test[,8], test_DT)
#rquare_test =  0.8430761

#------------------Random Forest-----------------#
library(randomForest)
#delevelop model using random forest
RF_model = randomForest(count~., dataBike, nTree = 500, importance = TRUE)

#apply on train data
RF_train_predict = predict(RF_model, train[,-8]) 

#apply on test data
RF_test_predict = predict(RF_model, test[,-8])

#MAPE for train
RF_MAPE_train = (MAPE(train[,8], RF_train_predict))*100
#MAPE 23.73%

#MAPE for test
RF_MAPE_test = (MAPE(test[,8], RF_test_predict))*100
#MAPE 10.27%

#RSquare for train
RSquare_train_RF= rsquare(train[,8], RF_train_predict)
#Rsquare 0.9635951

#RSquare for test
RSquare_test_RF = rsquare(test[,8], RF_test_predict)
#Rsquare 0.9744973

#----------------------Linear Regression----------------#
library(usdm)
cnames = c("temperature", "humidity" ,"windspeed")
vif(dataBike[,cnames])
vifcor(dataBike[,cnames], th = 0.9)

#develop linear regression model
LR_model = lm(count~., data = train)
summary(LR_model)

#apply on train data
LR_train = predict(LR_model, train[,-8])

#apply on test
LR_test = predict(LR_model, test[,-8])

#MAPE for train
LR_MAPE_train = (MAPE(train[,8],LR_train))*100
#MAPE 44.75%

#MAPE for test
LR_MAPE_test = (MAPE(test[,8], LR_test))*100
#MAPE 17.39%

#Rsquare for train
rsquare_train_LR = rsquare(train[,8],LR_train)
#0.8231024

#Rsquare for test
rsquare_test_LR = rsquare(test[,8], LR_test)
#0.8886749

#------------------Result-------------------#
result = data.frame(Model = c('Decision Tree', 'Ramdon Forest', 'Linear Regression'),
                    'MAPE Train' = c(MAPE_DT_train,RF_MAPE_train,LR_MAPE_train),
                    'MAPE Test' = c(MAPE_DT_test,RF_MAPE_test,LR_MAPE_test),
                    'RSquare Train' = c(rquare_train_DT,RSquare_train_RF,rsquare_train_LR),
                    'RSquare Test' = c(rquare_test_DT,RSquare_test_RF,rsquare_test_LR))
write.csv(result, "Result,csv", row.names = FALSE)


###############Thank You#####################