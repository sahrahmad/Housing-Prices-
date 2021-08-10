#Predicting housing prices model
#Opening all libraries that would be required

library(readxl)
library(caret)
library(ggplot2)
library(tidyr)
library(car)
library(mice)
library(corrplot)
library(plyr)
library(dplyr)
library(moments)


#uploading test data set
test <-read.csv("C:\\Users\\sahra\\OneDrive\\Queens University\\Courses\\MMA867- Predicting Quantities\\Assignments\\Individual Assignment\\test.csv", header=TRUE, sep=",", stringsAsFactors = TRUE)
#uploading train data set
train <- read.csv("C:\\Users\\sahra\\OneDrive\\Queens University\\Courses\\MMA867- Predicting Quantities\\Assignments\\Individual Assignment\\train.csv", header=TRUE, sep=",", stringsAsFactors = TRUE)

#Data exploration
#test data has 80 columns and train data has 81 columns (Train data set has the final sales price information)
summary(train)
str(train)
#in the train data we observe the following NA's: 

summary(test)
str(test)
str(train)
head(test)

#Exploring the data and looking at the Sales Price data we have to predict

summary(train$SalePrice)

#Creating a histogram out of the training data to see the prices of the data. We can see it is positively skewed towards the lower priced houses
ggplot(train, aes(x=SalePrice, fill = ..count..)) + geom_histogram(binwidth=5000) + ggtitle("Sale Price Histogram")+ylab("No. of houses") + xlab("Price of House")

Salesqqplot <- qqPlot(train[!is.na(train$SalePrice),]$SalePrice)
#log of salesprice would have to be used as the prices of appear to be skewed

ggplot(train, aes(x=log(SalePrice), fill = ..count..)) + geom_histogram(binwidth=0.05) + ggtitle("Log of Sale Price Histogram")+ylab("No. of houses") + xlab("Price of House")

#Adding sales column to the test set to combine the data
test$SalePrice <- NA

#Correlation analysis on train data set
numericVals_train.data <- train %>% select(where(is.numeric))
correlation_train.data <-cor(numericVals_train.data, use = "pairwise.complete.obs")

corrplot(correlation_train.data, method = "circle")

#Combining the test and train data for data cleaning
housing_data <- rbind(train,test)
summary(housing_data)
str(housing_data)

#Determine the total number of NA's in the dataset
sum(is.na(housing_data))
na_count <-sapply(housing_data, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
print(na_count)

#Determine number of numeric variables
numericVariables_housing <- which(sapply(housing_data, is.numeric)) 
numericVarNames <- names(numericVariables_housing) 
print(numericVarNames)
#38 numeric variables including the sales price from the train data

# Looking at the correlations of all numeric variables

housing_numVar <- housing_data[, numericVariables_housing]
cor_numericVar <- cor(housing_numVar, use="pairwise.complete.obs")
cor_sorted <- as.matrix(sort(cor_numericVar[,'SalePrice'], decreasing = TRUE))
#selecting only the high correlations
CorHigh1 <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.5)))
cor_numericVar <- cor_numericVar[CorHigh1, CorHigh1]

corrplot.mixed(cor_numericVar, tl.col="black", tl.pos = "lt")

#We can witness high correlations between GarageArea and GarageCars, x1stFlrSF and TotalBsmt, GarageYrBlt and YearBuilt, YearRemodAdd & GarageYrBlt, Total RmsAbvGrd and GrLivArea
#yearbuilt and OverallQual

print(cor_numericVar)
sort(cor_numericVar[,'SalePrice'], decreasing = TRUE)

#OverallQual (Overall Quality has the highest Correlation) - we will plot and look at this
ggplot(data=housing_data[!is.na(housing_data$SalePrice),], aes(x=factor(OverallQual), y=SalePrice))+
  geom_boxplot(col='blue') + labs(x='Overall Quality') +
  scale_y_continuous(breaks= seq(0, 800000, by=100000))

#We can see the positive correlation from the boxplot - there may be one outlier in number 4 as the overall quality is low but the sale price is high

#Exploring Neighbourhoods to SalesPrice
neighbourhoodplot <- ggplot(train, aes(x = Neighborhood, y = SalePrice)) +
  geom_boxplot() 
neighbourhoodplot
#from the summary data we can see the following integers and factors that have NA values in the data 
#data which is factors need to be converted into characters

str(housing_data)
summary(housing_data)

#We will begin fixing the data
#Impute any missing variables or using the dataset to determine any missing variables
#Create ordinal variables
#For ordinal values we will define the following
Vector_Ordinal <- c('None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)

#MSZoning - has 4 NA's
#We will plot MSZoning to understand the data more. 

ggplot(housing_data, aes(x=MSZoning, fill = MSZoning)) + geom_bar() + ggtitle("MSZoning Distribution")
#After looking at the box plot we can see most houses are in the RL category (Residential Low Density)
#Mode replacement would be the best for the NA values based on this as most houses appear to be in RL category

housing_data$MSZoning <- as.character(housing_data$MSZoning)
housing_data$MSZoning[is.na(housing_data$MSZoning)] <- 'RL'
housing_data$MSZoning <- as.factor(housing_data$MSZoning)
summary(housing_data$MSZoning)

#LotFrontage (int) - 486 NA's - As there are a large number of NA's lot frontage could be linked to the neighbourhoods as neighbourhoods tend to have the same lot frontage
#We will use the median of each neighbourhood to assign the lot frontage area. 

LotFrontage_median <- housing_data[!is.na(housing_data$Neighborhood), c("Neighborhood","LotFrontage")] %>% 
  group_by(Neighborhood) %>% 
  summarize(median = median(LotFrontage, na.rm = T))
LotFrontage_median

rIndx <- which(is.na(housing_data$LotFrontage))
for(i in rIndx){
  median_lotvalue <- LotFrontage_median[LotFrontage_median$Neighborhood == housing_data$Neighborhood[i],"median"]
  housing_data$LotFrontage[i] <- median_lotvalue[[1]]
}
table(housing_data$LotFrontage)

#Alley (factor) - 2721 NA's - NA means No Alley
housing_data$Alley <- as.character(housing_data$Alley)
housing_data$Alley[is.na(housing_data$Alley)] <- "None"
housing_data$Alley <- as.factor(housing_data$Alley)

#Utilites(int) - 2 NA's - using mode to replace NA's
#Plotting the data to explore more - we can see majority fall into the AllPub category - which is all public utilties
ggplot(housing_data, aes(x=Utilities, fill = Utilities)) + geom_bar() + ggtitle("Utilities Distribution")
housing_data$Utilities <- as.character(housing_data$Utilities)
housing_data$Utilities[is.na(housing_data$Utilities)]<-'All Pub'
housing_data[housing_data[,"Utilities"] == "All Pub", "Utilities"] = "AllPub"
housing_data$Utilities <- as.factor(housing_data$Utilities)
table(housing_data$Utilities)

#Exterior1st - 1NA - using mode to replace NA
ggplot(housing_data, aes(x=Exterior1st, fill = Exterior1st)) + geom_bar() + ggtitle("Exterior Covering Distribution")
summary(housing_data$Exterior1st)
housing_data$Exterior1st <- as.character(housing_data$Exterior1st)
housing_data$Exterior1st[is.na(housing_data$Exterior1st)] <-'VinylSd'
housing_data$Exterior1st <- as.factor(housing_data$Exterior1st)
table(housing_data$Exterior1st)


#Exterior2nd - 1NA - using mode to replace NA - Exterior covering on house (if more than one material)
#Vinyl siding appears to be the highest through the plot and summary. We use the mode to replace this 1 NA value (since it is only 1 value)
ggplot(housing_data, aes(x=Exterior2nd, fill = Exterior2nd)) + geom_bar() + ggtitle("Exterior Covering (more than one material")
summary(housing_data$Exterior2nd)
housing_data$Exterior2nd <- as.character(housing_data$Exterior2nd)
housing_data$Exterior2nd[is.na(housing_data$Exterior2nd)] <-'VinylSd'
housing_data$Exterior2nd <- as.factor(housing_data$Exterior2nd)
table(housing_data$Exterior2nd)

#ExterQual 
#Convert to ordinal
table(housing_data$ExterQual)
housing_data$ExterQual <- as.numeric(factor(housing_data$ExterQual, levels = c('Po', 'Fa', 'TA', "Gd", "Ex")))
table(housing_data$ExterQual)

#ExterCond 
#Convert to ordinal
table(housing_data$ExterCond)
housing_data$ExterCond <- as.numeric(factor(housing_data$ExterCond, levels = c('Po', 'Fa', 'TA', "Gd", "Ex")))
table(housing_data$ExterCond)

#MasVnrType - 24 NA's - using mode to replace NA
ggplot(housing_data, aes(x=MasVnrType, fill = MasVnrType)) + geom_bar() + ggtitle("Masonry Veneer Type")
#Majority of the houses appear to have None as the Masonary Veneer so we will replace the 24 NA's with the mode here which is 'None'
housing_data$MasVnrType <- as.character(housing_data$MasVnrType)
housing_data$MasVnrType[is.na(housing_data$MasVnrType)] <- 'None'
housing_data$MasVnrType <- as.factor(housing_data$MasVnrType)
table(housing_data$MasVnrType)

#MasVnrArea - 23 NA's - NA - mean replacement
housing_data$MasVnrArea[is.na(housing_data$MasVnrArea)] <- mean(housing_data$MasVnrArea, na.rm = TRUE)

#BsmtQual - 81 NA's - NA means no basement
housing_data$BsmtQual <- as.character(housing_data$BsmtQual)
housing_data$BsmtQual[is.na(housing_data$BsmtQual)] <- "No basement"
housing_data$BsmtQual <- as.factor(housing_data$BsmtQual)
table(housing_data$BsmtQual)

#convert to ordinal 
housing_data$BsmtQual <- as.numeric(factor(housing_data$BsmtQual, levels = c('No basement', 'Po', 'Fa', 'TA', "Gd", "Ex")))
table(housing_data$BsmtQual)

#BsmtCond - 82 NA's - NA means no basement
housing_data$BsmtCond <- as.character(housing_data$BsmtCond)
housing_data$BsmtCond[is.na(housing_data$BsmtCond)] <- "No basement"
housing_data$BsmtCond <- as.factor(housing_data$BsmtCond)
table(housing_data$BsmtCond)

#check for ordinal and covert
housing_data$BsmtCond <- as.numeric(factor(housing_data$BsmtCond, levels = c('No basement', 'Po', 'Fa', 'TA', "Gd", "Ex")))
table(housing_data$BsmtCond)

#BsmtExposure - 82 NA's - NA means no basement
housing_data$BsmtExposure <- as.character(housing_data$BsmtExposure)
housing_data$BsmtExposure[is.na(housing_data$BsmtExposure)] <- "No basement"
housing_data$BsmtExposure <- as.factor(housing_data$BsmtExposure)
table(housing_data$BsmtExposure)
#check for ordinal and convert
housing_data$BsmtExposure <- as.numeric(factor(housing_data$BsmtExposure, levels = c('No basement', 'No', 'Mn', 'Av', "Gd", "Ex")))
table(housing_data$BsmtExposure)

#BsmtFinType1 - 79 NA's - NA means no basement
housing_data$BsmtFinType1 <- as.character(housing_data$BsmtFinType1)
housing_data$BsmtFinType1[is.na(housing_data$BsmtFinType1)] <- "No basement"
housing_data$BsmtFinType1 <- as.factor(housing_data$BsmtFinType1)
table(housing_data$BsmtFinType1)

#BsmtFinSF1 - 1 NA - mean replacement
housing_data$BsmtFinSF1[is.na(housing_data$BsmtFinSF1)] <- mean(housing_data$BsmtFinSF1, na.rm = TRUE)

#BsmtFinType2 - 80 NA's - NA means no basement
housing_data$BsmtFinType2 <- as.character(housing_data$BsmtFinType2)
housing_data$BsmtFinType2[is.na(housing_data$BsmtFinType2)] <- "No basement"
housing_data$BsmtFinType2 <- as.factor(housing_data$BsmtFinType2)
table(housing_data$BsmtFinType2)

#BsmtFinSF2 - 1 NA - mean replacement
housing_data$BsmtFinSF2[is.na(housing_data$BsmtFinSF2)] <- mean(housing_data$BsmtFinSF2, na.rm = TRUE)

#BsmyUnfSF - 1 NA - mean replacement
housing_data$BsmtUnfSF[is.na(housing_data$BsmtUnfSF)] <- mean(housing_data$BsmtUnfSF, na.rm = TRUE)

#TotalBsmtSF - 1 NA - mean replacement
housing_data$TotalBsmtSF[is.na(housing_data$TotalBsmtSF)] <- mean(housing_data$TotalBsmtSF, na.rm = TRUE)

#Electrical - 1 NA - mode replacement
#SBrkr appears to be the most common - Standard Circuit Breakers & Romex - it is 1 NA so we can use the mode
ggplot(housing_data, aes(x=Electrical, fill = Electrical)) + geom_bar() + ggtitle("Electrical Distribution")
housing_data$Electrical <- as.character(housing_data$Electrical)
housing_data$Electrical[is.na(housing_data$Electrical)] <-'SBrkr'
housing_data$Electrical <- as.factor(housing_data$Electrical)
table(housing_data$Electrical)

#BsmtFullBath - 2 NA's - mode replacement
ggplot(housing_data, aes(x=BsmtFullBath, fill = BsmtFullBath)) + geom_bar() + ggtitle("Full Bath in Basement")
#Most basements in the data appear not to have a full bath so we can assume 0 for the NA values through mode replacement
housing_data$BsmtFullBath[is.na(housing_data$BsmtFullBath)] <- 0
table(housing_data$BsmtFullBath)

#BsmtHalfBath - 2 NA's - mode replacement
ggplot(housing_data, aes(x=BsmtHalfBath, fill = BsmtHalfBath)) + geom_bar() + ggtitle("Half Bath in Basement")
#Most basements in the data appear not to have a half bath so we can assume 0 for the NA values through mode replacement
housing_data$BsmtHalfBath[is.na(housing_data$BsmtHalfBath)] <- 0
table(housing_data$BsmtHalfBath)

#KitchenQual - 1 NA - mode replacement
ggplot(housing_data, aes(x=KitchenQual, fill = KitchenQual)) + geom_bar() + ggtitle("KitchenQuality")
#Majority of the kitchens appear to fall in the TA (Typical/Average) or Gd (Good) category. As it is 1 NA we will use mode replacement
housing_data$KitchenQual <-as.character(housing_data$KitchenQual)
housing_data$KitchenQual[is.na(housing_data$KitchenQual)] <-'TA'
housing_data$KitchenQual <- as.factor(housing_data$KitchenQual)
table(housing_data$KitchenQual)

#Convert the factors to ordinal values
housing_data$KitchenQual <- as.numeric(factor(housing_data$KitchenQual, levels = c('Po', 'Fa', 'TA', "Gd", "Ex")))
table(housing_data$KitchenQual)

#Functional - 2 NA's - mode replacement
ggplot(housing_data, aes(x=Functional, fill = Functional)) + geom_bar() + ggtitle("Home Functionality")
#Typical (Typ) appears to be the most common - the data dictionary also states to assume Typical unless deductions are warrented
housing_data$Functional <-as.character(housing_data$Functional)
housing_data$Functional[is.na(housing_data$Functional)] <- 'Typ'
table(housing_data$Functional)

#FireplaceQu - 1420 NA's - NA means no fireplace
table(housing_data$FireplaceQu)
housing_data$FireplaceQu <- as.character(housing_data$FireplaceQu)
housing_data$FireplaceQu[is.na(housing_data$FireplaceQu)] <- "No fireplace"
housing_data$FireplaceQu <- as.factor(housing_data$FireplaceQu)
table(housing_data$FireplaceQu)

#Convert to ordinal values
housing_data$FireplaceQu <- as.numeric(factor(housing_data$FireplaceQu, levels = c('No fireplace', 'Po', 'Fa', 'TA', "Gd", "Ex")))
table(housing_data$FireplaceQu)

#GarageType - 157 NA's - NA means no garage
table(housing_data$GarageType)
housing_data$GarageType <- as.character(housing_data$GarageType)
housing_data$GarageType[is.na(housing_data$GarageType)] <- "No garage"
housing_data$GarageType <- as.factor(housing_data$GarageType)
table(housing_data$GarageType)

#GarageYrsBlt - 159 NA's - For the year the garage was built we will assume that the garage was built the same year as the house
housing_data$GarageYrBlt[is.na(housing_data$GarageYrBlt)]<- housing_data$YearBuilt[is.na(housing_data$GarageYrBlt)]
housing_data %>% ggplot(aes(GarageYrBlt)) + geom_histogram()
#when plotting this there appears to be an outlier 
summary(housing_data$GarageYrBlt)
#we can see that in the housing_data there is a maximum value is coming to 2207 which is not possible. We will replace this value with the year the house was built
summary(housing_data$GarageYrBlt)

GarageYrsBlt_outlier <- housing_data %>%
  filter(GarageYrBlt >2021)
#Check ID number for the outlier to be removed
GarageYrsBlt_outlier %>% select(Id, YearBuilt, GarageYrBlt, SalePrice)
which( colnames(housing_data)=="GarageYrBlt" )

housing_data[2593,60] <- 2006
summary(housing_data$GarageYrBlt)

#GarageFinish - 159 NA's - NA means No Garage

table(housing_data$GarageFinish)
housing_data$GarageFinish <- as.character(housing_data$GarageFinish)
housing_data$GarageFinish[is.na(housing_data$GarageFinish)] <- "No garage"
housing_data$GarageFinish <- as.factor(housing_data$GarageFinish)
table(housing_data$GarageFinish)

#check for ordinal and convert to ordinal
housing_data$GarageFinish <- as.numeric(factor(housing_data$GarageFinish, levels = c('No garage', 'Unf', 'RFn', "Fin")))

#GarageCars - 1 NA - mean replacement
housing_data$GarageCars[is.na(housing_data$GarageCars)] <- mean(housing_data$GarageCars, na.rm = TRUE)

#GarageArea - 1NA - mean replacement
housing_data$GarageArea[is.na(housing_data$GarageArea)] <- mean(housing_data$GarageArea, na.rm = TRUE)

#GarageQual - 159 NA - NA means no garage
table(housing_data$GarageQual)
housing_data$GarageQual <- as.character(housing_data$GarageQual)
housing_data$GarageQual[is.na(housing_data$GarageQual)] <- "No garage"
housing_data$GarageQual <- as.factor(housing_data$GarageQual)
table(housing_data$GarageQual)

#Check for ordinal values and convert
housing_data$GarageQual <- as.numeric(factor(housing_data$GarageQual, levels = c('No garage', 'Po', 'Fa', 'TA', "Gd", "Ex")))
table(housing_data$GarageQual)

#GarageCond - 159 NA - NA means no garage
table(housing_data$GarageCond)
housing_data$GarageCond <- as.character(housing_data$GarageCond)
housing_data$GarageCond[is.na(housing_data$GarageCond)] <- "No garage"
housing_data$GarageCond <- as.factor(housing_data$GarageCond)
table(housing_data$GarageCond)

#check for ordinal and convert
housing_data$GarageCond <- as.numeric(factor(housing_data$GarageCond, levels = c('No garage', 'Po', 'Fa', 'TA', "Gd", "Ex")))
table(housing_data$GarageQual)

#HeatQC convert to ordinal
table(housing_data$HeatingQC)
housing_data$HeatingQC <- as.numeric(factor(housing_data$HeatingQC, levels = c( 'Po', 'Fa', 'TA', "Gd", "Ex")))
table(housing_data$HeatingQC)


#PoolQC - 2909 NA's - NA means no pool
table(housing_data$PoolQC)
housing_data$PoolQC <- as.character(housing_data$PoolQC)
housing_data$PoolQC[is.na(housing_data$PoolQC)] <- "No pool"
housing_data$PoolQC <- as.factor(housing_data$PoolQC)
table(housing_data$PoolQC)

housing_data$PoolQC <- as.numeric(factor(housing_data$PoolQC, levels = c('No pool', 'Fa', 'TA', "Gd", "Ex")))
table(housing_data$PoolQC)

#Fence - 2348 NA's - NA means no fence
table(housing_data$Fence)
housing_data$Fence <- as.character(housing_data$Fence)
housing_data$Fence[is.na(housing_data$Fence)] <- "No Fence"
housing_data$Fence <- as.factor(housing_data$Fence)
table(housing_data$Fence)

#MiscFeature - 2814NA's - NA means None
table(housing_data$MiscFeature)
housing_data$MiscFeature <- as.character(housing_data$MiscFeature)
housing_data$MiscFeature[is.na(housing_data$MiscFeature)] <- "None"
housing_data$MiscFeature <- as.factor(housing_data$MiscFeature)
table(housing_data$MiscFeature)

#SaleType - 1 NA - mode 
ggplot(housing_data, aes(x=SaleType, fill = SaleType)) + geom_bar() + ggtitle("Type of Sale")
#From the plot we can see that Warranty Deed - Conventional (WD) is the most common so we will substitute the NA with mode replacement
housing_data$SaleType <-as.character(housing_data$SaleType)
housing_data$SaleType[is.na(housing_data$SaleType)] <- 'WD'

#Check if there are any missing values in dataset

md.pattern(housing_data)
#No missing values except for the 1459 test sales price values which is to be predicted from the training data


#Feature engineering - Adding some new variables
#Age of House
housing_data.newVar <- mutate(housing_data, Age = 2021 - housing_data$YearBuilt)

#Total number of bathrooms
housing_data.newVar <- mutate(housing_data.newVar, total_bathrooms = housing_data.newVar$FullBath+0.5*(housing_data.newVar$HalfBath)+housing_data.newVar$BsmtFullBath+0.5*housing_data.newVar$BsmtHalfBath)

housingdata_newVar_1 <- housing_data.newVar
#New House Variable
housingdata_newVar_1$NewHouse <- (housingdata_newVar_1$YearBuilt == housingdata_newVar_1$YrSold) * 1
#Total Square Footage Variable
housingdata_newVar_1$TotalSF <- housingdata_newVar_1$GrLivArea + housingdata_newVar_1$TotalBsmtSF
#Total Porch Variable
housingdata_newVar_1$TotalPorchSF <- housingdata_newVar_1$WoodDeckSF + housingdata_newVar_1$OpenPorchSF + housingdata_newVar_1$EnclosedPorch + housingdata_newVar_1$X3SsnPorch + housingdata_newVar_1$ScreenPorch

# Multiplying Overall Quality with numeric factors= Quality * (looking at numeric features for possible feature engineering and impact on model)
housingdata_newVar_1$YearBuiltQual <- housingdata_newVar_1$YearBuilt*housingdata_newVar_1$OverallQual      
housingdata_newVar_1$YearRemodQual <- housingdata_newVar_1$OverallQual*housingdata_newVar_1$YearRemodAdd 
housingdata_newVar_1$BsmtQual <- housingdata_newVar_1$OverallQual*housingdata_newVar_1$TotalBsmtSF   
housingdata_newVar_1$LivAreaQual <- housingdata_newVar_1$OverallQual*housingdata_newVar_1$GrLivArea   
housingdata_newVar_1$BathroomQual <- housingdata_newVar_1$OverallQual*housingdata_newVar_1$FullBath       

md.pattern(housing_data.newVar)

#run correlation on all housing data set
num.data <- housing_data %>% select(where(is.numeric))

cor.data <- cor(num.data, use="pairwise.complete.obs") #correlations of all numeric variables

#sort on decreasing correlations with SalePrice
cor.data.srted <- as.matrix(sort(cor.data[,'SalePrice'], decreasing = TRUE))
#select only high corelations
CorHigh <- names(which(apply(cor.data.srted, 1, function(x) abs(x)>0.5)))

cor.data <- cor.data[CorHigh, CorHigh]
corrplot(cor.data, tl.pos = "lt",method = 'number',tl.col = 'black')


#Highest correlation of the following: OverallQual, GrLivArea, GarageCars, GarageArea, TotralBsmtSF, X1stFrSF, TotRmsAbvGrd, GarageYrBlt, YearRemodAdd
#Now we will run a linear regression model with all the variables
#We will plot graphs of the explanatory variables


graph1 <- ggplot(housing_data.newVar,aes(x= SalePrice)) + geom_histogram()
plot(graph1)

graph2 <- ggplot(housing_data.newVar,aes(OverallQual, SalePrice)) + geom_jitter() + geom_smooth()
plot(graph2)

graph3 <- ggplot(housing_data.newVar,aes(GrLivArea, SalePrice)) + geom_jitter() + geom_smooth() 
plot(graph3)

graph4 <- ggplot(housing_data.newVar,aes(GarageCars, SalePrice)) + geom_jitter() + geom_smooth()
plot(graph4)

graph5 <- ggplot(housing_data.newVar,aes(GarageArea, SalePrice)) + geom_jitter() + geom_smooth()
plot(graph5)

graph6 <- ggplot(housing_data.newVar,aes(TotalBsmtSF, SalePrice)) + geom_jitter() + geom_smooth()
plot(graph6)

graph7 <- ggplot(housing_data.newVar,aes(X1stFlrSF, SalePrice)) + geom_jitter() + geom_smooth()
plot(graph7)

graph8 <- ggplot(housing_data.newVar,aes(FullBath, SalePrice)) + geom_jitter() + geom_smooth()

plot(graph8)

graph9 <- ggplot(housing_data.newVar,aes(TotRmsAbvGrd, SalePrice)) + geom_jitter() + geom_smooth()
plot(graph9)

graph10 <- ggplot(housing_data.newVar,aes(YearBuilt, SalePrice)) + geom_jitter() + geom_smooth()
plot(graph10)

graph11 <- ggplot(housing_data.newVar,aes(GarageYrBlt, SalePrice)) + geom_jitter() + geom_smooth()
plot(graph11)

graph12 <- ggplot(housing_data.newVar,aes(YearRemodAdd, SalePrice)) + geom_jitter() + geom_smooth()
plot(graph12)

#after plotting the graphs we can see the following have outliers - Total BasementSF,X1stFlrSF, GrLivArea, GarageArea

#Check the summary to determine the outliers
summary(housing_data.newVar$TotalBsmtSF)
summary(housing_data.newVar$X1stFlrSF)
summary(housing_data.newVar$GrLivArea)
summary(housing_data.newVar$GarageArea)

#Replace outlier in TotalBsmtSF by median replacement
housing_data.newVar[housing_data.newVar[,"TotalBsmtSF"] == 6110, "TotalBsmtSF"] = median(housing_data.newVar[, "TotalBsmtSF"])
housing_data.newVar[housing_data.newVar[,"X1stFlrSF"] == 5095, "X1stFlrSF"] = median(housing_data.newVar[, "X1stFlrSF"])
housing_data.newVar[housing_data.newVar[,"GrLivArea"]>4500, "GrLivArea"] = median(housing_data.newVar[, "GrLivArea"])
summary(housing_data.newVar$GrLivArea)

#Check plots once again to check for outlier
graph13 <- ggplot(housing_data.newVar,aes(TotalBsmtSF, SalePrice)) + geom_jitter() + geom_smooth()
plot(graph13)

graph14 <- ggplot(housing_data.newVar,aes(X1stFlrSF, SalePrice)) + geom_jitter() + geom_smooth()
plot(graph14)

graph15 <- ggplot(housing_data.newVar,aes(GrLivArea, SalePrice)) + geom_jitter() + geom_smooth()
plot(graph15)

#still a problem in X1stFlrSF

summary(housing_data.newVar$X1stFlrSF)
housing_data.newVar[housing_data.newVar[,"X1stFlrSF"] == 4692, "X1stFlrSF"] = median(housing_data.newVar[, "X1stFlrSF"])
graph16 <- ggplot(housing_data.newVar,aes(X1stFlrSF, SalePrice)) + geom_jitter() + geom_smooth()
plot(graph16)

summary(housing_data.newVar$GrLivArea)

#Removing NA's in the sales column 
housing.data.clean.noNA <- housing_data.newVar
housing.data.clean.noNA[is.na(housing.data.clean.noNA)] <- 0

#Zerovariance variables
near.zero.var_data <- nearZeroVar( housing.data.clean.noNA)
colnames(housing.data.clean.noNA)[near.zero.var_data]

#The following are the variables with near zero variance data
# "Street"        "Alley"         "LandContour"   "Utilities"     "LandSlope"     "Condition2"    "RoofMatl"     
#"BsmtCond"      "BsmtFinType2"  "BsmtFinSF2"    "Heating"       "LowQualFinSF"  "KitchenAbvGr"  "Functional"   
# "OpenPorchSF"   "EnclosedPorch" "X3SsnPorch"    "ScreenPorch"   "PoolArea"      "PoolQC"        "MiscFeature"  
# "MiscVal" 

#drop the variables with near zero variance
drop.zerovar <- c("Street","Alley","LandContour","Utilities","LandSlope","Condition2","RoofMatl","BsmtCond", "BsmtFinType2","BsmtFinSF2", "Heating","LowQualFinSF","KitchenAbvGr","Functional","OpenPorchSF","EnclosedPorch","X3SsnPorch","ScreenPorch","PoolArea","PoolQC","MiscFeature","MiscVal")
housing.data.clean.noNA<-housing.data.clean.noNA[,!(names(housing.data.clean.noNA) %in% drop.zerovar)]

#drop highly correlated variables- GarageArea, X1stFlrSF, TotalRmsAboveGr, GarageYrBuilt
dropVars <- c( "GarageYrBlt", "GarageArea", "TotalRmsAbvGrd", "X1stFlrSF")
housing.data.clean.noNA <- housing.data.clean.noNA[,!(names(housing.data.clean.noNA) %in% dropVars)]

#Check for skewness - more than 0.8 skewness
#Firt determine which values are numeric and not ordinal
numericVariables <- which(sapply(housing.data.clean.noNA, is.numeric))
print(numericVariables)

#From the list of numeric variables the following are not ordinal and need to be tested for skewness
#"SalePrice", "BsmtFinSF1", "MSSubClass", "LotFrontage", "LotArea", "MasVnrArea", "BsmtUnfSF", "TotalBsmtSF"
#X2ndFlrSF", "GrLivArea", "Fireplaces", "GarageCars", "TotRmsAbvGrd", "BedroomAbvGr", "total_bathrooms"
#

skewness(housing.data.clean.noNA$SalePrice) #log required skewness of 1.098
hist(housing.data.clean.noNA$SalePrice, main = "Sale Price", xlab = "Sale Price")

skewness(housing.data.clean.noNA$BsmtFinSF1) #log required skewness of 1.42
hist(housing.data.clean.noNA$BsmtFinSF1, main = "Basement Finished SqFt1", xlab = "Finished Basement Sq Ft")

skewness(housing.data.clean.noNA$LotFrontage) # 0.022 - not required

skewness(housing.data.clean.noNA$LotArea) # log required skewness 12.822
hist(housing.data.clean.noNA$LotArea, main = "Lot Area", xlab = "Lot Area")

skewness(housing.data.clean.noNA$MasVnrArea) #log required skewness 2.6115
hist(housing.data.clean.noNA$MasVnrArea, main = "Masonry Veneer Area", xlab = "Masonry Veneer Area")

skewness(housing.data.clean.noNA$BsmtUnfSF) #log required skewness of 0.919
hist(housing.data.clean.noNA$BsmtUnfSF, main = "Unfinished Basement Sq Ft", xlab = "Unfinished Basement Sq Ft")
     
skewness(housing.data.clean.noNA$TotalBsmtSF)# 0.702 - not required
skewness(housing.data.clean.noNA$X2ndFlrSF)#log required skewness of 0.8616
hist(housing.data.clean.noNA$X2ndFlrSF, main = "2nd Floor Sq Ft", xlab = "2nd Floor Sq Ft")

skewness(housing.data.clean.noNA$GrLivArea) #log required skewness of 1.128475
hist(housing.data.clean.noNA$GrLivArea, main = "Above Ground Living Area", xlab = "Above Ground Living Area")

skewness(housing.data.clean.noNA$Fireplaces) # 0.733 not required
skewness(housing.data.clean.noNA$GarageCars) #-0.21 not required
skewness(housing.data.clean.noNA$TotRmsAbvGrd) #0.7583 not required
skewness(housing.data.clean.noNA$BedroomAbvGr) #0.326 not required
skewness(housing.data.clean.noNA$total_bathrooms) #0.4922 not required
skewness(housing.data.clean.noNA$FullBath) #0.167 not required
skewness(housing.data.clean.noNA$HalfBath) #0.69 not required
skewness(housing.data.clean.noNA$BsmtFullBath) #0.62 not required
skewness(housing.data.clean.noNA$BsmtHalfBath) #log required skewness of 3.931


#taking a log of all the skewed variables in the dataset

housing.data.clean.log <- housing.data.clean.noNA
housing.data.clean.log$BsmtFinSF1 <- log(housing.data.clean.log$BsmtFinSF1 + 1)
housing.data.clean.log$LotArea <- log(housing.data.clean.log$LotArea + 1)
housing.data.clean.log$MasVnrArea <- log(housing.data.clean.log$MasVnrArea +1)
housing.data.clean.log$BsmtUnfSF <- log(housing.data.clean.log$BsmtUnfSF + 1)
housing.data.clean.log$X2ndFlrSF <- log(housing.data.clean.log$X2ndFlrSF + 1)
housing.data.clean.log$GrLivArea <- log(housing.data.clean.log$GrLivArea + 1)
housing.data.clean.log$BsmtHalfBath <- log(housing.data.clean.log$BsmtHalfBath + 1)


#Splitting the cleaned data once again into the test and train datasets

train_clean <- slice(housing.data.clean.log, 1:1460)
test_clean <- slice(housing.data.clean.log, 1461:2919)

colnames(housing.data.clean.log)

#Building our model on the clean training data cleaned dataset

housing.subset.testing<-subset(train_clean, (Id>=1169 & Id <=1460)) #withold 20% of data into a "testing" data
housing.subset.training<-subset(train_clean, (Id<=1168)) #redefine the training data

#Conducting a stepwise regression


fit <- lm(SalePrice ~ MSSubClass +MSZoning + LotFrontage + LotArea + LotShape + LotConfig + Neighborhood + Condition1 + BldgType + HouseStyle + OverallQual
          + OverallCond +YearBuilt + YearRemodAdd + MasVnrType + MasVnrArea + + ExterQual + ExterCond
          + Foundation + BsmtQual + BsmtExposure + BsmtFinType1+ BsmtFinSF1 + BsmtUnfSF + TotalBsmtSF + HeatingQC + 
            CentralAir + Electrical + X2ndFlrSF + GrLivArea + BsmtFullBath + BsmtHalfBath + FullBath + HalfBath +
            BedroomAbvGr + KitchenQual + TotRmsAbvGrd + Fireplaces + FireplaceQu + GarageType + GarageFinish + GarageCars+
            GarageQual + GarageCond + PavedDrive + WoodDeckSF + Fence + MoSold + YrSold + SaleType + SaleCondition +Age 
          +total_bathrooms, housing.subset.training)

summary(fit)

#predicting prices on the cleaned test model

predicted.prices.testing <- predict(fit, housing.subset.testing)
percent.errors <-abs((housing.subset.testing$SalePrice-predicted.prices.testing)/housing.subset.testing$SalePrice)*100
mean(percent.errors)

#Testing regression again after removing variables Electrical, FUllBath, HalfBath, GarageTypeAttchd, GarageType, Fence, YrSold, WoodDeckSF
#PavedDrive, FireplaceQu, all bathrooms, MasVnrType, MasVnrArea, Year Built

fit1 <- lm(SalePrice ~ LotArea + LotShape + LotConfig + Neighborhood + Condition1 + BldgType + HouseStyle + OverallQual
          + OverallCond + YearRemodAdd + ExterQual + ExterCond
          + BsmtQual + BsmtExposure + BsmtFinType1+ BsmtFinSF1 + BsmtUnfSF + TotalBsmtSF + 
          + X2ndFlrSF + GrLivArea + BedroomAbvGr + KitchenQual + TotRmsAbvGrd + Fireplaces + GarageCars+
          + MoSold + SaleType + SaleCondition +Age 
          , housing.subset.training)

summary(fit1)
predicted.prices.testing1 <- predict(fit1, housing.subset.testing)
percent.errors <-abs((housing.subset.testing$SalePrice-predicted.prices.testing1)/housing.subset.testing$SalePrice)*100
mean(percent.errors)


# repeat the same for the log model 
fit.log<-lm(log(SalePrice)~ MSSubClass +MSZoning + LotFrontage + LotArea + LotShape + LotConfig + Neighborhood + Condition1 + BldgType + HouseStyle + OverallQual
            + OverallCond +YearBuilt + YearRemodAdd + MasVnrType + MasVnrArea + + ExterQual + ExterCond
            + Foundation + BsmtQual + BsmtExposure + BsmtFinType1+ BsmtFinSF1 + BsmtUnfSF + TotalBsmtSF + HeatingQC + 
              CentralAir + Electrical + X2ndFlrSF + GrLivArea + BsmtFullBath + BsmtHalfBath + FullBath + HalfBath +
              BedroomAbvGr + KitchenQual + TotRmsAbvGrd + Fireplaces + FireplaceQu + GarageType + GarageFinish + GarageCars+
              GarageQual + GarageCond + PavedDrive + WoodDeckSF + Fence + MoSold + YrSold + SaleType + SaleCondition +Age 
            +total_bathrooms,data = housing.subset.training)
summary(fit.log)

predicted.prices.testing.log<-exp(predict(fit.log, housing.subset.testing))

percent.errors.log <- abs((housing.subset.testing$SalePrice-predicted.prices.testing.log)/housing.subset.testing$SalePrice)*100
mean(percent.errors.log) 

#log model after removing the variables (moved additional variables ExterQual and ExterCond however this didnt impact the mean error much)

fit.log1 <- lm(log(SalePrice) ~MSZoning  +  LotArea  + LotShape + LotConfig + Neighborhood + Condition1 + BldgType + OverallQual
           + OverallCond  + YearBuilt + BsmtQual + BsmtFinType1 + BsmtExposure + BsmtFinSF1 + BsmtUnfSF + TotalBsmtSF + HeatingQC
           + GrLivArea + KitchenQual + Foundation + Fireplaces + GarageFinish + GarageCars+
             GarageQual + SaleType + SaleCondition, housing.subset.training)

summary(fit.log1)

predicted.prices.testing.log1<-exp(predict(fit.log1, housing.subset.testing))

percent.errors.log1 <- abs((housing.subset.testing$SalePrice-predicted.prices.testing.log1)/housing.subset.testing$SalePrice)*100
mean(percent.errors.log1) 

#improvement in the percentage error after we apply the log and remove the variables. 
#Will further test this model through Lasso and Ridge models 

library(glmnet)

#create the y variable and matrix (capital X) of x variables (will make the code below easier to read + will ensure that all interactoins exist)
y<-log(housing.subset.training$SalePrice)
X<-model.matrix(Id~.-SalePrice, housing.data.clean.log)[,-1]
X<-cbind(housing.data.clean.log$Id,X)

# split X into testing, training/holdout and prediction as before
X.training<-subset(X,X[,1]<=1168)
X.testing<-subset(X, (X[,1]>=1169 & X[,1]<=1460))
X.prediction<-subset(X,X[,1]>=1461)

#LASSO (alpha=1)
lasso.fit<-glmnet(x = X.training, y = y, alpha = 1)
plot(lasso.fit, xvar = "lambda")

#selecting the best penalty lambda
crossval <-  cv.glmnet(x = X.training, y = y, alpha = 1) #create cross-validation data
plot(crossval)
penalty.lasso <- crossval$lambda.min #determine optimal penalty parameter, lambda
log(penalty.lasso) #see where it was on the graph
plot(crossval,xlim=c(-6,-4),ylim=c(0,0.03))
lasso.opt.fit <-glmnet(x = X.training, y = y, alpha = 1, lambda = penalty.lasso) #estimate the model with the optimal penalty
coef(lasso.opt.fit) #resultant model coefficients

# predicting the performance on the testing set
lasso.testing <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx =X.testing))
mean(abs(lasso.testing-housing.subset.testing$SalePrice)/housing.subset.testing$SalePrice*100) #calculate and display MAPE

lasso.prediction <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx =X.prediction))

Submission_file <- select(test_clean,Id)

Submission_file <- cbind(Submission_file,lasso.prediction)

write.csv(Submission_file,"Seventh Attempt.csv")

#ridge (alpha=0)
ridge.fit<-glmnet(x = X.training, y = y, alpha = 0)
plot(ridge.fit, xvar = "lambda")

#selecting the best penalty lambda
crossval <-  cv.glmnet(x = X.training, y = y, alpha = 0)
plot(crossval)
penalty.ridge <- crossval$lambda.min 
log(penalty.ridge) 
ridge.opt.fit <-glmnet(x = X.training, y = y, alpha = 0, lambda = penalty.ridge) #estimate the model with that
coef(ridge.opt.fit)

ridge.testing <- exp(predict(ridge.opt.fit, s = penalty.ridge, newx =X.testing))
mean(abs(ridge.testing-housing.subset.testing$SalePrice)/housing.subset.testing$SalePrice*100) 

ridge.prediction <- exp(predict(ridge.opt.fit, s = penalty.ridge, newx =X.prediction))

Submission_file_ridge <- select(test_clean,Id)

Submission_file_ridge <- cbind(Submission_file,ridge.prediction)

write.csv(Submission_file_ridge,"Ridge 5th Attempt.csv")
