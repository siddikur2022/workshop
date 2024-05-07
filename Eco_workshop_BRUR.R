#Setting home directory
getwd()
setwd("/Users/siddikurrahman/Desktop/Economics_BRUR")
list.files()

## Problem with source 
.libPaths()
.libPaths("/Users/siddikurrahman/Desktop")

#Data read in Excel
# Install packages
install.packages("readxl") 
library(readxl) 
# read the first worksheet from the workbook myexcel.xlsx 
# first row contains variable names 
dataEXCEL <- read_excel("sales.xlsx",1)
dfEXCEL <- as.data.frame(dataEXCEL) 

# read in the worksheet named sheet1 
dataEXCEL <- read_excel("sales.xlsx",sheet = 1) 
dfEXCEL <- as.data.frame(dataEXCEL) 

# read data in csv format
dataCOMMA <-read.csv("sales.csv", header=TRUE)
dataCOMMA

# Read data in SPSS
# Method 1: Using haven Package
install.packages('haven')
library(haven) 
dataspss <- read_sav("real_estate.sav")                         

# Method 2: Using foreign Package
install.packages('foreign')
library("foreign") 
dataspss1 <- read.spss("airline_passengers.sav", 
                       to.data.frame = TRUE) 
head(dataspss1)

# Method 3: Using Hmisc Package
install.packages('Hmisc')
library("Hmisc") 
data2 <- spss.get("airline_passengers.sav", 
                  to.data.frame = TRUE) 
head(data2)

# file.choose() function 
datacsv<-read.csv(file.choose(),header = T);d
dataEXCELL <- read_excel(file.choose()) 
dataspss=read_sav(file.choose())

# Show data
head(dataspss)
dataspss <- as.data.frame(dataspss) 
dataspss
names(dataspss)

# check if necessary/not:  
options(max.print=999999)

# Check missing value
any(is.na(dataspss))
head(dataspss)

# Descriptive statistics
summary(dataspss)

# Graphics 
Selling_price=dataspss$Y
Number_of_bedroom=dataspss$X1
Home_size=dataspss$X2
Pool=dataspss$X3
Distance_from_the_center_of_the_city=dataspss$X4
Township = factor(dataspss$X5,  labels = c("Gulshan", "Uttara", "DOHS", "Dhanmondi", "Banani"))
Garage_attached=dataspss$X6 
Number_of_bathrooms=dataspss$X7
x=table(Township)
x
#adjust plot margins: Figures margin too large
par(mar = c(1, 1, 1, 1))
# Bar plot
barplot(x, main="Barplot of Township",col = "red")

#Histogram
hist(dataspss$Y,main= "Histogram of Selling Price", col = "green")
#Pie Chart
pie(x,main="Pie Chart of Township")

#Bivariate analysis:scatter plot with x and y 
plot(dataspss$X1, dataspss$Y, main = "Scatter plot of X1 and Y")

#Outlier handling:boxplot of x and y 
box = boxplot(dataspss$Y)

# Correlation in R
# cor Function
cor(dataspss)
#The rcorr Function
#create matrix of correlation coefficients and p-values
library(Hmisc)
rcorr(as.matrix(dataspss))

#The corrplot Function
install.packages("corrplot")
library(corrplot)
corrplot(cor(dataspss))

#visualize correlation matrix
install.packages("ggplot")
install.packages("ggcorrplot")
library(ggcorrplot)
ggcorrplot(cor(dataspss))

# Regression in R
lmrealestate = lm(Y~X1, data = dataspss) #Create the linear regression
summary(lmrealestate) #Review the results
lmrealestate = lm(Y~X1+X2+X3+X4+X5+X6+X7, data = dataspss) #Create the linear regression
summary(lmrealestate) #Review the results

# Prediction Regression in R
dataspss$predicted = predict(lmrealestate)
dataspss$predicted










#data frame in R
library(Hmisc) 
# variable labels
df <- data.frame(assists=c(4, 5, 5, 6, 7, 8, 8, 10),
                 rebounds=c(12, 14, 13, 7, 8, 8, 9, 13),
                 points=c(22, 24, 26, 26, 29, 32, 20, 14))
df
# Add (or replace) variable names to data frame object
names(dataspss) <- c("Y", "X1", "X2", "X3", "X4","X5","X6","X7",)
library(Hmisc)
label(dataspss$Y) <- "Selling Price"
names(dataspss)
head(dataspss)
install.packages("tidyverse")
library(tidyverse)
library(Hmisc)
label1 <- c(Y="Selling Price", X1="Number of bedrooms", X2= "Size of the home ",X3="Pool", X4="Distance", X5="Township", X6="Garage attached", X7="Number of bathrooms")
