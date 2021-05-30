#
# Template for Multiple Regression Project
#

# Set up with the dependent variable as the first column
#  and the two independent variables in columns 2 & 3
#
# The code will use column 1 as y, column 2 as x1, and 
#  column 3 as x2
#

# load library regclass for VIF function
library(regclass)
# load library lmtest for Breusch-Pagan test
library(lmtest)
library(ggplot2)
library(Hmisc)
options(max.print=1000000)
# In RStudio, use File, Import Dataset, From Excel...
#  to get Excel data file

library(readxl)
Marketing_Data <- read_excel("Downloads/Marketing_Data.xls")

##########
#
# NOTE: Replace RegProj with the name of your imported Excel file
#
str(Marketing_Data)

# To have most of our R code reuseable for future 
#  analyses, we will use a data object called dataobj

##########
# NOTE: Replace RegProj with the name of your imported Excel file
#
dataobj <- as.data.frame(Marketing_Data)
str(dataobj)
summary(dataobj)
par(mfrow = c(1, 1))

# Set column 1 as y, column 2 as x1, and column 3 as x2
dataobj$y <- dataobj[,1]
dataobj$x1 <- dataobj[,2]
dataobj$x2 <- dataobj[,3]
dataobj$x3 <- dataobj[,4]
summary(dataobj$y)
sd(dataobj$y)
hist(dataobj$y, col = "lightblue", main = "Histogram of Sales (in thousands)", xlab = "Sales (in thousands)")
summary(dataobj$x1)
sd(dataobj$x1)
hist(dataobj$x1, col = "lightblue", main = "Histogram of Youtube Expense (in thousands)", xlab = "Youtube Expense (in thousands)")
summary(dataobj$x2)
sd(dataobj$x2)
hist(dataobj$x2, col = "lightblue", main = "Histogram of Facebook Expense (in thousands)", xlab = "Facebook Expense (in thousands)", ylim = c(0,25))
summary(dataobj$x3)
sd(dataobj$x3)
hist(dataobj$x3, col = "lightblue", main = "Histogram of Newspaper Expense (in thousands)", xlab = "Newspaper Expense (in thousands)", ylim = c(0,35), xlim = c(0,140))


str(dataobj)


# Scatterplot Matrix
pairs(~y+x1+x2+x3,pch=19,main="Scatterplot Matrix",data=dataobj)

# correlation test for each pair of variables
mat <- as.matrix(dataobj[,5:8])
rcorr(mat, type="pearson")
cor.test(dataobj$y,dataobj$x1)
cor.test(dataobj$y,dataobj$x2)
cor.test(dataobj$x1,dataobj$x2)

par(mfrow = c(2, 2))
# Fit the model
model <- lm(y ~ x1 + x2 + x3, data=dataobj)
summary(model)

plot(model)

# Shapiro-Wilk test for normality of errors and use alpha=0.01
shapiro.test(resid(model))

# Breusch-Pagan Test for a common error variance and use alpha=0.01
bptest(model)

par(mfrow = c(1, 1))
# histogram of residuals
hist(resid(model), main="Histogram of Residuals",xlab="Residuals", col = "lightblue", xlim = c(-15, 5))
# boxplot of residuals
boxplot(resid(model), main="Boxplot of Residuals")

# Influential observations
influence.measures(model)


# Check for multicollinearity
VIF(model)



