# Input Variable = Years of Experience
# Output Varibale = Salary Hike

#Importing Dataset

Sal.hike <- read.csv(file.choose(), header = T)
colnames(Sal.hike) <- c("YOE", "SH") # SH = Salary Hike & YOE = Years of Experience
View(Sal.hike)
attach(Sal.hike)

# Exploratory data analysis
summary(Sal.hike)


install.packages("Hmisc")
library(Hmisc)
describe(Sal.hike) # To have a quick glance of a data


install.packages("lattice") # Highly used for data visualization
library("lattice") # dotplot is part of lattice package

# Graphical exploration
dotplot(Sal.hike$YOE, main = "Dot Plot of Years of Experience")
dotplot(Sal.hike$SH, main = "Dot Plot of Salary Hike")

#Boxplot Representation

boxplot(Sal.hike$YOE, col = "dodgerblue4")
boxplot(Sal.hike$SH, col = "red", horizontal = T)

#Histogram Representation

hist(Sal.hike$YOE)
hist(Sal.hike$SH)

# Normal QQ plot
qqnorm(Sal.hike$YOE)
qqline(Sal.hike$YOE)

qqnorm(Sal.hike$SH)
qqline(Sal.hike$SH)


# Bivariate analysis
# Scatter plot
plot(Sal.hike$YOE, Sal.hike$SH, main = "Scatter Plot", col = "Dodgerblue4", 
     col.main = "Dodgerblue4", col.lab = "Dodgerblue4", xlab = "Years of Experience", 
     ylab = "Salary Hike", pch = 20)  # plot(x,y)


# Correlation Coefficient
cor(YOE,SH)

# Linear Regression model
reg <- lm(SH ~ YOE, data = Sal.hike ) # Y ~ X
summary(reg)

confint(reg, level = 0.95)


pred <- predict(reg, interval = "predict")
pred <- as.data.frame(pred)

View(pred)


# ggplot for adding Regression line for data
library(ggplot2)

ggplot(data = Sal.hike, aes(YOE, SH) ) +
  geom_point() + stat_smooth(method = lm, formula = y ~ x)


# Evaluation the model for fitness 
cor(pred$fit, Sal.hike$SH)

reg$residuals
rmse <- sqrt(mean(reg$residuals^2))
rmse


# Transformation Techniques
# transform the variables to check whether the predicted values are better

# input = log(x); output = y

plot(log(YOE), SH)
cor(log(YOE), SH)

reg_log <- lm(SH ~ log(YOE), data = Sal.hike)
summary(reg_log)

confint(reg_log,level = 0.95)
pred <- predict(reg_log, interval = "predict")

pred <- as.data.frame(pred)
cor(pred$fit, Sal.hike$SH)

rmse <- sqrt(mean(reg_log$residuals^2))
rmse

# Regression line for data
ggplot(data = Sal.hike, aes(log(YOE), SH) ) +
  geom_point() + stat_smooth(method = lm, formula = y ~ log(x))



# Log transformation applied on 'y'
# input = x; output = log(y)

plot(YOE, log(SH))
cor(YOE, log(SH))

reg_log1 <- lm(log(SH) ~ YOE ,data= Sal.hike)
summary(reg_log1)

predlog <- predict(reg_log1, interval = "predict")
predlog <- as.data.frame(predlog)
reg_log1$residuals
sqrt(mean(reg_log1$residuals^2))

pred <- exp(predlog)  # Antilog = Exponential function
pred <- as.data.frame(pred)
cor(pred$fit, Sal.hike$SH)

res_log1 = SH - pred$fit
rmse <- sqrt(mean(res_log1^2))
rmse

# Regression line for data
ggplot(data = Sal.hike, aes(YOE, log(SH)) ) +
  geom_point() + stat_smooth(method = lm, formula = log(y) ~ x)



# Non-linear models = Polynomial models
# input = x & x^2 (2-degree) and output = log(y)

reg2 <- lm(log(SH) ~ YOE + I(SH*SH), data = Sal.hike)
summary(reg2)

predlog <- predict(reg2, interval = "predict")
pred <- exp(predlog)

pred <- as.data.frame(pred)
cor(pred$fit, Sal.hike$SH)

res2 = SH - pred$fit
rmse <- sqrt(mean(res2^2))
rmse

# Regression line for data
ggplot(data = Sal.hike, aes(YOE, log(SH)) ) +
  geom_point() + stat_smooth(method = lm, formula = y ~ poly(x, 2, raw = TRUE))


# Data Partition

# Random Sampling
n <- nrow(Sal.hike)
n1 <- n * 0.8
n2 <- n - n1

train_ind <- sample(1:n, n1)
train <- Sal.hike[train_ind, ]
test <-  Sal.hike[-train_ind, ]


plot(train$YOE, train$SH)
plot(test$YOE, test$SH)

model <- lm(SH ~ YOE, data = train )
summary(reg)

confint(model,level=0.95)

log_res <- predict(model,interval = "confidence", newdata = test)


predict_original <- as.data.frame(log_res)
test_error <- test$SH - predict_original$fit # calculate error/residual
test_error

test_rmse <- sqrt(mean(test_error^2))
test_rmse

log_res_train <- predict(model,interval = "confidence", newdata = train)


predict_original_train <- as.data.frame(log_res_train)
train_error <- train$SH - predict_original_train$fit # calculate error/residual
train_error

train_rmse <- sqrt(mean(train_error^2))
train_rmse
