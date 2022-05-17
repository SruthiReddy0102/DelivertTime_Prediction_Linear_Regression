# Input Variable = Sorting Time
# Output Variable = Delivery Time

#Import Dataset

Del.time <- read.csv(file.choose(), header = T)
colnames(Del.time) <- c("DT", "ST") # DT = Delivery Time & ST = Sorting Time
View(Del.time)
attach(Del.time)

# Exploratory data analysis
summary(Del.time)


install.packages("Hmisc")
library(Hmisc)
describe(Del.time) # To have a quick glance of a data


install.packages("lattice") # Highly used for data visualization
library("lattice") # dotplot is part of lattice package

# Graphical exploration
dotplot(Del.time$ST, main = "Dot Plot of Sorting Time")
dotplot(Del.time$DT, main = "Dot Plot of Delivery Time")

#Boxplot Representation

boxplot(Del.time$ST, col = "dodgerblue4")
boxplot(Del.time$DT, col = "red", horizontal = T)

#Histogram Representation

hist(Del.time$ST)
hist(Del.time$DT)

# Normal QQ plot
qqnorm(Del.time$ST)
qqline(Del.time$ST)

qqnorm(Del.time$DT)
qqline(Del.time$DT)


# Bivariate analysis
# Scatter plot
plot(Del.time$ST, Del.time$DT, main = "Scatter Plot", col = "Dodgerblue4", 
     col.main = "Dodgerblue4", col.lab = "Dodgerblue4", xlab = "Sorting Time", 
     ylab = "Delivery Time (Mins)", pch = 20)  # plot(x,y)


# Correlation Coefficient
cor(ST,DT)

# Linear Regression model
reg <- lm(DT ~ ST, data = Del.time) # Y ~ X
summary(reg)

confint(reg, level = 0.95)


pred <- predict(reg, interval = "predict")
pred <- as.data.frame(pred)

View(pred)


# ggplot for adding Regression line for data
library(ggplot2)

ggplot(data = Del.time, aes(ST, DT) ) +
  geom_point() + stat_smooth(method = lm, formula = y ~ x)


# Evaluation the model for fitness 
cor(pred$fit, Del.time$DT)

reg$residuals
rmse <- sqrt(mean(reg$residuals^2))
rmse


# Transformation Techniques
# transform the variables to check whether the predicted values are better

# input = log(x); output = y

plot(log(ST), DT)
cor(log(ST), DT)

reg_log <- lm(DT ~ log(ST), data = Del.time)
summary(reg_log)

confint(reg_log,level = 0.95)
pred <- predict(reg_log, interval = "predict")

pred <- as.data.frame(pred)
cor(pred$fit, Del.time$DT)

rmse <- sqrt(mean(reg_log$residuals^2))
rmse

# Regression line for data
ggplot(data = Del.time, aes(log(ST), DT) ) +
  geom_point() + stat_smooth(method = lm, formula = y ~ log(x))



# Log transformation applied on 'y'
# input = x; output = log(y)

plot(ST, log(DT))
cor(ST, log(DT))

reg_log1 <- lm(log(DT) ~ ST ,data= Del.time)
summary(reg_log1)

predlog <- predict(reg_log1, interval = "predict")
predlog <- as.data.frame(predlog)
reg_log1$residuals
sqrt(mean(reg_log1$residuals^2))

pred <- exp(predlog)  # Antilog = Exponential function
pred <- as.data.frame(pred)
cor(pred$fit, Del.time$DT)

res_log1 = DT - pred$fit
rmse <- sqrt(mean(res_log1^2))
rmse

# Regression line for data
ggplot(data = Del.time, aes(ST, log(DT)) ) +
  geom_point() + stat_smooth(method = lm, formula = log(y) ~ x)



# Non-linear models = Polynomial models
# input = x & x^2 (2-degree) and output = log(y)

reg2 <- lm(log(DT) ~ ST + I(ST*ST), data = Del.time)
summary(reg2)

predlog <- predict(reg2, interval = "predict")
pred <- exp(predlog)

pred <- as.data.frame(pred)
cor(pred$fit, Del.time$DT)

res2 = DT - pred$fit
rmse <- sqrt(mean(res2^2))
rmse

# Regression line for data
ggplot(data = Del.time, aes(ST, log(DT)) ) +
  geom_point() + stat_smooth(method = lm, formula = y ~ poly(x, 2, raw = TRUE))


# Data Partition

# Random Sampling
n <- nrow(Del.time)
n1 <- n * 0.8
n2 <- n - n1

train_ind <- sample(1:n, n1)
train <- Del.time[train_ind, ]
test <-  Del.time[-train_ind, ]


plot(log(train$ST), train$DT)
plot(log(test$ST), test$DT)

model <- lm(DT ~ log(ST), data = train)
summary(model)

confint(reg,level=0.95)

log_res <- predict(model,interval = "confidence", newdata = test)

predict_original <- as.data.frame(log_res)
test_error <- test$DT - predict_original$fit # calculate error/residual
test_error

test_rmse <- sqrt(mean(test_error^2))
test_rmse

log_res_train <- predict(reg_log,interval = "confidence", newdata = train)

predict_original_train <- as.data.frame(log_res_train)
train_error <- train$DT - predict_original_train$fit # calculate error/residual
train_error

train_rmse <- sqrt(mean(train_error^2))
train_rmse
