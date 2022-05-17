# Input Variable = Salary Hike
# Output Varibale = Churn out rate

#Importing Dataset

Emp.data <- read.csv(file.choose(), header = T)
colnames(Emp.data) <- c("SH", "CR") # SH = Salary Hike & CR = Churn out rate
View(Emp.data)
attach(Emp.data)

# Exploratory data analysis
summary(Emp.data)


install.packages("Hmisc")
library(Hmisc)
describe(Emp.data) # To have a quick glance of a data


install.packages("lattice") # Highly used for data visualization
library("lattice") # dotplot is part of lattice package

# Graphical exploration
dotplot(Emp.data$SH, main = "Dot Plot of Salary Hike")
dotplot(Emp.data$CR, main = "Dot Plot of Churn Out Rate")

#Boxplot Representation

boxplot(Emp.data$SH, col = "dodgerblue4")
boxplot(Emp.data$CR, col = "red", horizontal = T)

#Histogram Representation

hist(Emp.data$SH)
hist(Emp.data$CR)

# Normal QQ plot
qqnorm(Emp.data$SH)
qqline(Emp.data$SH)

qqnorm(Emp.data$CR)
qqline(Emp.data$CR)


# Bivariate analysis
# Scatter plot
plot(Emp.data$SH, Emp.data$CR, main = "Scatter Plot", col = "Dodgerblue4", 
     col.main = "Dodgerblue4", col.lab = "Dodgerblue4", xlab = "Salary Hike", 
     ylab = "Churn Out Rate", pch = 20)  # plot(x,y)


# Correlation Coefficient
cor(SH,CR)

# Linear Regression model
reg <- lm(CR ~ SH, data = Emp.data ) # Y ~ X
summary(reg)

confint(reg, level = 0.95)


pred <- predict(reg, interval = "predict")
pred <- as.data.frame(pred)

View(pred)


# ggplot for adding Regression line for data
library(ggplot2)

ggplot(data = Emp.data, aes(SH, CR) ) +
  geom_point() + stat_smooth(method = lm, formula = y ~ x)


# Evaluation the model for fitness 
cor(pred$fit, Emp.data$CR)

reg$residuals
rmse <- sqrt(mean(reg$residuals^2))
rmse


# Transformation Techniques
# transform the variables to check whether the predicted values are better

# input = log(x); output = y

plot(log(SH), CR)
cor(log(SH), CR)

reg_log <- lm(CR ~ log(SH), data = Emp.data)
summary(reg_log)

confint(reg_log,level = 0.95)
pred <- predict(reg_log, interval = "predict")

pred <- as.data.frame(pred)
cor(pred$fit, Emp.data$CR)

rmse <- sqrt(mean(reg_log$residuals^2))
rmse

# Regression line for data
ggplot(data = Emp.data, aes(log(SH), CR) ) +
  geom_point() + stat_smooth(method = lm, formula = y ~ log(x))



# Log transformation applied on 'y'
# input = x; output = log(y)

plot(SH, log(CR))
cor(SH, log(CR))

reg_log1 <- lm(log(CR) ~ SH ,data= Emp.data)
summary(reg_log1)

predlog <- predict(reg_log1, interval = "predict")
predlog <- as.data.frame(predlog)
reg_log1$residuals
sqrt(mean(reg_log1$residuals^2))

pred <- exp(predlog)  # Antilog = Exponential function
pred <- as.data.frame(pred)
cor(pred$fit, Emp.data$CR)

res_log1 = CR - pred$fit
rmse <- sqrt(mean(res_log1^2))
rmse

# Regression line for data
ggplot(data = Emp.data, aes(SH, log(CR)) ) +
  geom_point() + stat_smooth(method = lm, formula = log(y) ~ x)



# Non-linear models = Polynomial models
# input = x & x^2 (2-degree) and output = log(y)

reg2 <- lm(log(CR) ~ SH + I(SH*SH), data = Emp.data)
summary(reg2)

predlog <- predict(reg2, interval = "predict")
pred <- exp(predlog)

pred <- as.data.frame(pred)
cor(pred$fit, Emp.data$CR)

res2 = CR - pred$fit
rmse <- sqrt(mean(res2^2))
rmse

# Regression line for data
ggplot(data = Emp.data, aes(SH, log(CR)) ) +
  geom_point() + stat_smooth(method = lm, formula = y ~ poly(x, 2, raw = TRUE))


# Data Partition

# Random Sampling
n <- nrow(Emp.data)
n1 <- n * 0.8
n2 <- n - n1

train_ind <- sample(1:n, n1)
train <- Emp.data[train_ind, ]
test <-  Emp.data[-train_ind, ]


plot(train$SH, log(train$CR))
plot(test$SH, log(test$CR))

model <- lm(log(CR) ~ SH + I(SH*SH), data = train)
summary(model)

confint(model,level=0.95)

log_res <- predict(model,interval = "confidence", newdata = test)

predict_original <- exp(log_res) # converting log values to original values
predict_original <- as.data.frame(predict_original)
test_error <- test$CR - predict_original$fit # calculate error/residual
test_error

test_rmse <- sqrt(mean(test_error^2))
test_rmse

log_res_train <- predict(model,interval = "confidence", newdata = train)

predict_original_train <- exp(log_res_train) # converting log values to original values
predict_original_train <- as.data.frame(predict_original_train)
train_error <- train$CR - predict_original_train$fit # calculate error/residual
train_error

train_rmse <- sqrt(mean(train_error^2))
train_rmse
