
# x(input) - Calorie intake
# y(output) - Weight gain


# Load the dataset

wg.cc <- read.csv(file.choose(), header = T)
colnames(wg.cc) <- c("Weight gain", "Calories")
View(wg.cc)
attach(wg.cc)

# Exploratory data analysis
summary(wg.cc)


install.packages("Hmisc")
library(Hmisc)
describe(wg.cc) # To have a quick glance of a data


install.packages("lattice") # Highly used for data visualization
library("lattice") # dotplot is part of lattice package

# Graphical exploration
dotplot(wg.cc$Calories, main = "Dot Plot of Calories Consumed")
dotplot(wg.cc$`Weight gain`, main = "Dot Plot of Weight Gained")

#Boxplot Representation

boxplot(wg.cc$Calories, col = "dodgerblue4")
boxplot(wg.cc$`Weight gain`, col = "red", horizontal = T)

#Histogram Representation

hist(wg.cc$Calories)
hist(wg.cc$`Weight gain`)

# Normal QQ plot
qqnorm(wg.cc$Calories)
qqline(wg.cc$Calories)

qqnorm(wg.cc$`Weight gain`)
qqline(wg.cc$`Weight gain`)


# Bivariate analysis
# Scatter plot
plot(wg.cc$Calories, wg.cc$`Weight gain`, main = "Scatter Plot", col = "Dodgerblue4", 
     col.main = "Dodgerblue4", col.lab = "Dodgerblue4", xlab = "Calories Consumed", 
     ylab = "Weight Gained (gms)", pch = 20)  # plot(x,y)


attach(wg.cc)

# Correlation Coefficient
cor(Calories, `Weight gain`)

# Linear Regression model
reg <- lm(`Weight gain` ~ Calories, data = wg.cc) # Y ~ X
summary(reg)

confint(reg, level = 0.95)


pred <- predict(reg, interval = "predict")
pred <- as.data.frame(pred)

View(pred)


# ggplot for adding Regression line for data
library(ggplot2)

ggplot(data = wg.cc, aes(Calories, `Weight gain`) ) +
  geom_point() + stat_smooth(method = lm, formula = y ~ x)


# Evaluation the model for fitness 
cor(pred$fit, wg.cc$`Weight gain`)

reg$residuals
rmse <- sqrt(mean(reg$residuals^2))
rmse


# Transformation Techniques
# transform the variables to check whether the predicted values are better

# input = log(x); output = y

plot(log(Calories), `Weight gain`)
cor(log(Calories), `Weight gain`)

reg_log <- lm(`Weight gain`~ log(Calories), data = wg.cc)
summary(reg_log)

confint(reg_log,level = 0.95)
pred <- predict(reg_log, interval = "predict")

pred <- as.data.frame(pred)
cor(pred$fit, wg.cc$`Weight gain`)

rmse <- sqrt(mean(reg_log$residuals^2))
rmse

# Regression line for data
ggplot(data = wg.cc, aes(log(Calories), `Weight gain`) ) +
  geom_point() + stat_smooth(method = lm, formula = y ~ log(x))



# Log transformation applied on 'y'
# input = x; output = log(y)

plot(Calories, log(`Weight gain`))
cor(Calories, log(`Weight gain`))

reg_log1 <- lm(log(`Weight gain`) ~ Calories, data = wg.cc)
summary(reg_log1)

predlog <- predict(reg_log1, interval = "predict")
predlog <- as.data.frame(predlog)
reg_log1$residuals
sqrt(mean(reg_log1$residuals^2))

pred <- exp(predlog)  # Antilog = Exponential function
pred <- as.data.frame(pred)
cor(pred$fit, wg.cc$`Weight gain`)

res_log1 = `Weight gain` - pred$fit
rmse <- sqrt(mean(res_log1^2))
rmse

# Regression line for data
ggplot(data = wg.cc, aes(Calories, log(`Weight gain`)) ) +
  geom_point() + stat_smooth(method = lm, formula = log(y) ~ x)



# Non-linear models = Polynomial models
# input = x & x^2 (2-degree) and output = log(y)

reg2 <- lm(log(`Weight gain`) ~ Calories + I(Calories*Calories), data = wg.cc)
summary(reg2)

predlog <- predict(reg2, interval = "predict")
pred <- exp(predlog)

pred <- as.data.frame(pred)
cor(pred$fit, wg.cc$`Weight gain`)

res2 = `Weight gain` - pred$fit
rmse <- sqrt(mean(res2^2))
rmse

# Regression line for data
ggplot(data = wg.cc, aes(Calories, log(`Weight gain`)) ) +
  geom_point() + stat_smooth(method = lm, formula = y ~ poly(x, 2, raw = TRUE))


# Data Partition

# Random Sampling
n <- nrow(wg.cc)
n1 <- n * 0.8
n2 <- n - n1

train_ind <- sample(1:n, n1)
train <- wg.cc[train_ind, ]
test <-  wg.cc[-train_ind, ]


plot(train$Calories, train$`Weight gain`)
plot(test$Calories, test$`Weight gain`)

model <- lm(`Weight gain` ~ Calories, data = wg.cc) # Y ~ X
summary(model)

confint(reg,level=0.95)

res <- predict(model,interval = "confidence", newdata = test)

predict_original <- as.data.frame(res)
test_error <- test$`Weight gain` - predict_original$fit # calculate error/residual
test_error

test_rmse <- sqrt(mean(test_error^2))
test_rmse

res_train <- predict(reg,interval = "confidence", newdata = train)

predict_original_train <- as.data.frame(res_train)
train_error <- train$`Weight gain` - predict_original_train$fit # calculate error/residual
train_error

train_rmse <- sqrt(mean(train_error^2))
train_rmse

# Alternate Model 

reg <- lm(`Weight gain` ~ Calories+I(Calories*Calories), data = wg.cc)
summary(reg)

confint(reg,level=0.95)
res <- predict(reg,interval = "confidence", newdata = test)

predict_original <- as.data.frame(res)
test_error <- test$`Weight gain` - predict_original$fit # calculate error/residual
test_error

test_rmse <- sqrt(mean(test_error^2))
test_rmse

res_train <- predict(reg,interval = "confidence", newdata = train)

predict_original_train <- as.data.frame(res_train)
train_error <- train$`Weight gain` - predict_original_train$fit # calculate error/residual
train_error

train_rmse <- sqrt(mean(train_error^2))
train_rmse


