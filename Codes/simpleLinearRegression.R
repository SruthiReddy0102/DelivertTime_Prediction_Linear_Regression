# Load the data
wc.at <- read.csv(file.choose(), header = T)
View(wc.at)

# Exploratory data analysis
summary(wc.at)

install.packages("Hmisc")
library(Hmisc)
describe(wc.at)
?describe

install.packages("lattice")
library("lattice") # dotplot is part of lattice package

# Graphical exploration
dotplot(wc.at$Waist, main = "Dot Plot of Waist Circumferences")
dotplot(wc.at$AT, main = "Dot Plot of Adipose Tissue Areas")

?boxplot
boxplot(wc.at$Waist, col = "dodgerblue4")
boxplot(wc.at$AT, col = "red", horizontal = T)

hist(wc.at$Waist)
hist(wc.at$AT)

# Normal QQ plot
qqnorm(wc.at$Waist)
qqline(wc.at$Waist)

qqnorm(wc.at$AT)
qqline(wc.at$AT)

hist(wc.at$Waist, prob = TRUE)          # prob=TRUE for probabilities not counts
lines(density(wc.at$Waist))             # add a density estimate with defaults
lines(density(wc.at$Waist, adjust = 2), lty = "dotted")   # add another "smoother" density

hist(wc.at$AT, prob = TRUE)          # prob=TRUE for probabilities not counts
lines(density(wc.at$AT))             # add a density estimate with defaults
lines(density(wc.at$AT, adjust = 2), lty = "dotted")   # add another "smoother" density

# Bivariate analysis
# Scatter plot
plot(wc.at$Waist, wc.at$AT, main = "Scatter Plot", col = "Dodgerblue4", 
     col.main = "Dodgerblue4", col.lab = "Dodgerblue4", xlab = "Waist Ciscumference", 
     ylab = "Adipose Tissue area", pch = 20)  # plot(x,y)

?plot

## alternate simple command
plot(wc.at$Waist, wc.at$AT)

attach(wc.at)

# Correlation Coefficient
cor(Waist, AT)

# Covariance
cov(Waist, AT)

# Linear Regression model
reg <- lm(AT ~ Waist, data = wc.at) # Y ~ X
?lm
summary(reg)

confint(reg, level = 0.95)
?confint

pred <- predict(reg, interval = "predict")
pred <- as.data.frame(pred)

View(pred)
?predict

# ggplot for adding Regression line for data
library(ggplot2)

ggplot(data = wc.at, aes(Waist, AT) ) +
     geom_point() + stat_smooth(method = lm, formula = y ~ x)

# Alternate way
ggplot(data = wc.at, aes(x = Waist, y = AT)) + 
     geom_point(color = 'blue') +
     geom_line(color = 'red', data = wc.at, aes(x = Waist, y = pred$fit))

# Evaluation the model for fitness 
cor(pred$fit, wc.at$AT)

reg$residuals
rmse <- sqrt(mean(reg$residuals^2))
rmse


# Transformation Techniques

# input = log(x); output = y

plot(log(Waist), AT)
cor(log(Waist), AT)

reg_log <- lm(AT ~ log(Waist), data = wc.at)
summary(reg_log)

confint(reg_log,level = 0.95)
pred <- predict(reg_log, interval = "predict")

pred <- as.data.frame(pred)
cor(pred$fit, wc.at$AT)

rmse <- sqrt(mean(reg_log$residuals^2))
rmse

# Regression line for data
ggplot(data = wc.at, aes(log(Waist), AT) ) +
     geom_point() + stat_smooth(method = lm, formula = y ~ log(x))

# Alternate way
ggplot(data = wc.at, aes(x = log(Waist), y = AT)) + 
     geom_point(color = 'blue') +
     geom_line(color = 'red', data = wc.at, aes(x = log(Waist), y = pred$fit))



# Log transformation applied on 'y'
# input = x; output = log(y)

plot(Waist, log(AT))
cor(Waist, log(AT))

reg_log1 <- lm(log(AT) ~ Waist, data = wc.at)
summary(reg_log1)

predlog <- predict(reg_log1, interval = "predict")
predlog <- as.data.frame(predlog)
reg_log1$residuals
sqrt(mean(reg_log1$residuals^2))

pred <- exp(predlog)  # Antilog = Exponential function
pred <- as.data.frame(pred)
cor(pred$fit, wc.at$AT)

res_log1 = AT - pred$fit
rmse <- sqrt(mean(res_log1^2))
rmse

# Regression line for data
ggplot(data = wc.at, aes(Waist, log(AT)) ) +
     geom_point() + stat_smooth(method = lm, formula = log(y) ~ x)

# Alternate way
ggplot(data = wc.at, aes(x = Waist, y = log(AT))) + 
     geom_point(color = 'blue') +
     geom_line(color = 'red', data = wc.at, aes(x = Waist, y = predlog$fit))


# Non-linear models = Polynomial models
# input = x & x^2 (2-degree) and output = log(y)

reg2 <- lm(log(AT) ~ Waist + I(Waist*Waist), data = wc.at)
summary(reg2)

predlog <- predict(reg2, interval = "predict")
pred <- exp(predlog)

pred <- as.data.frame(pred)
cor(pred$fit, wc.at$AT)

res2 = AT - pred$fit
rmse <- sqrt(mean(res2^2))
rmse

# Regression line for data
ggplot(data = wc.at, aes(Waist, log(AT)) ) +
     geom_point() + stat_smooth(method = lm, formula = y ~ poly(x, 2, raw = TRUE))

# Alternate way
ggplot(data = wc.at, aes(x = Waist + I(Waist*Waist), y = log(AT))) + 
     geom_point(color = 'blue') +
     geom_line(color = 'red', data = wc.at, aes(x = Waist + I(Waist^2), y = predlog$fit))


# Data Partition

# Random Sampling
n <- nrow(wc.at)
n1 <- n * 0.8
n2 <- n - n1

train_ind <- sample(1:n, n1)
train <- wc.at[train_ind, ]
test <-  wc.at[-train_ind, ]

# Non-random sampling
train <- wc.at[1:90, ]
test <- wc.at[91:109, ]

plot(train$Waist, log(train$AT))
plot(test$Waist, log(test$AT))

model <- lm(log(AT) ~ Waist + I(Waist * Waist), data = train)
summary(model)

confint(model,level=0.95)

log_res <- predict(model,interval = "confidence", newdata = test)

predict_original <- exp(log_res) # converting log values to original values
predict_original <- as.data.frame(predict_original)
test_error <- test$AT - predict_original$fit # calculate error/residual
test_error

test_rmse <- sqrt(mean(test_error^2))
test_rmse

log_res_train <- predict(model, interval = "confidence", newdata = train)

predict_original_train <- exp(log_res_train) # converting log values to original values
predict_original_train <- as.data.frame(predict_original_train)
train_error <- train$AT - predict_original_train$fit # calculate error/residual
train_error

train_rmse <- sqrt(mean(train_error^2))
train_rmse
