# Importing necessary libraries
import pandas as pd # deals with data frame  
import numpy as np  # deals with numerical values

# Input Variable = Sorting Time
# Output Variable = Delivery Time

del_time = pd.read_csv("C:/Users/personal/Desktop/delivery_time.csv")
del_time.columns = "DT", "ST" # DT = Delivery Time & ST = Sorting Time
del_time.columns

# Exploratory data analysis:
del_time.describe()

#Graphical Representation
import matplotlib.pyplot as plt # mostly used for visualization purposes 

plt.bar(height = del_time.DT, x = np.arange(1,22, 1))
plt.hist(del_time.DT) #histogram
plt.boxplot(del_time.DT) #boxplot

plt.bar(height = del_time.ST, x = np.arange(1, 22, 1))
plt.hist(del_time.ST) #histogram
plt.boxplot(del_time.ST) #boxplot

# Scatter plot
plt.scatter(x = del_time['ST'], y = del_time['DT'], color = 'red') 

# correlation
np.corrcoef(del_time.ST, del_time.DT) 

# Covariance
cov_output = np.cov(del_time.ST, del_time.DT)[0, 1]
cov_output


# Import library
import statsmodels.formula.api as smf

# Simple Linear Regression
model = smf.ols('DT ~ ST', data = del_time).fit()
model.summary()

pred1 = model.predict(pd.DataFrame(del_time['ST']))

# Regression Line
plt.scatter(del_time.ST, del_time.DT)
plt.plot(del_time.ST, pred1, "r")
plt.legend(['Predicted line', 'Observed data'])
plt.show()

# Error calculation
res1 = del_time.DT - pred1
res_sqr1 = res1 * res1
mse1 = np.mean(res_sqr1)
rmse1 = np.sqrt(mse1)
rmse1

######### Model building on Transformed Data
# Log Transformation
# x = log(Sorting Time); y = Delivery Time

plt.scatter(x = np.log(del_time['ST']), y = del_time['DT'], color = 'brown')
np.corrcoef(np.log(del_time.ST), del_time.DT) #correlation

model2 = smf.ols('DT ~ np.log(ST)', data = del_time).fit()
model2.summary()

pred2 = model2.predict(pd.DataFrame(del_time['ST']))

# Regression Line
plt.scatter(np.log(del_time.ST), del_time.DT)
plt.plot(np.log(del_time.ST), pred2, "r")
plt.legend(['Predicted line', 'Observed data'])
plt.show()

# Error calculation
res2 = del_time.DT - pred2
res_sqr2 = res2 * res2
mse2 = np.mean(res_sqr2)
rmse2 = np.sqrt(mse2)
rmse2


#### Exponential transformation
# x = Sorting Time; y = log(Delivery Time)

plt.scatter(x = del_time['ST'], y = np.log(del_time['DT']), color = 'orange')
np.corrcoef(del_time.ST, np.log(del_time.DT)) #correlation

model3 = smf.ols('np.log(DT) ~ ST', data = del_time).fit()
model3.summary()

pred3 = model3.predict(pd.DataFrame(del_time['ST']))
pred3_at = np.exp(pred3)
pred3_at

# Regression Line
plt.scatter(del_time.ST, np.log(del_time.DT))
plt.plot(del_time.ST, pred3, "r")
plt.legend(['Predicted line', 'Observed data'])
plt.show()

# Error calculation
res3 = del_time.DT - pred3_at
res_sqr3 = res3 * res3
mse3 = np.mean(res_sqr3)
rmse3 = np.sqrt(mse3)
rmse3


#### Polynomial transformation
# x = ST; x^2 = ST*ST; y = log(DT)

model4 = smf.ols('np.log(DT) ~ ST + I(ST*ST)', data = del_time).fit()
model4.summary()

pred4 = model4.predict(pd.DataFrame(del_time))
pred4_at = np.exp(pred4)
pred4_at

# Regression line
from sklearn.preprocessing import PolynomialFeatures
poly_reg = PolynomialFeatures(degree = 2)
X = del_time.iloc[:, 0:1].values
X_poly = poly_reg.fit_transform(X)


plt.scatter(del_time.ST, np.log(del_time.DT))
plt.plot(X, pred4, color = 'red')
plt.legend(['Predicted line', 'Observed data'])
plt.show()


# Error calculation
res4 = del_time.DT - pred4_at
res_sqr4 = res4 * res4
mse4 = np.mean(res_sqr4)
rmse4 = np.sqrt(mse4)
rmse4


# Choose the best model using RMSE
data = {"MODEL":pd.Series(["SLR", "Log model", "Exp model", "Poly model"]), "RMSE":pd.Series([rmse1, rmse2, rmse3, rmse4])}
table_rmse = pd.DataFrame(data)
table_rmse

###################
# The best model - From the RMSE Log Model 1 is the best model with Low Rmse

from sklearn.model_selection import train_test_split

train, test = train_test_split(del_time, test_size = 0.2)

finalmodel = smf.ols('DT ~ np.log(ST)', data = train).fit()
finalmodel.summary()


# Predict on test data
test_pred = finalmodel.predict(pd.DataFrame(test))
test_pred

# Model Evaluation on Test data
test_res = test.DT - test_pred
test_sqrs = test_res * test_res
test_mse = np.mean(test_sqrs)
test_rmse = np.sqrt(test_mse)
test_rmse


# Prediction on train data
train_pred = finalmodel.predict(pd.DataFrame(train))
train_pred

# Model Evaluation on train data
train_res = train.DT -  train_pred
train_sqrs = train_res * train_res
train_mse = np.mean(train_sqrs)
train_rmse = np.sqrt(train_mse)
train_rmse


