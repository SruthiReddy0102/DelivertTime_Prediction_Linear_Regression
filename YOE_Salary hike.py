# Importing necessary libraries
import pandas as pd # deals with data frame  
import numpy as np  # deals with numerical values

# Input Variable = Years of Experience
# Output Varibale = Salary Hike

Sal_hike = pd.read_csv("C:/Users/personal/Desktop/Salary_Data.csv")
Sal_hike.columns = "YOE", "SH" # YOE = Years of Experience & SH = Salary Hike
Sal_hike.columns

# Exploratory data analysis:
Sal_hike.describe()

#Graphical Representation
import matplotlib.pyplot as plt # mostly used for visualization purposes 

plt.bar(height = Sal_hike.SH, x = np.arange(1, 31, 1))
plt.hist(Sal_hike.SH) #histogram
plt.boxplot(Sal_hike.SH) #boxplot

plt.bar(height = Sal_hike.YOE, x = np.arange(1, 31, 1))
plt.hist(Sal_hike.YOE) #histogram
plt.boxplot(Sal_hike.YOE) #boxplot

# Scatter plot
plt.scatter(x = Sal_hike['YOE'], y = Sal_hike['SH'], color = 'red') 

# correlation
np.corrcoef(Sal_hike.YOE, Sal_hike.SH) 

# Covariance
cov_output = np.cov(Sal_hike.YOE, Sal_hike.SH)[0, 1]
cov_output


# Import library
import statsmodels.formula.api as smf

# Simple Linear Regression
model = smf.ols('SH ~ YOE', data = Sal_hike).fit()
model.summary()

pred1 = model.predict(pd.DataFrame(Sal_hike['YOE']))

# Regression Line
plt.scatter(Sal_hike.YOE, Sal_hike.SH)
plt.plot(Sal_hike.YOE, pred1, "r")
plt.legend(['Predicted line', 'Observed data'])
plt.show()

# Error calculation
res1 = Sal_hike.SH - pred1
res_sqr1 = res1 * res1
mse1 = np.mean(res_sqr1)
rmse1 = np.sqrt(mse1)
rmse1

######### Model building on Transformed Data
# Log Transformation
# x = log(YOE); y = SH

plt.scatter(x = np.log(Sal_hike['YOE']), y = Sal_hike['SH'], color = 'brown')
np.corrcoef(np.log(Sal_hike.YOE), Sal_hike.SH) #correlation

model2 = smf.ols('SH ~ np.log(YOE)', data = Sal_hike).fit()
model2.summary()

pred2 = model2.predict(pd.DataFrame(Sal_hike['YOE']))

# Regression Line
plt.scatter(np.log(Sal_hike.YOE), Sal_hike.SH)
plt.plot(np.log(Sal_hike.YOE), pred2, "r")
plt.legend(['Predicted line', 'Observed data'])
plt.show()

# Error calculation
res2 = Sal_hike.SH - pred2
res_sqr2 = res2 * res2
mse2 = np.mean(res_sqr2)
rmse2 = np.sqrt(mse2)
rmse2


#### Exponential transformation
# x = YOE; y = log(SH)

plt.scatter(x = Sal_hike['YOE'], y = np.log(Sal_hike['SH']), color = 'orange')
np.corrcoef(Sal_hike.YOE, np.log(Sal_hike.SH)) #correlation

model3 = smf.ols('np.log(SH) ~ YOE', data = Sal_hike).fit()
model3.summary()

pred3 = model3.predict(pd.DataFrame(Sal_hike['YOE']))
pred3_at = np.exp(pred3)
pred3_at

# Regression Line
plt.scatter(Sal_hike.YOE, np.log(Sal_hike.SH))
plt.plot(Sal_hike.YOE, pred3, "r")
plt.legend(['Predicted line', 'Observed data'])
plt.show()

# Error calculation
res3 = Sal_hike.SH - pred3_at
res_sqr3 = res3 * res3
mse3 = np.mean(res_sqr3)
rmse3 = np.sqrt(mse3)
rmse3


#### Polynomial transformation
# x = YOE; x^2 = YOE*YOE; y = log(SH)

model4 = smf.ols('np.log(SH) ~ YOE + I(YOE*YOE)', data = Sal_hike).fit()
model4.summary()

pred4 = model4.predict(pd.DataFrame(Sal_hike))
pred4_at = np.exp(pred4)
pred4_at

# Regression line
from sklearn.preprocessing import PolynomialFeatures
poly_reg = PolynomialFeatures(degree = 2)
X = Sal_hike.iloc[:, 0:1].values
X_poly = poly_reg.fit_transform(X)


plt.scatter(Sal_hike.YOE, np.log(Sal_hike.SH))
plt.plot(X, pred4, color = 'red')
plt.legend(['Predicted line', 'Observed data'])
plt.show()


# Error calculation
res4 = Sal_hike.SH - pred4_at
res_sqr4 = res4 * res4
mse4 = np.mean(res_sqr4)
rmse4 = np.sqrt(mse4)
rmse4


# Choose the best model using RMSE
data = {"MODEL":pd.Series(["SLR", "Log model", "Exp model", "Poly model"]), "RMSE":pd.Series([rmse1, rmse2, rmse3, rmse4])}
table_rmse = pd.DataFrame(data)
table_rmse

###################
# The best model - From the RMSE SLR is the best model with Low Rmse

from sklearn.model_selection import train_test_split

train, test = train_test_split(Sal_hike, test_size = 0.2)

finalmodel = smf.ols('SH ~ YOE', data = train).fit()
finalmodel.summary()


# Predict on test data
test_pred = finalmodel.predict(pd.DataFrame(test))
test_pred

# Model Evaluation on Test data
test_res = test.SH - test_pred
test_sqrs = test_res * test_res
test_mse = np.mean(test_sqrs)
test_rmse = np.sqrt(test_mse)
test_rmse


# Prediction on train data
train_pred = finalmodel.predict(pd.DataFrame(train))
train_pred

# Model Evaluation on train data
train_res = train.SH -  train_pred
train_sqrs = train_res * train_res
train_mse = np.mean(train_sqrs)
train_rmse = np.sqrt(train_mse)
train_rmse


