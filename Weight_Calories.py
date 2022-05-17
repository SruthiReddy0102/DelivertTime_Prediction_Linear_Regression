# Importing necessary libraries
import pandas as pd # deals with data frame  
import numpy as np  # deals with numerical values

Wgcc = pd.read_csv("C:/Users/personal/Desktop/calories_consumed.csv")
Wgcc.columns = "Wg", "CC" # Wg = Weight Gain & CC = Calories Consumed
Wgcc.columns

# Exploratory data analysis:
Wgcc.describe()

#Graphical Representation
import matplotlib.pyplot as plt # mostly used for visualization purposes 

plt.bar(height = Wgcc.Wg, x = np.arange(1, 15, 1))
plt.hist(Wgcc.Wg) #histogram
plt.boxplot(Wgcc.Wg) #boxplot

plt.bar(height = Wgcc.CC, x = np.arange(1, 15, 1))
plt.hist(Wgcc.CC) #histogram
plt.boxplot(Wgcc.CC) #boxplot

# Scatter plot
plt.scatter(x = Wgcc['CC'], y = Wgcc['Wg'], color = 'red') 

# correlation
np.corrcoef(Wgcc.CC, Wgcc.Wg) 

# Covariance
cov_output = np.cov(Wgcc.CC, Wgcc.Wg)[0, 1]
cov_output


# Import library
import statsmodels.formula.api as smf

# Simple Linear Regression
model = smf.ols('Wg ~ CC', data = Wgcc).fit()
model.summary()

pred1 = model.predict(pd.DataFrame(Wgcc['CC']))

# Regression Line
plt.scatter(Wgcc.CC, Wgcc.Wg)
plt.plot(Wgcc.CC, pred1, "r")
plt.legend(['Predicted line', 'Observed data'])
plt.show()

# Error calculation
res1 = Wgcc.Wg - pred1
res_sqr1 = res1 * res1
mse1 = np.mean(res_sqr1)
rmse1 = np.sqrt(mse1)
rmse1

######### Model building on Transformed Data
# Log Transformation
# x = log(Calories); y = Wg

plt.scatter(x = np.log(Wgcc['CC']), y = Wgcc['Wg'], color = 'brown')
np.corrcoef(np.log(Wgcc.CC), Wgcc.Wg) #correlation

model2 = smf.ols('Wg ~ np.log(CC)', data = Wgcc).fit()
model2.summary()

pred2 = model2.predict(pd.DataFrame(Wgcc['CC']))

# Regression Line
plt.scatter(np.log(Wgcc.CC), Wgcc.Wg)
plt.plot(np.log(Wgcc.CC), pred2, "r")
plt.legend(['Predicted line', 'Observed data'])
plt.show()

# Error calculation
res2 = Wgcc.Wg - pred2
res_sqr2 = res2 * res2
mse2 = np.mean(res_sqr2)
rmse2 = np.sqrt(mse2)
rmse2


#### Exponential transformation
# x = Calories; y = log(Wg)

plt.scatter(x = Wgcc['CC'], y = np.log(Wgcc['Wg']), color = 'orange')
np.corrcoef(Wgcc.CC, np.log(Wgcc.Wg)) #correlation

model3 = smf.ols('np.log(Wg) ~ CC', data = Wgcc).fit()
model3.summary()

pred3 = model3.predict(pd.DataFrame(Wgcc['CC']))
pred3_at = np.exp(pred3)
pred3_at

# Regression Line
plt.scatter(Wgcc.CC, np.log(Wgcc.Wg))
plt.plot(Wgcc.CC, pred3, "r")
plt.legend(['Predicted line', 'Observed data'])
plt.show()

# Error calculation
res3 = Wgcc.Wg - pred3_at
res_sqr3 = res3 * res3
mse3 = np.mean(res_sqr3)
rmse3 = np.sqrt(mse3)
rmse3


#### Polynomial transformation
# x = Calories; x^2 = Calories*Calories; y = log(Wg)

model4 = smf.ols('np.log(Wg) ~ CC + I(CC*CC)', data = Wgcc).fit()
model4.summary()

pred4 = model4.predict(pd.DataFrame(Wgcc))
pred4_at = np.exp(pred4)
pred4_at

# Regression line
from sklearn.preprocessing import PolynomialFeatures
poly_reg = PolynomialFeatures(degree = 2)
X = Wgcc.iloc[:, 0:1].values
X_poly = poly_reg.fit_transform(X)


plt.scatter(Wgcc.CC, np.log(Wgcc.Wg))
plt.plot(X, pred4, color = 'red')
plt.legend(['Predicted line', 'Observed data'])
plt.show()


# Error calculation
res4 = Wgcc.Wg - pred4_at
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

train, test = train_test_split(Wgcc, test_size = 0.2)

finalmodel = smf.ols('Wg ~ CC', data = train).fit()
finalmodel.summary()


# Predict on test data
test_pred = finalmodel.predict(pd.DataFrame(test))
test_pred

# Model Evaluation on Test data
test_res = test.Wg - test_pred
test_sqrs = test_res * test_res
test_mse = np.mean(test_sqrs)
test_rmse = np.sqrt(test_mse)
test_rmse


# Prediction on train data
train_pred = finalmodel.predict(pd.DataFrame(train))
train_pred

# Model Evaluation on train data
train_res = train.Wg -  train_pred
train_sqrs = train_res * train_res
train_mse = np.mean(train_sqrs)
train_rmse = np.sqrt(train_mse)
train_rmse


