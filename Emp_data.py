# Importing necessary libraries
import pandas as pd # deals with data frame  
import numpy as np  # deals with numerical values

# Input Variable = Salary Hike
# Output Varibale = Churn out rate

emp_data= pd.read_csv("C:/Users/personal/Desktop/emp_data.csv")
emp_data.columns = "SH", "CR" # SH = Salary Hike & CR = Churn out rate
emp_data.columns

# Exploratory data analysis:
emp_data.describe()

#Graphical Representation
import matplotlib.pyplot as plt # mostly used for visualization purposes 

plt.bar(height = emp_data.CR, x = np.arange(1,11, 1))
plt.hist(emp_data.CR) #histogram
plt.boxplot(emp_data.CR) #boxplot

plt.bar(height = emp_data.SH, x = np.arange(1,11, 1))
plt.hist(emp_data.SH) #histogram
plt.boxplot(emp_data.SH) #boxplot

# Scatter plot
plt.scatter(x = emp_data['SH'], y = emp_data['CR'], color = 'red') 

# correlation
np.corrcoef(emp_data.SH, emp_data.CR) 

# Covariance
cov_output = np.cov(emp_data.SH, emp_data.CR)[0, 1]
cov_output


# Import library
import statsmodels.formula.api as smf

# Simple Linear Regression
model = smf.ols('CR ~ SH', data = emp_data).fit()
model.summary()

pred1 = model.predict(pd.DataFrame(emp_data['SH']))

# Regression Line
plt.scatter(emp_data.SH, emp_data.CR)
plt.plot(emp_data.SH, pred1, "r")
plt.legend(['Predicted line', 'Observed data'])
plt.show()

# Error calculation
res1 = emp_data.CR - pred1
res_sqr1 = res1 * res1
mse1 = np.mean(res_sqr1)
rmse1 = np.sqrt(mse1)
rmse1

######### Model building on Transformed Data
# Log Transformation
# x = log(Salary Hike); y = Churn out rate

plt.scatter(x = np.log(emp_data['SH']), y = emp_data['CR'], color = 'brown')
np.corrcoef(np.log(emp_data.SH), emp_data.CR) #correlation

model2 = smf.ols('CR ~ np.log(SH)', data = emp_data).fit()
model2.summary()

pred2 = model2.predict(pd.DataFrame(emp_data['SH']))

# Regression Line
plt.scatter(np.log(emp_data.SH), emp_data.CR)
plt.plot(np.log(emp_data.SH), pred2, "r")
plt.legend(['Predicted line', 'Observed data'])
plt.show()

# Error calculation
res2 = emp_data.CR - pred2
res_sqr2 = res2 * res2
mse2 = np.mean(res_sqr2)
rmse2 = np.sqrt(mse2)
rmse2


#### Exponential transformation
# x = Salary Hike; y = log(Churn out rate)

plt.scatter(x = emp_data['SH'], y = np.log(emp_data['CR']), color = 'orange')
np.corrcoef(emp_data.SH, np.log(emp_data.CR)) #correlation

model3 = smf.ols('np.log(CR) ~ SH', data = emp_data).fit()
model3.summary()

pred3 = model3.predict(pd.DataFrame(emp_data['SH']))
pred3_at = np.exp(pred3)
pred3_at

# Regression Line
plt.scatter(emp_data.SH, np.log(emp_data.CR))
plt.plot(emp_data.SH, pred3, "r")
plt.legend(['Predicted line', 'Observed data'])
plt.show()

# Error calculation
res3 = emp_data.CR - pred3_at
res_sqr3 = res3 * res3
mse3 = np.mean(res_sqr3)
rmse3 = np.sqrt(mse3)
rmse3


#### Polynomial transformation
# x = SH; x^2 = SH*SH; y = log(CR)

model4 = smf.ols('np.log(CR) ~ SH + I(SH*SH)', data = emp_data).fit()
model4.summary()

pred4 = model4.predict(pd.DataFrame(emp_data))
pred4_at = np.exp(pred4)
pred4_at

# Regression line
from sklearn.preprocessing import PolynomialFeatures
poly_reg = PolynomialFeatures(degree = 2)
X = emp_data.iloc[:, 0:1].values
X_poly = poly_reg.fit_transform(X)


plt.scatter(emp_data.SH, np.log(emp_data.CR))
plt.plot(X, pred4, color = 'red')
plt.legend(['Predicted line', 'Observed data'])
plt.show()


# Error calculation
res4 = emp_data.CR - pred4_at
res_sqr4 = res4 * res4
mse4 = np.mean(res_sqr4)
rmse4 = np.sqrt(mse4)
rmse4


# Choose the best model using RMSE
data = {"MODEL":pd.Series(["SLR", "Log model", "Exp model", "Poly model"]), "RMSE":pd.Series([rmse1, rmse2, rmse3, rmse4])}
table_rmse = pd.DataFrame(data)
table_rmse

###################
# The best model - From the RMSE poly model is the best model with Low Rmse

from sklearn.model_selection import train_test_split

train, test = train_test_split(emp_data, test_size = 0.2)

finalmodel = smf.ols('np.log(CR) ~ SH + I(SH*SH)', data = emp_data).fit()
finalmodel.summary()

# Predict on test data
test_pred = finalmodel.predict(pd.DataFrame(test))
pred_test_CR = np.exp(test_pred)
pred_test_CR

# Model Evaluation on Test data
test_res = test.CR - pred_test_CR
test_sqrs = test_res * test_res
test_mse = np.mean(test_sqrs)
test_rmse = np.sqrt(test_mse)
test_rmse


# Prediction on train data
train_pred = finalmodel.predict(pd.DataFrame(train))
pred_train_CR = np.exp(train_pred)
pred_train_CR

# Model Evaluation on train data
train_res = train.CR - pred_train_CR
train_sqrs = train_res * train_res
train_mse = np.mean(train_sqrs)
train_rmse = np.sqrt(train_mse)
train_rmse
