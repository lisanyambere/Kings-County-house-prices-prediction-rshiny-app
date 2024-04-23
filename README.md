This is a simple rshiny app for predicting house prices in Kings County using linear regression. The dataset is from Kaggle datasets. 
Started off by cleaning the dataset and conducting EDA.
Modelling is illustrated using Linear regression, decision trees and random forests. The models are then compared using MSE. The random forest in this case performed better than the others.
Next is creating the rshiny app. I used the linear regression model for the app due to simplicity but it can be modified to use the random forest model. The app takes inputs such as grade of the house,condition of the house,gps coordinates,square feet of the living room, year built, whether the house has been renovated, view or not and waterfront or not.
