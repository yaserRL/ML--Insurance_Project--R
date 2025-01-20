Predicting Customer Claims:
This repository contains a comprehensive case study on building a predictive model to estimate the total number of claims a customer might file with an insurance company. The project involves data cleaning, descriptive analysis, feature engineering, and model building using statistical and machine learning techniques.


Project Structure:
Data Preparation: The dataset is preprocessed by cleaning, transforming, and encoding features. Missing values are handled, and numerical variables are normalized.

Descriptive Statistics: Insights into the dataset are extracted through various statistical measures and visualizations.

Feature Engineering: Variables are analyzed and renamed for clarity, and correlations between features are explored to guide model development.

Modeling: Multiple statistical models, including Linear Regression and Poisson Regression, are applied to predict claim counts.

Performance Evaluation: Models are evaluated based on metrics such as Root Mean Square Error (RMSE), Adjusted R-squared, and residual analysis. 

Results

The final Poisson Regression model demonstrated superior performance with an RMSE of ~0.235 compared to Linear Regression.

Insights:

Features such as areacode, discount, and vehicle_age have significant predictive power.

Certain regions and vehicle brands exhibit unique trends in claim counts.
