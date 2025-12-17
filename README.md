# project-8
Group 8 Project for DSCI445 @ CSU


Project Description:
This project implements multiple predictive modeling approaches that include 
linear regression, LASSO, Elastic Net, and decision tree models. We wanted to 
identify the most influential predictors of high gross in films. This analysis
uses a dataset that had information scraped from IMDBs website. 

The objective is to compare these models and determine which predictors were
the most strongly associated with higher gross revenue. 


Data Cleaning:
- When loading in the data, set "fileEncoding" = to "latin1". 
Purpose: this helps take care of characters the system was unable to process.

- If budget value is 0, replace it with the budget value determined by the median 
by year. This is done by using the select, group_by, and mutate functions from 
the dplyr package. Purpose: to make sure we're not just dropping data and 
replace 0 with something meaningful.

- For region, identify which countries are neighbors and assign them a region 
using mutate function from the dplyr package. For example: "USA", "Canada", 
"Mexico", "Bahamas", "Aruba", "Panama") are all considered the North American 
region. Purpose: to make dummy encoding smoother later on.

- For director and company frequency encoding, we just add up the counts using 
count() function. Purpose: to make director and company useful in our 
investigation by making it numerical data.

::Add whatever you were in charge of for data cleaning::


Models:

Linear Regression:


LASSO:




Elastic Net:

- Log transform gross. Purpose: to ensure right skew is not overwhelming

- Build your elastic net recipe:
1. Using step_mutate(), logp() transform all predictors.
Purpose: ensuring that 0s are properly handled and not neglected and that skew 
is not out of hand for predictions
2. use step_novel() to take care of factor levels. Purpose: since we have 
numerical and categorical variables, we have to make sure things are handled 
as such
3. step_unknown() to handle unknown values. Purpose: so that this doesn't 
negatively impact our model.
4. step_dummy() dummy encoding the categorical variables: season, genre, rating,
and region. Purpose: to make the categorical variables are accounted for in the 
model.
5. step_zv() removes zero-variance predictors. Purpose: stability and making 
sure that predictors have varied values.
6. step_normalize() standaradizes all the values for the model. Purpose: to 
ensure all predictors are contributing equally toward the model. 

- Define the elastic net model with the lambda and alpha parameters chosen by R 
using tune().

- Define a workflow that includes the recipe that was made above and the elastic
net model made above. (need workflow and recipe packages)

- split data, using 10-fold cross-validation

- Make a grid for lambda and alpha

- Name the metrics you want in a variable

- Tuning of lambda and alpha using the workflow we defined above.

- Select the lowest RMSE and pull the lambda and the alpha that was associated 
with that output

- Define the final net model with the best parameters that were found with the
lowest RMSE

- Fit the final model using the final net model from the previous step

- Using the fit from above, then predict so we can compare this to the OG data.









Trees:





Figures:








