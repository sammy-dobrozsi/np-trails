## AllTrails National Trails Dataset Analysis
### Predicting trail popularity with physical trail characteristics and social media metrics
  Using a dataset from Kaggle originally scraped from the website AllTrails, this analysis sought to understand what metrics stored on AllTrails
were related to trail popularity.  Analysis includes data visualization, multiple regression, logistic regression, neural network, classification tree,
and random forest.

## Repository Contents
 - AllTrails data - nationalpark.csv -- The raw data file from Kaggle, which includes trail names and geographic data
 - Trails.csv -- Trail data pared down to just the columns used in analysis
 - TrailsProject_DataVis.R -- R code used to produce the visualizations used for exploratory data analysis that directed the remainder of the analysis
 - TrailsProject_DecTree_RanForest.R -- R code for Decisions Tree and Random Forest analysis, with results and explanation and visualizations shown in the Classification Models report
 - TrailsProject_LogReg.R -- R code for logistic regression analysis, with results and explanation and visualizations shown in the Classification Models report
 - TrailsProject_NN.R -- R code for Neural Netrowk analysis, with results and explanation and visualizations shown in the Classification Models report
 - TrailsProject_SLR_MLR.R -- R code for Multiple Regression analysis, wtih results, explanation, and visualizations shown in the MLR report
 - Trails_Classification_Models.pdf -- Report containing results, visualization, and discussion from Logistic Regression, Neural Network, Decision Tree, and Random Forest analyses
 - Trails_Data_Vis.pdf -- Report containing visualizations and discussion of initial exploratory data analysis
 - Trails_MLR.pdf -- Report containing results, visualization, and discussion from initial exploratory multiple regression analysis

## How To Use
R files are all discrete analyses containing their own required data cleaning and handling before proceeding with the described analysis.

To run and reproduce results, simply download Trails.csv to your directory, set up the import to point to where you downloaded it to, then run all code in the R file.
