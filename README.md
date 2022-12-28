# USA-Gun-Violence
Final Project for MGSC 661 as part of MMA program at McGill University

For the final project we had the option to choose to perform our analysis on 5 different datasets. I decided to use Gun Violence data in the USA which can be found on Kaggle.com. A complete description of all columns of the data can be found [here](https://www.kaggle.com/datasets/jameslko/gun-violence-data).

For the analysis, exploratory data analysis to determine distribution of variables and missing values. After, data cleaning was performed to remove or replace missing and empty values. Then, feature engineering was done to extract more information from select columns and prepare predictor variables for modelling. Next, principal component analysis and correlations between predictors was performed to determine highly correlated variables for final data cleaning. Finally, a random forest model was implemented using the cleaned data to predict the differences between cities with more than 1 gun violence act per week and cities with less than 1 gun violence act per week.

More information on the methodology can be found in Final_nagano.pdf

Below are a list of the files in the repository:
* **Final_Code.R:** Contains the code to perform data cleaning, feature engineering, implementing the model and generating plots
* **Final_nagano.pdf:** The final report outlining the significance of the problem, the methodology for the analysis and insights gained from the results
* **Sample_Gun_Violence.csv:** Contains the first 1000 rows of the Gun Violence Data Set. The complete dataset can be found [here](https://drive.google.com/drive/folders/1itmgl7L1omnPKtBN7TG6B0EQS1IpM1Ph?usp=share_link)