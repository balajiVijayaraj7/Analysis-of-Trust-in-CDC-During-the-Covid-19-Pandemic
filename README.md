# Analysis of Trust in CDC During the Covid-19 Pandemic

## Introduction
Welcome to the Trust in CDC Analysis repository. This project focuses on analyzing data from a survey conducted in the US in 2020 to explore people's beliefs about conspiracy theories concerning Covid-19, specifically the idea that it was a Chinese bioweapon. The dataset contains information about people's trust in the CDC during the pandemic. In this analysis, we aim to answer specific questions using suitable statistical models and propose a prediction model.

## Data Description
The dataset contains variables related to people's beliefs, trust in the CDC, and other relevant information. A description of the data can be found in the XML format file provided. We'll use this data to answer the following questions:

## Question 1: Trust in CDC
To determine which type of people did not trust the CDC during the Covid pandemic, we'll analyze the variable "trust_1". We'll categorize the answers as 1 & 2 (Distrust) and 3 & 4 (Trust). A suitable statistical model, such as logistic regression, will be used to identify the factors associated with distrust in the CDC.

## Question 2: Prediction Model for Trust in CDC
We'll propose a prediction model to predict whether one fully trusts the CDC. Those who answered 4 in "trust_1" will be considered as having full trust, and others will not. We'll use classification algorithms like logistic regression, decision trees, or random forests to build the prediction model.

## Analysis Approach

### Data Preprocessing
- The conspiracy variables (cons_*) will not be used as input features, as they are dependent variables specific to the Covid case.
- The survey weight (weight) will be ignored, as per the instructions.

### Question 1 Analysis
- Logistic regression will be used to analyze the factors influencing distrust in the CDC.
- The "trust_1" variable will be recoded as Distrust (1 & 2) and Trust (3 & 4) for analysis.

### Question 2 Prediction Model
- Classification algorithms like logistic regression, decision trees, or random forests will be used to predict full trust in the CDC.
- The variable "trust_1" will be used to create the target variable for the prediction model.

## Limitations and Assumptions
- Assumptions of the chosen statistical models will be discussed, and any violations will be acknowledged.
- Outliers and missing data will be handled appropriately if present.
- Explanations will be provided for any excluded variables from the analysis.

## Conclusion

The Trust in CDC Analysis repository offers an in-depth analysis of people's trust in the CDC during the Covid-19 pandemic. By using suitable statistical models and proposing a prediction model, this project aims to provide insights into factors influencing trust and predicting full trust. 

Feel free to utilize this repository for learning, adapting, and conducting similar analysis projects using R!
