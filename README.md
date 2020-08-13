# Kuehn, Foster, Czyz, & King - Supplemental Information for Psych Med

# Note: Because random data was generated (to ensure complete confidentiality of research participants), output from these R scripts and data files will vary in likely substantial ways between this tutorial and the results reported in the paper. The purpose of this tutorial is intended to provide you with R scripts for splitting a stacked data file, running imputation, creating splines, and generating residualized variables to then run Granger Causality VAR models. For output based on the anonymized observed data file click here: https://kskuehn.github.io/Idiographic-VAR/VAR.html

# Contents:
## Analysis Scripts
VAR.html: https://kskuehn.github.io/Idiographic-VAR/VAR.html 

## Random Data set (all 34 participants)
full.random.csv
This data file is a randomly generated data file that includes all variables for all 34 participants. This data file is meant to be used with the splitting R script which provides code to split a complete, stacked data file into individual data files for person-specific analysis

## Splitting R code
Splitting.R

This R command splits the stacked data file into person-specific models. 

## Imputation and Preprocessing R code
Imputation and preprocessing.R

This R script walks you through imputation, creating splines, and residualizing time effects and calculating difference scores so that the VAR model can be run. This is done for just one person (Person 2) but is repeated for all individuals in your data set (in this case, Person 1 through Person 11). 

## Individual data files
pdat2.csv
This data file is a random data file. This is meant to be used for imputation and preprocessing (Imputation and preprocessing.R) to set up the VAR model

pdat2.var.csv
This data file is randomly generated based on coefficients from Granger Causality Test of Person 2's VAR model between suicidal urges and whether or not they reached out to family (variables are residualized so continous in nature). 

## Grander Causality/VAR Scripts
granger.var.R

This R script provides code to run a VAR model with Granger Causality test. Designed to be used with the pdat2.var.csv data file. 
