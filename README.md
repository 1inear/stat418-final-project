# Shiny App-Predicting IMDb movie gross income 
## Table of Contects
1. [Introduction](README.md#Introduction)
2. [Deployment](README.md#Deployment)
3. [Data Cleaning and Exploratory Data Analysis](README.md#data-cleaning-and-exploratory-data-analysis)
4. [Modeling](README.md#Modeling)
5. [Conclusions](README.md#Conclusions)
## Introduction
This app was developed based on the movie data that I scraped from IMDb concerning the order of popularity through https://www.imdb.com/search/title?title_type=feature&start=1&ref_=adv_nxt. The web scraping process is done by the python package called beautiful soup. We scraped 50 pages so the csv data file contains 10000 rows. In this project we attempt to moedel the movie features and predict the box office of each movie. This model is deployed as an Shiny App hosted on shinyapps.io.
## Deployment
There are two ways to access this app.

**1. Locally, in terminal, git clone this repo to the directory of your choice:**

`$ git clone https://github.com/1inear/stat418-final-project.git`

**or download zip file to the same directory. Then change your working directory to docker and run:**

`$ R -e "shiny::runApp('app/')"`

**Copy the outputting link to any browser to run this app.** 

<br>

**2. Simply access this Shiny app at https://linear0323.shinyapps.io/movie/** 

<br>

After opening this app, you can adjust the sliders and radio buttons to observe the change of the prediction of box office(gross income).

## Data Cleaning and Exploratory Data Analysis
First, since the original data set contains some missing values, we decided to remove all the rows that contain the missing value. Second, the level "R", "PG" and "PG-13" took part 70% of all the levels, we dropped the rows containing other levels. Third, the genre of each row contains 1, 2 or 3 different categories, we intended to split them into 21 independent genre and created dummy variables. Fourth, we divided the movie runtime into three part, short(less than or equal to 100 minutes), normal(101-120minutes), and long(more than 120 minutes). What's more, for the movie release year, we reclassified them into the categories "before 2000", "first_ten"(2000-2010), "second_ten"(>2010).

<br>

Then we can proceed the exploratory data analysis. 
Since the histogram of gross_in_million and votes are skewed to the right. We need to do log transformation to make them be normal. 

The histogram of gross and log_gross.<br>
![](./images/gross.png)
![](./images/log_gross.png)

<br>

The histogram of votes and log_votes.<br>
![](./images/votes.png)
![](./images/log_votes.png)

## Modeling
We split the data into training data set and test data set for future cross validation. And we can now fit a multiple linear regression. We start with the full model and use backward selection to remove insignificant variables. The result of the final model is shown below.
![](./images/train.png)
This model explains more than 64% of the variations in the box office(gross income). 
Through cross validation, we can get the predicted box office(gross income) explains more than 56% of the variation in the box office(gross income), which indicates that our model is quite good. The result is shown below.
![](./images/test.png)

## Conclusion
Overall, Our multiple linear regression is quite good for the data. Our major limitation to this model is that only multiple linear regression was taken into consideration for the model. In the future, we may use some other models like random forest, neural network etc.






