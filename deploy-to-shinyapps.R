#deploy to shinyapps.io
#first you will need an account

#install.packages('rsconnect')

#name is account name, get both your authentication token and secret in your account
rsconnect::setAccountInfo(name='linear0323',
                          token='A7BAAD6502465582F7DD26B25EB90219',
                          secret='<SECRET>')

setwd("Desktop/stat418-final-project/vera")
library(rsconnect)
rsconnect::deployApp(appDir = 'docker/app/',appName="movie_gross")

#this is now running at
#https://linear0323.shinyapps.io/movie/



