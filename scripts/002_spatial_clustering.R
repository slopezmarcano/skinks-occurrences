#Author: Sebastian L
#Created: 20/03/2023
#Updated: 20/03/2023

#-- DESCRIPTION --#
#A script that examines differences in the convex hull areas of skink occurrences (human observations) in Australia between years

#-- LIBRARIES --# 
library(rjson) #json wrangling and requests 
library(jsonlite) #json wrangling
library(httr) #http request
library(tidyverse) #data wrangling
library(sp) #spatial mapping 
library(geosphere) #spatial wrangling - area calculation
library(ggthemes) #ggplot themese
library(patchwork) #plotting arrangement

#install.packages('rjson', 'jsonlite', 'httr', 'tidyverse', 'sp', 'geosphere', 'ggthemes') #Recommend using CRAN Mirror 2(CSIRO AU)
