#Author: Sebastian L
#Created: 14/03/2023
#Updated: 20/03/2023

#-- DESCRIPTION --#
#A script that calculates the area of the minimum convex hull from human observations from 2005 to 2022.
#The overall objective was to observe if the distribution (convex hull) decreased during covid years. 


#-- LIBRARIES --# 
library(rjson) #json wrangling and requests 
library(jsonlite) #json wrangling
library(httr) #http request
library(tidyverse) #data wrangling
library(sp) #spatial mapping 
library(geosphere) #spatial wrangling - area calculation
library(ggthemes) #ggplot themese
library(patchwork) #plotting arrangement

#install.packages(c('rjson', 'jsonlite', 'httr', 'tidyverse', 'sp', 'geosphere', 'ggthemes', 'patchwork') #Recommend using CRAN Mirror 2(CSIRO AU)

#-- SOURCE THE FUNCTIONS DEVELOPED FOR THIS SCRIPT --#
source('scripts/functions.R')

#-- API REQUEST AND JSON TO DATAFRAME --#
#api link provided by ALA
link <- "https://biocache-ws.ala.org.au/ws/occurrences/search?q=lsid%3Ahttps%3A%2F%2Fbiodiversity.org.au%2Fafd%2Ftaxa%2F682e1228-5b3c-45ff-833b-550efd40c399&qualityProfile=ALA&fq=state%3A%22Australian%20Capital%20Territory%22&pageSize=10000"

#get data
response <- GET(link)

#turn list of lists into simple vectors and obtain content from response into json
df <- purrr::flatten(fromJSON(content(response, as = 'text'))) 

#if continuing to tibble, you will get an error due to duplicated columns. state vs state.1
#therefore selecting important sublists from list.
keydf <- df[c('uuid', 'vernacularName', 'family', 'decimalLatitude', 
                            'decimalLongitude', 'year', 'basisOfRecord')]

#Convert list into a dataframe
data <- as_tibble(keydf)

#-- CLEANING AND DATASET WRANGLING --#
#wrangling data to select skinks, years of interest, human observations only and obsv with years. rename lat and long columns
data_skinks <- data %>%
    filter(
        vernacularName =='Dark-flecked Garden Sunskink',
        year >=2005 & year <=2022,
        basisOfRecord == 'HUMAN_OBSERVATION')%>%
    rename(
        latitude = decimalLatitude,
        longitude = decimalLongitude)

#-- OBTAIN UNIQUE YEARS OF OBSERVATION --#
years <- data_skinks %>%
    group_by(year) %>%
    count()

#-- CALCULATE CONVEX HULL --#
# Calculate the convex hull area for each year
area_per_year <- bind_rows(lapply(1:nrow(years), function(x){chull_area_function(data_skinks, years$year[x])}))

# -- INCLUDE OBSERVATIONS COUNT INTO THE DATAFRAME --#
area_per_year <- full_join(area_per_year, years, by='year') %>%
                rename(observations = 3)

#-- PLOT THE AREAS--#
p1<-ggplot(area_per_year, aes(x = as.factor(year), y = area)) + 
    geom_segment(aes(x=as.factor(year) ,xend=as.factor(year), y=0, yend=area), color="#f4a261") +
    geom_point(size=8, color="#e76f51")  +
    labs(x = "Year", y = "Area of Convex Hull (km2)") +
    coord_flip()+
    theme_clean(base_family = "Helvetica")+
    theme(panel.background = element_rect(fill = "#264653"),
        plot.background = element_rect(fill="#264653"),
        axis.text = element_text(color = "#f1faee", size = 13),
        axis.title = element_text(color = "#e9c46a", size = 16))

#-- PLOT THE OBSERVATIONS --#
p2<- ggplot(area_per_year, aes(x = as.factor(year), y = observations)) + 
    geom_segment(aes(x=as.factor(year) ,xend=as.factor(year), y=0, yend=observations), color="#f4a261")+
    geom_point(size=5, color="#E2725B")  +
    labs(x = "Year", y = "Human Observations ALA") +
    coord_flip()+
    theme_clean(base_family = "Helvetica")+
    theme(panel.background = element_rect(fill = "#264653"),
        plot.background = element_rect(fill="#264653"),
        axis.text = element_text(color = "#f1faee", size = 13),
        axis.title = element_text(color = "#e9c46a", size = 16))

#-- PLOT THE CONVEX HULL OF THE YEAR WITH MORE OBSERVATIONS OR LARGER AREA --#
#2021 and #2020
map_2020 <- mapping_chull(data_skinks, '2020')
map_2021 <- mapping_chull(data_skinks, '2021')

#-- PLOT ALL INTO A STATIC FIGURE
(p1 | p2) /(map_2020 | map_2021)

#--SAVE GGPPLOT --#
ggsave('outputs/convex_hull_garden_skink_through_years_newv2.png')



