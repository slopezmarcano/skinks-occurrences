#Author: Sebastian L
#Created: 14/03/2023
#Updated: 16/03/2023

#-- DESCRIPTION --#
#A script that calculates the area of the minimum convex hull from human observations from 2015 to 2022.
#The overall objective was to observe if the distribution (convex hull) decreased during covid years. 


#-- START TIME --#
    #11:30 AM AEST

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
        year >=2012 & year <=2022,
        basisOfRecord == 'HUMAN_OBSERVATION')%>%
    rename(
        latitude = decimalLatitude,
        longitude = decimalLongitude)

#-- OBTAIN UNIQUE YEARS OF OBSERVATION --#
years <- data_skinks %>%
    group_by(year) %>%
    count()

#-- CALCULATE CONVEX HULL --#
# Calculate the convex hull for each year
chull_function <- function(data, years){
    #data frame with occurrences
    #year column in a dataframe called years for each unique year of observation
    
    ds1 <- data %>%
        filter(year == years) %>%
        select(longitude, latitude) %>%
        na.omit() #omit any data with no lat and long
    
    hull <- chull(ds1) #convex polygon

    polygons <- as.data.frame(ds1[hull,])
    
    ds2<- as.data.frame(geosphere::areaPolygon(polygons))%>% #calculate area of polygon #TODO #3
        rename(area = 1) %>% #rename column to area
        mutate(year = years) #add year to the dataframe

    return(ds2)   
}

#-- RUN THE FUNCTION --#
area_per_year <- bind_rows(lapply(1:nrow(years), function(x){chull_function(data_skinks, years$year[x])}))

# -- INCLUDE OBSERVATIONS COUNT INTO THE DATAFRAME --#
area_per_year <- full_join(area_per_year, years, by='year') %>%
                rename(observations = 3)

#-- PLOT THE AREAS--#
p1<-ggplot(area_per_year, aes(x = as.factor(year), y = sqrt(area))) + #TODO: #1 transforming the area to a sqrt just for visualisation of this test. But values are incorrect.
    geom_segment(aes(x=as.factor(year) ,xend=as.factor(year), y=0, yend=sqrt(area)), color="grey") +
    geom_point(size=5, color="#69b3a2")  +
    labs(x = "Year", y = "Area of Convex Hull") +
    coord_flip()+
    theme_clean(base_size = 13)

#-- PLOT THE OBSERVATIONS --#
p2<- ggplot(area_per_year, aes(x = as.factor(year), y = observations)) + 
    geom_segment(aes(x=as.factor(year) ,xend=as.factor(year), y=0, yend=observations), color="grey")+
    geom_point(size=5, color="#E2725B")  +
    labs(x = "Year", y = "Human Observations ALA") +
    coord_flip()+
    theme_clean(base_size = 13)

#-- PLOT THE CONVEX HULL OF THE YEAR WITH MORE OBSERVATIONS --#
#2021
chull2021 <- data_skinks %>%
        filter(year == 2021) %>%
        select(longitude, latitude) %>%
        na.omit()  #omit any data with no lat and long 
hull2021 <- chull(chull2021)  #convex polygon
polygons <- as.data.frame(chull2021[hull2021,])

map <- ggplot(polygons, aes(x=longitude, y = latitude)) +
        geom_polygon(fill = "lightblue", alpha =0.5) +
        theme_classic(base_size=13) +
        labs(title = '2021 Convex Hull')

#-- PLOT ALL INTO A STATIC FIGURE
(p1 | p2) /map

#--SAVE GGPPLOT --#
ggsave('outputs/convex_hull_garden_skink_through_years.png')

#-- END TIME --#
    #12:45 PM AEST