prepare_data_for_chull <- function(data, years){
    ds1 <- data %>%
        filter(year == years) %>%
        select(longitude, latitude) %>%
        na.omit() #omit any data with no lat and long
    return(ds1)
}

convex_polygon_dataframe <- function(data){
    hull <- chull(data) #convex polygon

    polygons <- as.data.frame(data[hull,])

    return(polygons)
}

convex_area_calculation <- function(data){
    ds2<- as.data.frame(geosphere::areaPolygon(data)*0.000001) #turning square m into km2
    return(ds2)
}

chull_area_function <- function(data, years){
    #data frame with occurrences
    #year column in a dataframe called years for each unique year of observation
    
   d1<- prepare_data_for_chull(data, years)
   d2 <- convex_polygon_dataframe(d1)
   d3<- convex_area_calculation(d2)

   d4<- d3 %>%
   rename(area=1) %>% #rename column to area
   mutate(year=years) #add year to dataframe

   return(d4)
}

mapping_chull <- function(data, years){
    
    d1<- prepare_data_for_chull(data, years)
   d2 <- convex_polygon_dataframe(d1)
    
    if (years == 2020){
        map <- ggplot(d2, aes(x=longitude, y = latitude)) +
            geom_polygon(fill = "#2a9d8f", alpha =0.5) +
            labs(title = paste(years, 'Convex Hull', sep=' '))+
            theme_clean(base_family = "Helvetica")+
            theme(panel.background = element_rect(fill = "#264653"),
            plot.background = element_rect(fill="#264653"),
            axis.text = element_text(color = "#f1faee", size = 13),
            axis.title = element_text(color = "#e9c46a", size = 16),
            plot.title = element_text(color="#f1faee", size = 20))
    }
    else {
        map <- ggplot(d2, aes(x=longitude, y = latitude)) +
            geom_polygon(fill = "#f4a261", alpha =0.5) +
            labs(title = paste(years, 'Convex Hull', sep=' '),
                    x= "Longitude", y = "Latitude")+
            theme_clean(base_family = "Helvetica")+
            theme(panel.background = element_rect(fill = "#264653"),
            plot.background = element_rect(fill="#264653"),
            axis.text = element_text(color = "#f1faee", size = 13),
            axis.title = element_text(color = "#e9c46a", size = 16),
            plot.title = element_text(color="#f1faee", size = 20))
    }
    return(map)
}

#Theme palette
#background = #264653
#2a9d8f
#e9c46a
#f4a261
#e76f51
#colour text for contrast = #f1faee