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
    ds2<- as.data.frame(geosphere::areaPolygon(polygons)) #calculate area of polygon #TODO #3
    return(ds2)
}

chull_area_function <- function(data, years){
    #data frame with occurrences
    #year column in a dataframe called years for each unique year of observation
    
   d1<- prepare_data_for_chull(data, years)
   d2 <- convex_polygon_dataframe(data)
   d3<- convex_area_calculation(data)

   d4<- d3 %>%
   rename(area=1) %>% #rename column to area
   mutate(year=years) #add year to dataframe

   return(d4)
}

mapping_chull <- function(data, years){
    
    d1<- prepare_data_for_chull(data, years)
   d2 <- convex_polygon_dataframe(data)
   d3<- convex_area_calculation(data)
    
    if (years == 2020){
        map <- ggplot(polygons, aes(x=longitude, y = latitude)) +
            geom_polygon(fill = "#2D5D7B", alpha =0.5) +
            theme_classic(base_size=13) +
            labs(title = paste0(years, 'Convex Hull', sep=' '))
    }
    else {
        map <- ggplot(polygons, aes(x=longitude, y = latitude)) +
            geom_polygon(fill = "#C2AFF0", alpha =0.5) +
            theme_classic(base_size=13) +
            labs(title = paste0(years, 'Convex Hull', sep=' '))
    }
    return(map)
}
