#load packages
library(here)
library(sf)
library(readr)
library(dplyr)
library(tidyr)
library(janitor)
library(tmap)
library(grid)

# read data
index <- read_csv("hw4/data/hdr-data.csv")
country <- st_read("hw4/data/World_Countries.geojson")

Datatypelist <- index %>% 
  summarise_all(class) %>%
  pivot_longer(everything(), 
               names_to = "All_variables", 
               values_to = "Variable_class")

# Cerys edited to filter for GII
# select and filter
index2 <- index %>%
  clean_names() %>%
  filter(indicator_code == "gii") %>%
  select(country, year, value) %>%
  filter(year == 2019 | year == 2010 ) %>%
  group_by(country) %>%
  summarise(
    index_2010 = value[year == 2010],   
    index_2019 = value[year == 2019],   
    index_diff = index_2010 - index_2019 
  )%>%
  na.omit()

#! id code instead of name
# join to the spatial data
joined_data <- country %>% 
  clean_names(.) %>%
  left_join(.,index2, by = "country")  
# output  sf object: columns of your difference, and then  country name or some ID value

# Cerys taking over here
tmap_mode("plot")

breaks = c(0,0.2,0.4,0.6,0.8,1)

map_2010 <- tm_shape(joined_data) +
  tm_polygons("index_2010",
              breaks = breaks, 
              palette = "Oranges") +
  tm_legend(show=FALSE)+
  tm_layout(frame=FALSE)+
  tm_credits("2010", position=c(0.1,0.85), size=1)

map_2019 <- tm_shape(joined_data) +
  tm_polygons("index_2019",
              breaks = breaks,
              palette = "Oranges") +
  tm_legend(show=FALSE)+
  tm_layout(frame=FALSE)+
  tm_credits("2019", position=c(0.1,0.85), size=1)+ 
  tm_compass(north=0, position=c(0.8,0))+ 
  tm_credits("(c) ESRI and UNDP", position=c(0.3,0.0)) 

legend <- tm_shape(joined_data) +
  tm_polygons("index_2010",
              breaks = breaks,
              palette = "Oranges",
              title = "GII",
              legend.is.portrait = FALSE)+
  tm_layout(legend.only = TRUE, 
            legend.width = 25)

t = tmap_arrange(map_2010,map_2019,legend,ncol=2)

map_diff <- tm_shape(joined_data) +
  tm_polygons("index_diff",
              fill.legend = tm_legend(title = "GII difference",
                                      orientation = "landscape"),
              fill.scale = tm_scale_intervals(n=4,values = "spectral"))+
  tm_layout(frame=FALSE,inner.margins = c(0.1,0.1,0.1,0.1))+
  tm_title("GII difference between 2019 and 2010", position = (c(0,1)))+
  tm_compass(north=0, position = c(0.7,-0.02))+
  tm_credits("(c) ESRI and UNDP", position = c(0.8, 0))

tmap_save(map_diff, "map_diff.png")

tmap_save(t, "my_map.png")







