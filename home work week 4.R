title:"homework week 4"
Name:"Jize Chu 24225531"


library(tidyverse)
library(here)
library(sf)
library(janitor)
library(tmap)
library(ggplot2)
library(readr)
library(dplyr)
install.packages("countrycode")

library(countrycode)

# read csv
ggidata<- read.csv(here::here("HDR23-24_Composite_indices_complete_time_series.csv"),
                             header = TRUE, sep = ",",  
                             encoding = "latin1")

# read World map
World_map<- sf::st_read(here::here("World_Countries_(Generalized).geojson"))

#select iso3 and gii data
select_1 <- ggidata %>%
  select(iso3, contains("gii"))

# make a new colum of difference between 2019 and 2010
ggidata_diff <- select_1 %>%
  mutate(giidiff_2010_2019 = gii_2019 - gii_2010)

# transfer ISO from is02c to iso3c and store in ISO3 
countries_data <- World_map %>%
  mutate(ISO3 = countrycode(ISO, origin = "iso2c", destination = "iso3c"))

# join data
combined_data <- countries_data %>%
  left_join(ggidata_diff, by = c("ISO3" = "iso3"))

#visulization
library(tmaptools)
tmap_mode("plot")


tm_shape(combined_data)+
  tm_polygons("giidiff_2010_2019", 
              style="pretty",
              palette="PiYG",
              breaks=5,
              title="Number of years",
              midpoint=0
              ) + 
  tm_compass(position = c("left", "bottom"),type = "arrow") + 
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(title = "the difference of gender inequality index between 2010 and 2019", legend.position = c("right", "bottom"))