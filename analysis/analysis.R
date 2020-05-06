library(readr)
library(tidyverse)
library(ggplot2)
library(leaflet)
library(mgcv)
library(htmltools)
library(htmlwidgets)

setwd('C:/Users/rnussba1/OneDrive/ARK/ARK - Science/Madagascar_pratincole_publication')
d <- read_csv('./data/data.csv', col_types = cols(
  source = col_character(),
  date = col_date(format = "%d/%m/%Y"),
  number = col_integer(),
  location = col_character(),
  latitude = col_double(),
  longitude = col_double(),
  country = col_character(),
  observer = col_character(),
  picture = col_logical(),
  description = col_character(),
  validity = col_character()
))

# Filter
df = d %>%
  filter(is.na(validity)) %>% 
  mutate(number = as.numeric(number)) %>%
  mutate(julian = as.numeric(format(date,'%j'))) %>% 
  mutate(year = as.numeric(format(date,'%Y'))) %>% 
  mutate(source = if_else(grepl("East African Bird Report",source),"East African Bird Report",source)) %>% 
  mutate(period = if_else(year <2000, '<2000','>2000')) %>% 
  mutate(popup = iconv(paste('<b>Number</b>: ', number ,
                       '<br><b>Date</b>: ',date,
                       '<br><b>Observer</b>: ', observer,
                       '<br><b>Validity</b>: ', validity,
                       '<br><b>Picture</b>: ', picture,
                       '<br><b>Description</b>: ', description,
                       '<br><b>Source</b>: ', source
                       )),
          "UTF-8", "UTF-8",sub='')

#View Dataset
df

# Map sightings
leaflet::leaflet(df) %>%
  leaflet::addTiles() %>%
  leaflet::addAwesomeMarkers(lng = ~longitude, lat = ~latitude, popup = ~htmlEscape(popup))


# Plot count over years for each countries. 
df %>% 
  filter(!is.na(number)) %>% 
  ggplot(aes(x = year, y = number, col=country)) +
  geom_point() +
  geom_smooth(method = "glm", formula = y ~ x) +
  facet_grid(rows = vars(country =='Kenya'), scales = "free") 

# Count over years at Sabaki
df %>% 
  filter(location=="Sabaki") %>% 
  #filter(number>0) %>% 
  #filter(source == "Britton (1977)" | source == "Water Bird Count" | source == "eBird") %>%
  filter(source %in% c("Water Bird Count","East African Bird Report","Britton (1977)")) %>% 
  ggplot(aes(x= year, y= number, col = number==0)) +
  geom_point() +
  geom_smooth(method = "glm", formula = y ~ x , method.args = list(family = "poisson")) + 
  ylim(-10,3600)

# Phenology curve at Sabaki
df %>% 
  filter(location=="Sabaki") %>% 
  #filter(number>0) %>% 
  #filter(source == "Britton (1977)" | source == "Water Bird Count" | source == "eBird") %>%
  filter(source %in% c("Water Bird Count","East African Bird Report","Britton (1977)")) %>% 
  ggplot(aes(x= julian, y= number, col = period)) +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x, k=5) , method.args = list(family = "poisson")) +
  scale_x_continuous(breaks=cumsum(c(1,31,28,31,30,31,30,31,31,30,31,30)), 
                     labels =c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec") ) + 
  ylim(-10,3600)

# Fit GAM
dff = df %>% 
  filter(location=="Sabaki") %>% 
  filter(number>0) %>% 
  #filter(number>0) %>% 
  #filter(source == "Britton (1977)" | source == "Water Bird Count" | source == "eBird") %>%
  filter(source %in% c("Water Bird Count","East African Bird Report","Britton (1977)"))

gam.fitted = gam(dff$number ~ s(dff$julian, k=6) + dff$year, family = 'poisson')

# rmarkdown::render("analysis/analysis.R")
