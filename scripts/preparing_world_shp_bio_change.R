# Preparing world map for shiny app with biomass change on land
# Author: Denisse Fierro Arcos

library(sf)
library(tidyverse)
library(glue)

#Load shapefile for the world
world_360 <- read_sf("data/world_360deg.shp")

#Load biomass change data
table_stats_admin <- read_csv("data/table_stats_country_admin.csv") |> 
  select(Country_FAO:decade, mean, ISO3_country, fill_category, agreement) |> 
  mutate(mean = round(mean, 2))

#Finding countries missing in biomass data
missing_countries <- world_360 |>
  #Turn to data frame
  st_drop_geometry() |> 
  #Select only two columns for easy identification of missing polygons
  select(iso_a3, admin) |>
  #Finding countries from world map not in biomass data
  anti_join(table_stats_admin, by = c("iso_a3" = "ISO3_country")) |> 
  #Count unique countries
  mutate(count_unique = n(),
         #number of times each row will be repeated to match scenario/decade
         count = 4) |> 
  #Repeat each row
  uncount(count)

#Find unique scenario/decade combinations
scenarios <- table_stats_admin |> 
  distinct(scenario, decade) |> 
  #Repeat data frame to match unique missing countries
  slice(rep(row_number(), unique(missing_countries$count_unique)))

#Join two tables together
missing_countries <- missing_countries |> 
  select(-count_unique) |> 
  bind_cols(scenarios)

#Join world shapefile and biomass data
world_360_small <- world_360 |> 
  #Keep same two columns chosen at the beginning of the script
  select(iso_a3, admin) |>
  #Join to biomass data
  left_join(table_stats_admin, by = c("iso_a3" = "ISO3_country"))

#Add missing columns to missing_countries data frame
missing_countries <- missing_countries |> 
  #Join with shapefile created above
  left_join(select(world_360_small, !c(scenario, decade)), 
            join_by(iso_a3, admin))

#Defining levels for biomass categories
levs <- c("Decrease >30%", "Increase <10%",  "Decrease 20 to 30%", 
          "Increase 10 to 20%", "Decrease 10 to 20%", "Increase 20 to 30%", 
          "Decrease <10%",  "Increase >30%", "No data")

#Add missing countries to world shapefile
world_360_small <- world_360_small |> 
  #Remove countries without scenario information
  drop_na(scenario) |> 
  #Join with missing_countries shapefile
  bind_rows(missing_countries) |> 
  #Ensuring names of missing countries are included in Country_FAO column
  mutate(Country_FAO = case_when(is.na(Country_FAO) ~ admin,
                                 T ~ Country_FAO)) |> 
  #Remove columns that are not relevant
  select(!admin) |>
  #Add 'No data' category for plotting
  mutate(fill_category = case_when(is.na(fill_category) ~ "No data",
                                   T~ fill_category),
         #Add columns for mapping
         mean_tooltip = case_when(mean > 0 ~ glue("+{mean}%"), 
                                  mean < 0 ~ glue("{mean}%"),
                                  is.na(mean) ~ "No data"),
         tooltip = case_when(mean_tooltip == "No data" ~ 
                               glue(paste0("Country name: {Country_FAO}\n",
                                           "Biomass change = {mean_tooltip}\n", 
                                           "Model agreement = {mean_tooltip}")),
                             T ~ 
                               glue(paste0("Country name: {Country_FAO}\n", 
                                           "Biomass change = {mean_tooltip}\n",
                                           "Model agreement = {agreement}%")))) |> 
  #Applying Mollweide projection
  st_transform(st_crs("+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m
                      +no_defs")) |> 
  mutate(fill_category = factor(fill_category, levels = levs, ordered = T)) |> 
  select(!c(mean, agreement, mean_tooltip)) |> 
  rename(iso_code = iso_a3, country = Country_FAO, category = fill_category)

#Save shapefile
world_360_small |> 
  write_sf("data/biomass_shapefile_projected.shp")

