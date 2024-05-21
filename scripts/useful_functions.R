################################################################################
# Functions developed to process netCDF files containing FishMIP outputs
# Author: Denisse Fierro Arcos
# Date: 2024-05-21
################################################################################


# Libraries ---------------------------------------------------------------
library(terra)
library(stringr)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(data.table)

#Function transforming netcdf files containing FishMIP outputs to data frames 
#containing metadata
netcdf_to_df <- function(file_path, annual = F, dir_out = NULL){
  #Inputs:
  #- file_path: (str) Path where file is located
  #- annual:  (boolean) Default is FALSE. If TRUE, only years are kept in data
  #           frame
  #- dir_out: (str) Optional. Folder where data frame will be saved
  #
  #Outputs:
  #- df: (data frame) Transformed netcdf containing metadata
  
  #Getting name of file
  file_name <- basename(file_path)
  
  #Extracting FishMIP, ESM and scenario from file name
  mem <- str_extract(file_name,  "(.*)_(gf|ip)", group = 1)
  esm <- str_extract(file_name,  "_(gf.*|ip.*)_no", group = 1)
  scenario <- str_extract(file_name, "sd_(.*)_", group = 1) |> 
    str_remove("_.*")
  
  #Loading netcdf from path
  ras <- rast(file_path)
  
  #Getting metadata to include in data frame
  name_var <- varnames(ras)
  unit_var <- unique(units(ras))
  long_var <- longnames(ras)
  time_var <- time(ras) |> 
    as.character()
  
  #Transform raster to data frame
  ras_df <- ras |> 
    #Keep coordinates from each grid cell
    as.data.frame(xy = T)
  
  #Replace names of data frame for time steps
  names(ras_df) <- c("x", "y", time_var)
  
  #Reorganise data
  ras_df <- ras_df |> 
    pivot_longer(!x:y, names_to = "date", values_to = name_var) |>
    #Add metadata
    mutate(units = unit_var, long_name = long_var, date = ymd(date),
           mem = mem, esm = esm, scenario = scenario) 
  
  #Check if "annual" parameter is TRUE
  if(annual){
    #Replace "date" column with "year" column
    ras_df <- ras_df |> 
      mutate(year = year(date), .before = date) |> 
      select(!date)
  }
  
  #Check if "dir_out" was given as parameter
  if(!is.null(dir_out)){
    #Build path where data frame will be saved. Keep same name as original file
    file_out <- file.path(dir_out, str_replace(file_name, ".nc$", ".csv"))
    
    #Save data frame
    ras_df |>
      fwrite(file_out)
  }
  
  #Return data frame
  return(ras_df)
}





