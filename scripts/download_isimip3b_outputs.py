#!/usr/bin/env python3

#Calculating annual means from FishMIP 3b outputs
#Author: Denisse Fierro Arcos
#Last updated: 2024-05-20

#Libraries
import xarray as xr
import os
from glob import glob
import pandas as pd

#Base folder containing Earth System Model (ESM) data
base_dir = os.path.join('/work/bb0820/ISIMIP/ISIMIP3b/OutputData',
                        'marine-fishery_global')

#Define output folder
base_out = 'annual_mean'
os.makedirs(base_out, exist_ok = True)

#Variables of interest (from FAO Report - Table A3)
var_int = ['tcb', 'tdb', 'bd30cm', 'bd30to90cm', 'bd90cm', 'tpb', 'bp30cm', 
           'bp30to90cm', 'bp90cm', 'tc', 'tclog10', 'tdc', 'cd30cm', 
           'cd30to90cm', 'cd90cm', 'tpc', 'cp30cm', 'cp30to90cm', 'cp90cm']

for v in var_int:
  #Get list of files for variable of interest
  var_files = glob(os.path.join(base_dir, f'*/*/*/*{v}_*.nc'))
  
  #Loop through all files
  for f in var_files:
    #Base file name out
    out_file = os.path.basename(f)
    #Folder where summaries will be saved
    out_folder = os.path.dirname(f).replace(base_dir, base_out)
    os.makedirs(out_folder, exist_ok = True)
    
    #Open data array
    ds = xr.open_dataset(f)[v]
    #Change format of time dimension if needed
    if not isinstance(ds.indexes['time'], pd.DatetimeIndex):
      new_time = ds.indexes['time'].to_datetimeindex()
      ds['time'] = new_time
    
    #Check if this is a mothly file
    if 'monthly' in f:
      #Replace "monthly" for "annual" in file name
      out_file = out_file.replace('monthly', 'annual')
      #Calculate yearly mean
      ds = ds.groupby('time.year').mean('time')
      #Formatting years as date to match annual data
      new_time = pd.DatetimeIndex([f'{yr}-01-01' for yr in ds.indexes['year'].values])
      ds['year'] = new_time
      ds = ds.rename({'year': 'time'})
      
    #Save annual file
    ds.to_netcdf(os.path.join(out_folder, out_file))
  
