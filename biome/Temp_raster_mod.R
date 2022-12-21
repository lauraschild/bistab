#temp raster with modelled data

rm(list = ls())

library(raster)
library(ncdf4)
library(tidyverse)

nc_data <- nc_open("N:/bioing/data/Modelling_AnneDallmeyer/TraCE21ka/T/trace.01.22000-20001BP.cam2.h0.T.0000101-0200012.nc")
# Save the print(nc) dump to a text file
{
  sink('new.txt')
  print(nc_data)
  sink()
}
lon <- ncvar_get(nc_data, "lon")
lat <- ncvar_get(nc_data,"lat")
t <- ncvar_get(nc_data,"date")
t
ncvar_get(nc_data,"date")

t <- brick("N:/bioing/data/Modelling_AnneDallmeyer/TraCE21ka/T/trace.01.22000-20001BP.cam2.h0.T.0000101-0200012.nc")
#monthly data --> aggreate into 500 year slices --> start at 10ka BP
#data in Kelvin convert to C = K - 273.15
