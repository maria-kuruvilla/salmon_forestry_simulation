# pull SST data from NOAA ERSST Data

install.packages("devtools")
library(devtools)
install_github(repo = "michaelmalick/r-ersst")
library(ersst)
install.packages("tidync")
library(tidync)


# get sst data from 1950 to 20
ersst::sst_download(years = 1950:2022,
                    months = 1:12,
                    save.dir = "./data-downloaded/climate-data/sst_raw/",
                    version = 5)
#not working

#read data from http://psl.noaa.gov/thredds/dodsC/Datasets/COBE2/sst.mon.ltm.1991-2020.nc

sst <- tidync("http://psl.noaa.gov/thredds/dodsC/Datasets/COBE2/sst.mon.ltm.1991-2020.nc")
sst_data <- sst %>% 
  activate("sst") %>% 
  hyper_tibble()

#time is in days since 1891-01-01
sst_data$year <- as.numeric(format(as.Date("1891-01-01") + sst_data$time, "%Y"))