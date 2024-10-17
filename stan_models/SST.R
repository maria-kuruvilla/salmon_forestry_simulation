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

#getting chum CU from DFO data

chum_cu <- read.csv(here("data","CM_CU_SITES_En.csv"))

chum_cu_subset <- chum_cu %>% 
  select(FULL_CU_IN, X_LONG, Y_LAT, WS_CDE_20K, GFE_ID)
  # mutate(WATERSHED_CDE_subset = substr(WS_CDE_20K,1,7)) %>% 
  # rename(CU = FULL_CU_IN) %>%
  # select(-c(WS_CDE_20K))

#get latitude and longitude of unique CUs
# chum_cu_subset <- chum_cu %>% 
#   select(FULL_CU_IN, X_LONG, Y_LAT) %>%
#   # group_by(FULL_CU_IN) %>% 
#   unique()

#read dataset
chum_data <- read.csv(here("data","chum_SR_20_hat_yr.csv"))



#left join with lat long data
chum_data_w_coord <- chum_data %>% 
  left_join(chum_cu_subset)

#save dataset

write.csv(chum_data_w_coord, here("data","chum_SR_20_hat_yr_w_coord.csv"))

