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

#read data with SST data

chum_sst <- read.csv(here("data","chum_SR_20_hat_yr_w_coord_w_SSTCOBE2.csv"))

#make correlation plot using ggpairs between sst, npgo, eca values

library(GGally)

cu_list <- ch20r %>% 
  group_by(CU) %>% 
  summarise(n = n_distinct(River)) %>% 
  arrange(n) %>% 
  filter(n>10) %>% 
  select(CU)
panel.ts <- function(data, mapping){
  # print(data$BroodYear)
  # print(data[,mapping$x])
  x <- unique(unlist(lapply(mapping, all.vars)))
  # print(x)
  # print(data[,x])
  new_data <- cbind(values = data[,x], time=data$BroodYear)
  # print(x)
  ggplot(new_data, aes(x = time, y = values))+
    geom_line(size = 1.2, alpha = 0.5, color = "#C9C5BA")
}

ggpairs(chum_sst %>% 
          filter(CU %in% cu_list$CU) %>% 
         select(ECA_age_proxy_forested_only, meanSST, npgo, disturbedarea_prct_cs ,CU),
        columnLabels =  c("ECA","SST","NPGO","CPD"),

        axisLabels = "external", columns = 1:4, aes(alpha = 0.5, color = CU),
        title = "Correlation plot between ECA, SST, NPGO, CPD for CUs with >10 rivers")+
  scale_color_brewer(type = "qual", palette = "Set2")+
  theme_classic()+
  theme(strip.background = element_rect(fill = "white", color = "white"),
        strip.text.x.top = element_text(size = 12),
        strip.text.y.right = element_text(size = 12))
ggsave(here("figures","correlation_plot_sst_npgo.png"), width = 12, height = 10)

