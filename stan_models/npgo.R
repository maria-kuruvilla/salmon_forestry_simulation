# read  NPGO index data from https://www.o3d.org/npgo/data/NPGO.txt


# Load necessary library
library(ggplot2)
library(tidyverse)
library(here)

# data

npgo <- read.table("https://www.o3d.org/npgo/data/NPGO.txt", header = FALSE, col.names = c("year", "month","npgo"))

glimpse(npgo)

#plot NPGO index from 1950 to 2022

ggplot(npgo %>%
         group_by(year) %>%
         summarise(npgo = mean(npgo)),
                   aes(x = year, y = npgo)) +
  geom_line(color = "slategray", size = 1.2, alpha = 0.5) +
  scale_x_continuous(breaks = seq(1950, 2022, 10)) +
  labs(title = "NPGO index from 1950 to 2022",
       x = "Year",
       y = "NPGO index") +
  theme_minimal()

#average over all months of a year

npgo_avg <- npgo %>%
  group_by(year) %>%
  summarise(npgo = mean(npgo))

#plot NPGO index from 1950 to 2022, average an not averaged

ggplot(npgo_avg,
       aes(x = year, y = npgo)) +
  geom_line(color = "slategray", size = 1.2, alpha = 0.5) +
  geom_line(data = npgo,
             aes(x = year,y = npgo),
             color = "coral", alpha = 0.5, size = 1.2) +
  scale_x_continuous(breaks = seq(1950, 2022, 10)) +
  labs(title = "NPGO index from 1950 to 2022",
       x = "Year",
       y = "NPGO index") +
  theme_minimal()


#read chum csv
chum_data <- read.csv(here("data","chum_SR_20_hat_yr_w_coord.csv"))

#left_join npgo data with chum data

chum_data_w_npgo <- chum_data %>%
  mutate(year = BroodYear+1) %>% 
  left_join(npgo_avg %>% rename(npgo_new = npgo))

#plot npgo and npgo_new from 1950 to 2022 from chum_Data

ggplot(chum_data_w_npgo,
       aes(x = BroodYear)) +
  geom_line(aes(y = npgo , color = "old"), size = 1.2, alpha = 0.5) +
  geom_line(aes(y = npgo_new, color = "new"), size = 1.2, alpha = 0.5) +
  labs(title = "NPGO index from 1950 to 2022",
       x = "Year",
       y = "NPGO index")+
  scale_color_manual(name = "NPGO", values = c("old" = "coral", "new" = "slategray"))+
  theme_minimal()+
  theme(legend.position = "bottom")

#save dataset

write.csv(chum_data_w_npgo, here("data","chum_SR_20_hat_yr_w_npgo.csv"))



