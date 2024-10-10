# read  NPGO index data from https://www.o3d.org/npgo/data/NPGO.txt


# Load necessary library
library(ggplot2)
library(tidyverse)
library(here)

# data

npgo <- read.table("https://www.o3d.org/npgo/data/NPGO.txt", header = FALSE, col.names = c("year", "month","npgo"))

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
