# look at correlations between forestry metrics - eca, cpd, sqrt eca, sqrt cpd
# within each CU

#libraries

library(ggplot2)
library(tidyverse)
library(here)
library(GGally)

ch20r <- read.csv(here("data","chum_SR_20_hat_yr_reduced_VRI90.csv"))
ch20r %>% 
  filter(WATERSHED_CDE == '950-169400-00000-00000-0000-0000-000-000-000-000-000-000')

ch20r %>% 
  filter(WATERSHED_CDE == '915-486500-05300-00000-0000-0000-000-000-000-000-000-000')

ch20r %>% 
  filter(River == 'SALMON RIVER')

#two rivers with duplicated names:
ch20r$River=ifelse(ch20r$WATERSHED_CDE=='950-169400-00000-00000-0000-0000-000-000-000-000-000-000','SALMON RIVER 2',ch20r$River)
ch20r$River=ifelse(ch20r$WATERSHED_CDE=="915-486500-05300-00000-0000-0000-000-000-000-000-000-000",'LAGOON CREEK 2',ch20r$River)

ch20r=ch20r[order(factor(ch20r$River),ch20r$BroodYear),]
rownames(ch20r)=seq(1:nrow(ch20r))

glimpse(ch20r)

#look at the CU with the least number of rivers

ch20r %>% 
  group_by(CU) %>% 
  summarise(n = n_distinct(River)) %>% 
  arrange(n) %>% 
  head(15)

#make list of unique CU
cu_list <- ch20r %>% 
  group_by(CU) %>% 
  summarise(n = n_distinct(River)) %>% 
  arrange(n) %>% 
  filter(n>1, n <10) %>% 
  select(CU) %>%
  unique()

#within each CU, plot pairwise correlation of ECA_age_proxy_forested_only in each River

#make wide dataframe so that each column is a ECA of a river from CU = 'CM-13'

df_cm_13 <- ch20r %>% 
  filter(CU == 'CM-13') %>% 
  select(River, ECA_age_proxy_forested_only, BroodYear) %>% 
  pivot_wider(names_from = River, values_from = ECA_age_proxy_forested_only)


#plot pairwise correlation of ECA_age_proxy_forested_only in each River
#title is the CU, alpha of points is .5
ggpairs(df_cm_13, columns = 2:4, aes(alpha = 0.5))+
  
  ggtitle("CM-13")+
  theme_classic()
  
#loop through all CU

for (i in 1:nrow(cu_list)){
  
  df <- ch20r %>% 
    filter(CU == cu_list$CU[i]) %>% 
    select(River, ECA_age_proxy_forested_only, BroodYear) %>% 
    pivot_wider(names_from = River, values_from = ECA_age_proxy_forested_only)
    
  
  p <- ggpairs(df, columns = 2:dim(df)[2], aes(alpha = 0.5))+
    
    ggtitle(cu_list$CU[i])+
    theme_classic()
  ggplot2::ggsave(here("figures",paste0(cu_list$CU[i],".png")),p, width = 16, height = 12)
}
panel.ts <- function(data, mapping, ...){
  # y_var <- as_label(mapping$y)
  ggplot(data = data, mapping) + 
    geom_line(color = "coral") +
    theme_classic()
}
for (i in 1:nrow(cu_list)){
  
  df_cpd <- ch20r %>% 
    filter(CU == cu_list$CU[i]) %>% 
    select(River, disturbedarea_prct_cs, BroodYear) %>% 
    pivot_wider(names_from = River, values_from = disturbedarea_prct_cs)
  
  #customize pairs plot with diagonals as time series plots instead of density plots
  
  
  
  
  p <- ggpairs(df_cpd, columns = 2:dim(df_cpd)[2], 
               diag = list(continuous = wrap(panel.ts, mapping = aes(x = BroodYear))),
               aes(alpha = 0.5))+
    
    ggtitle(cu_list$CU[i])+
    theme_classic()
  ggplot2::ggsave(here("figures",paste0(cu_list$CU[i],"_cpd.png")),p, width = 16, height = 12)
}


#customizing pairs plot

#Panel of correlations
panel.corr <- function(x, y){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- round(cor(x, y), digits=3)
  txt <- paste0("Corr: ", r)
  text(0.5, 0.5, txt, cex = 1)
}

#Panel of histograms
panel.hist <- function(x, ...){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks
  len <- length(breaks)
  y <- h$counts/max(h$counts)
  rect(breaks[-len], 0, breaks[-1], y, col = "lightblue")
}

#panel of time series

panel.ts <- function(x,BroodYear){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  plot(BroodYear,x, type = "l", col = "coral")
}



#Panel of scatterplots
panel.scat <- function(x, y){
  points(x,y, pch = 19, cex = 1, col = "coral")
}

#Plot
pairs(df_cpd[, columns = 2:dim(df_cpd)[2]],
      lower.panel = panel.scat,
      upper.panel = panel.corr,
      diag.panel = panel.ts,
      labels = c("Miles","Displacement","Horsepower",
                 "Rear axle ratio","Weight","1/4 mile time"),
      gap = 0.3, 
      main = "Scatterplot matrix of `mtcars`")


