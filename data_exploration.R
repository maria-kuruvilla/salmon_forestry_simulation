# look at correlations between forestry metrics - eca, cpd, sqrt eca, sqrt cpd
# within each CU

#libraries

library(ggplot2)
library(tidyverse)
library(here)
library(GGally)

ch20r <- read.csv(here("data","chum_SR_20_hat_yr.csv"))
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
  head(20)

#make list of unique CU
cu_list <- ch20r %>% 
  group_by(CU) %>% 
  summarise(n = n_distinct(River)) %>% 
  arrange(n) %>% 
  filter(n>1,n<15) %>%
  # select(CU) %>%
  unique()

#within each CU, plot pairwise correlation of ECA_age_proxy_forested_only in each River

#make wide dataframe so that each column is a ECA of a river from CU = 'CM-13'

df_cm_13 <- ch20r %>% 
  filter(CU == 'CM-13') %>% 
  select(River, ECA_age_proxy_forested_only, BroodYear) %>% 
  pivot_wider(names_from = River, values_from = ECA_age_proxy_forested_only)

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
 
panel_two_ts <- function(data, mapping){
  x <- unique(unlist(lapply(mapping, all.vars)))[1]
  y <- unique(unlist(lapply(mapping, all.vars)))[2]
  # print(x)
  # print(y)
  new_data <- cbind(x = data[,x], y = data[,y], time=data$BroodYear)
  ggplot(new_data)+
    geom_line(aes(x = time, y = x), color = "#B89898", size = 1, alpha = 0.8)+
    geom_line(aes(x = time, y = y), color = "#97B1A6", size = 1, alpha =0.8)
}

#plot pairwise correlation of ECA_age_proxy_forested_only in each River
#title is the CU, alpha of points is .5
#x axis and y axis should only have 3 ticks
#diagonal should be time series plot
ggpairs(df_cm_13, columns = 2:4, 
        diag = list(continuous = wrap(panel.ts, data = df_cm_13)),
        lower = list(continuous = wrap(panel_two_ts, data = df_cm_13)),
        aes(alpha = 0.5))+
  ggtitle("CM-13")+
  # scale_x_continuous(n.breaks = 3)+
  # scale_y_continuous(n.breaks = 3)+
  theme_classic()
  
  #loop through all CU

for (i in 1:nrow(cu_list)){
  
  df <- ch20r %>% 
    filter(CU == cu_list$CU[i]) %>% 
    select(River, ECA_age_proxy_forested_only, BroodYear) %>% 
    pivot_wider(names_from = River, values_from = ECA_age_proxy_forested_only)
    
  
  p <- ggpairs(df, columns = 2:dim(df)[2],
               #use only first word of column names
               columnLabels = strsplit(colnames(df[2:dim(df)[2]]), " ") %>% sapply(`[`, 1),
               diag = list(continuous = wrap(panel.ts, data = df)),
               lower = list(continuous = wrap(panel_two_ts, data = df)),
               aes(alpha = 0.5))+
    scale_x_continuous(n.breaks = 3)+
    scale_y_continuous(n.breaks = 3)+
    theme_classic()+

    theme(strip.background = element_rect(fill = "#C9C5BA", color = "#C9C5BA"),,
          strip.text.x.top = element_text(size = 8, face = "bold"),
          strip.text.y.right = element_text(size = 6, face = "bold"))+
          # strip.text.y = element_text(size = 12, face = "bold"))+
    ggtitle(cu_list$CU[i])
  if(cu_list$n[i] > 10){
    ggplot2::ggsave(here("figures",paste0(cu_list$CU[i],"_eca.png")),p, width = 20, height = 18)
  }
  else{
    ggplot2::ggsave(here("figures",paste0(cu_list$CU[i],"_eca.png")),p, width = 16, height = 12)
  }
  # ggplot2::ggsave(here("figures",paste0(cu_list$CU[i],".png")),p, width = 16, height = 12)
}

for (i in 1:nrow(cu_list)){
  
  df_cpd <- ch20r %>% 
    filter(CU == cu_list$CU[i]) %>% 
    select(River, disturbedarea_prct_cs, BroodYear) %>% 
    pivot_wider(names_from = River, values_from = disturbedarea_prct_cs)
  
  #customize pairs plot with diagonals as time series plots instead of density plots
  
  
  
  
  p <- ggpairs(df_cpd, columns = 2:dim(df_cpd)[2],
               #use only first word of column names
               columnLabels = strsplit(colnames(df[2:dim(df_cpd)[2]]), " ") %>% sapply(`[`, 1),
               diag = list(continuous = wrap(panel.ts, data = df_cpd)),
               lower = list(continuous = wrap(panel_two_ts, data = df_cpd)),
               aes(alpha = 0.5))+
    scale_x_continuous(n.breaks = 3)+
    scale_y_continuous(n.breaks = 3)+
    theme_classic()+
    
    theme(strip.background = element_rect(fill = "#C9C5BA", color = "#C9C5BA"),,
          strip.text.x.top = element_text(size = 8, face = "bold"),
          strip.text.y.right = element_text(size = 6, face = "bold"))+
    # strip.text.y = element_text(size = 12, face = "bold"))+
    ggtitle(cu_list$CU[i])
  if(cu_list$n[i] > 10){
    ggplot2::ggsave(here("figures",paste0(cu_list$CU[i],"_cpd.png")),p, width = 20, height = 18)
  }
  else{
    ggplot2::ggsave(here("figures",paste0(cu_list$CU[i],"_cpd.png")),p, width = 16, height = 12)
  }
  # ggplot2::ggsave(here("figures",paste0(cu_list$CU[i],".png")),p, width = 16, height = 12)
}



