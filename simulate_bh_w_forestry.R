# Goal - simulate data for one population of salmon with Beverton
# Holt model, with forestry effects.

# Load necessary library
library(ggplot2)
library(tidyverse)
library(here)


#getting some values from empirical data

# load datasets####

ch20r <- read.csv(here("data","chum_SR_20_hat_yr_reduced_VRI90.csv"))

bh_chm_cpd <- read.csv(here("data","bh_chm_cpd.csv"))

alpha_t <- bh_chm_cpd %>% 
  filter(str_detect(variable, "alpha_t")) %>%
  select(mean) %>% 
  summarise(alpha_t = mean(mean))

Rk <- bh_chm_cpd %>% 
  filter(str_detect(variable, "Rk")) %>%
  select(mean) %>% 
  summarise(Rk = mean(mean))

b <- bh_chm_cpd %>% 
  filter(str_detect(variable, "b_for_rv")) %>%
  select(mean,sd) %>% 
  summarise(b = min(mean), sd = max(sd))


## data formatting ####

#two rivers with duplicated names:
ch20r$River=ifelse(ch20r$WATERSHED_CDE=='950-169400-00000-00000-0000-0000-000-000-000-000-000-000','SALMON RIVER 2',ch20r$River)
ch20r$River=ifelse(ch20r$WATERSHED_CDE=="915-486500-05300-00000-0000-0000-000-000-000-000-000-000",'LAGOON CREEK 2',ch20r$River)


ch20r=ch20r[order(factor(ch20r$River),ch20r$BroodYear),]
rownames(ch20r)=seq(1:nrow(ch20r))

#normalize ECA 2 - square root transformation (ie. sqrt(x))
ch20r$sqrt.ECA=sqrt(ch20r$ECA_age_proxy_forested_only)
ch20r$sqrt.ECA.std=(ch20r$sqrt.ECA-mean(ch20r$sqrt.ECA))/sd(ch20r$sqrt.ECA)

#normalize CPD 2 - square root transformation (ie. sqrt(x))
ch20r$sqrt.CPD=sqrt(ch20r$disturbedarea_prct_cs)
ch20r$sqrt.CPD.std=(ch20r$sqrt.CPD-mean(ch20r$sqrt.CPD))/sd(ch20r$sqrt.CPD)

##average ECA by stock
#just to see an overview of ECA by river
eca_s=ch20r%>%group_by(River)%>%summarize(m=mean(ECA_age_proxy_forested_only*100),range=max(ECA_age_proxy_forested_only*100)-min(ECA_age_proxy_forested_only*100),cu=unique(CU))

#extract max S for priors on capacity & eq. recruitment
smax_prior=ch20r%>%group_by(River) %>%summarize(m.s=max(Spawners),m.r=max(Recruits))


#cus by stock
cu=distinct(ch20r,River,.keep_all = T)
cu.nrv=summary(factor(cu$CU))

#select eca values from river that has max eca in the whole dataset

forest_loss_river = ch20r %>%
  filter(sqrt.ECA.std == max(sqrt.ECA.std)) %>%
  select(River,sqrt.ECA.std) %>%
  summarise(River = first(River))
  
eca_max = ch20r %>%
  filter(River == forest_loss_river$River) %>%
  select(sqrt.ECA.std) %>%
  #select first 50 values
  head(50)

N0 <- ch20r %>% 
  select(Spawners) %>%
  summarise(N0 = mean(Spawners))

e_t <- bh_chm_cpd %>% 
  filter(str_detect(variable, "e_t")) %>%
  select(mean,sd) %>% 
  summarise(e_t = mean(mean), sd = max(sd))


# Beverton-Holt model function
beverton_holt <- function(N0, alpha, Rk, BroodYear, w_sd, b, forest_loss) {
  N <- numeric(BroodYear)  # Vector to store population sizes
  N[1] <- N0                # Initial population size
  
  for (t in 1:(BroodYear - 1)) {
    # N[t + 1] <- (alpha * N[t]) / (1 + (N[t] / Rk))* exp(rnorm(1, mean = 0, sd = w_sd))  # Beverton-Holt equation
    ln_RS <- alpha  - log(1 + (exp(alpha)/Rk)*N[t]) + b*forest_loss[t] + rnorm(1, mean = 0, sd = w_sd)
    N[t + 1] <- exp(ln_RS) * N[t]
  }
  
  return(N)
}

# Parameters
N0 <- ch20r %>% 
  select(Spawners) %>%
  summarise(N0 = mean(Spawners))        # Initial population size

alpha <- alpha_t$alpha_t        # Intrinsic growth rate

Rk <- bh_chm_cpd %>% 
  filter(str_detect(variable, "Rk")) %>%
  select(mean) %>% 
  summarise(Rk = mean(mean))   # Carrying capacity

BroodYear <- 50 # Number of time steps

w_sd <- e_t$sd  # Standard deviation of the noise

b <- bh_chm_cpd %>% 
  filter(str_detect(variable, "b_for_rv")) %>%
  select(mean,sd) %>% 
  summarise(b = min(mean), sd = max(sd))

forest_loss <- eca_max$sqrt.ECA.std

# Simulate the population dynamics
population <- beverton_holt(N0$N0, alpha, Rk$Rk, BroodYear, w_sd, b$b, forest_loss)

# Create a data frame for plotting
df <- data.frame(
  Time = 1:BroodYear,
  Population = population
)

# Plot the results
ggplot(df, aes(x = Time, y = Population)) +
  geom_line(color = "blue", size = 1) +
  theme_minimal() +
  labs(title = "Beverton-Holt Population Dynamics", 
       x = "Time", y = "Population Size")

#plot (log (N(t+1)/N(t))) vs N(t)

data_frame <- data.frame(
  N = population[-BroodYear],
  N1 = population[-1]
)

data_frame <- data_frame %>%
  mutate(log_ratio = log(N1/N))

#add line for beverton holt model

ggplot(data_frame, aes(x = N1, y = N)) +
  geom_point(color = "blue") +
  geom_line(aes(x = N, y = (alpha*N ) / (1 + (N / Rk)), color = "red")) +
  theme_minimal() +
  labs(title = "Beverton-Holt Population Dynamics", 
       x = "Spawners", y = "Recruits")

