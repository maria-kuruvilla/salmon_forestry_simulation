# Goal - simulate data for one population of salmon with Beverton
# Holt model, without forestry effects.

# Load necessary library
library(ggplot2)
library(tidyverse)
library(here)

#real data

ch20r <- read.csv(here("data","chum_SR_20_hat_yr_reduced_VRI90.csv"))
#two rivers with duplicated names:
ch20r$River=ifelse(ch20r$WATERSHED_CDE=='950-169400-00000-00000-0000-0000-000-000-000-000-000-000','SALMON RIVER 2',ch20r$River)
ch20r$River=ifelse(ch20r$WATERSHED_CDE=="915-486500-05300-00000-0000-0000-000-000-000-000-000-000",'LAGOON CREEK 2',ch20r$River)

ch20r=ch20r[order(factor(ch20r$River),ch20r$BroodYear),]
rownames(ch20r)=seq(1:nrow(ch20r))

#bh function

beverton_holt <- function(N0, alpha, Rk, years, w_sd) {
  N <- numeric(years)  # Vector to store population sizes
  ln_RS <- numeric(years)  # Vector to store ln(R/S)
  N[1] <- N0                # Initial population size
  ln_RS[1] <- alpha - log(1 + (exp(alpha) / Rk) * N[1])  # Initial ln(R/S)
  
  for (t in 1:(years - 1)) {
    # N[t + 1] <- (alpha * N[t]) / (1 + (N[t] / Rk))* exp(rnorm(1, mean = 0, sd = w_sd))  # Beverton-Holt equation
    ln_RS[t+1] <- alpha  - log(1 + (exp(alpha)/Rk)*N[t]) + rnorm(1, mean = 0, sd = w_sd)
    N[t + 1] <- exp(ln_RS[t+1]) * N[t]
  }
  
  return(data.frame(N = N, ln_RS = ln_RS))
}


alpha <- rnorm(1,4,5)

#getting some values from empirical data

N0 <- ch20r %>% 
  select(Spawners, BroodYear) %>%
  filter(BroodYear < 1960) %>% 
  summarise(N0_mean = mean(Spawners), N0_sd = sd(Spawners))

Rk <- bh_chm_cpd %>% 
  filter(str_detect(variable, "Rk")) %>%
  select(mean) %>% 
  summarise(Rk_mean = mean(mean), Rk_sd = sd(mean))

w_sd <- 0.1
years <- 50
Rk_true <- rlnorm(1,log(Rk$Rk_mean), log(Rk$Rk_sd))


#generate population data
population <- beverton_holt(N0$N0_mean, 
                            alpha, 
                            Rk_true, 
                            years, 
                            w_sd)
#true parameter values
true_para_df <- data.frame(
  parameter = factor(c("alpha", "Rk", "sigma", "b")),
  value = c(alpha, Rk_true, w_sd, 0)
)

# Create a data frame for plotting
df <- data.frame(
  Time = 1:BroodYear,
  Population = population$N
)

# Plot the results
ggplot(df, aes(x = Time, y = Population)) +
  geom_line(color = "slategray", size = 1.5, alpha = 0.8) +
  theme_minimal() +
  labs(title = "Beverton-Holt Population Dynamics", 
       x = "Time", y = "Population Size") +
  theme_classic()

dl <- list(N = length(population$N), 
           R_S = population$ln_RS,
           S = population$N,
           forest_loss = rep(0, length(population$N)),
           pRk_mean = Rk$Rk_mean,
           pRk_sig = Rk$Rk_sd)

# load Stan model sets####
file_bh=file.path(here('stan_models',  'bh_sim.stan'))
mbh=cmdstanr::cmdstan_model(file_bh) #compile stan code to C++

options(mc.cores = 8)
#fit model to data
bh_wo_forestry <- mbh$sample(data=dl,
             chains = 6, 
             iter_warmup = 100,
             iter_sampling =500,
             refresh = 10,
             adapt_delta = 0.999,
             max_treedepth = 20)

posterior <- bh_wo_forestry$draws(variables = c("alpha", "Rk", "sigma", "b"), format = 'draws_matrix')

posterior <- as.data.frame(posterior)

#use model fit to predict population data

#####

# sr_years <- 50
# max_samples <- length(posterior$alpha)
# 
# 
# spwn <- exp(model_pars$lnS)
# spwn.quant <- apply(spwn, 2, 
#                     quantile, probs=c(0.05,0.5,0.95))[,1:(sr_years-4)]
# 
# rec <-exp(model_pars$lnR)
# rec.quant <- apply(rec, 2, quantile, probs=c(0.05,0.5,0.95))[,8:dim(model_pars$R)[2]]
# 
# brood_t <- as.data.frame(cbind(1:(sr_years-4),t(spwn.quant), t(rec.quant)))
# colnames(brood_t) <- c("BroodYear","S_lwr","S_med","S_upr","R_lwr","R_med","R_upr")
# 
# brood_t <- as.data.frame(brood_t)
# 
# # SR relationship
# spw <- seq(0,max(brood_t[,4]),length.out=100)
# SR_pred <- matrix(NA,100,max_samples)
# 
# for(i in 1:max_samples){
#   r <- sample(seq(1,max_samples),1,replace=T)
#   a <- model_pars$lnalpha[r]
#   b <- model_pars$beta[r]
#   SR_pred[,i] <- (exp(a)*spw*exp(-b*spw))
# }
# 
# SR_pred <- cbind(spw,t(apply(SR_pred,c(1),quantile,probs=c(0.05,0.5,0.95),na.rm=T)))
# colnames(SR_pred) <- c("Spawn", "Rec_lwr","Rec_med","Rec_upr")
# SR_pred <- as.data.frame(SR_pred)





#####




S_pred <- seq(0, max(population$N), length.out = 50)
R_S_pred <- numeric(length(S_pred))
R_S_pred[1] <- NA
for(t in 2:length(S_pred)) {
  R_S_pred[t] <- exp(median(posterior$alpha) - log(1 + (exp(median(posterior$alpha)) / median(posterior$Rk)) * S_pred[t-1]))*S_pred[t-1]
}
df <- df %>% 
  mutate(Recruits = c(Population[2:length(Population)], NA), S_pred = S_pred, R_S_pred = R_S_pred)

ggplot(df) +
  geom_point(aes(x = Population, y = Recruits),color = "slategray", size = 1.5, alpha = 0.8) +
  geom_line(aes(x = S_pred, y = R_S_pred), color = "red", size = 1.5, alpha = 0.8) +
  theme_minimal() +
  labs(title = "Beverton-Holt Population Dynamics", 
       x = "Time", y = "Population Size") +
  theme_classic()

post_df <- data.frame(
  parameter = factor(rep(c("alpha", "Rk", "sigma", "b"), each=dim(posterior)[1])),
  posterior = c(posterior$alpha, posterior$Rk, posterior$sigma, posterior$b)
)



ggplot(post_df, aes(x = posterior)) + 
  geom_density(color = "slategray", linewidth = 1.5, alpha = 0.5) +
  facet_wrap(~ parameter, scales="free") +
  geom_vline(data = true_para_df, aes(xintercept=value), color = "red") +
  theme_classic()+
  theme(strip.background = element_blank())

#check r version

sessionInfo()
