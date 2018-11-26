####################################################################
# Julia Wrobel
# November 26, 2018
#
# R code used in P8157 Special Topics lecture on Bayesian Regression
####################################################################

##### load libraries
library(tidyverse)
library(viridis)
library(gridExtra)
library(rstanarm)
library(broom)

# load pigs data
pigs_wide = read.table("bayesian_lecture/pigs.txt") 

####################################################################
# one sample normal-normal
####################################################################

set.seed(232324)
sigma_y = (500)^2 # treat population variance as known
mu = 10000
n = 10 # number of students in the random sample
steps = round(rnorm(n, mu, sd = sqrt(sigma_y)))

# look at the data 
range(steps)
head(steps)
mean(steps)
sd(steps)

# frequentist estimate of mu
ybar = mean(steps)
ci_minus = ybar - 1.96 * sd(steps) / sqrt(n)
ci_plus = ybar + 1.96 * sd(steps) / sqrt(n)

sample_hist = data.frame(steps = steps) %>%
  ggplot(aes(steps)) + geom_histogram(bins = 5, fill = "grey66") +
  geom_vline(xintercept = mean(steps), color = "blue", size = 2, linetype = 3) +
  geom_vline(xintercept = ci_minus, color = "indianred", size = 2) +
  geom_vline(xintercept = ci_plus, color = "indianred", size = 2)

sample_hist

#ggsave(filename = "bayesian_lecture/Figs/sample_hist.png", sample_hist)

### bayesian posterior mean

# informative prior
mu_prior = 4000
sigma2_prior = (200)^2
mu_post = sigma2_prior / (sigma_y/n + sigma2_prior) * ybar + sigma_y/n / (sigma_y/n + sigma2_prior) * mu_prior
mu_post

sigma2_post = sigma_y/n * sigma2_prior / (sigma_y/n + sigma2_prior)
sqrt(sigma2_post)

# plot the results
densities_df = tibble(
  steps = seq(2000, 13000, length.out = 1000),
  prior = dnorm(steps, mu_prior, sd = sqrt(sigma2_prior)),
  likelihood = dnorm(steps, ybar, sd = sqrt(sigma_y)),
  posterior = dnorm(steps, mu_post, sd = sqrt(sigma2_post))
)

densities_df %>%
  ggplot(aes(steps, likelihood)) + geom_line(size = 1) +
  geom_line(aes(y = prior), color = "green", size = 1) +
  geom_line(aes(y = posterior), color = "blue", size = 1) 

#ggsave(filename = "bayesian_lecture/Figs/informative_prior.png", width = 5, height = 5)

# weakly informative prior
mu_prior = 4000
sigma2_prior = (800)^2
mu_post = sigma2_prior / (sigma_y/n + sigma2_prior) * ybar + sigma_y/n / (sigma_y/n + sigma2_prior) * mu_prior
mu_post

sigma2_post = sigma_y/n * sigma2_prior / (sigma_y/n + sigma2_prior)
sqrt(sigma2_post)

densities_df = tibble(
  steps = seq(2000, 13000, length.out = 1000),
  prior = dnorm(steps, mu_prior, sd = sqrt(sigma2_prior)),
  likelihood = dnorm(steps, ybar, sd = sqrt(sigma_y)),
  posterior = dnorm(steps, mu_post, sd = sqrt(sigma2_post))
)

densities_df %>%
  ggplot(aes(steps, likelihood)) + geom_line(size = 1) +
  geom_line(aes(y = prior), color = "green", size = 1) +
  geom_line(aes(y = posterior), color = "blue", size = 1) 
  
#ggsave(filename = "bayesian_lecture/Figs/weak_prior.png", width = 5, height = 5)

# diffuse prior
mu_prior = 0
sigma2_prior = (4000)^2
mu_post = sigma2_prior / (sigma_y/n + sigma2_prior) * ybar + sigma_y/n / (sigma_y/n + sigma2_prior) * mu_prior
mu_post

sigma2_post = sigma_y/n * sigma2_prior / (sigma_y/n + sigma2_prior)
sigma_post = sqrt(sigma2_post)

densities_df = tibble(
  steps = seq(-5000, 12000, length.out = 1000),
  prior = dnorm(steps, mu_prior, sd = sqrt(sigma2_prior)),
  likelihood = dnorm(steps, ybar, sd = sqrt(sigma_y)),
  posterior = dnorm(steps, mu_post, sd = sqrt(sigma2_post))
)

densities_df %>%
  ggplot(aes(steps, likelihood)) + geom_line(size = 1) +
  geom_line(aes(y = prior), color = "green", size = 1) +
  geom_line(aes(y = posterior), color = "blue", size = 1) 

#ggsave(filename = "bayesian_lecture/Figs/uninformative_prior.png", width = 5, height = 5)

# posterior 95% interval
qnorm(c(0.025, 0.927), mean = mu_post, sd = sigma_post)

# posterior probability that mu < 10000
pnorm(10000, mu_post, sigma_post)

####################################################################
# Bayesian linear regression
####################################################################

## simulate the data
set.seed(677799)
age = rnorm(N, 25, 4)
dog_owner = rbinom(N, 1, 0.25) # 0 = not a dog owner, 1 = dog owner
subway_distance = runif(N, 0, 1.25) # distance (in miles) from the subway stop closest to home
statistician = rbinom(N, 1, 0.5)  # 0 = bayesian, 1 = frequentist

sigma_y = 500
error = rnorm(N, 0, sigma_y)
beta0 = 6000
beta1 = 1300
beta2 = 1800
beta3 = -500
beta4 = -50
steps = beta0 + beta1 * dog_owner + beta2 * subway_distance + beta3 * statistician + 
  beta4 * age + error

steps_df = data.frame(steps = steps, dog_owner = dog_owner, 
                      subway_distance = subway_distance, statistician = statistician,
                      age = age)

hist(steps)

## do some EDA
steps_plot = steps_df %>%
  mutate(dog_owner = ifelse(dog_owner == 1, "yes", "no")) %>%
  ggplot(aes(subway_distance, steps, color = dog_owner)) +
  geom_point() + theme(legend.position = "bottom")

statistician = steps_df %>%
  mutate(statistician = ifelse(statistician == 0, "bayesian", "frequentist")) %>%
  ggplot(aes(y = steps, group = statistician, fill = statistician)) + 
  geom_boxplot() + theme(legend.position = "bottom") +
  scale_fill_viridis(discrete = TRUE)

grid.arrange(steps_plot, statistician, ncol = 2)

#p = arrangeGrob(steps_plot, statistician, ncol = 2)
#ggsave(filename = "bayesian_lecture/Figs/blr.png", width = 10, height = 5, p)

##### define model specifications

# assumes likelihood is normal unless family is specified
# don't have to specify your priors - will choose normal for you
blr_mod = stan_glm(steps ~ subway_distance + 
                     age + 
                     dog_owner + 
                     statistician, 
               data = steps_df,
               prior_intercept = normal(0, 10),
               prior = normal(0, 10),
               prior_aux = exponential(rate = 1))

# extract regression coefficients
coefs_blr = tidy(blr_mod) %>% 
  rename(coef_blr = estimate, sd_blr = std.error) %>%
  select(term, coef_blr)


##### compare with least squares
mod_lm = lm(steps ~ subway_distance + age + dog_owner + statistician, data = steps_df)

# extract regression coefficients from least squares
coefs_lm = tidy(mod_lm) %>% 
  rename(coef_lm = estimate, sd_lm = std.error) %>%
  select(term, coef_lm)

coefs = left_join(coefs_blr, coefs_lm)

# add in true values from the simulation
coefs = coefs %>% mutate(true_value = c(6000, 1800, -50, 1300, -500))

# table of regression coefficients
knitr::kable(coefs, digits = 1)

####################################################################
# Bayesian random slope model
####################################################################

colnames(pigs_wide) = paste0("week", 1:9)
pigs_wide = pigs_wide %>% mutate(id = row_number())

pigs_long = pigs_wide %>%
  gather(week, weight, starts_with("week")) %>%
  mutate(week = as.numeric(str_replace(week, "week", "")))

## plot the data
ggplot(pigs_long, aes(week, weight, group = id)) + 
  geom_line(alpha = 0.5)

#ggsave(filename = "bayesian_lecture/Figs/pig_weights.png", width = 10, height = 6)

## specify model
bayesian_lmer = stan_lmer(weight ~ week + 
                            (1 + week|id), 
                          data = pigs_long)

summary(bayesian_lmer)

# get summary of priors used
prior_summary(bayesian_lmer)


# get fitted values
pigs_long$fitted_values = bayesian_lmer$fitted.values

pigs_long %>%
  ggplot(aes(week, weight, group = id)) + 
  geom_line(color = "gray", alpha = 0.95) +
  geom_line(aes(y = fitted_values), color = "blue", alpha = 0.5)

#ggsave(filename = "bayesian_lecture/Figs/fitted.png", width = 10, height = 6)
