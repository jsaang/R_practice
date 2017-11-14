library(tidyverse)
library(Lahman)
library(stats4)

# Simple Simulation

num_trials <- 10e6

simulations <- data_frame(
  true_average = rbeta(num_trials, 81, 219),
  hits = rbinom(num_trials, 300, true_average)
)

hit_100 <- simulations %>%
  filter(hits == 100)

simulations %>%
  filter(hits %in% c(60, 80, 100)) %>%
  ggplot(aes(true_average, color = factor(hits))) +
  geom_density()

## Play with baseball data ----------------------------------

# Making dataset
career <- Batting %>% 
  filter(AB > 0) %>% 
  anti_join(Pitching, by = 'playerID') %>% # filtered out pitchers
  group_by(playerID) %>%
  summarise(H = sum(H), AB = sum(AB)) %>%
  mutate(average = H/AB) # calculate average hit ratio

# Add player's name
career <- Master %>%
  tbl_df() %>%
  dplyr::select(playerID, nameFirst, nameLast) %>% 
  unite(name, nameFirst, nameLast, sep = " ") %>% 
  inner_join(career, by = 'playerID') %>% 
  dplyr::select(-playerID)

career_filtered <- career %>%
  filter(AB > 500)

# Estimate coef of prior dist
ll <- function(alpha, beta) {
  x <- career_filtered$H
  total <- career_filtered$AB
  -sum(VGAM::dbetabinom.ab(x, total, alpha, beta, log = TRUE))
}

# By Maximum likelihood method
m <- mle(ll, start = list(alpha = 1, beta = 10), method = 'L-BFGS-B',
         lower = c(0.0001, 0.1))

ab <- coef(m)

# Coef of prior dist
alpha0 <- ab[1]
beta0 <- ab[2]

# Update each player's prior dist
career_eb <- career %>%
  mutate(eb_estimate = (H + alpha0)/(AB + alpha0 + beta0))

career_eb <- career_eb %>%
  mutate(alpha1 = alpha0 + H, beta1 = beta0 + AB - H)

# Filter some favorite players
yankee_1998_career <- career_eb %>%
  filter(name %in% c('Scott Brosius','Derek Jeter','Chuck Knoblauch',
                     'Tino Martinez','Jorge Posada',
                     'Darryl Strawberry','Bernie Williams'))

# Caculate credible intervals
yankee_1998_career <- yankee_1998_career %>%
  mutate(low = qbeta(0.025, alpha1, beta1),
         high = qbeta(0.975, alpha1, beta1))

# Visualization
yankee_1998_career %>%
  mutate(name = reorder(name, eb_estimate)) %>%
  ggplot(aes(eb_estimate, name)) +
  geom_point() +
  geom_errorbarh(aes(xmin = low, xmax = high)) +
  geom_vline(xintercept = alpha0 / (alpha0 + beta0),
             color = 'red', lty = 2) +
  xlab('Estimated batting average (w/ 95% interval)') +
  ylab('Player')

