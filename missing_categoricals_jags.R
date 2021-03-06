#doing linear regression with uncertainty in cataegoricals

library(rethinking)
library(rjags)
library(tidybayes)
library(ggplot2)
library(dplyr)

#get Wafflehouse Divorce data
data(WaffleDivorce)
d <- WaffleDivorce
head(d)

#create dataset where there is uncertainty in whether the state is in the South or not
# Assign .95, .8, .5, .2, .05 in a 60:20:10:5:5 ratio
# use multinomial

p_south_options <- c(0.95, 0.8, 0.5, 0.2, 0.05)
p_south_ratios <- c(.6, .2, .1, .05, .05)

make_south_var <- function(x, probs, ratios) {
    choice <- sum(t(rmultinom(1, 1, p_south_ratios)) * p_south_options)
    ifelse(x == 1, choice, 1 - choice)
}

#check
z <-
    sapply(rep(0, 1000),
           FUN = make_south_var,
           probs = p_south_options,
           ratios = p_south_ratios)
table(z)


# generate data and append to data frame
d$prob_south <-
    sapply(d$South,
           FUN = make_south_var,
           probs = p_south_options,
           ratios = p_south_ratios)
# NB should vectorise properly for bigger datasets
head(d[, c('Location', 'Divorce', 'South', 'prob_south')])

#visualise
ggplot(d, aes(x = MedianAgeMarriage, y = Divorce)) + 
    geom_point(shape = 21) + geom_smooth(method='lm') + theme_bw()
ggplot(d, aes(x = as.factor(South), y = MedianAgeMarriage)) + geom_boxplot() +
    geom_point(position = position_jitter(width = 0.1), shape = 21) + theme_bw()
ggplot(d, aes(x = as.factor(South), y = Divorce)) + geom_boxplot() +
    geom_point(position = position_jitter(width = 0.1), shape = 21) + theme_bw()
ggplot(d, aes(x = South, y = Divorce)) +
    geom_point(position = position_jitter(width = 0.02), shape = 21) + theme_bw() +
    geom_smooth(method = 'lm')

# verify against original
lm(Divorce ~ South, data = d)

# look at prob_south
ggplot(d, aes(x = prob_south, y = Divorce)) +
    geom_point(position = position_jitter(width = 0.02), shape = 21) + theme_bw() +
    geom_smooth(method = 'lm')

#make dataset for rjags
dlist <- list(
    div_obs = d$Divorce,
    div_sd = d$Divorce.SE,
    south_obs = d$prob_south,
    south_start = round(d$prob_south, 0),
    south_actual = d$South,
    state = 1:nrow(d)
)


# straightforward linear regression ---------------------------------------

# define the model
m1 <- 'model {
    for (i in state){
        div_obs[i] ~ dnorm(mu[i], sigma ^ (-2))
        mu[i] <- a + b * south_actual[i]
    }
    a ~ dnorm(0, 100 ^ (-2))
    b ~ dnorm(0, 100 ^ (-2))
    sigma ~ dunif(0, 50)
}'

#compile and fit the model
m1_jags <- jags.model(
    textConnection(m1),
    data = dlist[c('div_obs', 'south_actual', 'state')],
    n.chains = 4,
    n.adapt = 1000
)

update(m1_jags, 5000)

#extract samples
m1_samples <- coda.samples(m1_jags,
                           c('a', 'b', 'sigma'),
                           1000)

#reformat samples
plot_samples_m1 <- m1_samples %>%
    gather_samples(a, b, sigma) %>%
    mutate(model = 'm1_jags')

#plot samples
ggplot(plot_samples_m1, aes(x = term, y = estimate)) +
    geom_eye() + coord_flip() +  theme_bw()

#summarise samples
plot_samples_m1 %>%
    group_by(model, term) %>%
    median_qi(estimate)

# verify against original
lm(Divorce ~ South, data = d)


# linear regression with probabilities ------------------------------------

#define the model
m2 <- 'model {
    for (i in state){
        div_obs[i] ~ dnorm(mu[i], sigma ^ (-2))
        mu[i] <- a + b * south_est[i]
        south_est[i] ~ dbinom(south_obs[i],1)
    }
    a ~ dnorm(0, 100 ^ (-2))
    b ~ dnorm(0, 100 ^ (-2))
    sigma ~ dunif(0, 50)
}'

#compile and fit the model
m2_jags <- jags.model(
    textConnection(m2),
    data = dlist[c('div_obs', 'south_obs',  'state')],
    n.chains = 4,
    n.adapt = 1000
)

update(m2_jags, 5000)

#extract posterior samples
m2_samples <- coda.samples(m2_jags,
                           c('a', 'b', 'sigma'),
                           1000)

#reformat posterior samples
plot_samples_m2 <- m2_samples %>%
    gather_samples(a, b, sigma) %>%
    mutate(model = 'm2_jags')

#plot the posteriors
ggplot(plot_samples_m2, aes(x = term, y = estimate)) +
    geom_eye() + coord_flip() +  theme_bw()

#summarise samples
plot_samples_m2 %>%
    group_by(model, term) %>%
    median_qi(estimate)


#combined view of both models
bind_rows(plot_samples_m1, plot_samples_m2) %>%
    ggplot(aes(x = model, y = estimate)) +
    geom_eye() + coord_flip() +
    facet_wrap( ~ term, scales = 'free', ncol = 1) + theme_bw()


lm(Divorce ~ South, data = d)



# now use brms ------------------------------------------------------------

# how to fit a normal linear model in brms
library(brms)

#specify and run model
brms_mod <- brm(formula = Divorce  ~ South, data=d, 
                prior = c(set_prior("normal(0, 100)", class='Intercept'),
                          set_prior("normal(0, 100)", class = "b"),
                          set_prior("cauchy(0, 40)", class = "sigma")),
                chains = 4, iter = 2000, cores=4, silent = TRUE, refresh = 0 ) 

#view model output
brms_mod
broom::tidy(brms_mod)

#view original
lm(Divorce ~ South, data = d)

#get samples
plot_samples_brms <- brms_mod %>%
    gather_samples(b_Intercept, b_South, sigma) %>%
    ungroup() %>%
    mutate(term = case_when(term == 'b_Intercept' ~ 'a',
                            term == 'b_South' ~ 'b',
                            TRUE ~ 'sigma')) %>%
    mutate(model = 'm3_brms')
plot_samples_brms

#plot posteriors from all models
bind_rows(plot_samples_m1, plot_samples_m2, plot_samples_brms) %>%
    ggplot(aes(x=model, y=estimate)) + 
    geom_eye() + coord_flip() + facet_wrap(~term, scales = 'free') + theme_bw()

#also check out MCMC diagnostics
launch_shinystan(brms_mod)


