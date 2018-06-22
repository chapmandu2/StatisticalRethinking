library(dplyr)
library(ggplot2)
library(rjags)
library(tidybayes)


#truncated normal distribution parameter estimation

#say that we have two distributions this time 

set.seed(1238)
df <- data_frame(p1 = rbinom(1000, 1, 0.5),
              d = 50,
              data = rnorm(1000, mean = 500 + p1 * d , sd=80))

ggplot(df, aes(x=data, fill=as.factor(p1))) + 
  geom_histogram(bins=50) +
  facet_wrap(~as.factor(p1), ncol = 1)

#now the truncated data
trunc_df <- df %>% 
  filter(data < 600)

ggplot(trunc_df, aes(x=data, fill=as.factor(p1))) + 
  geom_histogram(bins=50) +
  facet_wrap(~as.factor(p1), ncol = 1)

#let's estimate d using conventional linear regression
lm(data ~ as.factor(p1), data=df)

#this won't work very well with the truncated data
lm(data ~ as.factor(p1), data=trunc_df)

#so let's try a bayesian approach
m3_jags_txt <- 'model {
for (i in 1:n) {
d[i] ~ dnorm(mu[i], tau)T(,600)
mu[i] <- a + b * p1[i]
}
a ~ dunif(100,700)
b ~ dnorm(0, 0.0001)
tau <- pow(sigma, -2)
sigma ~ dunif(0, 150)
}'


dlist <- list(d=trunc_df$data, p1=trunc_df$p1, n=nrow(trunc_df))

m3_jags <- jags.model(textConnection(m3_jags_txt),
                      data = dlist,
                      n.chains = 4,
                      n.adapt = 1000)

update(m3_jags, 5000)
samples_m3 <- coda.samples(m3_jags,
                           c('sigma', 'a', 'b'),
                           1000)
plot(samples_m3)

samples_m3 %>%
  as.mcmc.list() %>%
  gather_samples(a, b, sigma) %>%
  ggplot(aes(x=term, y=estimate)) + 
  geom_eye() + coord_flip() +  theme_bw() + ylim(-100, 600)


lm(data ~ as.factor(p1), data=df)
samples_m3 %>%
  as.mcmc.list() %>%
  gather_samples(a,b,sigma) %>%
  tidybayes::mean_qi()

#try to use brms instead
library(brms)

#normal linear regression on untruncated data
m1_brms <- brm(data ~ p1, data=df)
m1_brms
broom::tidy(m1_brms)

#from truncated data - see ?resp_trunc for details
m2_brms <- brm(data | trunc(ub=600) ~ p1, data=trunc_df)
m2_brms
broom::tidy(m2_brms)
shinystan::launch_shinystan(m2_brms)

m2_brms %>%
  gather_samples(b_Intercept, b_p1, sigma) %>%
  ggplot(aes(x=term, y=estimate)) + 
  geom_eye() + coord_flip() +  theme_bw() + 
  facet_wrap(~term, scales = 'free', ncol=1) 

#that was depressingly easy.