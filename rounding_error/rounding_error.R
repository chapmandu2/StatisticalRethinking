library(rethinking)
library(rjags)
library(tidybayes)
library(ggplot2)
library(dplyr)

# create a simple dataset

a <- 3
b <- 0.5

dat <- tibble(x = runif(300, 0, 10), error = rnorm(300, 0, 2)) %>%
    dplyr::mutate(y = a + x * b + error,
                  y_rounded = round(y, 0))

#make dataset for rjags
dlist <- list(
    x = dat$x,
    y = dat$y,
    y_rounded = dat$y_rounded,
    row_idx = 1:nrow(dat)
)


ggplot(dat, aes(x,y)) + geom_point()
ggplot(dat, aes(x,y_rounded)) + geom_point()

lm(y ~ x, dat) %>% summary()
lm(y_rounded ~ x, dat) %>% summary()

# jags
# define the model
m1 <- 'model {
for (i in row_idx){
y[i] ~ dnorm(mu[i], sigma ^ (-2))
mu[i] <- a + b * x[i]
}
a ~ dnorm(0, 100 ^ (-2))
b ~ dnorm(0, 100 ^ (-2))
sigma ~ dunif(0, 50)
}'

#compile and fit the model
m1_jags <- jags.model(
    textConnection(m1),
    data = dlist[c('x', 'y', 'row_idx')],
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
    gather_draws(a, b, sigma) %>%
    mutate(model = 'm1_jags')

#plot samples
ggplot(plot_samples_m1, aes(x = .variable, y = .value)) +
    geom_eye() + coord_flip() +  theme_bw()

#summarise samples
plot_samples_m1 %>%
    group_by(model, .variable) %>%
    median_qi(.value)

# verify against original
lm(y ~ x, data = dat)

# using rounded values
# define the model
m2 <- 'model {
for (i in row_idx){
y_rounded[i] ~ dnorm(mu[i], sigma ^ (-2))
mu[i] <- a + b * x[i]
}
a ~ dnorm(0, 100 ^ (-2))
b ~ dnorm(0, 100 ^ (-2))
sigma ~ dunif(0, 50)
}'

#compile and fit the model
m2_jags <- jags.model(
    textConnection(m2),
    data = dlist[c('x', 'y_rounded', 'row_idx')],
    n.chains = 4,
    n.adapt = 1000
)

update(m2_jags, 5000)

#extract samples
m2_samples <- coda.samples(m2_jags,
                           c('a', 'b', 'sigma'),
                           1000)

#reformat samples
plot_samples_m2 <- m2_samples %>%
    gather_draws(a, b, sigma) %>%
    mutate(model = 'm2_jags')

#plot samples
ggplot(plot_samples_m2, aes(x = .variable, y = .value)) +
    geom_eye() + coord_flip() +  theme_bw()

#summarise samples
plot_samples_m2 %>%
    group_by(model, .variable) %>%
    median_qi(.value)

# verify against original
lm(y_rounded ~ x, data = dat)
