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
    choice <- sum(t(rmultinom(1, 1, p_south_ratios))*p_south_options)
    ifelse(x==1, choice, 1-choice)
}

#check
z <- sapply(rep(0,1000), FUN=make_south_var, probs=p_south_options, ratios=p_south_ratios)
table(z)


# generate data and append to data frame
d$prob_south <- sapply(d$South, FUN=make_south_var, probs=p_south_options, ratios=p_south_ratios)
# NB should vectorise properly for bigger datasets
head(d[,c('Location', 'Divorce', 'South', 'prob_south')])

#make dataset for rjags
dlist <- list(
div_obs=d$Divorce,
div_sd=d$Divorce.SE,
south_obs=d$prob_south,
south_start=round(d$prob_south,0),
south_actual=d$South,
state=1:nrow(d))

#visualise
ggplot(d, aes(x=as.factor(South), y=Divorce)) + geom_boxplot() +
geom_point(position = position_jitter(width=0.1), shape=21) + theme_bw()
ggplot(d, aes(x=South, y=Divorce)) +
geom_point(position = position_jitter(width=0.02), shape=21) + theme_bw() +
geom_smooth(method='lm')
ggplot(d, aes(x=prob_south, y=Divorce)) +
geom_point(position = position_jitter(width=0.02), shape=21) + theme_bw() +
geom_smooth(method='lm')

#now try to do the straightforward linear regression in JAGs
m1 <- 'model {
for (i in state){
    div_obs[i] ~ dnorm(y.hat[i], sigma ^ (-2))
    y.hat[i] <- a + b * south_actual[i]
}
a ~ dnorm(0, 0.0001)
b ~ dnorm(0, 0.0001)
sigma ~ dunif(0, 50)
}'

m1_jags <- jags.model(textConnection(m1),
data = dlist[c('div_obs', 'south_actual')],
n.chains = 4,
n.adapt = 1000)

update(m1_jags, 5000)

jags.samples(m1_jags,
c('a', 'b', 'sigma'),
1000)

m1_samples <- coda.samples(m1_jags,
c('a', 'b', 'sigma'),
1000)

plot_samples_m1 <- m1_samples %>%
gather_samples(a, b, sigma) %>%
mutate(model='m1_jags')

ggplot(plot_samples_m1, aes(x=term, y=estimate)) +
geom_eye() + coord_flip() +  theme_bw()

lm(Divorce ~ South, data=d)

#now try linear regression with probabilities...
m2 <- 'model {
for (i in state){
    div_obs[i] ~ dnorm(mu[i], sigma ^ (-2))
    mu[i] <- a + b * south_est[i]
    south_est[i] ~ dbinom(south_obs[i],1)
}
a ~ dnorm(0, 0.0001)
b ~ dnorm(0, 0.0001)
sigma ~ dunif(0, 50)
}'

m2_jags <- jags.model(textConnection(m2),
data = dlist[c('div_obs', 'south_obs',  'state')],
n.chains = 4,
n.adapt = 1000)

update(m2_jags, 5000)

m2_samples <- coda.samples(m2_jags,
c('a', 'b', 'sigma'),
1000)

plot_samples_m2 <- m2_samples %>%
gather_samples(a, b, sigma) %>%
mutate(model='m2_jags')

ggplot(plot_samples_m2, aes(x=term, y=estimate)) +
geom_eye() + coord_flip() +  theme_bw()

#combined
bind_rows(plot_samples_m1, plot_samples_m2) %>%
ggplot(aes(x=model, y=estimate)) +
geom_eye() + coord_flip() + facet_wrap(~term, scales = 'free', ncol=1) + theme_bw()


lm(Divorce ~ South, data=d)

