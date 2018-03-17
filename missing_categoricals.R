library(rethinking)
data(WaffleDivorce)
d <- WaffleDivorce

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

#do
d$prob_south <- sapply(d$South, FUN=make_south_var, probs=p_south_options, ratios=p_south_ratios)
# NB should vectorise properly for bigger datasets

#check again
library(ggplot2)
ggplot(d, aes(x=as.factor(South), y=Divorce)) + geom_point()
ggplot(d, aes(x=South, y=Divorce)) + geom_point() + geom_smooth(method='lm')
ggplot(d, aes(x=prob_south, y=Divorce)) + geom_point() + geom_smooth(method='lm')

# next need to use the prob_south field as the parameter for a binomial distribution 
# with size 1 and prob = prob_south  ie dbinom(1, p)

## based on R code 14.5

# define data
dlist <- list(
    div_obs=d$Divorce,
    div_sd=d$Divorce.SE,
    south_obs=d$prob_south,
    south_start=round(d$prob_south,0),
    south_actual=d$South,
    state=1:nrow(d))

m14.2 <- map2stan(
    alist(
        div_est ~ dnorm(mu, sigma),
        mu <- a + bR * south_est[i],
        div_obs ~ dnorm(div_est, div_sd),
        south_est ~ dbinom(1, south_obs),  #problem is that dbinom returns an int and there aren't int vectors in stan
        a ~ dnorm(0,10),
        bR ~ dnorm(0,10),
        sigma ~ dcauchy(0,2.5)
    ) ,
   data=dlist ,
    start=list(div_est=dlist$div_obs, south_est=dlist$south_actual),
   types=list(south_est="int"),
    WAIC=FALSE , iter=300 , warmup=100 , chains=1 , cores=1 ,
    control=list(adapt_delta=0.95) )
