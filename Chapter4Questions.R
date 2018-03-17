
library(rethinking)
#Q4M1
mu <- rnorm(10000, 0,10)
sigma <- runif(10000, 0, 10)
dens(mu)
y <- rnorm(10000, mu, sigma)
dens(y)

m4m1_formula <- alist(
    y ~ dnorm(mu, sigma),
    mu ~ dnorm(0,10),
    sigma ~ dunif(0,10)
) 

# Q4H1 --------------------------------------------------------------------
# First need a model to predict unknown heights from
# load data again, since it's a long way back
library(rethinking)
data(Howell1)
d <- Howell1
d2 <- d[ d$age >= 18 , ]

# fit model
m4.3 <- map(
    alist(
        height ~ dnorm( mu , sigma ) ,
        mu <- a + b*weight ,
        a ~ dnorm( 156 , 100 ) ,
        b ~ dnorm( 0 , 10 ) ,
        sigma ~ dunif( 0 , 50 )
    ) ,
    data=d2 )
#check our fit
plot( height ~ weight , data=d2 )
abline( a=coef(m4.3)["a"] , b=coef(m4.3)["b"] )

#get the posterior distribution
post <- extract.samples(m4.3)

#calculate height at unknown weights
weights <- c(46.95, 43.72, 64.78, 32.59, 54.63)
mu_at_p1 <- post$a + post$b * weights[1]
dens( mu_at_p1 , col=rangi2 , lwd=2 , xlab="mu|weight=50" )
mean(mu_at_p1)
HPDI(mu_at_p1)
PI(mu_at_p1)

#now do the same using the link function
mu <- link( m4.3 , data=data.frame(weight=weights) )
mu.mean <- apply( mu , 2 , mean )
mu.HPDI <- apply( mu , 2 , HPDI , prob=0.89 )

results <- tibble::data_frame(weights=weights,
                              heights=mu.mean,
                              lower_HPDI=mu.HPDI[1,],
                              upper_HPDI=mu.HPDI[2,])   

# Q4H2 --------------------------------------------------------------------
library(rethinking)
data(Howell1)
d <- Howell1
d2 <- d[ d$age < 18 , ]

# fit model
m4H2 <- map(
    alist(
        height ~ dnorm( mu , sigma ) ,
        mu <- a + b*weight ,
        a ~ dnorm( 40 , 100 ) ,
        b ~ dnorm( 0 , 10 ) ,
        sigma ~ dunif( 0 , 50 )
    ) ,
    data=d2 )
#check our fit
plot( height ~ weight , data=d2 )
abline( a=coef(m4H2)["a"] , b=coef(m4H2)["b"] )
precis(m4H2)

#compute mu for each sample from posterior and for weight weight in weight.seq
weight.seq = 0:50
mu <- link(m4H2, data.frame(weight=weight.seq))
mu.mean <- apply(mu, 2, mean) #mean of mu
mu.HPDI <- apply(mu, 2, HPDI, prob=0.89) #mean HPDI
sim.height <- sim(m4H2, data=list(weight=weight.seq))  #simulate heights from model
height.HPDI <- apply(sim.height, 2, HPDI, prob=0.89) #HDPI of avg heights
plot( height ~ weight , data=d2 )
lines(weight.seq, mu.mean) #estimated mean height
shade(mu.HPDI, weight.seq) #HPDI for estimated mean height
shade(height.HPDI, weight.seq)  #HPDI for simulated heights

# Q4H3 --------------------------------------------------------------------
library(rethinking)
data(Howell1)
d <- Howell1

# fit model
m4H3 <- map(
    alist(
        height ~ dnorm( mu , sigma ) ,
        mu <- a + b*log(weight) ,
        a ~ dnorm( 178 , 100 ) ,
        b ~ dnorm( 0 , 10 ) ,
        sigma ~ dunif( 0 , 50 )
    ) ,
    data=d )
precis(m4H3)


# predicted mean height as function of weight
# 97% HPDI for mean 
# 97% HPDI for predicted heights
weight.seq = 0:60
mu <- link(m4H3, data.frame(weight=weight.seq))
mu.mean <- apply(mu, 2, mean) #mean of mu
mu.HPDI <- apply(mu, 2, HPDI, prob=0.97) #mean HPDI
sim.height <- sim(m4H3, data=list(weight=weight.seq))  #simulate heights from model
height.HPDI <- apply(sim.height, 2, HPDI, prob=0.97) #HDPI of avg heights
plot(height~weight, data=d,
     col=col.alpha(rangi2, 0.4))
lines(weight.seq, mu.mean) #estimated mean height
shade(mu.HPDI, weight.seq) #HPDI for estimated mean height
shade(height.HPDI, weight.seq)  #HPDI for simulated heights

