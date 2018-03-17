library(rethinking)


# Q5H1 --------------------------------------------------------------------

data(foxes)

m5H1a <- map(
    alist(
        weight ~ dnorm( mu , sigma ) ,
        mu <- a + Ba*area ,
        a ~ dnorm( 3 , 10 ) ,
        Ba ~ dnorm( 0 , 10 ) ,
        sigma ~ dunif( 0 , 50 )
    ) ,
    data=foxes )
precis(m5H1a)

#check our fit
plot( weight ~ area , data=foxes )
abline( a=coef(m5H1a)["a"] , b=coef(m5H1a)["Ba"] )

#compute mu for each sample from posterior and for weight weight in weight.seq
area.seq = seq(0,6,0.1)
mu <- link(m5H1a, data.frame(area=area.seq))
mu.mean <- apply(mu, 2, mean) #mean of mu
mu.PI <- apply(mu, 2, PI, prob=0.95) #mean PI
sim.weight <- sim(m5H1a, data=list(area=area.seq))  #simulate heights from model
weight.PI <- apply(sim.weight, 2, PI, prob=0.95) #PI of avg heights
plot( weight ~ area , data=foxes )
lines(area.seq, mu.mean) #estimated mean height
shade(mu.PI, area.seq) #HPDI for estimated mean height
shade(weight.PI, area.seq)  #HPDI for simulated heights

m5H1b <- map(
    alist(
        weight ~ dnorm( mu , sigma ) ,
        mu <- a + Bg*groupsize ,
        a ~ dnorm( 3 , 10 ) ,
        Bg ~ dnorm( 0 , 10 ) ,
        sigma ~ dunif( 0 , 50 )
    ) ,
    data=foxes )
precis(m5H1b)

#check our fit
plot( weight ~ groupsize , data=foxes )
abline( a=coef(m5H1b)["a"] , b=coef(m5H1b)["Bg"] )

#compute mu for each sample from posterior and for weight weight in weight.seq
groupsize.seq = seq(1,9,1)
mu <- link(m5H1b, data.frame(groupsize=groupsize.seq))
mu.mean <- apply(mu, 2, mean) #mean of mu
mu.PI <- apply(mu, 2, PI, prob=0.95) #mean PI
sim.weight <- sim(m5H1b, data=list(groupsize=groupsize.seq))  #simulate heights from model
weight.PI <- apply(sim.weight, 2, PI, prob=0.95) #PI of avg heights
plot( weight ~ groupsize , data=foxes )
lines(groupsize.seq, mu.mean) #estimated mean height
shade(mu.PI, groupsize.seq) #HPDI for estimated mean height
shade(weight.PI, groupsize.seq)  #HPDI for simulated heights


# Q5H2 --------------------------------------------------------------------

m5H2 <- map(
    alist(
        weight ~ dnorm( mu , sigma ) ,
        mu <- a + Ba*area + Bg*groupsize ,
        a ~ dnorm( 3 , 10 ) ,
        Ba ~ dnorm( 0 , 10 ) ,
        Bg ~ dnorm( 0 , 10 ) ,
        sigma ~ dunif( 0 , 50 )
    ) ,
    data=foxes )
plot(precis(m5H2))

# prepare new counterfactual data
area.avg <- mean( foxes$area )
groupsize.seq <- seq(1,9,1)
pred.data <- data.frame(
    area=area.avg,
    groupsize=groupsize.seq
)

# compute counterfactual mean weigt (mu)
mu <- link( m5H2 , data=pred.data )
mu.mean <- apply( mu , 2 , mean )
mu.PI <- apply( mu , 2 , PI )

# simulate counterfactual weight outcomes
groupsize.sim <- sim( m5H2 , data=pred.data , n=1e4 )
groupsize.PI <- apply( groupsize.sim , 2 , PI )

# display predictions, hiding raw data with type="n"
plot( weight ~ groupsize , data=foxes , type="n" )
lines( groupsize.seq , mu.mean )
shade( mu.PI , groupsize.seq )
shade( R.PI , groupsize.seq )


#######
# prepare new counterfactual data
groupsize.avg <- mean( foxes$groupsize )
area.seq = seq(0,6,0.1)
pred.data <- data.frame(
    area=area.seq,
    groupsize=groupsize.avg
)

# compute counterfactual mean weigt (mu)
mu <- link( m5H2 , data=pred.data )
mu.mean <- apply( mu , 2 , mean )
mu.PI <- apply( mu , 2 , PI )

# simulate counterfactual weight outcomes
area.sim <- sim( m5H2 , data=pred.data , n=1e4 )
area.PI <- apply( area.sim , 2 , PI )

# display predictions, hiding raw data with type="n"
plot( weight ~ area , data=foxes , type="n" )
lines( area.seq , mu.mean )
shade( mu.PI , area.seq )
shade( area.PI , area.seq )


# Q5H3 --------------------------------------------------------------------

m5H3a <- map(
    alist(
        weight ~ dnorm( mu , sigma ) ,
        mu <- a + Bf*avgfood + Bg*groupsize ,
        a ~ dnorm( 3 , 10 ) ,
        Bf ~ dnorm( 0 , 10 ) ,
        Bg ~ dnorm( 0 , 10 ) ,
        sigma ~ dunif( 0 , 50 )
    ) ,
    data=foxes )
plot(precis(m5H3a))

m5H3b <- map(
    alist(
        weight ~ dnorm( mu , sigma ) ,
        mu <- a + Bf*avgfood + Bg*groupsize + Ba*area,
        a ~ dnorm( 3 , 10 ) ,
        Ba ~ dnorm( 0 , 10 ) ,
        Bf ~ dnorm( 0 , 10 ) ,
        Bg ~ dnorm( 0 , 10 ) ,
        sigma ~ dunif( 0 , 50 )
    ) ,
    data=foxes )
plot(precis(m5H3b))

precis(m5H2)
precis(m5H3a)
precis(m5H3b)
pairs(~ weight + groupsize + area + avgfood, data=foxes)

#######
# prepare new counterfactual data
groupsize.avg <- mean( foxes$groupsize )
avgfood.seq = seq(0.2,1.4,0.1)
pred.data <- data.frame(
    avgfood=avgfood.seq,
    groupsize=groupsize.avg
)

# compute counterfactual mean weigt (mu)
mu <- link( m5H3a , data=pred.data )
mu.mean <- apply( mu , 2 , mean )
mu.PI <- apply( mu , 2 , PI )

# simulate counterfactual weight outcomes
avgfood.sim <- sim( m5H3a , data=pred.data , n=1e4 )
avgfood.PI <- apply( avgfood.sim , 2 , PI )

# display predictions, hiding raw data with type="n"
plot( weight ~ avgfood , data=foxes , type="n" )
lines( avgfood.seq , mu.mean )
shade( mu.PI , avgfood.seq )
shade( avgfood.PI , avgfood.seq )
