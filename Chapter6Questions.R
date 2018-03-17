library(rethinking)


# Q6E2 --------------------------------------------------------------------
p2 <- c(0.3,0.7)
-sum(p2*log(p2))

# Q6E3 --------------------------------------------------------------------
p3 <- c(0.2,0.25, 0.25, 0.3)
-sum(p3*log(p3))

# Q6E4 --------------------------------------------------------------------
p4 <- c(1/3, 1/3, 1/3)
-sum(p4*log(p4))

# Q6M4 --------------------------------------------------------------------

data(milk)
dcc <- milk[ complete.cases(milk) , ]

m6M4a <- map(
    alist(
        kcal.per.g ~ dnorm( mu , sigma ) ,
        mu <- a + bn*neocortex.perc ,
        a ~ dnorm( 0 , 100 ) ,
        bn ~ dnorm( 0 , 1 ) ,
        sigma ~ dunif( 0 , 1 )
    ) ,
    data=dcc )

m6M4b <- map(
    alist(
        kcal.per.g ~ dnorm( mu , sigma ) ,
        mu <- a + bn*neocortex.perc ,
        a ~ dnorm( 0 , 200 ) ,
        bn ~ dnorm( 0 , 2 ) ,
        sigma ~ dunif( 0 , 2 )
    ) ,
    data=dcc )
compare(m6M4a, m6M4b)
