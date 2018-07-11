library(rjags, quietly = TRUE)
library(ggplot2, quietly = TRUE)
library(dplyr, quietly = TRUE)
library(tidybayes, quietly = TRUE)
library(brms, quietly = TRUE)
library(broom, quietly = TRUE)
library(future, quietly = TRUE)


set.seed(1238)
df <- data_frame(p1 = rbinom(1000, 1, 0.5),
                 d = 50,
                 data = rnorm(1000, mean = 500 + p1 * d , sd=80))

ggplot(df, aes(x=data, fill=as.factor(p1))) + 
    geom_histogram(bins=50) +
    facet_wrap(~as.factor(p1), ncol = 1) + theme_bw()

trunc_df <- df %>% 
    mutate(data = ifelse(data >= 600, 600, data),
           censored = ifelse(data >= 600, 'right', 'none'))
trunc_df

m2_brms <- brm(formula = data  ~ p1, data=df, 
               prior = c(set_prior("normal(500, 1000)", class='Intercept'),
                         set_prior("normal(0,100)", class = "b"),
                         set_prior("cauchy(0, 40)", class = "sigma")),
               chains = 4, iter = 2000,
               future = TRUE)

summary(m2_brms)
tidy(m2_brms)

lm (data ~ p1, data=df)


m3_brms <- brm(formula = data | cens(censored) ~ 1 + p1, 
               data=trunc_df, 
               family = gaussian,
               prior = c(set_prior("normal(500, 1000)", class='Intercept'),
                         set_prior("normal(0,100)", class = "b"),
                         set_prior("cauchy(0, 40)", class = "sigma")),
               chains = 4, iter = 2000, 
               init_r = 5,  #not sure what this does but it seems to be needed!
               cores = 4)

summary(m3_brms)
tidy(m3_brms)

lm (data ~ p1, data=df)


m4_brms <- brm(formula = data | trunc(ub=600) ~ 1 + p1, 
               data=filter(trunc_df, censored == 'none'), 
               family = gaussian,
               prior = c(set_prior("normal(500, 1000)", class='Intercept'),
                         set_prior("normal(0,100)", class = "b"),
                         set_prior("cauchy(0, 40)", class = "sigma")),
               chains = 4, iter = 2000, 
               init_r = 5,  #not sure what this does but it seems to be needed!
               cores = 4)

summary(m4_brms)
tidy(m4_brms)

lm (data ~ p1, data=df)

plot_samples_m2_brms <- m2_brms %>%
    gather_samples(b_Intercept, b_p1, sigma) %>%
    mutate(model='m2_nontrunc')

plot_samples_m3_brms <- m3_brms %>%
    gather_samples(b_Intercept, b_p1, sigma) %>%
    mutate(model='m3_censored')

plot_samples_m4_brms <- m4_brms %>%
    gather_samples(b_Intercept, b_p1, sigma) %>%
    mutate(model='m4_truncated')


bind_rows(plot_samples_m2_brms, plot_samples_m3_brms, plot_samples_m4_brms) %>%
    ggplot(aes(x=model, y=estimate)) + 
    geom_eye() + coord_flip() + facet_wrap(~term, scales = 'free') + theme_bw()

#try updating with new data
set.seed(122238)
df2 <- data_frame(p1 = rbinom(1000, 1, 0.5),
                  d = 50,
                  data = rnorm(1000, mean = 500 + p1 * d , sd=80))

ggplot(df2, aes(x=data, fill=as.factor(p1))) + 
    geom_histogram(bins=50) +
    facet_wrap(~as.factor(p1), ncol = 1) + theme_bw()

trunc_df2 <- df2 %>% 
    mutate(data = ifelse(data >= 600, 600, data),
           censored = ifelse(data >= 600, 'right', 'none'))


m3_brms2 <- update(m3_brms, newdata=trunc_df2, recompile = FALSE,  
                   chains = 4, iter = 2000, init_r = 5, cores = 4)
m3_brms2

library(furrr)
library(purrr)

#simulate 10 datasets
make_data <- function(id) {
    data_frame(id=id,
               p1 = rbinom(1000, 1, 0.5),
               d = 50,
               data = rnorm(1000, mean = 500 + p1 * d , sd=80))
}

sim_data <- data_frame(id=1:128) %>%
    mutate(df=map(id, make_data))

plan(multicore)
     
results <- sim_data %>%
 mutate(res=future_map(df, ~update(m2_brms, newdata=., recompile=FALSE, chains=4, iter=1000, init_r=5, cores=1)))
     
     
     