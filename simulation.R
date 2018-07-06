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

m2_brms <- brm(formula = data  ~ p1, data=df, 
               prior = c(set_prior("normal(500, 1000)", class='Intercept'),
                         set_prior("normal(0,100)", class = "b"),
                         set_prior("cauchy(0, 40)", class = "sigma")),
               chains = 4, iter = 2000,
               future = TRUE)

summary(m2_brms)
tidy(m2_brms)

lm (data ~ p1, data=df)



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
  mutate(res=future_map(df, ~update(m2_brms, newdata=., recompile=FALSE, 
                                    chains=4, iter=1000, init_r=5, cores=1)))

zzz <- results %>%
  transmute(id,
            res,
            brms_est=map(res, broom::tidy),
            lm_mod=map(df, ~lm(data ~ p1, data=.)),
            lm_est=map(lm_mod, broom::tidy))

zzz %>%
  select(id, lm_est) %>%
  tidyr::unnest()

zzz %>%
  select(id, brms_est) %>%
  tidyr::unnest()


res <- sim_data %>%
  mutate(mod=map(df, ~lm(data ~ p1, data=.)),
         cf=map(mod, ~broom::tidy(.)))


# how to continue?
# define functions for truncated and censored data
# simulate 128 datasets
# for each do lm, truncated and censored
# then compare lm, truncated and censored for each simulation
# potential next step would be to have a grid of averages, sd's, and differences?
# use furrr to parallelise, possibly even do on a bigger AWS instance
# do as a Rmd?
# test with small no of reps and parameters then scale up
# shiny app?


# make paramater grid
params_df <- tidyr::crossing(d=50, ub=600, g1=c(400, 500, 600), sd=80, n=1000) %>%
    mutate(param_id = row_number()) %>%
    tidyr::crossing(sim_id=1:10)

make_data <- function(d, ub, g1, sd, n) {
    data_frame(p1 = rbinom(n, 1, 0.5),
               d = d,
               data = rnorm(n, mean = g1 + p1 * d , sd=sd),
               censored = ifelse(data >= 600, 1, 0),
               cens_data = ifelse(censored == 1, 600, data),
               trunc_data = ifelse(censored == 1, NA, data))
}

plan(multicore)
sim_data <- params_df %>%
    mutate(data = future_pmap(.l=list(d=d, ub=ub, g1=g1, sd=sd, n=n), .f=make_data))

results <- sim_data %>%
    mutate(b_mod = future_map(data, ~update(m2_brms, newdata=., recompile=FALSE, 
                                            chains=4, iter=1000, init_r=5, cores=1)))

proc_results <- results %>%
    mutate(b_mod_tidy=map(b_mod, ~broom::tidy(.) %>% tidyr::gather('metric', 'value', -term))) %>%
    select(-data, -b_mod) %>%
    tidyr::unnest()

ggplot(proc_results %>% filter(metric=='estimate'), aes(x=as.factor(g1), y=value)) + 
    geom_boxplot() + facet_wrap(~term, scales = 'free_y')