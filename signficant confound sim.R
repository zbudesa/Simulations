###### Significant but Unrelated Confound Simulation ######

library(tidyverse)

n <- 500

set.seed(3)

(
  d <- tibble(
    id = 1:n,
    age = rnorm(n, 37.84, 11.32),
    leo = rep(0:1, times = c(1,3), length.out = n),
    pre = rnorm(n, 2.87, 0.74) + leo*rnorm(n, .25, .12),
    post = pre - rnorm(n, .5) + leo*rnorm(n, .15, .06) ) %>% 
    pivot_longer(-c(id:leo), names_to = "post") %>% 
    mutate(post = ifelse(post == "post", 1, 0))
)

formula <- value ~ post + leo + age

mod <- lm(formula, d)

summary(mod)





