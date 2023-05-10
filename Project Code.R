#read in the data
HOPE <- readxl::read_excel("C:/Users/Kayley/OneDrive/Spring 2023/Bayesian Statistics/Project/Condensed HOPE Data.xlsx")

#collinearity checks

#using VIF, VIF that exceeds 5 or 10 is problematic, VIF does not depend on outcome variable
library(car)
lmod_adult <- lm(adult_health ~ food_security + primary_care + housing_quality + income + health_insurance
                + low_poverty + physical_assault, data = HOPE)
vif(lmod_adult)

#no variables exceed 10, but low_poverty is at 6.29 and income is at 5.66,
#try eliminating low_poverty, which has the highest VIF 

lmod_reduced <- lm(adult_health ~ food_security + primary_care + housing_quality + income + health_insurance
                   + physical_assault, data = HOPE)
vif(lmod_reduced)

#collinearity problems solved, all values are below 3 now

# model/variable selection based on forward-step
library(rstanarm)
HOPE$int <- rep(1, length = length(HOPE$state))

#single variable models, step 1
HOPE_a1 <- stan_glm(adult_health ~ int - 1  + food_security,
                      family = gaussian(link = "identity"),
                      prior = normal(0, 10),
                      prior_intercept = normal(0, 10),
                      data = HOPE,
                      iter = 50000, chains = 4)

HOPE_a2 <- stan_glm(adult_health ~ int - 1  + primary_care,
                    family = gaussian(link = "identity"),
                    prior = normal(0, 10),
                    prior_intercept = normal(0, 10),
                    data = HOPE,
                    iter = 50000, chains = 4)

HOPE_a3 <- stan_glm(adult_health ~ int - 1  + housing_quality,
                    family = gaussian(link = "identity"),
                    prior = normal(0, 10),
                    prior_intercept = normal(0, 10),
                    data = HOPE,
                    iter = 50000, chains = 4)

HOPE_a4 <- stan_glm(adult_health ~ int - 1  + health_insurance,
                    family = gaussian(link = "identity"),
                    prior = normal(0, 10),
                    prior_intercept = normal(0, 10),
                    data = HOPE,
                    iter = 50000, chains = 4)

HOPE_a5 <- stan_glm(adult_health ~ int - 1  + income,
                    family = gaussian(link = "identity"),
                    prior = normal(0, 10),
                    prior_intercept = normal(0, 10),
                    data = HOPE,
                    iter = 50000, chains = 4)

HOPE_a6 <- stan_glm(adult_health ~ int - 1  + physical_assault,
                    family = gaussian(link = "identity"),
                    prior = normal(0, 10),
                    prior_intercept = normal(0, 10),
                    data = HOPE,
                    iter = 50000, chains = 4)

#model comparison
waic_1 <- waic(HOPE_a1)
waic_2 <- waic(HOPE_a2)
waic_3 <- waic(HOPE_a3)
waic_4 <- waic(HOPE_a4)
waic_5 <- waic(HOPE_a5)
waic_6 <- waic(HOPE_a6)

library(loo)
loo_1 <- loo(HOPE_a1)
loo_2 <- loo(HOPE_a2)
loo_3 <- loo(HOPE_a3)
loo_4 <- loo(HOPE_a4)
loo_5 <- loo(HOPE_a5)
loo_6 <- loo(HOPE_a6)

waic_simple <- loo_compare(waic_1, waic_2, waic_3, waic_4,
            waic_5, waic_6)
save(waic_simple, file = "waic_simple.rda")

loo_simple <- loo_compare(loo_1, loo_2, loo_3, loo_4,
            loo_5, loo_6)
save(loo_simple, file = "loo_simple.rda")

#model 5 is the best
save(HOPE_a5, file = "HOPE_A5_simple.rda")

# second step
HOPE$int <- rep(1, length = length(HOPE$state))

HOPE_a2_1 <- stan_glm(adult_health ~ int - 1  + income + food_security,
                    family = gaussian(link = "identity"),
                    prior = normal(0, 10),
                    prior_intercept = normal(0, 10),
                    data = HOPE,
                    iter = 50000, chains = 4)

HOPE_a2_2 <- stan_glm(adult_health ~ int - 1  + income + primary_care,
                      family = gaussian(link = "identity"),
                      prior = normal(0, 10),
                      prior_intercept = normal(0, 10),
                      data = HOPE,
                      iter = 50000, chains = 4)

HOPE_a2_3 <- stan_glm(adult_health ~ int - 1  + income + housing_quality,
                      family = gaussian(link = "identity"),
                      prior = normal(0, 10),
                      prior_intercept = normal(0, 10),
                      data = HOPE,
                      iter = 50000, chains = 4)

HOPE_a2_4 <- stan_glm(adult_health ~ int - 1  + income + health_insurance,
                      family = gaussian(link = "identity"),
                      prior = normal(0, 10),
                      prior_intercept = normal(0, 10),
                      data = HOPE,
                      iter = 50000, chains = 4)

HOPE_a2_5 <- stan_glm(adult_health ~ int - 1  + income + physical_assault,
                      family = gaussian(link = "identity"),
                      prior = normal(0, 10),
                      prior_intercept = normal(0, 10),
                      data = HOPE,
                      iter = 50000, chains = 4)


setwd("C:/Users/Kayley/OneDrive/Spring 2023/Bayesian Statistics/Project/First Round of Simple Model fitting")
load(file = "HOPE_A5_simple.rda")

#model comparison
waic_1 <- waic(HOPE_a2_1)
waic_2 <- waic(HOPE_a2_2)
waic_3 <- waic(HOPE_a2_3)
waic_4 <- waic(HOPE_a2_4)
waic_5 <- waic(HOPE_a2_5)
waic_OG <- waic(HOPE_a5)


library(loo)
loo_1 <- loo(HOPE_a2_1)
loo_2 <- loo(HOPE_a2_2)
loo_3 <- loo(HOPE_a2_3)
loo_4 <- loo(HOPE_a2_4)
loo_5 <- loo(HOPE_a2_5)
loo_OG <- loo(HOPE_a5)

waic_round2 <- loo_compare(waic_1, waic_2, waic_3, waic_4,
                           waic_5, waic_OG)
save(waic_round2, file = "waic_round2.rda")

loo_round2 <- loo_compare(loo_1, loo_2, loo_3, loo_4,
                          loo_5, loo_OG)
save(loo_round2, file = "loo_round2.rda")

#HOPE_a2_3 is best
save(HOPE_a2_3, file = "HOPE_A2_3.rda")


#round 3
HOPE_a3_1 <- stan_glm(adult_health ~ int - 1  + income + housing_quality + food_security,
                      family = gaussian(link = "identity"),
                      prior = normal(0, 10),
                      prior_intercept = normal(0, 10),
                      data = HOPE,
                      iter = 50000, chains = 4)

HOPE_a3_2 <- stan_glm(adult_health ~ int - 1  + income + housing_quality + primary_care,
                      family = gaussian(link = "identity"),
                      prior = normal(0, 10),
                      prior_intercept = normal(0, 10),
                      data = HOPE,
                      iter = 50000, chains = 4)

HOPE_a3_3 <- stan_glm(adult_health ~ int - 1  + income + housing_quality + physical_assault,
                      family = gaussian(link = "identity"),
                      prior = normal(0, 10),
                      prior_intercept = normal(0, 10),
                      data = HOPE,
                      iter = 50000, chains = 4)

HOPE_a3_4 <- stan_glm(adult_health ~ int - 1  + income + housing_quality + health_insurance,
                      family = gaussian(link = "identity"),
                      prior = normal(0, 10),
                      prior_intercept = normal(0, 10),
                      data = HOPE,
                      iter = 50000, chains = 4)

#model comparison
load(file = "HOPE_A5_simple.rda")
load(file = "HOPE_A2_3.rda")

#model comparison
waic_1 <- waic(HOPE_a3_1)
waic_2 <- waic(HOPE_a3_2)
waic_3 <- waic(HOPE_a3_3)
waic_4 <- waic(HOPE_a3_4)
waic_2_3 <- waic(HOPE_a2_3)
waic_1_5<- waic(HOPE_a5)


library(loo)
loo_1 <- loo(HOPE_a3_1)
loo_2 <- loo(HOPE_a3_2)
loo_3 <- loo(HOPE_a3_3)
loo_4 <- loo(HOPE_a3_4)
loo_2_3 <- loo(HOPE_a2_3)
loo_1_5 <- loo(HOPE_a5)


waic_round3 <- loo_compare(waic_1, waic_2, waic_3, waic_4,
                           waic_2_3, waic_1_5)
save(waic_round3, file = "waic_round3.rda")

loo_round3 <- loo_compare(loo_1, loo_2, loo_3, loo_4,
                          loo_2_3, loo_1_5)
save(loo_round3, file = "loo_round3.rda")

#model a3_2 was the best 
save(HOPE_a3_2, file = "HOPE_A3_2.rda")


#round 4
HOPE_a4_1 <- stan_glm(adult_health ~ int - 1  + income + housing_quality + primary_care + food_security,
                      family = gaussian(link = "identity"),
                      prior = normal(0, 10),
                      prior_intercept = normal(0, 10),
                      data = HOPE,
                      iter = 50000, chains = 4)

HOPE_a4_2 <- stan_glm(adult_health ~ int - 1  + income + housing_quality + primary_care + physical_assault,
                      family = gaussian(link = "identity"),
                      prior = normal(0, 10),
                      prior_intercept = normal(0, 10),
                      data = HOPE,
                      iter = 50000, chains = 4)

HOPE_a4_3 <- stan_glm(adult_health ~ int - 1  + income + housing_quality + primary_care + health_insurance,
                      family = gaussian(link = "identity"),
                      prior = normal(0, 10),
                      prior_intercept = normal(0, 10),
                      data = HOPE,
                      iter = 50000, chains = 4)

load(file = "HOPE_A5_simple.rda")
load(file = "HOPE_A2_3.rda")
load(file = "HOPE_A3_2.rda")

#model comparison
waic_1 <- waic(HOPE_a4_1)
waic_2 <- waic(HOPE_a4_2)
waic_3 <- waic(HOPE_a4_3)
waic_3_2 <- waic(HOPE_a3_2)
waic_2_3 <- waic(HOPE_a2_3)
waic_1_5<- waic(HOPE_a5)


library(loo)
loo_1 <- loo(HOPE_a4_1)
loo_2 <- loo(HOPE_a4_2)
loo_3 <- loo(HOPE_a4_3)
loo_3_2 <- loo(HOPE_a3_2)
loo_2_3 <- loo(HOPE_a2_3)
loo_1_5<- loo(HOPE_a5)

waic_round4 <- loo_compare(waic_1, waic_2, waic_3, waic_3_2,
                           waic_2_3, waic_1_5)
save(waic_round4, file = "waic_round4.rda")

loo_round4 <- loo_compare(loo_1, loo_2, loo_3, loo_3_2,
                          loo_2_3, loo_1_5)
save(loo_round4, file = "loo_round4.rda")

save(HOPE_a4_2, file = "HOPE_A4_2.rda")


#posterior predictive checks
pp_check(HOPE_a4_2, plotfun = "error_scatter", nreps = 3)
pp_check(HOPE_a4_2, plotfun = "boxplot")
pp_check(HOPE_a4_2, plotfun = "dens_overlay")
pp_check(HOPE_a4_2, plotfun = "scatter", nreps = 9)

#convergence checks
library(coda)
library(rstan)

mcmc2 <- As.mcmc.list(HOPE_a4_2$stanfit)

effectiveSize(mcmc2)
heidel.diag(mcmc2)
coda::traceplot(mcmc2)


#############################################################################

# model 2: child health

HOPE$int <- rep(1, length = length(HOPE$state))

#single variable models
library(rstanarm)
HOPE_c1 <- stan_glm(child_health ~ int - 1  + food_security,
                    family = gaussian(link = "identity"),
                    prior = normal(0, 10),
                    prior_intercept = normal(0, 10),
                    data = HOPE,
                    iter = 50000, chains = 4)


HOPE_c2 <- stan_glm(child_health ~ int - 1  + primary_care,
                    family = gaussian(link = "identity"),
                    prior = normal(0, 10),
                    prior_intercept = normal(0, 10),
                    data = HOPE,
                    iter = 50000, chains = 4)


HOPE_c3 <- stan_glm(child_health ~ int - 1  + housing_quality,
                    family = gaussian(link = "identity"),
                    prior = normal(0, 10),
                    prior_intercept = normal(0, 10),
                    data = HOPE,
                    iter = 50000, chains = 4)


HOPE_c4 <- stan_glm(child_health ~ int - 1  + health_insurance,
                    family = gaussian(link = "identity"),
                    prior = normal(0, 10),
                    prior_intercept = normal(0, 10),
                    data = HOPE,
                    iter = 50000, chains = 4)


HOPE_c5 <- stan_glm(child_health ~ int - 1  + income,
                    family = gaussian(link = "identity"),
                    prior = normal(0, 10),
                    prior_intercept = normal(0, 10),
                    data = HOPE,
                    iter = 50000, chains = 4)


HOPE_c6 <- stan_glm(child_health ~ int - 1  + physical_assault,
                    family = gaussian(link = "identity"),
                    prior = normal(0, 10),
                    prior_intercept = normal(0, 10),
                    data = HOPE,
                    iter = 50000, chains = 4)

#model comparison
waic_1 <- waic(HOPE_c1)
waic_2 <- waic(HOPE_c2)
waic_3 <- waic(HOPE_c3)
waic_4 <- waic(HOPE_c4)
waic_5 <- waic(HOPE_c5)
waic_6 <- waic(HOPE_c6)

library(loo)
loo_1 <- loo(HOPE_c1)
loo_2 <- loo(HOPE_c2)
loo_3 <- loo(HOPE_c3)
loo_4 <- loo(HOPE_c4)
loo_5 <- loo(HOPE_c5)
loo_6 <- loo(HOPE_c6)

waic_simple_c <- loo_compare(waic_1, waic_2, waic_3, waic_4,
                           waic_5, waic_6)
save(waic_simple_c, file = "waic_simple_c.rda")

loo_simple_c <- loo_compare(loo_1, loo_2, loo_3, loo_4,
                          loo_5, loo_6)
save(loo_simple_c, file = "loo_simple_c.rda")

#model 5 is the best
save(HOPE_c5, file = "HOPE_C5_simple.rda")

#round 2
HOPE_c2_1 <- stan_glm(child_health ~ int - 1  + income + food_security,
                      family = gaussian(link = "identity"),
                      prior = normal(0, 10),
                      prior_intercept = normal(0, 10),
                      data = HOPE,
                      iter = 50000, chains = 4)

HOPE_c2_2 <- stan_glm(child_health ~ int - 1  + income + primary_care,
                      family = gaussian(link = "identity"),
                      prior = normal(0, 10),
                      prior_intercept = normal(0, 10),
                      data = HOPE,
                      iter = 50000, chains = 4)

HOPE_c2_3 <- stan_glm(child_health ~ int - 1  + income + housing_quality,
                      family = gaussian(link = "identity"),
                      prior = normal(0, 10),
                      prior_intercept = normal(0, 10),
                      data = HOPE,
                      iter = 50000, chains = 4)

HOPE_c2_4 <- stan_glm(child_health ~ int - 1  + income + health_insurance,
                      family = gaussian(link = "identity"),
                      prior = normal(0, 10),
                      prior_intercept = normal(0, 10),
                      data = HOPE,
                      iter = 50000, chains = 4)

HOPE_c2_5 <- stan_glm(child_health ~ int - 1  + income + physical_assault,
                      family = gaussian(link = "identity"),
                      prior = normal(0, 10),
                      prior_intercept = normal(0, 10),
                      data = HOPE,
                      iter = 50000, chains = 4)


setwd("C:/Users/Kayley/OneDrive/Spring 2023/Bayesian Statistics/Project/First Round of Simple Model fitting")
load(file = "HOPE_C5_simple.rda")

#model comparison
waic_1 <- waic(HOPE_c2_1)
waic_2 <- waic(HOPE_c2_2)
waic_3 <- waic(HOPE_c2_3)
waic_4 <- waic(HOPE_c2_4)
waic_5 <- waic(HOPE_c2_5)
waic_OG <- waic(HOPE_c5)


library(loo)
loo_1 <- loo(HOPE_c2_1)
loo_2 <- loo(HOPE_c2_2)
loo_3 <- loo(HOPE_c2_3)
loo_4 <- loo(HOPE_c2_4)
loo_5 <- loo(HOPE_c2_5)
loo_OG <- loo(HOPE_c5)

waic_round2_c <- loo_compare(waic_1, waic_2, waic_3, waic_4,
                           waic_5, waic_OG)
save(waic_round2_c, file = "waic_round2_c.rda")

loo_round2_c <- loo_compare(loo_1, loo_2, loo_3, loo_4,
                          loo_5, loo_OG)
save(loo_round2_c, file = "loo_round2_c.rda")

#HOPE_c2_4 is best
save(HOPE_c2_4, file = "HOPE_C2_4.rda")


#round 3
HOPE_c3_1 <- stan_glm(child_health ~ int - 1  + income + health_insurance + food_security,
                      family = gaussian(link = "identity"),
                      prior = normal(0, 10),
                      prior_intercept = normal(0, 10),
                      data = HOPE,
                      iter = 50000, chains = 4)

HOPE_c3_2 <- stan_glm(child_health ~ int - 1  + income + health_insurance + primary_care,
                      family = gaussian(link = "identity"),
                      prior = normal(0, 10),
                      prior_intercept = normal(0, 10),
                      data = HOPE,
                      iter = 50000, chains = 4)

HOPE_c3_3 <- stan_glm(child_health ~ int - 1  + income + health_insurance + housing_quality,
                      family = gaussian(link = "identity"),
                      prior = normal(0, 10),
                      prior_intercept = normal(0, 10),
                      data = HOPE,
                      iter = 50000, chains = 4)

HOPE_c3_4 <- stan_glm(child_health ~ int - 1  + income + health_insurance + physical_assault,
                      family = gaussian(link = "identity"),
                      prior = normal(0, 10),
                      prior_intercept = normal(0, 10),
                      data = HOPE,
                      iter = 50000, chains = 4)

load(file = "HOPE_C5_simple.rda")
load(file = "HOPE_C2_4.rda")

#model comparison
waic_1 <- waic(HOPE_c3_1)
waic_2 <- waic(HOPE_c3_2)
waic_3 <- waic(HOPE_c3_3)
waic_4 <- waic(HOPE_c3_4)
waic_2_4 <- waic(HOPE_c2_4)
waic_5 <- waic(HOPE_c5)


library(loo)
loo_1 <- loo(HOPE_c3_1)
loo_2 <- loo(HOPE_c3_2)
loo_3 <- loo(HOPE_c3_3)
loo_4 <- loo(HOPE_c3_4)
loo_2_4 <- loo(HOPE_c2_4)
loo_5 <- loo(HOPE_c5)

waic_round3_c <- loo_compare(waic_1, waic_2, waic_3, waic_4,
                             waic_2_4, waic_5)
save(waic_round3_c, file = "waic_round3_c.rda")

loo_round3_c <- loo_compare(loo_1, loo_2, loo_3, loo_4,
                            loo_2_4, loo_5)
save(loo_round3_c, file = "loo_round3_c.rda")

#HOPE_c3_3 is best
save(HOPE_c3_3, file = "HOPE_C3_3.rda")


#round 4
HOPE_c4_1 <- stan_glm(child_health ~ int - 1  + income + health_insurance + housing_quality + food_security,
                      family = gaussian(link = "identity"),
                      prior = normal(0, 10),
                      prior_intercept = normal(0, 10),
                      data = HOPE,
                      iter = 50000, chains = 4)

HOPE_c4_2 <- stan_glm(child_health ~ int - 1  + income + health_insurance + housing_quality + primary_care,
                      family = gaussian(link = "identity"),
                      prior = normal(0, 10),
                      prior_intercept = normal(0, 10),
                      data = HOPE,
                      iter = 50000, chains = 4)

HOPE_c4_3 <- stan_glm(child_health ~ int - 1  + income + health_insurance + housing_quality + physical_assault,
                      family = gaussian(link = "identity"),
                      prior = normal(0, 10),
                      prior_intercept = normal(0, 10),
                      data = HOPE,
                      iter = 50000, chains = 4)

load(file = "HOPE_C5_simple.rda")
load(file = "HOPE_C2_4.rda")
load(file = "HOPE_C3_3.rda")

#model comparison
waic_1 <- waic(HOPE_c4_1)
waic_2 <- waic(HOPE_c4_2)
waic_3 <- waic(HOPE_c4_3)
waic_2_4 <- waic(HOPE_c2_4)
waic_5 <- waic(HOPE_c5)
waic_3_3 <- waic(HOPE_c3_3)


library(loo)
loo_1 <- loo(HOPE_c4_1)
loo_2 <- loo(HOPE_c4_2)
loo_3 <- loo(HOPE_c4_3)
loo_2_4 <- loo(HOPE_c2_4)
loo_5 <- loo(HOPE_c5)
loo_3_3 <- loo(HOPE_c3_3)

waic_round4_c <- loo_compare(waic_1, waic_2, waic_3, waic_3_3,
                             waic_2_4, waic_5)
save(waic_round4_c, file = "waic_round4_c.rda")

loo_round4_c <- loo_compare(loo_1, loo_2, loo_3, loo_3_3,
                            loo_2_4, loo_5)
save(loo_round4_c, file = "loo_round4_c.rda")

#model 3_3 is the best still, no improvement by adding a 4th predictor 

#posterior predictive checks
pp_check(HOPE_c3_3, plotfun = "error_scatter", nreps = 3)
pp_check(HOPE_c3_3, plotfun = "boxplot")
pp_check(HOPE_c3_3, plotfun = "dens_overlay")
pp_check(HOPE_c3_3, plotfun = "scatter", nreps = 9)

#checking convergence
library(coda)
library(rstan)

mcmc1 <- As.mcmc.list(HOPE_c3_3$stanfit)

coda::traceplot(mcmc1)
effectiveSize(mcmc1)
heidel.diag(mcmc1)









