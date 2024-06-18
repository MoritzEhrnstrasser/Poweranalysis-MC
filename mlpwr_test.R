library(lme4)
library(lmerTest)
library(mlpwr)
library(pwr)
library(tidyr)
library(tmvtnorm)
library(truncnorm)



#### ALM ####

simfun_alm <- function(N) { 
  N=100
  beta <- c(1, 0.2, 0.1)
  n_pred <- length(beta)-1
  sigma <- matrix(c(2,0,
                    0,2), nrow = 2, ncol = 2 )
  
  pred <- rtmvnorm(N, mean = rep(0, n_pred), sigma = sigma)
  
  e<- rnorm(N, mean = 0, sd = 1)
  
  x <- cbind(1, predictors)
  y <- x %*% beta + + e
  
 dat <-data.frame( y=y,x=x,e=e)
 
results <- lm(y ~ x.2 + x.3, data =dat)
 sum<-summary(results)
 
  0.01 >sum$coefficients["x.2","Pr(>|t|)"]
  }

alm <- find.design(simfun = simfun_alm, 
            boundaries = c(1,350),
            power = 0.95, surrogate = "logreg")
summary(alm)
plot(alm)



#### log #### 
simfun_log <- function(N) {
  
logistic <- function(x) 1/(1 + exp(-x))
dat <- data.frame(pred1 = stats::rnorm(N), pred2 = stats::rnorm(N))
beta <- c(1.2, 0.8)
prob <- logistic(as.matrix(dat) %*% beta)
dat$criterion <- stats::runif(N) < prob
mod <- stats::glm(criterion ~ pred1 + pred2, data = dat, 
                  family = stats::binomial(link=logit))
pValue <-summary(mod)$coefficients["pred2", "Pr(>|z|)"] 
pValue < 0.01
}

log <- find.design(simfun = simfun_log, 
                   boundaries = list(N = c(10,350)),
                   power = 0.95, surrogate = "logreg")

summary(log)
plot(log)


#### LMM ####

n.schools = simfun_multilevel <- function(n.per.school, n.schools) {
  
  # generate data
  group <- rep(1:n.schools, each = n.per.school)
  pred <- factor(rep(c("old", "new"), n.per.school * n.schools), levels = c("old",
                                                                            "new"))
  dat <- data.frame(group = group, pred = pred)
  
  params <- list(theta = c(0.5, 0, 0.5), beta = c(0, 1), sigma = 1.5)
  names(params$theta) <- c("group.(Intercept)", "group.prednew.(Intercept)", "group.prednew")
  names(params$beta) <- c("(Intercept)", "prednew")
  dat$y <- simulate.formula(~pred + (1 + pred | group), newdata = dat, newparams = params)[[1]]
  
  # test hypothesis
  mod <- lmer(y ~ pred + (1 + pred | group), data = dat)
  pvalue <- summary(mod)[["coefficients"]][2, "Pr(>|t|)"]
  pvalue < 0.01
}

costfun_multilevel <- function(n.per.school, n.schools) {
  100 * n.per.school + 200 * n.schools
}

MLM <- find.design(simfun = simfun_multilevel, costfun = costfun_multilevel, 
                   boundaries = list(n.per.school = c(5,25), n.schools = c(10, 30)),
                   power = 0.95)
summary(MLM)


