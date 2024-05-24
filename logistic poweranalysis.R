
N <- 650
simulations <- 4000

B0 = 0.2
B1 = 0.5
B2 = 0.5
B3 = 0.5

function_simulate <- function(data) {
  
  x1 <- rbinom(N, 1, 0.5)
  x2 <- rbinom(N, 1, 0.5)
  x3 <- rbinom(N, 1, 0.5)
  
  z <- B0 + B1*x1 + B2*x2 + B3*x3 # linear model
  prob <- exp(z) / (1 + exp(z))  # logistic function
  y <- rbinom(N, 1, prob)
  
  model <- glm(y ~ x1 + x2 + x3,  family=binomial(link=logit))
  
  
  model_summary <- summary(model)
  
  
  coefficients_table <- model_summary$coefficients
  
  
  p_values <- coefficients_table[, "Pr(>|z|)"]
  
  return(p_values)
  
}
results<-sapply(1:simulations, function(i) 
  function_simulate(), simplify = "array")


power <- results["x2",] < 0.05

mean(power)


#### check if it works ####
apply(results, c(1,2), mean)

