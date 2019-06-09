a = -30/3479
b = -120 * a
c = 3600 * a + 80

day = c(1:100)
theta = a * day^2 + b * day + c + runif(length(day), -5, 5)
theta = round(theta, 0)
plot(day, theta, cex = 0.5, pch = 3)

beta1 = 0.95
beta2 = 0.8
beta3 = 0.5

exp_weight_avg = function(beta, theta) {
  v = rep(0, length(theta))
  
  for (i in 1:length(theta)) {
    if (i == 1) {
      v[i] = (1 - beta) * theta[i]
    } else {
      v[i] = beta * v[i - 1] + (1 - beta) * theta[i]
    }
  }
  return(v)
}

v1 = exp_weight_avg(beta = beta1, theta)
v2 = exp_weight_avg(beta = beta2, theta)
v3 = exp_weight_avg(beta = beta3, theta)

lines(day, v1, col = 1)
lines(day, v2, col = 2)
lines(day, v3, col = 3)

exp_weight_avg_correct = function(beta, theta) {
  v = rep(0, length(theta))
  
  for (i in 1:length(theta)) {
    if (i == 1) {
      v[i] = (1 - beta) * theta[i]
    } else {
      v[i] = beta * v[i - 1] + (1 - beta) * theta[i]
    }
  }
  v = v/(1 - beta^c(1:length(v)))
  return(v)
}

v1_correct = exp_weight_avg_correct(beta = beta1, theta)
v2_correct = exp_weight_avg_correct(beta = beta2, theta)
v3_correct = exp_weight_avg_correct(beta = beta3, theta)

lines(day, v1_correct, col = 4)
lines(day, v2_correct, col = 5)
lines(day, v3_correct, col = 6)