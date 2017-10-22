lg = function(mu, n, ybar){
  mu2 = mu^2
  n * (ybar* mu - mu2 / 2.0) - log(1.0 + mu2)
}

# Metropolis- Hastings Algorithms
mh = function(n, ybar, n_iter, mu_init, cand_std){
  
  mu_out = numeric(n_iter)
  accept = 0
  
  mu_now = mu_init
  lg_now = lg(mu = mu_now, n=n, ybar = ybar)
  for (i in 1:n_iter){
    mu_candidate = rnorm(1, mean = mu_now, sd = cand_std) #draw out one sample from normal distrubution
    
    lg_cand = lg(mu = mu_candidate, n = n, ybar = ybar)
    lalpha = lg_cand - lg_now
    alpha = exp(lalpha)
    
    u = runif(1) #uniform distribution that takes value between 0 and 1
    if(u < alpha){
      mu_now = mu_candidate
      accept = accept + 1
      lg_now = lg_cand
    }
    mu_out[i] = mu_now
  }
  
  list(mu = mu_out, accept_rate = accept / n_iter)
}

#Set Up 

y = c(1.2, 1.4, -0.5, 0.3, 0.9, 2.3, 1.0, 0.1, 1.3, 1.9)
ybar = mean(y)
n = length(y)

hist(y, freq = FALSE, xlim = c(-1.0, 3.0))
points(y, rep(0.0, n))
points(ybar, 0.0, pch=19) #this is what the data says mu should be

# this the the prior for mu
curve(dt(x, df = 1), lty = 2, add = TRUE)

#posterior sampling
set.seed(43)
post_data = mh(n = n, ybar = ybar, n_iter = 1e3, mu_init = 30.0, cand_std = 0.9)

str(post_data)

install.packages("coda")
library("coda")

traceplot(as.mcmc(post_data$mu))


## posterior analysis
post_data$mu_keep = post_data$mu[-c(1:100)]
str(post_data)

plot(density(post_data$mu_keep), xlim = c(-1.0, 3.0))








