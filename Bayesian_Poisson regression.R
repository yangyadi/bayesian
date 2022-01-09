#### Poisson regression ####

library("COUNT")

data("badhealth")
?badhealth
head(badhealth)

any(is.na(badhealth))

hist(badhealth$numvisit, breaks=20)

library("rjags")

mod_string = " model {
    for (i in 1:length(numvisit)) {
        numvisit[i] ~ dpois(lam[i])
        log(lam[i]) = int + b_badh*badh[i] + b_age*age[i] + b_intx*age[i]*badh[i]
    }
    
    int ~ dnorm(0.0, 1.0/1e6)
    b_badh ~ dnorm(0.0, 1.0/1e4)
    b_age ~ dnorm(0.0, 1.0/1e4)
    b_intx ~ dnorm(0.0, 1.0/1e4)
} "

set.seed(102)

data_jags = as.list(badhealth)

params = c("int", "b_badh", "b_age", "b_intx")

mod = jags.model(textConnection(mod_string), data=data_jags, n.chains=3)
update(mod, 1e3)

mod_sim = coda.samples(model=mod,
                       variable.names=params,
                       n.iter=5e3)
mod_csim = as.mcmc(do.call(rbind, mod_sim))

## convergence diagnostics
plot(mod_sim)

gelman.diag(mod_sim)
autocorr.diag(mod_sim)
autocorr.plot(mod_sim)
effectiveSize(mod_sim)

## compute DIC
dic = dic.samples(mod, n.iter=1e3)


#### remove the interaction term ####
mod1_string = " model {
    for (i in 1:length(numvisit)) {
        numvisit[i] ~ dpois(lam[i])
        log(lam[i]) = int + b_badh*badh[i] + b_age*age[i]
    }
    
    int ~ dnorm(0.0, 1.0/1e6)
    b_badh ~ dnorm(0.0, 1.0/1e4)
    b_age ~ dnorm(0.0, 1.0/1e4)
} "

set.seed(12)

data_jags = as.list(badhealth)

params = c("int", "b_badh", "b_age")

mod1 = jags.model(textConnection(mod1_string), data=data_jags, n.chains=3)
update(mod, 1e3)

mod1_sim = coda.samples(model=mod1,
                       variable.names=params,
                       n.iter=5e3)
mod1_csim = as.mcmc(do.call(rbind, mod1_sim))

## compute DIC
dic1 = dic.samples(mod1, n.iter=1e3)


X = as.matrix(badhealth[,-1])
X = cbind(X, with(badhealth, badh*age))
head(X)

(pmed_coef = apply(mod_csim, 2, median))

llam_hat = pmed_coef["int"] + X %*% pmed_coef[c("b_badh", "b_age", "b_intx")]
lam_hat = exp(llam_hat)

hist(lam_hat)

resid = badhealth$numvisit - lam_hat
plot(resid) # the data were ordered


#### Quiz 10: 5

dat = read.csv(file="callers.csv", header=TRUE)
head(dat)


mod2_string = " model {
for (i in 1:length(calls)) {
  calls[i] ~ dpois( days_active[i] * lam[i] )
  log(lam[i]) = b0 + b[1]*age[i] + b[2]*isgroup2[i]
}
  b0 ~ dnorm(0.0, 1.0/1e2)
  for (j in 1:2) {
  b[j] ~ dnorm(0.0, 1.0/1e2)
  }
} "

set.seed(55)

data_jags = as.list(dat)

params = c("b0", "b")

mod2 = jags.model(textConnection(mod2_string), data=data_jags, n.chains=3)
update(mod2, 1e3)

mod2_sim = coda.samples(model=mod2,
                       variable.names=params,
                       n.iter=5e3)
mod2_csim = as.mcmc(do.call(rbind, mod2_sim))

## compute DIC
dic2 = dic.samples(mod1, n.iter=1e3)

## convergence diagnostics
plot(mod_sim)

gelman.diag(mod_sim)
autocorr.diag(mod_sim)
autocorr.plot(mod_sim)
effectiveSize(mod_sim)

X = as.matrix(dat[,-(1:2)])
head(X)

(pmed_coef = apply(mod2_csim, 2, median))

llam_hat = pmed_coef["b0"] + X %*% pmed_coef[c("b[1]", "b[2]")]
lam_hat = exp(llam_hat*dat$days_active)

hist(lam_hat)

resid = dat$calls - lam_hat
plot(resid) # the data were ordered



