#### Hierachical Bayesian ####

dat = read.csv(file="pctgrowth.csv", header=TRUE)

head(dat)

means_anova = tapply(dat$y, INDEX=dat$grp, FUN=mean)
## dat is the data read from pctgrowth.csv

plot(means_anova)
points(means_theta, col="red") ## where means_theta are the posterior point estimates for the industry means.

library("rjags")

mod_string = " model {
for (i in 1:length(dat)) {
  y[i] ~ dnorm(theta[[i]], prec)
}

for (j in 1:max(grp)) {
  theta[j] ~ dnorm(mu, invtau2)
  mu ~ dnorm(0, 1000000)
  invtau2 ~ dgamma(1/2, 1*3.0/2.0)
  tau <- sqrt(1 / invtau2)
}

prec ~ dgamma(2/2,2*1.0/2)
sig =sqrt(1.0 / prec)

} "

set.seed(113)

data_jags = as.list(dat)

params = c("theta", "mu", "tau", "sig")

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




#### Quiz 11 ####
library("MASS")
data("OME")
?OME # background on the data
head(OME)

dat = subset(OME, OME != "N/A")
dat$OME = factor(dat$OME) # relabel OME
dat$ID = as.numeric(factor(dat$ID)) # relabel ID so there are no gaps in numbers (they now go from 1 to 63)

## Original reference model and covariate matrix
mod_glm = glm(Correct/Trials ~ Age + OME + Loud + Noise, data=dat, weights=Trials, family="binomial")
X = model.matrix(mod_glm)[,-1]

## Original model (that needs to be extended)
mod_string = " model {
	for (i in 1:length(y)) {
		y[i] ~ dbin(phi[ID[i]], n[i])
		logit(phi[i]) = alpha[ID[i]] + b[1]*Age[i] + b[2]*OMElow[i] + b[3]*Loud[i] + b[4]*Noiseincoherent[i]
	}
	
	for (j in 1:max(ID)){
	alpha[j] ~ dnorm(mu, invtau2)
	}
	mu ~ dnorm(0, 10^2)
	invtau2 ~ dgamma(1/2, 1/2)
	tau <- sqrt(1 / invtau2)
	
	for (j in 1:4) {
		b[j] ~ dnorm(0.0, 1.0/4.0^2)
	}
	
} "


data_jags = as.list(as.data.frame(X))
data_jags$y = dat$Correct # this will not work if there are missing values in dat (because they would be ignored by model.matrix). Always make sure that the data are accurately pre-processed for JAGS.
data_jags$n = dat$Trials
data_jags$ID = dat$ID
str(data_jags) # make sure that all variables have the same number of observations (712).

params = c("alpha", "b", "mu", "tau")

library(rjags)
mod2 = jags.model(textConnection(mod_string), data=data_jags, n.chains=3)
update(mod2, 1e3)

mod2_sim = coda.samples(model=mod2,
                        variable.names=params,
                        n.iter=5e3)
mod2_csim = as.mcmc(do.call(rbind, mod2_sim))

plot(mod2_sim, ask=TRUE)

gelman.diag(mod_sim)
autocorr.diag(mod_sim)
autocorr.plot(mod_sim)
effectiveSize(mod_sim)

dic.samples(mod2, n.iter=1e3)
