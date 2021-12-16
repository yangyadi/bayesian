### Beyesian Anova

data("PlantGrowth")
?PlantGrowth
head(PlantGrowth)

boxplot(weight ~ group, data=PlantGrowth)

lmod = lm(weight ~ group, data=PlantGrowth)
summary(lmod)

anova(lmod)

library("rjags")

mod_string = " model {
    for (i in 1:length(y)) {
        y[i] ~ dnorm(mu[grp[i]], prec)
    }
    
    for (j in 1:3) {
        mu[j] ~ dnorm(0.0, 1.0/1.0e6)
    }
    
    prec ~ dgamma(5/2.0, 5*1.0/2.0)
    sig = sqrt( 1.0 / prec )
} "

set.seed(82)
str(PlantGrowth)
data_jags = list(y=PlantGrowth$weight, 
                 grp=as.numeric(PlantGrowth$group))

params = c("mu", "sig")

inits = function() {
  inits = list("mu"=rnorm(3,0.0,100.0), "prec"=rgamma(1,1.0,1.0))
}

mod = jags.model(textConnection(mod_string), data=data_jags, inits=inits, n.chains=3)
update(mod, 1e3)

mod_sim = coda.samples(model=mod,
                       variable.names=params,
                       n.iter=5e3)
mod_csim = as.mcmc(do.call(rbind, mod_sim)) # combined chains

plot(mod_sim)

gelman.diag(mod_sim)
autocorr.diag(mod_sim)
effectiveSize(mod_sim)

(pm_params = colMeans(mod_csim))

yhat = pm_params[1:3][data_jags$grp]
resid = data_jags$y - yhat
plot(resid)

plot(yhat, resid)

summary(mod_sim)

HPDinterval(mod_csim)

mean(mod_csim[,3] > mod_csim[,1])

mean(mod_csim[,3] > 1.1*mod_csim[,1])

### with a separate variance for each of the three groups

mod_string1 = " model {
    for (i in 1:length(y)) {
        y[i] ~ dnorm(mu[grp[i]], prec[grp[i]])
    }
    
    for (j in 1:3) {
        mu[j] ~ dnorm(0.0, 1.0/1.0e6)
        prec[j] ~ dgamma(5/2.0, 5*1.0/2.0)
        sig[j] = sqrt( 1.0 / prec[j] )
        
    }

} "

set.seed(88)
str(PlantGrowth)
data_jags1 = list(y=PlantGrowth$weight, 
                 grp=as.numeric(PlantGrowth$group))

params1 = c("mu", "sig")

inits1 = function() {
  inits = list("mu"=rnorm(3,0.0,100.0), "prec"=rgamma(3,1.0,1.0))
}

mod1 = jags.model(textConnection(mod_string1), data=data_jags1, inits=inits1, n.chains=3)
update(mod1, 1e3)

mod_sim1 = coda.samples(model=mod1,
                       variable.names=params,
                       n.iter=5e3)
mod_csim1 = as.mcmc(do.call(rbind, mod_sim1)) # combined chains

plot(mod_sim1)

summary(mod_sim)
summary(mod_sim1)

dic1 <- dic.samples(mod, 5e3)
dic2 <- dic.samples(mod1, 5e3)
dic1 - dic2


HPDinterval(mod_csim[,3]-mod_csim[,1])


mod_cm = lm(weight ~ -1 + group, data=PlantGrowth)
summary(mod_cm)
