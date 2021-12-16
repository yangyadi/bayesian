library("car")  # load the 'car' package
data("Anscombe")  # load the data set
?Anscombe  # read a description of the data
head(Anscombe)  # look at the first few lines of the data
pairs(Anscombe)  # scatter plots for each pair of variables

library("rjags")

mod_string = " model {
    for (i in 1:length(education)) {
        education[i] ~ dnorm(mu[i], prec)
        mu[i] = b0 + b[1]*income[i] + b[2]*young[i] + b[3]*urban[i]
    }
    
    b0 ~ dnorm(0.0, 1.0/1.0e6)
    for (i in 1:3) {
        b[i] ~ dnorm(0.0, 1.0/1.0e6)
    }
    
    prec ~ dgamma(1.0/2.0, 1.0*1500.0/2.0)
    	## Initial guess of variance based on overall
    	## variance of education variable. Uses low prior
    	## effective sample size. Technically, this is not
    	## a true 'prior', but it is not very informative.
    sig2 = 1.0 / prec
    sig = sqrt(sig2)
} "

data_jags = list(education=Anscombe$education, income = Anscombe$income, young = Anscombe$young, urban = Anscombe$urban)

params = c("b", "sig")

inits = function() {
  inits = list("b"=rnorm(3,0.0,100.0), "prec"=rgamma(1,1.0,1.0))
}

mod = jags.model(textConnection(mod_string), data=data_jags, inits=inits, n.chains=3)
update(mod, 100000) # burn-in

dic.samples(mod, n.iter=1e5)

mod_sim = coda.samples(model=mod,
                        variable.names=params,
                        n.iter=5000)

mod_csim = do.call(rbind, mod_sim) # combine multiple chains

plot(mod_sim)
# 
# gelman.diag(mod_sim)
# autocorr.diag(mod_sim)
# autocorr.plot(mod_sim)
# effectiveSize(mod_sim)
# summary(mod_sim)
# lmod0 = lm(education ~ income + young + urban, data=Anscombe)
# plot(resid(lmod0)) # to check independence (looks okay)mod1_sim = coda.samples(model=mod1,
#                         variable.names=params1,
#                         n.iter=5000)
# 
# mod1_csim = do.call(rbind, mod1_sim) # combine multiple chains

# plot(mod1_sim)
# 
# gelman.diag(mod1_sim)
# autocorr.diag(mod1_sim)
# autocorr.plot(mod1_sim)
# effectiveSize(mod1_sim)
# summary(mod1_sim)
# lmod0 = lm(education ~ income + young + urban, data=Anscombe)
# plot(resid(lmod0)) # to check independence (looks okay)
# 
# plot(predict(lmod0), resid(lmod0)) # to check for linearity, constant variance (looks bad)
# qqnorm(resid(lmod0)) # to check Normality assumption (we want this to be a straight line)

nburn = 1000 # remember to discard early iterations
mod_sim[[1]] = mod_sim[[1]][-c(1:1000)]
summary(as.mcmc(mod_sim[[1]]))

mean(mod_csim[,3] > 0)
mean(mod_sim[[2]] > 0)
mean(mod_sim[[3]] > 0)

### model1
mod_string1 = " model {
    for (i in 1:length(education)) {
        education[i] ~ dnorm(mu[i], prec)
        mu[i] = b0 + b[1]*income[i] + b[2]*young[i]
    }
    
    b0 ~ dnorm(0.0, 1.0/1.0e6)
    for (i in 1:2) {
        b[i] ~ dnorm(0.0, 1.0/1.0e6)
    }
    
    prec ~ dgamma(1.0/2.0, 1.0*1500.0/2.0)
    	## Initial guess of variance based on overall
    	## variance of education variable. Uses low prior
    	## effective sample size. Technically, this is not
    	## a true 'prior', but it is not very informative.
    sig2 = 1.0 / prec
    sig = sqrt(sig2)
} "

data1_jags = list(education=Anscombe$education, income = Anscombe$income, young = Anscombe$young)

params1 = c("b", "sig")

inits1 = function() {
  inits = list("b"=rnorm(2,0.0,100.0), "prec"=rgamma(1,1.0,1.0))
}

mod1 = jags.model(textConnection(mod_string1), data=data1_jags, inits=inits1, n.chains=3)
update(mod1, 100000) # burn-in

dic.samples(mod1, n.iter=1e5)

### model2
mod_string2 = " model {
    for (i in 1:length(education)) {
        education[i] ~ dnorm(mu[i], prec)
        mu[i] = b0 + b[1]*income[i] + b[2]*young[i] + b[3]*income[i]*young[i]
    }
    
    b0 ~ dnorm(0.0, 1.0/1.0e6)
    for (i in 1:3) {
        b[i] ~ dnorm(0.0, 1.0/1.0e6)
    }
    
    prec ~ dgamma(1.0/2.0, 1.0*1500.0/2.0)
    	## Initial guess of variance based on overall
    	## variance of education variable. Uses low prior
    	## effective sample size. Technically, this is not
    	## a true 'prior', but it is not very informative.
    sig2 = 1.0 / prec
    sig = sqrt(sig2)
} "

data2_jags = list(education=Anscombe$education, income = Anscombe$income, young = Anscombe$young)

params2 = c("b", "sig")

inits2 = function() {
  inits = list("b"=rnorm(3,0.0,100.0), "prec"=rgamma(1,1.0,1.0))
}

mod2 = jags.model(textConnection(mod_string2), data=data2_jags, inits=inits2, n.chains=3)
update(mod1, 100000) # burn-in

dic.samples(mod2, n.iter=1e5)
