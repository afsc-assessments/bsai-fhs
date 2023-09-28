## Quick MCMC run to check for weirdness
library(adnuts)

file.copy(list.files('Run06_francis_tuning/', full.names=TRUE), 'rwm')

tt <- 100
fit <- sample_rwm('ss', 'rwm', iter=1000*tt, warmup=250*tt,
                  thin=tt, chains=5)
saveRDS(fit, file='fit.rwm.RDS')
pairs_admb(fit, pars=1:6, order='slow')
pairs_admb(fit, pars=1:4, order='fast')



file.copy(list.files('Run06_francis_tuning/', full.names=TRUE), 'nuts')
setwd('nuts')
system('ss -nox -hbf')
setwd('..')

td <- 12
fit.nuts <- sample_nuts('ss', 'nuts', iter=750, warmup=250,
                        chains=3, control=list(max_treedepth=td))
pairs_admb(fit.nuts, pars=1:6, order='slow')
pairs_admb(fit.nuts, pars=1:6, order='fast')
plot_sampler_params(fit.nuts)
