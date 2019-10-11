# library(survival)
# library(survminer)
LRPower <- function(total.sample.size, type.I.error = 0.05, effect.size=0.6, simulation.n=1000,group.sample.size.ratio=1, 
                    reference.group.incidence=0.5, time.distribution.para=c(100, 0.8)){
  ## need to simulation 1000 simulation and do logrank test 1000 times and calculate 1-type II error (if H1 is ture but conclude H0)
  ## so power is the proportion of p>0.05
  simulation.p <- sapply(1:simulation.n, function(x){
    tryCatch({
      reference.group.n <- round(total.sample.size/(group.sample.size.ratio+1))
      contrast.group.n <- total.sample.size - reference.group.n
      
      nb.size <- time.distribution.para[1]
      nb.prob <- time.distribution.para[2]
      reference.group.time <- rnbinom(reference.group.n, nb.size, nb.prob) ## negative binomial sample
      reference.group.event.indicator <- rbinom(reference.group.n, 1,reference.group.incidence) 
      reference.data <- data.frame(time=reference.group.time, event=reference.group.event.indicator, group="group 1")
      
      contrast.group.time <- rnbinom(reference.group.n, nb.size, nb.prob)
      contrast.group.event.indicator <- rbinom(reference.group.n, 1,reference.group.incidence*(1-effect.size))
      contrast.data <- data.frame(time=contrast.group.time, event=contrast.group.event.indicator, group="group 2")
      
      surv.data <- rbind(reference.data, contrast.data)
      surv.data$SurvObj <- with(surv.data, Surv(time, event))
      
      tmp.model <- survdiff(surv.data$SurvObj ~ surv.data$group) #  With rho = 0 (default) this is the log-rank test
      return(1 - pchisq(tmp.model$chisq, 1))},
      error=function(e){return(NA)})
  })
  return(length(which(na.omit(simulation.p)<=0.05))/simulation.n) ## return power
}
