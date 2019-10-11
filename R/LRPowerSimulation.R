# library(survival)
# library(survminer)
LRPowerSimulation <- function(total.sample.size, effect.size=0.6, group.sample.size.ratio=1, 
                    reference.group.incidence=0.5, time.distribution.para=c(100, 0.8)){

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
      
  return(surv.data) ## return 1 simulation dataset based on the assume parameters
}
