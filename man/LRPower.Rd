\name{LRPower}
\alias{LRPower}
\title{Log-rank Test Power Calculation}
\description{This function is used to perform power calculation of the Log-rank test based on simulation datasets, with user specified total sample size (in one simulation), type I error, effect size, total number of simulation datasets, sample size ratio between comparison groups, the death rate in the reference group, and the distribution of follow-up time (negative binomial). 
}
\usage{
LRPower(total.sample.size, type.I.error = 0.05, effect.size=0.6, 
simulation.n=1000,group.sample.size.ratio=1, reference.group.incidence=0.5, 
time.distribution.para=c(100, 0.8))
}
\arguments{
  \item{total.sample.size}{
    Total sample size in this simulation dataset. 
  }
  \item{type.I.error}{
    The significance level for performing the Log-rank test. type.I.error=0.05 by default.
  }
  \item{effect.size}{
    A scalar parameter (between 0 and 1). effect.size=0.2 (small effect size); effect.size=0.5 (median effect size); effect.size=0.8 (large effect size). effect.size=0.6 by default.
  }
  \item{simulation.n}{
    Total number of simulation datasets used for power calculation. simulation.n=1000 by default. 
  }
  \item{group.sample.size.ratio}{
    Ratio between samples of contrast group and the reference or control group. group.sample.size.ratio=1 by default. 
  }
  \item{reference.group.incidence}{
    Incidence of death or event of interest in the reference patient group. reference.group.incidence=0.5 by default. 
  }
    \item{time.distribution.para}{
    A numeric vector of length 2. This vector is used to specify the negative binomial distribution that will be used to simulate follow-up time. The 1st element in this vector is the target for number of successful trials, or dispersion parameter (the shape parameter of the gamma mixing distribution). Must be strictly positive, need not be integer. Then 2nd parameter is the probability of success in each trial. time.distribution.para=c(100, 0.8) by default.  
  }
}
\details{By default, this function will generate \code{simulation.n=1000} simulation datasets to calculate the power of the log-rank test at significance leve of \code{type.I.error = 0.05}. 

The simulation datasets are generated following the steps below: 

1) determine sample size of the reference patient group (group 1) by calculating 

    \code{round(total.sample.size/(group.sample.size.ratio+1))};  

2) determine sample size of the contrast group (group 2) \code{total.sample.size - reference.group.n};

3) generate follow-up time for both the referencee and the contrast group using negative bionomial distribution with user specified parameters \code{time.to.event.distribution.para}; 

4) generate vital status indictor for the reference group by sampling from the bionomial distribution with the number of trials = 1 and the probability of success/death = \code{reference.group.incidence};

5) generate vital status indictor for the contrast group by sampling from the bionomial distribution with the number of trials = 1 and 

    the probability of success/death = \code{reference.group.incidence*(1-effect.size)}; 

The log-rank test p-values are calculated based on the results of \code{survdiff()} with \code{rho=0}.
}
\value{\code{LRPower()} returns a point estimate of the power.}
%% \references{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
%% Manuscript "An Empirical Approach Leveraging Tumorgrafts to Dissect the Tumor Microenvironment in Renal Cell Carcinoma Identifies Missing Link to Prognostic Inflammatory Factors" (recently submitted to Nature Genetics). 
%% }
\examples{
  set.seed(1234)
  
  LRPower(total.sample.size=160, type.I.error = 0.05, effect.size=0.2, 
          simulation.n=1000,group.sample.size.ratio=1, reference.group.incidence=0.5)\donttest{
  LRPower(total.sample.size=160, effect.size=0.5)
  LRPower(total.sample.size=160, effect.size=0.8)
  
  total.sample.size <- 160
  effect.size <- c(1:20)/20
  power <- sapply(effect.size, function(x){
  LRPower(total.sample.size, effect.size=x, simulation.n=100)
  })
  plot(effect.size, power, 
  main=paste("Log-rank Test Power Calculation (n=", total.sample.size,")", sep=""), 
  type="b")}
}