\name{LRPowerSimulation}
\alias{LRPowerSimulation}
\title{Data Simulation for Log-rank Test Power Calculation}
\description{This function is used to generate 1 simulation dataset for power calculation of the Log-rank test, with user specified total sample size, effect size, sample size ratio between comparison groups, the death rate in the reference group, and the distribution of follow-up time (negative binomial). 
}
\usage{
LRPowerSimulation(total.sample.size, effect.size=0.6, group.sample.size.ratio=1, 
reference.group.incidence=0.5, time.distribution.para=c(100, 0.8))
}
\arguments{
  \item{total.sample.size}{
    Total sample size in this simulation dataset. 
  }
  \item{effect.size}{
    A scalar parameter (between 0 and 1). effect.size=0.2 (small effect size); effect.size=0.5 (median effect size); effect.size=0.8 (large effect size). effect.size=0.6 by default. 
  }
  \item{group.sample.size.ratio}{
    Ratio between samples of contrast group and the reference or control group. group.sample.size.ratio=1 by default. 
  }
  \item{reference.group.incidence}{
    The incidence of death or event of interest in the reference patient group. reference.group.incidence=0.5 by default. 
  }
    \item{time.distribution.para}{
    A numeric vector of length 2. This vector is used to specify the negative binomial distribution that will be used to simulate follow-up time. The 1st element in this vector is the target for number of successful trials, or dispersion parameter (the shape parameter of the gamma mixing distribution). Must be strictly positive, need not be integer. Then 2nd parameter is the probability of success in each trial. time.distribution.para=c(100, 0.8) by default.  
  }
}
\details{This function is used to generate one simulation dataset following the steps below: 

1) determine sample size of the reference patient group (group 1) by calculating 

    \code{round(total.sample.size/(group.sample.size.ratio+1))};  

2) determine sample size of the contrast group (group 2) \code{total.sample.size - reference.group.n}; 

3) generate follow-up time for both the referencee and the contrast group using negative bionomial distribution with user specified parameters \code{time.to.event.distribution.para}; 

4) generate vital status indictor for the reference group by sampling from the bionomial distribution with the number of trials = 1 and the probability of success/death = \code{reference.group.incidence}; 

5) generate vital status indictor for the contrast group by sampling from the bionomial distribution with the number of trials = 1 and 

    the probability of success/death = \code{reference.group.incidence*(1-effect.size)}. 
}
\value{\code{LRPowerSimulation()} returns a data frame of size \code{total.sample.size} by 4. The 1st column is the follow-up time. The 2nd column is the vital status (death=1). The 3rd column indicates the comparison groups. The 4th column is an object of class \code{Surv}.
}
%% \references{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
%% Manuscript "An Empirical Approach Leveraging Tumorgrafts to Dissect the Tumor Microenvironment in Renal Cell Carcinoma Identifies Missing Link to Prognostic Inflammatory Factors" (recently submitted to Nature Genetics). 
%% }
\examples{
  %%load(system.file("example/example_data.RData",package="DisHet"))
  set.seed(1234)
  surv.data <- LRPowerSimulation(total.sample.size=160, effect.size=0.5, 
  group.sample.size.ratio=1, reference.group.incidence=0.5)
  ggsurvplot(survfit(SurvObj ~ group, surv.data), surv.median.line = "hv", 
            conf.int = TRUE, pval=TRUE)
}