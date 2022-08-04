
### Here's a little analysis exercise for fun (based on the territory size idea we chatted about):

################################################################################
### Load Packages:  ############################################################
################################################################################

library(tidyverse)
library(dplyr)
library(ggplot2)

################################################################################
### Simulate a population under Ideal Free Distribution:  ######################
################################################################################

### Set territory parameters:
pine.ts = 2.5
oak.ts = 1.2

pine.ts.sd = 0.3
oak.ts.sd = 0.3

### Set reproductive success parameters (for an OK year):
pine.fldg = 1.3
oak.fldg = 1.3

pine.fldg = 1.3
oak.fldg = 1.3

### Set frequency of good vs. poor years for reproductive success:
pPoor = 0.3; pGood = 0.1; pOK = 1-pPoor-pGood

### Simulate true data for the population: 
n.index = 200
IDFpopulation = data.frame(index = numeric(), 
                   habitat = character(),
                   territoryID = character(),
                   territory_size = numeric(),
                   num_fldgs = numeric())
for(i in 1:n.index){
  
  # Sample number of territories for year i
  pine.nts = sample(x=15:30, size=1) 
  oak.nts = sample(25:40, 1)
  
  # Is it a good year for breeding?
  rain.effect = sample(x=c(-1, 0, 1), size=1, prob=c(0.3, 0.5, 0.1) ) 
  
  # Simulate pine territory sizes and number of fledglings for year i:
  p.ts = p.fldg = numeric(pine.nts)
  for(p in 1:pine.nts){
    p.ts[p] = rnorm(1, mean=pine.ts, sd=pine.ts.sd)
    p.fldg[p] = rpois(1, pine.fldg+rain.effect)
  } #p
  pine.i = data.frame(index = i, 
                      habitat = "Pine", 
                      territoryID = paste0("P-", 1:pine.nts),
                      territory_size = p.ts,
                      num_fldgs = p.fldg)
  
  # Simulate oak territory sizes and number of fledglings for year i:
  o.ts = o.fldg = numeric(oak.nts)
  for(o in 1:oak.nts){
    o.ts[o] = rnorm(1, mean=oak.ts, sd=oak.ts.sd)
    o.fldg[o] = rpois(1, oak.fldg+rain.effect)
  } #o

  oak.i = data.frame(index = i, 
                      habitat = "Oak", 
                      territoryID = paste0("O-", 1:oak.nts),
                      territory_size = o.ts,
                      num_fldgs = o.fldg)
  
  # Add data for year i into larger dataset:
  IDFpopulation = bind_rows(IDFpopulation, pine.i, oak.i)
}# i


### Sample a limited number of years to create an actual data set: ##############

n.years = 15
i.samples = sample(1:n.index, n.years)
data = IDFpopulation %>% filter(index %in% i.samples)

data$year = as.factor(data$index) %>% as.numeric()
data$year = data$year + 2022 - n.years
data = data[,-1]

summary(data)

################################################################################
### Make some plots:  ##########################################################
################################################################################
