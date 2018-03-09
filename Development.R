### Set up packages
rm(list=ls())
library(devtools)
library(roxygen2)
package.skeleton()

### Function development

# Probability function
Probability = function(raschObj, theta){
  P = vector("numeric", length(raschObj@answers))
  PQ = vector("numeric", length(raschObj@answers)) #create two blank vectors, one for each output
  
  for (i in 1:length(raschObj@answers)){
    prob = (exp(theta-raschObj@difficulty[i]))/(1+(theta-raschObj@difficulty[i])) #calculates prob not taking into account answer
    P[i] = prob
  }
  
  for (i in 1:length(raschObj@answers)){
    if (raschObj@answers[i] == 1){ #calculate prob given answer is correct
      right = (exp(theta-raschObj@difficulty[i]))/(1+(theta-raschObj@difficulty[i]))
      PQ[i] = right
    }
    else { #calculate prob given answer is not correct
      wrong = 1-(exp(theta-raschObj@difficulty[i]))/(1+(theta-raschObj@difficulty[i]))
      PQ[i] = wrong
    }
  }
  return(list(P, PQ))
}

# Likelihood function
Likelihood = function(raschObj, theta){
  PQ = Probability(raschObj, theta)[[2]] #gets only the second list from Probability function
  
  for (i in length(raschObj@difficulty)){ 
    if (raschObj@answers[i] == 1){ #if answer is right
      PQ[i] = (PQ[i]^(raschObj@answers[i]))
    }
    else { #if answer is wrong
      PQ[i] = (PQ[i]^(1-raschObj@answers[i])) #calculates probabilities given respondent's answers
    }
  }
  likelihood = prod(PQ) #takes product of all the numbers in the vector
  return(likelihood)
}

# Prior function
Prior <- function(theta){
  height <- dnorm(x = theta, mean = 0, sd = 3)
  return(height)
}

#EAP function
EAP = function(raschObj, lower = 6, upper = 6){
  
  num = function(raschObj, theta){
    n = theta * Likelihood(raschObj, theta) * Prior(theta)
    return(num)
  }
  
  denom = function(raschObj, theta){
    d = Likelihood(raschObj, theta) * Prior(theta)
    return(denom)
  }
  
  numerator = integrate(num, lower = lower, upper = upper, raschObj)
  denominator = integrate(denom, lower = lower, upper = upper, raschObj)
  ability_estimate = numerator/denominator
  return(ability_estimate)
}

EAP(testSubject) #do not understand why it isn't working

# Print method embedded in S4 package structure

#Build and check out the package
setwd("/Users/kalendavison/Desktop/Applied Statistical Programming/GitHub/Poli-Sci-5625-Midterm/Master") #directory above package
current.code<-as.package("easyRasch")
load_all(current.code)
document(current.code)
check(current.code)

?Likelihood
?Probability
?EAP #error says rdFile must be a single element character vector?

## Examples
testSubject = new("Rasch", name = "Benny", difficulty = sample(c(.2, .4, 1.7, 1.3, 1.6, 1.6, 5.6), 10, replace = TRUE), answers = sample(0:1, 10, replace=TRUE))
Probability(raschObj = testSubject, theta = 2) 
# probability may not make sense in context because I don't know what the range of theta and difficulty is supposed to be

Likelihood(testSubject, .28) 
#seems to work but again doesn't make sense in context because I'm not sure what scale the difference and theta should be on. 

Prior(theta = 1)

EAP(testSubject) 
# I can't figure out what the error message means so it's not running, but it seems like it should work.

print(testSubject)
