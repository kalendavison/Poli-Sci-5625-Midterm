### Set up packages
rm(list=ls())
library(devtools)
library(roxygen2)

#Function development

testSubject = new("Rasch", name = "Benny", difficulty = 1:10, answers = sample(0:1, 10, replace=TRUE))
testSubject

# Probability function
Probability = function(raschObj, theta){
  P = vector("numeric", length(raschObj@answers))
  PQ = vector("numeric", length(raschObj@answers))
  
  for (i in 1:length(raschObj@answers)){
    prob = (exp(theta-raschObj@difficulty[i]))/(1+(theta-raschObj@difficulty[i]))
    P[i] = prob
  }
  
  for (i in 1:length(raschObj@answers)){
    if (raschObj@answers[i] == 1){
      right = (exp(theta-raschObj@difficulty[i]))/(1+(theta-raschObj@difficulty[i]))
      PQ[i] = right
    }
    else {
      wrong = 1-(exp(theta-raschObj@difficulty[i]))/(1+(theta-raschObj@difficulty[i]))
      PQ[i] = wrong
    }
  }
  return(list(P, PQ))
}

Probability(raschObj = thing, theta = .5)

#Build and check out the package
package.skeleton()
setwd("/Users/kalendavison/Desktop/Applied Statistical Programming/GitHub/Poli-Sci-5625-Midterm/Master") #directory above package
current.code<-as.package("easyRasch")
load_all(current.code)
document(current.code)
check(current.code)