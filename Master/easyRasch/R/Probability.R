#' Rasch Model Probability
#'
#' Calculates probability using the Rasch formula
#'
#' @param raschObj an object of class Rasch
#' @param theta A numeric object 
#' 
#' @return A list of two vectors
#'  \item{P}{The probability of each person answering the question correctly}
#'  \item{PQ}{The probability of each person answering correctly if they got it right, or the probability of each person answering incorrectly if they got it wrong} 
#' @author Kalen Davison: \email{davison@wustl.edu}
#' @note Theta value can be manually chosen.
#' @examples
#' 
#' testRasch = new("Rasch", name = "Benny", difficulty = 1:10, answers = sample(0:1, 10, replace=TRUE))
#' Probability(raschObj = thing, theta = .5)
#' @seealso \code{\link{raschClass}}
#' @rdname Probability
#' @aliases Print
#' @export
setGeneric("Probability", 
           function(raschObj = "Rasch", theta = "numeric") {
             standardGeneric("Probability")
           } )

setMethod(f="Probability", c("Rasch", "numeric"),
  definition = function(raschObj, theta){
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
    if (raschObj@answers[i] == 0){
      wrong = 1-(exp(theta-raschObj@difficulty[i]))/(1+(theta-raschObj@difficulty[i]))
      PQ[i] = wrong
    }
  }
  return(list(P, PQ))
  }
)
