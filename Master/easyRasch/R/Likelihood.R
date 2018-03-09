#' Rasch Model Likelihood
#'
#' Calculates the Likelihood of a proposed "ability" given the subjects answers
#'
#' @param raschObj an object of class Rasch
#' @param theta A numeric object 
#' 
#' @return A numeric
#'  \item{likelihood}{The likelihood of a respond having a proposed ability score given their answers}
#' @author Kalen Davison: \email{davison@wustl.edu}
#' @note Theta value can be manually chosen.
#' @examples
#' 
#' testRasch = new("Rasch", name = "Benny", difficulty = 1:10, answers = sample(0:1, 10, replace=TRUE))
#' Likelihood(raschObj = thing, theta = .5)
#' @seealso \code{\link{raschClass}}
#' @rdname Likelihood
#' @aliases Print
#' @export
setGeneric("Likelihood", 
           function(raschObj = "Rasch", theta = "numeric") {
             standardGeneric("Likelihood")
           } )

setMethod(f = "Likelihood",
          definition = function(raschObj = "Rasch", theta = "numeric"){
            PQ = Probability(raschObj, theta)[[2]]
            
            for (i in length(raschObj@difficulty)){
              if (raschObj@answers[i] == 1){
                PQ[i] = (PQ[i]^(raschObj@answers[i]))
              }
              else {
                PQ[i] = (PQ[i]^(1-raschObj@answers[i]))
              }
            }
            likelihood = prod(PQ)
            return(likelihood)
          }
            )
