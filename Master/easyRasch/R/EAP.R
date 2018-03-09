#' Rasch Model Ability Estimation
#'
#' Estimates the ability of the subject
#'
#' @param raschObj an object of class Rasch
#' @param theta A numeric object 
#' 
#' @return A numeric
#'  \item{ability_estimate}{An estimate of the subject's ability}
#' @author Kalen Davison: \email{davison@wustl.edu}
#' @note I do not really understand the math here and am likely to have done this wrong.
#' @examples
#' 
#' @seealso \code{\link{raschClass}}
#' @rdname EAP
#' @aliases Print
#' @export
setGeneric("EAP", 
           function(raschObj = "Rasch", lower = "numeric", upper = "numeric") {
             standardGeneric("EAP")
           } )

setMethod("EAP", 
          definition = function(raschObj, lower = 6, upper = 6){
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
)

          