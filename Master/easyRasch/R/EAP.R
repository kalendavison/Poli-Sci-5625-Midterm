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
setGeneric("EAP", #create a generic in S4
           function(raschObj = "Rasch", lower = "numeric", upper = "numeric") {
             standardGeneric("EAP")
           } )

setMethod("EAP", 
          definition = function(raschObj, lower = 6, upper = 6){
            num = function(raschObj, theta){ #create the function in the numerator
              n = theta * Likelihood(raschObj, theta) * Prior(theta)
              return(num)
            }
            
            denom = function(raschObj, theta){ #create the function in the denominator
              d = Likelihood(raschObj, theta) * Prior(theta)
              return(denom)
            }
            
            #integrate numerator over theta passing through raschObj
            numerator = integrate(num, lower = lower, upper = upper, raschObj, stop.on.error = FALSE) 
           
             #integrate denominator over theta passing through raschObj
            denominator = integrate(denom, lower = lower, upper = upper, raschObj, stop.on.error = FALSE) 
            ability_estimate = numerator/denominator #divide to get final estimate
            return(ability_estimate)
          }
)

          