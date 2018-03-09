#' Rasch Model Prior
#'
#' Calculates the height of the normal curve given a theta value
#'
#' @param theta A numeric object 
#' 
#' @return A numeric
#'  \item{height}{The height of the normal curve at a proposed theta}
#' @author Kalen Davison: \email{davison@wustl.edu}
#' @note This is a very simple function
#' @examples
#' 
#' theta = 1.2
#' Prior(theta)
#' @seealso \code{\link{Likelihood}}
#' @rdname Prior
#' @aliases Print
#' @export
setGeneric("Prior", 
           function(theta="numeric") {
             standardGeneric("Prior")
           } )

setMethod(f = "Prior",
          definition = function(theta){
            height = dnorm(x = theta, mean = 0, sd = 3) #calculates height of normal curve at theta 
            return(height)
          }
            )
