#' Prints the name of the test taker and the EAP results
#'
#' 
#'
#' @param x An object of class Rasch
#'
#' @return A character object
#'   \item{name}{The subject's name}
#' @return A numeric
#'   \item{ability_estimate}{An estimate of the subject's ability}
#' @author Kalen Davison: \email{davison@wustl.edu}
#' @note Should return both the name and the estimated ability level (theta)
#' @examples
#' 

#' @seealso \code{\link{EAP}}
#' @rdname printfunction
#' @aliases EAP
#' @export

setMethod("print", "Rasch",
          function(x){
            name = x@name
            ability_estimate = EAP(x, 6, 6)
            print(c(name, ability_estimate))
            }
)

