#' Creates a Rasch Class
#'
#' 
#'
#' @param name A character object
#' @param difficulty A numeric vector 
#' @param answer A numeric vector
#'
#' @return Creates a Rasch Class
#' @author Kalen Davison: \email{davison@wustl.edu}
#' @note Creates Simpson Rasch Class with 3 slots
#' @examples
#' 

#' @seealso \code{\link{Prior}}
#' @rdname raschClass
#' @aliases EAP
#' @export
setClass(Class = "Rasch",
         representation = representation(
           name = "character",
           difficulty = "numeric",
           answers = "numeric"
         ),
         prototype = prototype(
           name = c(),
           difficulty = c(),
           answers = c()
         )
)

#' @export
setValidity("Rasch", function(object){ 
  nameLength = (length(object@name) == 1)
  questionLength = (length(object@difficulty) == length(object@answers))
  
  if(!questionLength){
    return("Rasch is not valid! Difficulty and answers must be of the same length.")
  } 
  if (!nameLength){
    return("Rasch is not valid! Only one name can be inputted.")
  }
  
})

#' @export
setMethod("initialize", "Rasch", function(.Object, ...) {  
  value = callNextMethod()
  validObject(value)
  return(value)
})
