#' Return positions of items in a [character] efficiently
#'
#' First use index elements, then rapidly retrieve the positions.
#'
#' @param items [character] with elements to find position
#' @param collection [character] where to search
#' @return [integer] with position. May be shorter than `items` if one or more elements are not found in `collection`
#' @export
"%where%" <- function(items, collection) {
  p_index <- attr(x = collection, which = "index", exact = TRUE)
  if (is.null(p_index)) {
    variable <- deparse(substitute(collection))
    p_index <- new(projector, row_names_r = collection)
    attr(x = collection, which = "index") <- p_index
    assign(variable, collection, envir = parent.frame())
  }
  p_index$get_position(items)
}

