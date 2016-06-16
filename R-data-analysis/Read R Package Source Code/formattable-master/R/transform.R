#' Vectorized map from element to case by index or string value
#'
#' This function is a vectorized version of \code{switch}, that is, for
#' each element of input vector, \code{switch} is evaluated and the results are
#' combined.
#'
#' @param EXPR an expression evaluated to be character or numeric vector/list.
#' @param ... The list of alternatives for each \code{switch}.
#' @param SIMPLIFY \code{TRUE} to simplify the resulted list to vector, matrix
#' or array if possible.
#' @seealso \code{\link{switch}}
#' @export
#' @examples
#' x <- c("normal","normal","error","unknown","unknown")
#' vmap(x, normal = 0, error = -1, unknown = -2)
#'
#' x <- c(1,1,2,1,2,2,1,1,2)
#' vmap(x, "type-A", "type-B")

# Done!
vmap1 <- function(EXPR1, ..., SIMPLIFY = TRUE) {

  if (is.factor(EXPR1)) EXPR1 <- as.character.factor(EXPR1)
  #res <- lapply(EXPR, switch, ...) # 这一写的牛逼！利用lapply将...传来的值传到switch中。
  print(paste(EXPR1))
  for(ch in EXPR1)
    cat(ch,":", switch(EXPR = ch, ...), "\n")
#   if (SIMPLIFY) simplify2array(res) else res
}

x <- c("normal","normal","error","unknown","unknown")
# ... -> normal = 0, error = -1, unknown = -2
vmap1(x,  error = -1, normal = 10,unknown = -2)

#' Quantile ranks of a vector
#'
#' The quantile rank of a number in a vector is the relative
#' position of ranking resulted from rank divided by the length
#' of vector.
#' @param x a vector
#' @param ... additional parameters passed to \code{rank}
#' @seealso \code{\link{rank}}
#' @export
#' @examples
#' qrank(mtcars$mpg)
qrank <- function(x, ...) {
  rank(x = x, ...) / length(x)
}

rank -- 样本排名

#' Normalize a vector to fit zero-to-one scale
#'
#' @param x a numeric vector
#' @param min numeric value. The lower bound of the interval to normalize \code{x}.
#' @param max numeric value. The upper bound of the interval to normalize \code{x}.
#' @param na.rm a logical indicating whether missing values should be removed
#' @export
#' @examples
#' normalize(mtcars$mpg)
normalize <- function(x, min = 0, max = 1, na.rm = FALSE) {
  if (all(is.na(x))) return(rep(0, length(x)))
  if (!is.numeric(x)) stop("x must be numeric")
  x <- unclass(x)
  if (min > max) stop("min <= max must be satisfied")
  if (all(x == 0, na.rm = na.rm)) return(x)
  xmax <- max(x, na.rm = na.rm)
  xmin <- min(x, na.rm = na.rm)
  if (xmax == xmin) return(rep(1, length(x)))
  min + (max - min) * (x - xmin) / (xmax - xmin)
}

#' Rescale a vector relative to the maximal absolute value in the vector
#'
#' @param x a numeric vector
#' @param na.rm a logical indicating whether missing values should be removed
#' @export
#' @examples
#' proportion(mtcars$mpg)
proportion <- function(x, na.rm = FALSE) {
  x / max(abs(x), na.rm = na.rm)
}
