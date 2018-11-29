#' permuted Brunner-Munzel test
#'
#' This function performs the permuted Brunner-Munzel test.
#'
#' @return A list containing the following components:
#'  \item{method}{the characters ``Brunner-Munzel Permutation Test''}
#'  \item{p.value}{the \eqn{p}-value of the test.}
#'  \item{data.name}{a character string giving the name of the data.}
#'
#' @references Karin Neubert and Edgar Brunner, ``A studentized permutation test for the non-parametric Behrens-Fisher problem'', Computational Statistics and Data Analysis, Vol. 51, pp. 5192-5204 (2007).
#'
#' @seealso This function is made in reference to following cites (in Japanese): Prof. Haruhiko Okumura \url{https://oku.edu.mie-u.ac.jp/~okumura/stat/brunner-munzel.html} and Anonymous \url{https://blog.goo.ne.jp/r-de-r/e/83dc811baf41ecfe469fa794a4c51b84?fm=rss&utm_medium=twitter&utm_source=twitterfeed}
#'
#' @examples
#' Y <-  c(1,2,1,3,1,2,2,4,1,1)
#' N <-  c(3,4,5,2,3,2,1,5,4,3)
#'
#' dat <- data.frame(
#'     value = c(Y, N),
#'     group = factor(rep(c("Y", "N"), c(length(Y), length(N))),
#'                    levels = c("Y", "N"))
#' )
#'
#' \dontrun{
#' brunner.munzel.permutation.test(Y, N)
#' brunner.munzel.permutation.test(value ~ group, data = dat)
#' }
#'
#' @export
#'
brunner.munzel.permutation.test <-  function(x, ...)
    UseMethod("brunner.munzel.permutation.test")


#' @rdname brunner.munzel.permutation.test
#' @method brunner.munzel.permutation.test default
#'
#' @importFrom utils combn
#'
#' @param x the numeric vector of data values from the sample 1.
#' @param y the numeric vector of data values from the sample 2.
#' @param alternative a character string specifying the alternative
#' hypothesis, must be one of \code{"two.sided"} (default), \code{"greater"} or
#' \code{"less"}. User can specify just the initial letter.
#'
#' @export
#'
brunner.munzel.permutation.test.default <-
    function(x, y,
             alternative = c("two.sided", "greater", "less"),
             ...) {
        alternative <- match.arg(alternative)
        DNAME <-  paste(deparse(substitute(x)), "and",
                        deparse(substitute(y)))

        BM <- function(X) {
            x <-  xandy[X]
            y <-  xandy[-X]
            r1 <-  rank(x)
            r2 <-  rank(y)
            r <-  rank(c(x, y))
            m1 <-  mean(r[1:n1])
            m2 <-  mean(r[n1 + 1:n2])
            v1 <-  sum((r[1:n1] - r1 - m1 + (n1 + 1)/2)^2)/(n1 - 1)
            v2 <-  sum((r[n1 + 1:n2] - r2 - m2 + (n2 + 1)/2)^2)/(n2 - 1)
            (m2 - m1)/sqrt(n1 * v1 + n2 * v2)
        }

        x <- na.omit(x)
        y <-  na.omit(y)
        n1 <-  length(x)
        n2 <-  length(y)
        xandy <-  c(x, y)

        z0 <- BM(1:n1)              # provided data
        z1 <- combn(n1+n2, n1, BM)  # permutation

        p.value <- switch(alternative,
                          "two.sided" = mean(abs(z1) >= abs(z0)),
                          "greater" = mean(z1 <= z0),
                          "less" = mean(z1 >= z0))

        structure(
            list(
                method = "Brunner-Munzel Permutation Test",
                p.value = p.value,
                data.name = DNAME),
            class = "htest")
    }


#' @rdname brunner.munzel.permutation.test
#' @method brunner.munzel.permutation.test formula
#'
#' @param formula a formula of the form \code{lhs ~ rhs} where \code{lhs}
#' is a numeric variable giving the data values and \code{rhs} a factor
#' with two levels giving the corresponding groups.
#' @param data an optional matrix or data frame (or similar: see
#' \code{\link{model.frame}}) containing the variables in the
#' formula \code{formula}.  By default the variables are taken from
#' \code{environment(formula)}.
#' @param subset an optional vector specifying a subset of observations
#' to be used.
#' @param na.action a function which indicates what should happen when
#' the data contain \code{NA}s.  Defaults to
#' \code{getOption("na.action")}.
#' @param \dots further arguments to be passed to or from methods
#' (This argument is for only formula).
#'
#' @export
#'
brunner.munzel.permutation.test.formula <-
    function(formula, data, subset, na.action, ...)
{
    if(missing(formula)
       || (length(formula) != 3L)
       || (length(attr(terms(formula[-2L]), "term.labels")) != 1L))
        stop("'formula' missing or incorrect")
    m <- match.call(expand.dots = FALSE)
    if(is.matrix(eval(m$data, parent.frame())))
        m$data <- as.data.frame(data)
    ## need stats:: for non-standard evaluation
    m[[1L]] <- quote(stats::model.frame)
    m$... <- NULL
    mf <- eval(m, parent.frame())
    DNAME <- paste(names(mf), collapse = " by ")
    names(mf) <- NULL
    response <- attr(attr(mf, "terms"), "response")
    g <- factor(mf[[-response]])
    if(nlevels(g) != 2L)
        stop("grouping factor must have exactly 2 levels")
    DATA <- setNames(split(mf[[response]], g), c("x", "y"))
    y <- do.call("brunner.munzel.permutation.test", c(DATA, list(...)))
    y$data.name <- DNAME
    y
}

