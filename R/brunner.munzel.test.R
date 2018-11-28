#' brunner.munzel.test
#'
#' This function performs the Brunner--Munzel test for stochastic
#' equality of two samples, which is also known as the Generalized Wilcoxon
#' Test. \code{NA}s from the data are omitted.
#' This function is a wrapper of lawstat::brunner.munzel.test
#'  by Vyacheslav Lyubchich et al.
#'  Please see lawstat package.
#'
#' @return A list containing the following components:
#'  \item{statistic}{the Brunner--Munzel test statistic.}
#'  \item{parameter}{the degrees of freedom.}
#'  \item{conf.int}{the confidence interval.}
#'  \item{p.value}{the \eqn{p}-value of the test.}
#'  \item{data.name}{a character string giving the name of the data.}
#'  \item{estimate}{an estimate of the effect size, i.e., \eqn{P(X < Y) + 0.5 * P(X =Y )}}
#'
#' @note There exist discrepancies with Brunner and Munzel (2000)
#'  because there is a typo in the paper. The corrected version is
#'  in Neubert and Brunner (2007) (e.g., compare the estimates for
#'  the case study on pain scores).
#' The current R function follows Neubert and Brunner (2007).
#'
#'@examples
#' 
#' ## Pain score on the third day after surgery for 14 patients under
#' ## the treatment Y and 11 patients under the treatment N
#' ## (see Brunner and Munzel, 2000; Neubert and Brunner, 2007).
#' 
#' Y <- c(1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 2, 4, 1, 1)
#' N <- c(3, 3, 4, 3, 1, 2, 3, 1, 1, 5, 4)
#' 
#' brunner.munzel.test(Y, N)
#'
#' ##       Brunner-Munzel Test
#' ## data: Y and N
#' ## Brunner-Munzel Test Statistic = 3.1375,  df = 17.683, p-value = 0.005786
#' ## 95 percent confidence interval:
#' ##  0.5952169 0.9827052
#' ## sample estimates:
#' ## P(X<Y)+.5*P(X=Y)
#' ##        0.788961
#' 
#' 
#' ## Formula interface.
#' dat <- data.frame(
#'     value = c(Y, N),
#'     group = rep(c("Y", "N"), c(length(Y), length(N)))
#' )
#' 
#' brunner.munzel.test(value ~ group, data = dat)
#' 
#' ##       Brunner-Munzel Test
#' ## data:  value by group
#' ## Brunner-Munzel Test Statistic = -3.1375, df = 17.683, p-value =
#' ## 0.005786
#' ## 95 percent confidence interval:
#' ##  0.01729479 0.40478314
#' ## sample estimates:
#' ## P(X<Y)+.5*P(X=Y)
#' ##         0.211039
#' 
#' ## The same p-value is obtained.
#'
#' @export
brunner.munzel.test <- function(x, ...) UseMethod("brunner.munzel.test")

#' @rdname brunner.munzel.test
#' @method brunner.munzel.test default
#' @importFrom lawstat brunner.munzel.test
#'
#' @param x the numeric vector of data values from the sample 1.
#' @param y the numeric vector of data values from the sample 2.
#' @param alpha significance level, default is 0.05 for 95\% confidence
#' interval.
#' @param alternative a character string specifying the alternative
#' hypothesis, must be one of \code{"two.sided"} (default), \code{"greater"} or
#' \code{"less"}. User can specify just the initial letter.
#'
#' @export
#'
brunner.munzel.test.default <- function (x, y,
                                         alternative = c("two.sided", "greater", "less"),
                                         alpha = 0.05, ...) {
    lawstat::brunner.munzel.test(x, y, alternative, alpha)
}


#' @rdname brunner.munzel.test
#' @method brunner.munzel.test formula
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
brunner.munzel.test.formula <-
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
    y <- do.call("brunner.munzel.test", c(DATA, list(...)))
    y$data.name <- DNAME
    y
}
