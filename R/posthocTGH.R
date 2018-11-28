#' posthocTGH
#'
#' This function is used by the 'oneway' function for oneway analysis of
#' variance in case a user requests post-hoc tests using the Tukey or
#' Games-Howell methods.
#' This function is a wrapper of userfriendlyscience::posthocTGH
#'  by Gjalt-Jorn Peters (Open University of the Netherlands) & Jeff Bagget
#'  (University of Wisconsin - La Crosse)
#'  Please see userfriendlyscience package.
#'
#' @importFrom stats na.omit pt qt setNames terms
#' @importFrom userfriendlyscience posthocTGH
#'
#' @examples
#' ### Compute post-hoc statistics using the tukey method
#' posthocTGH(y=ChickWeight$weight, x=ChickWeight$Diet, method="tukey")
#' posthocTGH(weight ~ Diet, data = ChickWeight, method="tukey")
#'
#' ### Compute post-hoc statistics using the games-howell method
#' posthocTGH(y=ChickWeight$weight, x=ChickWeight$Diet)
#' posthocTGH(weight ~ Diet, data = ChickWeight)
#'
#' @export
#'
posthocTGH <- function(...) UseMethod("posthocTGH")


#' @rdname posthocTGH
#' @export
#'
#' @param y y has to be a numeric vector.
#' @param x x has to be vector that either is a factor or can be converted into
#' one.
#' @param method Which post-hoc tests to conduct. Valid values are "tukey" and
#' "games-howell".
#' @param conf.level Confidence level of the confidence intervals.
#' @param digits The number of digits to show in the output.
#' @param p.adjust Any valid \code{\link{p.adjust}} method.
#' @param formatPvalue Whether to format the p values according to APA
#' standards (i.e. replace all values lower than .001 with '<.001'). This only
#' applies to the printing of the object, not to the way the p values are
#' stored in the object.
#'
#' @return A list of three elements: \item{input}{List with input arguments}
#' \item{intermediate}{List of intermediate objects.} \item{output}{List with
#' two objects 'tukey' and 'games.howell', containing the outcomes for the
#' respective post-hoc tests.}
#'
posthocTGH.default <- function(y, x, method=c("games-howell", "tukey"),
                               conf.level = 0.95, digits=2,
                               p.adjust="none", formatPvalue = TRUE, ...) {
    userfriendlyscience::posthocTGH(y = y, x = x, method = method,
                               conf.level = conf.level, digits = digits,
                               p.adjust = p.adjust, formatPvalue = formatPvalue)
}


#' @rdname posthocTGH
#' @export
#'
#' @param formula A formula specifying the model.
#' @param data an optional matrix or data frame (or similar: see
#' \code{\link{model.frame}}) containing the variables in the formula
#' \code{formula}.  By default the variables are taken from
#' \code{environment(formula)}.
#' @param subset an optional vector specifying a subset of observations
#' to be used.
#' @param na.action a function which indicates what should happen when the data
#' contain \code{NA}s. Defaults to \code{getOption("na.action")}.
#' @param \dots further arguments to be passed to or from methods
#' (this argument is only for formula).
#'
posthocTGH.formula <- function(formula, data, subset, na.action, ...)
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
    DATA <- list(mf[,1], mf[,2])
    y <- do.call("posthocTGH", c(DATA, list(...)))
    y
}

