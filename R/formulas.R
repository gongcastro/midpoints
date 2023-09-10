#' Update model formula to exclude `x` predictor
#'
#' Internal function that removes any predictor in which `x` (the variable for which to compute the mid-point of the logistic curve) is involved (main effects and interactions). The resulting formula will be used to compute the linear component of the numerator in the mid-point formula. For more details see [use_formula()].
#' @param object An object of class 'glm' fitted with `family = logistic("logit")`.
#' @param x String character indicating them name of the variable or which to compute the mid-point of the logistic curve.
#' @author Gonzalo Garcia-Castro
#' @md
update_formula.glm <- function(object, x)
{
    # check that class, family, and link are correct
    stopifnot("glm" %in% class(object))
    stopifnot(family(object)$family == "binomial")
    stopifnot(family(object)$link == "logit")

    form <- formula(object)
    form.terms <- attr(terms(form), "term.labels")
    form.terms.new <- form.terms[!grepl(x, form.terms)]
    form.new <- reformulate(form.terms.new)

    return(form.new)
}

#' Update model formula to exclude `x` predictor
#'
#' Internal function that removes any predictor in which `x` (the variable for which to compute the mid-point of the logistic curve) is involved (main effects and interactions). The resulting formula will be used to compute the linear component of the numerator in the mid-point formula. For more details see [use_formula()].
#' @param object An object of class 'glmerMod', fitted with `family = logistic("logit")`.
#' @param x String character indicating them name of the variable or which to compute the mid-point of the logistic curve.
#' @author Gonzalo Garcia-Castro
#' @md
update_formula.merMod <- function(object, x)
{
    # check that class, family, and link are correct
    stopifnot("glmerMod" %in% class(object))
    stopifnot(family(object)$family == "binomial")
    stopifnot(family(object)$link == "logit")

    form <- formula(object)
    form.terms <- attr(terms(form), "term.labels")
    form.terms.new <- form.terms[!grepl(x, form.terms)]
    form.new <- reformulate(form.terms.new)

    return(form.new)
}

#' Update model formula to exclude `x` predictor
#'
#' Internal function that removes any predictor in which `x` (the variable for which to compute the mid-point of the logistic curve) is involved (main effects and interactions). The resulting formula will be used to compute the linear component of the numerator in the mid-point formula. For more details see [use_formula()].
#' @param object An object of class 'brmsfit', fitted with `family = logistic("logit")` or `family = bernoulli("logit")`.
#' @param x String character indicating them name of the variable or which to compute the mid-point of the logistic curve.
#' @importFrom brms is.brmsfit
#' @author Gonzalo Garcia-Castro
#' @md
update_formula.brmsfit <- function(object, x)
{
    # check that class, family, and link are correct
    stopifnot(brms::is.brmsfit(object))
    stopifnot(family(object)$family %in% c("binomial", "bernoulli"))
    stopifnot(family(object)$link == "logit")

    form <- formula(object)$formula
    form.terms <- attr(terms(form), "term.labels")
    form.terms.new <- form.terms[!grepl(x, form.terms)]
    form.new <- reformulate(form.terms.new)

    return(form.new)
}
