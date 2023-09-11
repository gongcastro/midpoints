#' Get mid-point of the logistic growth curve
#'
#' This function computes the mid-point of a logistic curve from the coefficients of a logistic regression model.
#' @param newdata
#' @param object An object of class 'glm', 'glmerMod', or 'brmsfit', fitted with `family = logistic("logit")` or `family = bernoulli("logit")`.
#' @param x String character indicating the variable for which the mid-point of the logistic curve will be calculated.
#' @param ...
#' @export midpoint
#' @author Gonzalo Garcia-Castro
#' @md
midpoint <- function(newdata, object, x, ...)
{
    UseMethod("midpoint")
}

# function to compute midpoints
midpoint.glm <- function(object, newdata, x = "x")
{
    # get regression coefficients
    coefs <- coef(object)
    coefs.num <- coefs[!grepl(x, names(coefs))]
    coefs.den <- coefs[grepl(x, names(coefs))]

    # update model formula
    form <- formula(object)
    form.terms <- attr(terms(form), "term.labels")
    is.intercept.only <- length(coefs.num)==1L
    is.univariable <- length(coefs.den)==1L
    form.terms.new <- ifelse(is.intercept.only, "1",
                             form.terms[!grepl(x, form.terms)])
    form.new <- reformulate(form.terms.new)

    # get model matrix
    if (is.intercept.only) {
        nd <- as.data.frame(1)
        mat <- model.matrix(form.new, frame)
        numerator <- mat %*% coefs.num
        denominator <- mat %*% coefs.den
    } else if (is.univariable) {
        nd <- newdata[!duplicated(newdata[, form.terms.new]), , drop = FALSE]
        frame <- model.frame(form.new, nd)
        mat <- model.matrix(form.new, frame)
        numerator <- mat %*% coefs.num
        denominator <- mat[, 1] * coefs.den
    } else {
        nd <- newdata[!duplicated(newdata[, form.terms.new]), , drop = FALSE]
        frame <- model.frame(form.new, nd)
        mat <- model.matrix(form.new, frame)
        numerator <- mat %*% coefs.num
        denominator <- mat %*% coefs.den
    }

    # compute midpoints
    midpoints <- as.vector(-numerator / denominator)

    # prepare output data frame
    nd$.mid <- midpoints

    return(nd)
}


# function to compute midpoints
midpoint.glmer.fixed <- function(newdata,
                                 object,
                                 x = "age",
                                 levels = NULL)
{

    # get regression coefficients
    coefs <- fixef(object)
    coefs.num <- coefs[!grepl(x, names(coefs))]
    coefs.den <- coefs[grepl(x, names(coefs))]



    # get model matrix
    nd <- newdata[!duplicated(newdata[, form.terms.new]), , drop = FALSE]
    nd <- nd[!(names(nd) %in% names(coefs.den))]
    frame <- model.frame(form.new, nd)
    mat <- model.matrix(form.new, frame)

    # compute midpoints
    numerator <- mat %*% coefs.num
    denominator <- mat %*% coefs.den
    .mid <- as.vector(-numerator / denominator)

    # prepare output data frame
    nd$.mid <- midpoints

    return(midpoints)
}


# lme4::glmer() random ---------------------------------------------------------

# function to compute midpoints
midpoint.glmer.random <- function(newdata,
                                  object,
                                  x = "age",
                                  group = "word",
                                  levels = NULL)
{

    if (is.null(levels)) {
        levels <- unique(newdata[, group])[[1]]
    }
    nd <- newdata[newdata[, group][[1]] %in% levels, ]

    # extract regression coefficients
    coefs <- coef(object)[[group]][levels, ]
    coefs.den <- coefs[, grepl(x, names(coefs))]
    coefs.num <- coefs[, !grepl(x, names(coefs))]

    # update model formula
    form <- formula(object)
    form.terms <- attr(terms(form), "term.labels")
    form.terms.new <- form.terms[!grepl(x, form.terms)]
    form.new <- reformulate(form.terms.new)

    # get model matrix
    nd <- newdata[!duplicated(newdata[, c(form.terms.new, group)]), , drop = FALSE]
    nd <- nd[!(names(nd) %in% names(coefs.den))]
    frame <- model.frame(form.new, nd)
    mat <- model.matrix(form.new, frame)

    # compute midpoints
    numerator <- rowSums(mat * coefs.num[rep(seq(nrow(coefs.num)), 2), ])
    denominator <- rowSums(mat * coefs.den[rep(seq(nrow(coefs.den)), 2), ])
    midpoints <- as.vector(-numerator / denominator)

    # prepare output data frame
    nd$.mid <- midpoints

    return(nd)

}


# brms::brm() fixed (summary) --------------------------------------------------

# function to compute midpoints
midpoint.brmsfit.fixed.summary <- function(newdata,
                                           object,
                                           x = "age")
{

    # extract posterior draws of regression coefficients
    coefs <- fixef(object)[, 1]
    coefs.num <- coefs[!grepl(x, names(coefs))]
    coefs.den <- coefs[grepl(x, names(coefs))]

    # update model formula
    form <- formula(object)$formula
    form.terms <- attr(terms(form), "term.labels")
    form.terms.new <- form.terms[!grepl(x, form.terms)]
    form.new <- reformulate(form.terms.new)

    # get model matrix
    nd <- newdata[!duplicated(newdata[, form.terms.new]), , drop = FALSE]
    nd <- nd[!(names(nd) %in% names(coefs.den))]
    frame <- model.frame(form.new, nd)
    mat <- model.matrix(form.new, frame)

    # compute midpoints
    numerator <- mat %*% coefs.num
    denominator <- mat %*% coefs.den
    midpoints <- as.vector(-numerator / denominator)

    # prepare output data frame
    nd$.mid <- midpoints

    return(nd)

}

# brms::brm() fixed (draws) ----------------------------------------------------

# function to compute midpoints
midpoint.brmsfit.fixed.draws <- function(newdata,
                                         object,
                                         x = "age",
                                         ndraws = 20)
{

    if (is.null(ndraws)) {
        ndraws(object)
    }

    # extract posterior draws of regression coefficients
    coefs <- fixef(object, summary = FALSE)
    draws <- sample(1:nrow(coefs), ndraws)
    coefs <- coefs[draws, ]
    coefs.num <- coefs[, !grepl(x, colnames(coefs))]
    coefs.den <- coefs[, grepl(x, colnames(coefs))]

    # update model formula
    form <- formula(object)$formula
    form.terms <- attr(terms(form), "term.labels")
    form.terms.new <- form.terms[!grepl(x, form.terms)]
    form.new <- reformulate(form.terms.new)

    # get model matrix
    nd <- newdata[!duplicated(newdata[, form.terms.new]), , drop = FALSE]
    nd <- nd[!(names(nd) %in% colnames(coefs.den))]
    nd <- nd[rep(seq(nrow(nd)), each = ndraws), ]
    frame <- model.frame(form.new, nd)
    mat <- model.matrix(form.new, frame)

    # compute midpoints
    repeat.rows <- rep(seq(nrow(nd)/2), 2)
    numerator <- rowSums(mat * coefs.num[repeat.rows, ])
    denominator <- rowSums(mat * coefs.den[repeat.rows, ])
    midpoints <- as.vector(-numerator / denominator)

    # prepare output data frame
    nd$.draw <- rep(draws, 2)
    nd$.mid <- midpoints

    return(nd)

}

# brms::brm() random (summary) -------------------------------------------------

# function to compute midpoints
midpoint.brmsfit.random.summary <- function(newdata,
                                            object,
                                            x = "age",
                                            group = NULL,
                                            levels = NULL)
{

    if (is.null(levels)) {
        levels <- unique(newdata[, group])[[1]]
    }
    nd <- newdata[newdata[, group][[1]] %in% levels, ]

    # extract regression coefficients
    coefs <- coef(object)[[group]][, 1, ]
    coefs <- coefs[rownames(coefs) %in% levels, ]
    coefs.den <- coefs[, grepl(x, colnames(coefs))]
    coefs.num <- coefs[, !grepl(x, colnames(coefs))]

    # update model formula
    form <- formula(object)$formula
    form.terms <- attr(terms(form), "term.labels")
    form.terms.new <- form.terms[!grepl(x, form.terms)]
    form.new <- reformulate(form.terms.new)

    # get model matrix
    nd <- newdata[!duplicated(newdata[, c(form.terms.new, group)]), , drop = FALSE]
    nd <- nd[!(names(nd) %in% names(coefs.den))]
    frame <- model.frame(form.new, nd)
    mat <- model.matrix(form.new, frame)

    #compute midpoints
    numerator <- rowSums(mat * coefs.num[rep(seq(nrow(coefs.num)), 2), ])
    denominator <- rowSums(mat * coefs.den[rep(seq(nrow(coefs.den)), 2), ])
    midpoints <- as.vector(-numerator / denominator)

    # prepare output data frame
    nd$.mid <- midpoints

    return(nd)

}

# brms::brm() random (draws) ---------------------------------------------------

# function to compute midpoints
midpoint.brmsfit.random.draws <- function(newdata,
                                          object,
                                          x = "age",
                                          ndraws = 20,
                                          group = NULL,
                                          levels = NULL)
{

    if (is.null(levels)) {
        levels <- unique(newdata[, group])[[1]]
    }
    nd <- newdata[newdata[, group][[1]] %in% levels, ]

    if (is.null(ndraws)) {
        ndraws <- posterior::ndraws(object)
    }
    draws <- sample(1:nrow(coefs), ndraws)

    # extract regression coefficients
    coefs <- coef(object, summary = FALSE)[[group]]
    coefs <- coefs[draws, colnames(coefs) %in% levels, ]
    coefs.nm <- dimnames(coefs)[[3]]
    dim(coefs) <- c(dim(coefs)[1] * dim(coefs)[2] , dim(coefs)[3])
    colnames(coefs) <- coefs.nm

    coefs.den <- coefs[, grepl(x, colnames(coefs))]
    coefs.num <- coefs[, !grepl(x, colnames(coefs))]

    # update model formula
    form <- formula(object)$formula
    form.terms <- attr(terms(form), "term.labels")
    form.terms.new <- form.terms[!grepl(x, form.terms)]
    form.new <- reformulate(form.terms.new)

    # get model matrix
    nd <- newdata[!duplicated(newdata[, c(form.terms.new, group)]), , drop = FALSE]
    nd <- nd[!(names(nd) %in% names(coefs.den))]
    nd <- nd[rep(seq(nrow(nd)), each = ndraws), ]
    frame <- model.frame(form.new, nd)
    mat <- model.matrix(form.new, frame)

    # compute midpoints
    numerator <- rowSums(mat * coefs.num[rep(seq(nrow(coefs.num)), 2), ])
    denominator <- rowSums(mat * coefs.den[rep(seq(nrow(coefs.den)), 2), ])
    midpoints <- as.vector(-numerator / denominator)

    # prepare output data frame
    nd$.draw <- rep(draws, 2*length(unique(levels)))
    nd$.mid <- midpoints

    return(nd)

}
