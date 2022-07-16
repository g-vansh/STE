#'@title Coherence
#'@description
#'  This function calculates the value of coherence for a model.
#'@section Dependencies:
#'  \describe {
#'    \item{glmnet}
#'  }
#'@param y Treatment Effect to be studied.
#'@param x Variables with interactions.
#'@param x.no_inter Variables without interactions.
#'@returns The value of coherence.
#'@examples
#'  estimate_coherence(
#'    y = df$teffect,
#'    x = df[, ml_variables],
#'    x.no_inter = df[[ml_variables[grep("^[^X]", ml_variables)]]]
#'    )
#'@keywords
#'  Coherence
#'@export
estimate_coherence <- function(y, x, x.no_inter) {
    require(glmnet)

    # Get the complete R-Squared.
    rsq.all <- rsq_calc(x, y)

    # Get non-interacted R-Squared.
    rsq.no_inter <- rsq_calc(x.no_inter, y)

    # Calculate the coherence.
    coherence_val <- rsq.all / rsq.no_inter - 1

    return(coherence_val)
}

rsq_calc <- function(x, y) {

    # Running Lasso RGLM.
    lasso_ste <- cv.glmnet(
        x = data.matrix(x),
        y = y,
        standardize = TRUE
    )

    # Getting the R-squared.
    top_fit <- which(lasso_ste$lambda == lasso_ste$lambda.min)
    rsq <- lasso_ste$glmnet.fit$dev.ratio[top_fit]

    return(rsq)
}
