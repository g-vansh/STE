#' @title Study Determinants of the STE.
#' @description
#'  This function studies the determinants of the strategic treatment effect.
#'@section Dependencies:
#'  \itemize{
#'    \item{glmnet}
#'    \item{dplyr}
#'  }
#' @param X x-variables for the LASSO.
#' @param teffect Treatment effect of the model.
#' @returns ste_features A dataframe containing the STE features.
#' @examples
#' get_top_ste_determinants(
#'    X = df[, ml_variables],
#'    teffect = df$teffect
#' )
#' @keywords
#'  STE Determinants
#' @export
get_top_ste_determinants <- function(X, teffect) {
    require(dplyr)
    require(glmnet)

    # Setup Variables
    nFolds <- 10
    set.seed(7)
    X <- X[sample(1:nrow(X)), ] %>% filter(!is.na(teffect))
    foldid <- sample(rep(1:nFolds, length.out = nrow(X)))
    ate <- mean(teffect, na.rm = T)

    # Run LASSO.
    lasso_ste <- cv.glmnet(
        x = data.matrix(X),
        y = teffect,
        standardize = TRUE,
        foldid = foldid
    )

    # Get Features.
    coefs <- coef(lasso_ste, s = lasso_ste$lambda.1se)
    lasso_ste.selected_vars <- data.frame(cbind(row.names(coefs)[coefs[, 1] != 0], coefs[coefs[, 1] != 0]))

    # Clean Table
    names(lasso_ste.selected_vars) <- c("variable", "coef")
    ste_features <- lasso_ste.selected_vars %>%
        mutate(
            coef = round(as.double(as.character(coef)), 4),
            rel_effect = round(coef / ate, 2)
        ) %>%
        arrange(desc(coef)) %>%
        dplyr::rename(
            Coefficient = coef,
            "Coefficient/ATE" = rel_effect,
            Variable = variable
        )
    ste_features <- ste_features %>%
        filter(!grepl("Intercept", Variable))

    return(ste_features)
}
