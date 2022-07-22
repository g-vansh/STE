#' @title Estimate the Strategic Treatment Effect.
#' @description
#'  This function estimates the strategic treatment effect.
#' @section Dependencies:
#'  \itemize{
#'    \item{dplyr}
#'    \item{fANCOVA}
#'  }
#' @param y Outcome variable to be studied.
#' @param treatment Treatment variable.
#' @param propensity Propensity score generated earlier.
#' @param df The complete dataset.
#' @returns df The same as input dataframe with the added column of the strategic treatment effect.
#' @examples
#' estimate_ste(
#'    y = df$equity_growth,
#'    treatment = df$treatment,
#'    propensity = p_scores,
#'    df = df
#' )
#' @keywords
#'  Strategic Treatment Effect
#' @export
estimate_ste <- function(y, treatment, propensity, df) {
    require(dplyr)
    require(fANCOVA)
    # Setup models and variables.
    df$x <- propensity
    df$y <- y
    df_treated <- df %>% filter(treatment == 1)
    df_untreated <- df %>% filter(treatment == 0)

    # Setup models with automatic span selection using the AICC criterion.
    model.treated <- loess.as(y = df_treated$y, x = df_treated$x)
    model.not_treated <- loess.as(y = df_untreated$y, x = df_untreated$x)
    print(paste0("Span (Treated): ", model.treated$pars$span))
    print(paste0("Span (Untreated): ", model.not_treated$pars$span))

    # Calculate predicted values.
    pos_prediction <- predict(model.treated, newdata = df)
    neg_prediction <- predict(model.not_treated, newdata = df)

    # Calculate treatment effects.
    df$teffect <- pos_prediction - neg_prediction
    ate <- mean(df$teffect, na.rm = T)
    df$ste <- df$teffect - ate
    df <- subset(df, select = -c(y, x))

    return(df)
}


