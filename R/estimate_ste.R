#' @title Estimate the Strategic Treatment Effect.
#' @description
#'  This function estimates the strategic treatment effect.
#' @section Dependencies:
#'  \itemize{
#'    \item{dplyr}
#'  }
#' @param y Outcome variable to be studied.
#' @param treatment Treatment variable.
#' @param propensity Propensity score generated earlier.
#' @param df The complete dataset.
#' @param span_treated Optional span value for the treated model. Default is 0.75, which is the loess default.
#' @param span_untreated Optional span value for the untreated model. Default is 0.75, which is the loess default.
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
estimate_ste <- function(y, treatment, propensity, df, span_treated = 0.75, span_untreated = 0.75) {
    require(dplyr)
    # Setup models and variables.
    df$propensity <- propensity
    df$y <- y
    model.treated <- loess(y ~ propensity, data = df %>% filter(treatment == 1), span = span_treated)
    model.not_treated <- loess(y ~ propensity, data = df %>% filter(treatment == 0), span = span_untreated)

    # Calculate predicted values.
    pos_prediction <- predict(model.treated, newdata = df)
    neg_prediction <- predict(model.not_treated, newdata = df)

    # Calculate treatment effects.
    df$teffect <- pos_prediction - neg_prediction
    ate <- mean(df$teffect, na.rm = T)
    df$ste <- df$teffect - ate
    df <- subset(df, select = -c(y, propensity))

    return(df)
}


