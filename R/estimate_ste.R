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
    require(stats)
    # Setup models and variables.
    df$x <- propensity
    df$y <- y
    df_treated <- df %>% filter(treatment == 1)
    df_untreated <- df %>% filter(treatment == 0)

    ## Find the optimal spans
    #span_treated = optim(par = c(0.5), calcSSE, method = "SANN", df = df_treated)$par
    #print(paste0("Span (Treated): ", span_treated))
    #span_untreated = optim(par = c(0.5), calcSSE, method = "SANN", df = df_untreated)$par
    #print(paste0("Span (Untreated): ", span_untreated))

    ## Run Local Regressions
    #model.treated <- loess(y ~ propensity, data = df_treated, span = span_treated)
    #model.not_treated <- loess(y ~ propensity, data = df_untreated, span = span_untreated)

    model.treated <- loess.as(y = df_treated$y, x = df_treated$x, plot = T)
    model.not_treated <- loess.as(y = df_untreated$y, x = df_untreated$x, plot = T)
    View(model.not_treated)
    View(model.treated)

    print(paste0("Span (Treated): ", model.treated$pars$span))
    print(paste0("Span (Untreated): ", model.not_treated$pars$span))


    # Calculate predicted values.
    pos_prediction <- predict.loess(model.treated, newdata = df)
    neg_prediction <- predict.loess(model.not_treated, newdata = df)

    # Calculate treatment effects.
    df$teffect <- pos_prediction - neg_prediction
    ate <- mean(df$teffect, na.rm = T)
    df$ste <- df$teffect - ate
    df <- subset(df, select = -c(y, x))

    return(df)
}

# Function that returns the SSE
calcSSE <- function(x, df){

  loessMod <- try(loess(y ~ propensity, data = df, span = x), silent = T)
  res <- try(loessMod$residuals, silent = T)

  if(class(res) != "try-error"){

    if((sum(res, na.rm = T) > 0)){
      sse <- sum(res^2)
    } else {
      sse <- 99999
    }

  }else{
    sse <- 99999
  }

  return(sse)
}




