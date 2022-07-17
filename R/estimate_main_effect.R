#'@title Estimate Main Effect
#'@description
#'  This function estimates the main effect of the treatment.
#'@section Dependencies:
#'  \itemize{
#'    \item{glmnet}
#'    \item{stringr}
#'    \item{fixest}
#'  }
#'@param treatment_var String containing the name of treatment variable to be studied.
#'@param y_var String containing the name of the outcome variable to be studies.
#'@param X X-variables.
#'@param data_df The complete dataframe.
#'@returns The regression output from the fixed-effects OLS regression.
#'@examples
#'  estimate_main_effect(
#'    y_var = "outcome_var_name",
#'    treatment_var = "treatment_var_name",
#'    X = df[, ml_variables],
#'    data_df = df
#'    )
#'@keywords
#'  Main Effect
#'@export
estimate_main_effect <- function(y_var, treatment_var, X, data_df) {
    # Setup Variables
    require(glmnet)
    require(stringr)
    require(fixest)
    lm_formula <- paste0(y_var,"~",treatment_var)
    y <- data_df[[y_var]]
    treatment <- data_df[[treatment_var]]
    vars <- colnames(X)

    # Run Lassos.
    l1 <- cv.glmnet(data.matrix(X), y = as.double(treatment))
    l2 <- cv.glmnet(data.matrix(X), y = as.double(y))


    # Get the coefficients.
    coefs_l1 <- vars[coef(l1)[-1] != 0]
    coefs_l2 <- vars[coef(l2)[-1] != 0]
    dl_coefs <- union(coefs_l1, coefs_l2)

    # Setup the final regression.
    for (var in dl_coefs) {
      if(str_length(var)>0){
        lm_formula <- paste0(lm_formula, "+", var)
      }
    }
    lm_formula <- as.formula(lm_formula)
    reg <- feols(lm_formula, data=data_df)

    # Return the coefficients.
    return(reg)
}
