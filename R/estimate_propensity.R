#'@title Estimate Propensity Score
#'@description
#'  This function estimates the propensity score using a RandomForest model.
#'@section Dependencies:
#'  \itemize{
#'    \item{randomForest}
#'    \item{dplyr}
#'  }
#'@param treatment Treatment to be studied.
#'@param X Complete sample of the x-variables to estimate the propensity score for all the observations in the dataframe.
#'@returns The propensity scores for each row in our dataset.
#'@examples
#'  estimate_propensity(
#'    treatment = df$treatment,
#'    X = df[[ml_variables]]
#'    )
#'@keywords
#'  Propensity Score
#'@export
estimate_propensity <- function(treatment, X){
    require(randomForest)
    require(dplyr)

    # Setup Variables
    print("Training the Random Forest Model.")
    df <- df_pre_process(X, treatment, 10)
    treatment <- df$temp_treatment
    df <- subset(df, select = -c(rown, temp_treatment))
    vars <- colnames(X)
    set.seed(7)

    # Tune Model
    print("Finding the optimal mtry parameter for the Random Forest model.")
    mtry_obj <- tuneRF(
      x = df[, vars],
      y = as.factor(treatment),
      type = "regression")
    mtry_optimal <- as.integer(mtry_obj[which.min(mtry_obj[, 2])])
    print(paste0("Using optimal mtry hyper-parameter: ", mtry_optimal))

    # Conduct Out-Of-Fold (OOF) predictions for 10 folds.
    for (i in 0:9) {
        print(paste0("In Fold: ", i+1))
        train_sample <- df$fold != i
        pred_sample <- df$fold == i

        rf <- randomForest(
          x = df[train_sample, vars],
          y = as.factor(treatment[train_sample]),
          type = "regression",
          importance = TRUE,
          mtry = mtry_optimal
        )

        # Make Predictions For Fold i
        pred <- predict(rf, newdata=df[, vars], type = "prob")[,2]
        df$propensity[which(pred_sample)] <- pred[pred_sample]
      }

    # Get the propensity scores, and sort them according to their original index.
    df <- df[with(df, order(idx)),]
    propensity_scores <- df$propensity
    return(propensity_scores)
}

df_pre_process <- function(df, treatment, num_folds) {
  df$idx <- 1:nrow(df)
  df$temp_treatment <- treatment
  set.seed(7)
  random_order <- sample(1:nrow(df))
  df <- df[random_order, ]
  df$rown <- 1:nrow(df)
  df <- df %>% mutate(fold = floor((rown - 1) * num_folds / nrow(df)))
  df$propensity <- NA
  return(df)
}
