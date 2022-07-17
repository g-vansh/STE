## Strategic Treatment Effects

The goal of STE is to allow a user to estimate and study the Strategic
Treatment Effect of a strategic choice, as outlined by the paper:
Guzman, Jorge, Treatment Effects in Strategic Management (September 1,
2021). Available at SSRN: <https://ssrn.com/abstract=3915606> or
<http://dx.doi.org/10.2139/ssrn.3915606>. <br> <br> The five functions
in the package allow one to systematically study the strategic treatment
effects of a strategic choice by using a Random Forest model to estimate
the propensity score (as mentioned in the paper), which is then used to
estimate the treatment effects as well as the strategic treatment
effects of the strategic choice. This information is then used to
estimate the value of coherence for firms.

## Installation

You can install the development version of STE like so:

``` r
install.packages("STE")
```

## Package Contents

This package contains 5 functions:

-   `STE::estimate_main_effect(y, treatment, X)`
-   `STE::estimate_propensity(treatment, X)`
-   `STE::estimate_ste(y, treatment, propensity, df)`
-   `STE::get_top_ste_determinants(ste, X, teffect)`
-   `STE::estimate_coherence(y, x, x.no_inter)`

## Example

This is a basic example which shows you how to use the package in one
scenario:

``` r
library(STE)
## Basic Example Code:

# Load cb_startups as a dataframe from cb_startups.Rdata file.
load("cb_startups.Rdata")

# Estimate the main effect of the treatment.
reg_coefs <- STE::estimate_main_effect(
    y = cb_startups$equity_growth,
    treatment = cb_startups$bearly_stage_has_vc,
    X = cb_startups[, ml_vars]
)
print(reg_coefs)

# Estimate the propensity score of the treatment.
p_scores <- STE::estimate_propensity(
    treatment = cb_startups$bearly_stage_has_vc,
    X = cb_startups[, ml_vars]
)

# Estimate the strategic treatment effect.
cb_startups <- STE::estimate_ste(
    y = cb_startups$equity_growth,
    treatment = cb_startups$bearly_stage_has_vc,
    propensity = p_scores,
    df = cb_startups
)

# Remove NA values for analysis. 
cb_startups.clean <- cb_startups %>%
    filter(!is.na(ste))

# Study the determinants of STE.
ste_features <- STE::get_top_ste_determinants(
    ste = cb_startups.clean$ste,
    X = cb_startups.clean[, ml_vars],
    teffect = cb_startups.clean$teffect
    )
View(ste_features)

# Estimate the coherence value.
ml_vars.no_inter <- ml_vars[grep("^[^X]",ml_vars)]
coherence_value <- STE::estimate_coherence(
    y = cb_startups.clean$teffect,
    x = cb_startups.clean[, ml_vars],
    x.no_inter = cb_startups.clean[, ml_vars.no_inter]
)
print(paste0("Coherence Value: ",coherence_value))
```
