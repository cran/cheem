% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/7_presaved_attribution.r
\docType{data}
\name{penguin_xgb_pred}
\alias{penguin_xgb_pred}
\alias{penguin_xgb_shap}
\title{Penguins xgb model predictions and shap values}
\format{
\code{penguin_xgb_pred} is a n=333 length vector of the prediction of an
xgb model predicting the number of the factor level of the species of penguin.
\code{penguin_xgb_shap} is a (333 x 4) data frame of the shapviz SHAP attribution of
the xgb model for each observation.

\strong{Replicating}

\if{html}{\out{<div class="sourceCode">}}\preformatted{library(cheem)
library(xgboost)
library(shapviz)
set.seed(135)

## Classification setup
X    <- spinifex::penguins_na.rm[, 1:4]
Y    <- spinifex::penguins_na.rm$species
clas <- spinifex::penguins_na.rm$species

## Model and predict
peng_train    <- data.matrix(X) \%>\%
  xgb.DMatrix(label = Y)
peng_xgb_fit  <- xgboost(data = peng_train, max.depth = 3, nrounds = 5)
penguin_xgb_pred <- predict(peng_xgb_fit, newdata = peng_train)

## shapviz
penguin_xgb_shap <- shapviz(peng_xgb_fit, X_pred = peng_train, X = X)
penguin_xgb_shap <- penguin_xgb_shap$S

if(F)\{ ## Don't accidentally save
  save(penguin_xgb_pred, file = "./data/penguin_xgb_pred.rda")
  save(penguin_xgb_shap, file = "./data/penguin_xgb_shap.rda")
  #usethis::use_data(penguin_xgb_pred)
  #usethis::use_data(penguin_xgb_shap)
\}
}\if{html}{\out{</div>}}

An object of class \code{matrix} (inherits from \code{array}) with 333 rows and 4 columns.
}
\usage{
penguin_xgb_pred

penguin_xgb_shap
}
\description{
Predictions and shapviz attribution of an xgb model of Penguin data
classifying penguin species.
}
\examples{
library(cheem)

## Classification setup
X    <- spinifex::penguins_na.rm[, 1:4]
Y    <- spinifex::penguins_na.rm$species
clas <- spinifex::penguins_na.rm$species

## Precomputed predictions and shap attribtion
str(penguin_xgb_pred)
str(penguin_xgb_shap)

## Cheem
peng_chm <- cheem_ls(X, Y, penguin_xgb_shap, penguin_xgb_pred, clas,
                     label = "Penguins, xgb, shapviz")

## Save for use with shiny app (expects an rds file)
if(FALSE){ ## Don't accidentally save.
  saveRDS(peng_chm, "./chm_peng_xgb_shapviz.rds")
  run_app() ## Select the saved rds file from the data dropdown.
}

## Cheem visuals
if(interactive()){
  prim <- 1
  comp <- 2
  global_view(peng_chm, primary_obs = prim, comparison_obs = comp)
  bas <- sug_basis(penguin_xgb_shap, prim, comp)
  mv  <- sug_manip_var(penguin_xgb_shap, primary_obs = prim, comp)
  ggt <- radial_cheem_tour(peng_chm, basis = bas, manip_var = mv)
  animate_plotly(ggt)
}
}
\keyword{datasets}
