## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  echo       = TRUE,   # code
  include    = TRUE,   # plots
  results    = "show", # text: "hide", "show"
  ## stop("REPLACE ME"):
  eval       = FALSE,   # chunk code
  message    = FALSE,
  warning    = FALSE,
  error      = FALSE,
  collapse   = TRUE,
  comment    = "#>",
  fig.height = 4,
  fig.width  = 6,
  fig.align  = "center",
  cache      = FALSE
)

## ----limenonlinear, echo=FALSE, fig.cap="Illustration of non-linear classification boundary. The use of local explanations approximates the feature importance in the vicinity of one instance. This allow us to understand a change in which features would result in a red plus being classified as a blue circle. From _Ribiro, M. et. all. (2017). Why should I trust you?_"----
#  knitr::include_graphics("../inst/shiny_apps/cheem/www/lime_nonlinear.png")

## -----------------------------------------------------------------------------
#  ## Download if not installed
#  if(!require(cheem))    install.packages("cheem", dependencies = TRUE)
#  if(!require(treeshap)) install.packages("treeshap", dependencies = TRUE)
#  if(!require(shapviz))  install.packages("shapviz", dependencies = TRUE)
#  ## Load onto session
#  library(cheem)
#  library(xgboost)
#  library(shapviz)
#  
#  ## Setup
#  X    <- amesHousing2018_NorthAmes[, 1:9]
#  Y    <- amesHousing2018_NorthAmes$SalePrice
#  clas <- amesHousing2018_NorthAmes$SubclassMS
#  
#  ## Model and predict
#  ames_train    <- data.matrix(X) %>% xgb.DMatrix(label = Y)
#  ames_xgb_fit  <- xgboost(data = ames_train, max.depth = 3, nrounds = 25)
#  ames_xgb_pred <- predict(ames_xgb_fit, newdata = ames_train)
#  ames_xgb_pred %>% head()
#  
#  ## SHAP values
#  shp <- shapviz(ames_xgb_fit, X_pred = ames_train, X = X)
#  ## Keep just the [n, p] local explanations
#  ames_xgb_shap <- shp$S
#  ames_xgb_shap %>% head()

## -----------------------------------------------------------------------------
#  ## Preprocessing for cheem analysis
#  ames_chm <- cheem_ls(X, Y,
#                       class      = clas,
#                       attr_df    = ames_xgb_shap,
#                       pred       = ames_xgb_pred,
#                       label      = "Ames, xgb, shap")
#  names(ames_chm)

## ---- out.width="100%"--------------------------------------------------------
#  prim <- 1
#  comp <- 17
#  global_view(ames_chm, primary_obs = prim, comparison_obs = comp,
#              height_px = 240, width_px = 720,
#              as_ggplot = TRUE, color = "log_maha.data")

## ---- out.width="100%", eval = FALSE------------------------------------------
#  ## Normalized attribution basis of the PI
#  bas <- sug_basis(ames_xgb_shap, rownum = prim)
#  ## Default feature to manipulate:
#  #### the feature with largest separation between PI and CI attribution
#  mv  <- sug_manip_var(
#    ames_xgb_shap, primary_obs = prim, comparison_obs = comp)
#  ## Make the radial tour
#  ggt <- radial_cheem_tour(
#    ames_chm, basis = bas, manip_var = mv,
#    primary_obs = prim, comparison_obs = comp, angle = .15)
#  
#  ## Animate it
#  animate_gganimate(ggt, fps = 6)
#    #height = 2, width = 4.5, units = "in", res = 150
#  ## Or as a plotly html widget
#  #animate_plotly(ggt, fps = 6)

## ---- echo=FALSE, out.width="100%"--------------------------------------------
#  ## To mitigate file size (CRAN note) and run time create a gif and include that instead of executing code to make inline.
#  if(FALSE){
#    prim <- 1
#    comp <- 17
#    bas <- sug_basis(shap_df, rownum = prim)
#    mv  <- sug_manip_var(
#      shap_df, primary_obs = prim, comparison_obs = comp)
#    ggt <- radial_cheem_tour(
#      this_ls, basis = bas, manip_var = mv,
#      primary_obs = prim, comparison_obs = comp, angle = .15)
#  
#    #### .gif is about .2 Mb saved, while HTML widget was about 7 Mb.
#    anim <- animate_gganimate(
#      ggt, fps = 6, height = 2, width = 4.5, units = "in", res = 150)
#    gganimate::anim_save("tour_penguins.gif", animation = anim)#, path = "./vignettes")
#    beepr::beep()
#  }
#  #knitr::include_graphics("tour_penguins.gif")

## -----------------------------------------------------------------------------
#  library(ggplot2)
#  prim <- 1
#  
#  ggplot(penguins_na.rm, aes(x = bill_length_mm,
#                             y = flipper_length_mm,
#                             colour = species,
#                             shape = species)) +
#    geom_point() +
#    ## Highlight PI, *
#    geom_point(data = penguins_na.rm[prim, ],
#               shape = 8, size = 5, alpha = 0.8) +
#    ## Theme, scaling, color, and labels
#    theme_bw() +
#    theme(aspect.ratio = 1) +
#    scale_color_brewer(palette = "Dark2") +
#    labs(y = "Flipper length [mm]", x = "Bill length [mm]",
#         color = "Observed species", shape = "Observed species")

## ---- eval=FALSE, echo=TRUE---------------------------------------------------
#  if(!require(shapviz)) install.packages("shapviz")
#  if(!require(xgboost)) install.packages("xgboost")
#  library(shapviz)
#  library(xgboost)
#  set.seed(3653)
#  
#  ## Setup
#  X    <- spinifex::penguins_na.rm[, 1:4]
#  Y    <- spinifex::penguins_na.rm$species
#  clas <- spinifex::penguins_na.rm$species
#  
#  ## Model and predict
#  peng_train    <- data.matrix(X) %>%
#    xgb.DMatrix(label = Y)
#  peng_xgb_fit  <- xgboost(data = peng_train, max.depth = 3, nrounds = 25)
#  peng_xgb_pred <- predict(peng_xgb_fit, newdata = peng_train)
#  
#  ## SHAP
#  peng_xgb_shap <- shapviz(peng_xgb_fit, X_pred = peng_train, X = X)
#  ## Keep just the [n, p] local explanations
#  peng_xgb_shap <- peng_xgb_shap$S

## ---- eval=FALSE, echo=TRUE---------------------------------------------------
#  if(!require(treeshap)) install.packages("treeshap")
#  if(!require(randomForest)) install.packages("randomForest")
#  library(treeshap)
#  library(randomForest)
#  
#  ## Setup
#  X    <- spinifex::wine[, -1:2]
#  Y    <- spinifex::wine$Alcohol
#  clas <- spinifex::wine$Type
#  
#  ## Fit randomForest::randomForest
#  wine_rf_fit  <- randomForest::randomForest(
#    X, Y, ntree = 125,
#    mtry = ifelse(is_discrete(Y), sqrt(ncol(X)), ncol(X) / 3),
#    nodesize = max(ifelse(is_discrete(Y), 1, 5), nrow(X) / 500))
#  wine_rf_pred <- predict(wine_rf_fit)
#  
#  ## treeshap::treeshap()
#  wine_rf_tshap <- wine_rf_fit %>%
#    treeshap::randomForest.unify(X) %>%
#    treeshap::treeshap(X, interactions = FALSE, verbose = FALSE)
#  ## Keep just the [n, p] local explanations
#  wine_rf_tshap <- wine_rf_tshap$shaps

## ---- eval=FALSE, echo=TRUE---------------------------------------------------
#  if(!require(DALEX)) install.packages("DALEX")
#  library(DALEX)
#  
#  ## Setup
#  X    <- dragons[, c(1:4, 6)]
#  Y    <- dragons$life_length
#  clas <- dragons$colour
#  
#  ## Model and predict
#  drag_lm_fit  <- lm(data = data.frame(Y, X), Y ~ .)
#  drag_lm_pred <- predict(drag_lm_fit)
#  
#  ## SHAP via DALEX, versatile but slow
#  drag_lm_exp <- explain(drag_lm_fit, data = X, y = Y,
#                         label = "Dragons, LM, SHAP")
#  ## DALEX::predict_parts_shap is flexible, but slow and one row at a time
#  drag_lm_shap <- matrix(NA, nrow(X), ncol(X))
#  sapply(1:nrow(X), function(i){
#    pps <- predict_parts_shap(drag_lm_exp, new_observation = X[i, ])
#    ## Keep just the [n, p] local explanations
#    drag_lm_shap[i, ] <<- tapply(
#      pps$contribution, pps$variable, mean, na.rm = TRUE) %>% as.vector()
#  })
#  drag_lm_shap <- as.data.frame(drag_lm_shap)

