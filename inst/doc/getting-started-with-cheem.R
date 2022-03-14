## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  echo       = TRUE,   # code
  include    = TRUE,   # plots
  results    = "show", # text: "hide", "show"
  eval       = TRUE,   # chunk
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

## -----------------------------------------------------------------------------
library(cheem)
library(spinifex)

## Classification case, Penguins data
X           <- penguins_na.rm[, 1:4]
clas        <- penguins_na.rm$species
Y           <- as.integer(clas)
colnames(X) <- c("bl", "bd", "fl", "bm")

## Create a random forest classifying penguin species
rf_fit <- default_rf(X, Y, verbose = FALSE)

## For other compatible tree based models see examples in:
if(FALSE)
  ?is_randomForest

## -----------------------------------------------------------------------------
## Local attribution of all observations
#### !! This is expensive for complex models/data
shap_df <- attr_df_treeshap(rf_fit, X, verbose = FALSE, noisy = FALSE)
## Extract statistics and prep for cheem consumption
this_ls <- cheem_ls(X, Y, class = clas,
                    model = rf_fit, attr_df = shap_df, verbose = FALSE)

## ---- out.width="100%"--------------------------------------------------------
global_view(this_ls, primary_obs = 243, comparison_obs = 169,
            height_px = 240, width_px = 720, as_ggplot = TRUE)

## ---- out.width="100%", eval = FALSE------------------------------------------
#  ## Normalized attribution basis of the PI
#  bas <- basis_attr_df(shap_df, rownum = 243)
#  ## Default to feature to manipulation;
#  #### the feature with largest separation between PI and CI attribution
#  mv  <- manip_var_of_attr_df(shap_df, primary_obs = 243, comparison_obs = 169)
#  ## Make the radial tour
#  ggt <- radial_cheem_tour(
#    this_ls, basis = bas, manip_var = mv,
#    primary_obs = 243, comparison_obs = 169, angle = .25)
#  
#  ## Animate it
#  animate_gganimate(ggt, fps = 6)
#    #height = 2, width = 4.5, units = "in", res = 150
#  ## Or as a plotly html widget
#  #animate_plotly(ggt, fps = 6)

## ---- echo=FALSE, out.width="100%"--------------------------------------------
if(FALSE){ ## To mitigate file size (CRAN note) create a gif and include that
  bas <- basis_attr_df(shap_df, rownum = 243)
  mv  <- manip_var_of_attr_df(shap_df, primary_obs = 243, comparison_obs = 169)
  ggt <- radial_cheem_tour(
    this_ls, basis = bas, manip_var = mv,
    primary_obs = 243, comparison_obs = 169, angle = .25)
  
  #### .gif is about .2 Mb saved, while HTML widget was about 7 Mb.
  anim <- animate_gganimate(ggt, fps = 6,
                            height = 2, width = 4.5, units = "in", res = 150)
  gganimate::anim_save("tour_penguins.gif", animation = anim)#, path = "./vignettes")
  beepr::beep()
}
#knitr::include_graphics("tour_penguins.gif")

## -----------------------------------------------------------------------------
library(ggplot2)

ggplot(penguins_na.rm, aes(x = bill_length_mm,
                           y = flipper_length_mm,
                           colour = species,
                           shape = species)) +
  geom_point() +
  ## Highlight PI, *
  geom_point(data = penguins_na.rm[243, ], shape = 8, size = 5, alpha = 0.8) +
  ## Theme, scaling, color, and labels
  theme_bw() +
  theme(aspect.ratio = 1) +
  scale_color_brewer(palette = "Dark2") +
  labs(y = "Flipper length [mm]", x = "Bill length [mm]", 
       color = "Observed species", shape = "Observed species")

