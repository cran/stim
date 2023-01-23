## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----installation, eval = FALSE-----------------------------------------------
#  devtools::install_github("https://github.com/AnnaWysocki/stim")

## ----data---------------------------------------------------------------------
library(stim)

S <-  matrix(c(1, .3, .3,
              .3,  1, .3,
              .3, .3,  1), 
             nrow = 3, ncol = 3,
             dimnames = list(c("X", "Y", "Z"), 
                             c("X", "Y", "Z")))



## ----modelspec1---------------------------------------------------------------
model <- 'Y ~ X' # outcome ~ predictor

## ----modelspec2---------------------------------------------------------------
model2 <- 'Y ~ X
           Z ~ X + Y'


## ----modelspec3---------------------------------------------------------------
model2 <- 'Y ~ X
           Z ~ X + Y
          
           X ~~ Y' # Allows X and Y to have covarying residuals

## ----modelspec4---------------------------------------------------------------
model2 <- 'Y ~ .6 * X  # fix effect of X on Y to .6
           Z ~ X + Y
          
           X ~~  Y' 


## ----modelspec5---------------------------------------------------------------
model2 <- 'Y ~ .6 * X 
           Z ~ Effect1 * X + Y # label the estimated effect of X on Z
          
           X ~~ Y'


## ----stability1---------------------------------------------------------------
stability <- c(X = .5, Y = .1, Z = .1)

## ----stability2---------------------------------------------------------------
stability <- data.frame(X = c(.5, .55), Y = c(.1, .15), Z = c(.1, .2))

rownames(stability) <- c("Model 1", "Model 2")

stability


## ----stim1--------------------------------------------------------------------
modelFit <- stim(S = S, n = 1000, model = model2, stability = stability) 


## -----------------------------------------------------------------------------
summary(modelFit)

## ----output 1-----------------------------------------------------------------
modelFit$stability 


## ----output 2-----------------------------------------------------------------
modelFit$CLEffectTable 


## ----output 3-----------------------------------------------------------------
modelFit$CLMatrices 


## ----output 4-----------------------------------------------------------------
modelFit$RCovMatrices 


## ----output 5-----------------------------------------------------------------
modelFit$ARVector


## ----output 6-----------------------------------------------------------------
lavaanSummary(modelFit)


## ----output 6.2---------------------------------------------------------------
lavaanSummary(modelFit, subset = 1)


## ----output 7-----------------------------------------------------------------
modelFit$NoWarnings # Means no warnings for both models


## ----output 8-----------------------------------------------------------------
modelFit$CSModelSyntax 


## ----output 9-----------------------------------------------------------------
modelFit$SIMSyntax 


## ----output 10----------------------------------------------------------------
modelFit$modelImpliedEquations 


