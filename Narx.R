library(R6)
library(R.oo)
library(tidyverse)

NeuralNet <- R6Class(
  classname = "NeuralNet",
  list(
    ## Declare constants
    learnrate <- 1,
    randomConstraint <- 1,
    epochs <- 1,
    layers <-
      list(
        "Learning" = list(NULL, NULL),
        "Validation" = list(NULL, NULL),
        "Test" = list(NULL, NULL)
      ),
    allInputs <- list(),
    allTargets <- list(),
    timeDelay <- list(),
    allResults <- list(),
    mse <- list(),
    mseAccum <- list(),
    validationTargetsActivations <- list(),
    testTargetsActivations <- list(),
    haltOnExtremes <- FALSE,
    inputLayer <- NULL,
    outputLayer <- NULL,
    
    initialize = function(inputNodes,
                          totalHiddenNodesList,
                          outputNodes,
                          recurrentMods) {
      
    }
    
  )
)
