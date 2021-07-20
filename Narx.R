library(R6)
library(R.oo)

NeuralNet <- R6Class(classname = "NeuralNet", list(
  ## Declare constants
  learnrate <- 1,
  randomConstraint <- 1,
  epochs <- 1,
  layers <- list("Learning" = list(NULL, NULL), "Validation" = list(NULL, NULL), "Test" = list(NULL, NULL)),
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
  
  initialize = function(inputNodes, totalHiddenNodesList, outputNodes, recurrentMods) {
    
  }
  
))

## Necessary Static Constants
ACTIVATION_SIGMOID <- 'sigmoid'
ACTIVATION_TANH <- 'tanh'
ACTIVATION_LINEAR <- 'linear'

RANDOM_CONSTRAINT <- 1.0

NODE_OUTPUT <- 'output'
NODE_HIDDEN <- 'hidden'
NODE_INPUT <- 'input'
NODE_COPY <- 'copy'
NODE_BIAS <- 'bias'

## Prototype node class
ProtoNode <- R6Class(classname = "ProtoNode", public = list(
  ## initializes the prototype node with default values
  initialize = function(){
    ## Initializes values that will be reset in actual node class
    self$node_no = NULL
    self$node_type = NULL
    self$value_ = 0.0
    self$input_connections = list()
    self$activation_type_ = NULL
    self$error = 0.0
    self$target = NULL
  },
  ## stub function
  activate_ = function(value){
    value
  },
  ## stub function
  error_func_ = function(value){
    value
  },
  ## Applies activation function to value of the node
  activate = function(){
    self$activate_(self$value_)
  },
  ## Computes the error function
  error_func = function(value){
    self$error_func_(value)
  },
  ## Returns a random weight centered around 0.  The constrain limits
  ## the value to the maximum that it would return.  For example, if
  ## .5 is the constraint, then the returned weight would between -.5 and +.5.
  rand_weight = function(constraint = 1.0){
    runif(1) * constraint * 2.0 - constraint
  },
  ## This function assigns a random value to the input connections.
  ## The random constraint limits the scope of random variables.
  randomize = function(random_con = RANDOM_CONSTRAINT){
    for(conn in self$input_connections){
      conn.set_weight(self$rand_weight(random_con))
    }
  },
  ## This function updates the error of the node from upstream errors.
  ## Depending upon halting on extremes, it also may adjust or halt if
  ## overflows occur.
  ## Finally, it computes the derivative of the activation type, and
  ## modifies the error.
  update_error = function(halt_on_extremes){
    if(self$node_type == NODE_OUTPUT){
      self$error = self$target - self$activate()
    }
    else {
      ## Other than output layer, will have accumulated errors from above
      self$error = self$error * self$error_func(self$activate())
      if(halt_on_extremes){
        if(is.nan(self$error)){
          throw("Number has turned into NaN.")
        }
      }
    }
    self$update_lower_node_errors_(halt_on_extremes)
  },
  ## This function goes through each of the input connections to the node
  ## and updates the lower nodes.
  ## The error from the current node is multiplied times the connection
  ## weight, inspected for bounds limits and posted in the lower node's error.
  update_lower_node_errors_ = function(halt_on_extremes){
    for(conn in self$input_connections){
      conn$lower_node.error = conn$lower_node$error + conn$get_weight() * self$error
      if(halt_on_extremes){
        if(is.nan(conn$lower_node$error)){
          throw("Number has turned into NaN")
        }
      }
    }
  }
))

## Creates a