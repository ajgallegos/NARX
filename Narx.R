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
    self$node_no <- NULL
    self$node_type <- NULL
    self$value_ <- 0.0
    self$input_connections <- list()
    self$activation_type_ <- NULL
    self$error <- 0.0
    self$target <- NULL
  },
  ## stub function
  activate_ = function(value){
    value
    invisible(self)
  },
  ## stub function
  error_func_ = function(value){
    value
    invisible(self)
  },
  ## Applies activation function to value of the node
  activate = function(){
    self$activate_(self$value_)
    invisible(self)
  },
  ## Computes the error function
  error_func = function(value){
    self$error_func_(value)
    invisible(self)
  },
  ## Returns a random weight centered around 0.  The constrain limits
  ## the value to the maximum that it would return.  For example, if
  ## .5 is the constraint, then the returned weight would between -.5 and +.5.
  rand_weight = function(constraint = 1.0){
    runif(1) * constraint * 2.0 - constraint
    invisible(self)
  },
  ## This function assigns a random value to the input connections.
  ## The random constraint limits the scope of random variables.
  randomize = function(random_con = RANDOM_CONSTRAINT){
    for(conn in self$input_connections){
      conn.set_weight(self$rand_weight(random_con))
    }
    invisible(self)
  },
  ## This function updates the error of the node from upstream errors.
  ## Depending upon halting on extremes, it also may adjust or halt if
  ## overflows occur.
  ## Finally, it computes the derivative of the activation type, and
  ## modifies the error.
  update_error = function(halt_on_extremes){
    if(self$node_type == NODE_OUTPUT){
      self$error <- self$target - self$activate()
    }
    else {
      ## Other than output layer, will have accumulated errors from above
      self$error <- self$error * self$error_func(self$activate())
      if(halt_on_extremes){
        if(is.nan(self$error)){
          throw("Number has turned into NaN.")
        }
      }
    }
    self$update_lower_node_errors_(halt_on_extremes)
    invisible(self)
  },
  ## This function goes through each of the input connections to the node
  ## and updates the lower nodes.
  ## The error from the current node is multiplied times the connection
  ## weight, inspected for bounds limits and posted in the lower node's error.
  update_lower_node_errors_ = function(halt_on_extremes){
    for(conn in self$input_connections){
      conn$lower_node.error <- conn$lower_node$error + conn$get_weight() * self$error
      if(halt_on_extremes){
        if(is.nan(conn$lower_node$error)){
          throw("Number has turned into NaN")
        }
      }
    }
    invisible(self)
  }
))

## Creates Node class to be implemented
Node <- R6Class(classname = "Node", 
                inherit = ProtoNode,
                public = list(
  ## initialize the node with inheritance from protonode
  initialize = function(node_type = NULL){
    node <- ProtoNode$new() 
    node$node_type <- node_type 
    node$error_func_ <- NULL
  },
  ## This function sets the activation type for the node.  Currently
  ## available values are ACTIVATION_SIGMOID, ACTIVATION_TANH,
  ## ACTIVATION_LINEAR. When specifying the activation type, the
  ## corresponding derivative type for the error functions are assigned as
  ## well.
  set_activation_type = function(activation_type){
    ## Check this one this may be wrong
    if(activation_type == ACTIVATION_SIGMOID){
      node$activate_ = "sigmoid"
    }
    else if(activation_type == ACTIVATION_TANH){
      node$activate_ = "tanh"
    }
    else if(activation_type == ACTIVATION_LINEAR){
      node$activate_ = "linear"
    }
    else{
      throw("Invalid Activation Type.")
    }
    
    self$set_error_func_(activation_type)
    node$activation_type_ <- activation_type
    invisible(self)
  },
  ## Sets error function type to be used 
  set_error_func_ = function(activation_type){
    if(activation_type == ACTIVATION_SIGMOID){
      node$error_func_ = "sigmoid_derivative"
    }
    else if(activation_type == ACTIVATION_TANH){
      node$error_func_ = "tanh_derivative"
    }
    else if(activation_type == ACTIVATION_LINEAR){
      node$error_func_ = "linear_derivative"
    }
    else{
      throw("Invalid Activation Type.")
    }
    invisible(self)
  },
  ## Set value used to avoid the accidental use of setting a value on a
  ## bias node.  The bias node value is always 1.0.
  set_value = function(value){
    node$value_ <- value
    invisible(self)
  },
  ## This function returns the internal value of the node.
  get_value = function(){
    node$value_
  },
  ## This function walks the input connections, summing gets the lower node
  ## activation values times the connection weight.  Then, node is
  ## activated.
  feed_forward = function(){
    sum1 <- 0
    for(conn in node$input_connections){
      if(is.null(conn$lower_value$get_value)){
        throw("Uninitialized Node")
      }
      sum1 = sum1 + conn$lower_node$activate() * conn$get_weight()
    }
    self$set_value(sum1)
    invisible(self)
  },
  ## This function adds an input connection.  This is defined as a
  ## connection that comes from a layer on the input side, or in this
  ## applicaion, a lower number layer.
  ## The reason that there is a specific function rather than using just an
  ## append is to avoid accidentally adding an input connection to a bias
  ## node.
  add_input_connection = function(conn){
    if(conn$upper_node == conn){
      append(node$input_connections, conn)
    }
    else{
      throw("The upper node is always the current node.")
    }
    invisible(self)
  },
  ## This function adjusts incoming weights as part of the back propagation
  ## process, taking into account the node error.  The learnrate moderates
  ## the degree of change applied to the weight from the errors.
  adjust_weights = function(learn_rate, halt_on_extremes){
    for(conn in node$inputconnections){
      conn$add_weight(self$adjust_weight_(learn_rate,
                                         conn$lower_node$activate(),
                                         node$error))
      conn$weight_adjusted <- TRUE
      weight <- conn$get_weight
      if(halt_on_extremes){
        if(is.nan(weight)){
          throw("Weight term has become NaN.")
        }
      }
      conn$set_weight(weight)
      invisible(self)
    }
  },
  ## This function accepts the learn rate, the activated value received
  ## from a node connected from below, and the current error of the node.
  ## It then multiplies those altogether, which is an adjustment to the
  ## weight of the connection as a result of the error.
  adjust_weight_ = function(learn_rate, activation_value, error){
    learn_rate * activation_value * error
  }
))