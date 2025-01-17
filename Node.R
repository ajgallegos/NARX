library(R6)
library(R.oo)
library(tidyverse)


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
ProtoNode <- R6Class(
  classname = "ProtoNode",
  public = list(
    ## Initializes variables to be initialized
    node_no = NULL,
    node_type = NULL,
    value_ = NULL,
    input_connections = NULL,
    activation_type_ = NULL,
    error = NULL,
    target = NULL,
    activate_ = NULL,
    ## initializes the prototype node with default values
    ## Initializes values that will be reset in actual node class
    initialize = function(){
      self$node_no <- NULL
      self$node_type <- NULL
      self$value_ <- 0.0
      self$input_connections <- list()
      self$activation_type_ <- NULL
      self$error <- 0.0
      self$target <- NULL
      self$activate_ <- NULL
    },
    
    ## Returns a random weight centered around 0.  The constrain limits
    ## the value to the maximum that it would return.  For example, if
    ## .5 is the constraint, then the returned weight would between -.5 and +.5.
    rand_weight = function(constraint = 1.0) {
      random <- runif(1) * constraint * 2.0 - constraint
      return(random)
      invisible(self)
    },
    
    ## linear activation
    activate_l = function(value) {
      return(value)
    },
    
    ## sigmoid activation
    activate_s = function(value) {
      sig <- (1 / (1 + exp(-value)))
      return(sig)
    },
    
    ## tanh activation
    activate_t = function(value) {
      val <- tanh(value)
      return(val)
    },
    
    ## linear derivative
    error_func_l = function(value) {
      val <- 1
      return(val)
    },
    
    ## tanh derivative
    error_func_t = function(value){
      val <- 1 - (tanh(value) ^ 2)
      return(val)
    },
    
    ## sig derivative
    error_func_s = function(value){
      val <- value * (1 - value)
      return(val)
    },
    ## Applies activation function to value of the node
    activate = function() {
      if(self$activate_ == 'sigmoid') {
        return(self$activate_s(self$value_))
      }
      else if(self$activate_ == 'tanh') {
        return(self$activate_t(self$value_))
      }
      else if(self$activate_ == 'linear') {
        return(self$activate_l(self$value_))
      }
      else {
        throw("Error, wrong value supplied")
      }
      invisible(self)
    },
    ## Computes the error function
    error_func = function(value) {
      if(self$error_funct_ == 'linear_derivative'){
        return(self$error_func_l(value))
      }
      else if(self$error_funct_ == 'tanh_derivative'){
        return(self$error_func_t(value))
      }
      else if(self$error_funct_ == 'sigmoid_derivative'){
        return(self$error_func_s(value))
      }
      else {
        throw("Wrong error function supplied.")
      }
      invisible(self)
    },
    ## This function assigns a random value to the input connections.
    ## The random constraint limits the scope of random variables.
    randomize = function(random_con = RANDOM_CONSTRAINT) {
      for (conn in self$input_connections) {
        conn$set_weight(self$rand_weight(random_con))
      }
      invisible(self)
    },
    
    ## Get the activation type
    get_activation_type = function(){
      return(self$activation_type_)
    },
    ## This function updates the error of the node from upstream errors.
    ## Depending upon halting on extremes, it also may adjust or halt if
    ## overflows occur.
    ## Finally, it computes the derivative of the activation type, and
    ## modifies the error.
    update_error = function(halt_on_extremes) {
      if (self$node_type == NODE_OUTPUT) {
        self$error <- self$target - self$activate()
      }
      else {
        ## Other than output layer, will have accumulated errors from above
        self$error <- self$error * self$error_func(self$activate())
        if (halt_on_extremes) {
          if (is.nan(self$error)) {
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
    update_lower_node_errors_ = function(haltOnExtremes) {
      for (conn in self$input_connections) {
        conn$lower_node$error <- conn$lower_node$error + conn$get_weight() * self$error
        
        if (haltOnExtremes) {
          if (is.nan(conn$lower_node$error)) {
            throw("Number has turned into NaN")
          }
        }
      }
      invisible(self)
    }
  )
)

## Creates Node class to be implemented
Node <- R6Class(
  classname = "Node",
  inherit = ProtoNode,
  public = list(
    ## initialize variable to be assigned
    error_funct_ = NULL,
    node_type = NULL,
    
    ## assign the node with inheritance from protonode
    initialize = function(node_type = NULL) {
      super$initialize()
      self$node_type <- node_type
      self$error_funct_ <- NULL
    },
    ## This function sets the activation type for the node.  Currently
    ## available values are ACTIVATION_SIGMOID, ACTIVATION_TANH,
    ## ACTIfVATION_LINEAR. When specifying the activation type, the
    ## corresponding derivative type for the error functions are assigned as
    ## well.
    set_activation_type = function(this_activation_type) {
      ## Check this one this may be wrong
      if (this_activation_type == ACTIVATION_SIGMOID) {
        self$activate_ <- 'sigmoid'
      }
      else if (this_activation_type == ACTIVATION_TANH) {
        self$activate_ <- 'tanh'
      }
      else if (this_activation_type == ACTIVATION_LINEAR) {
        self$activate_ <- 'linear'
      }
      else{
        throw("Invalid Activation Type.")
      }
      
      self$set_error_func_(this_activation_type)
      self$activation_type_ <- this_activation_type
      invisible(self)
    },
    ## Sets error function type to be used
    set_error_func_ = function(activation_type) {
      if (activation_type == ACTIVATION_SIGMOID) {
        self$error_funct_ = 'sigmoid_derivative'
      }
      else if (activation_type == ACTIVATION_TANH) {
        self$error_funct_ = 'tanh_derivative'
      }
      else if (activation_type == ACTIVATION_LINEAR) {
        self$error_funct_ = 'linear_derivative'
      }
      else{
        throw("Invalid Activation Type.")
      }
      invisible(self)
    },
    ## Set value used to avoid the accidental use of setting a value on a
    ## bias node.  The bias node value is always 1.0.
    set_value = function(value) {
      self$value_ <- value
      invisible(self)
    },
    ## This function returns the internal value of the node.
    get_value = function() {
      self$value_
    },
    ## This function walks the input connections, summing gets the lower node
    ## activation values times the connection weight.  Then, node is
    ## activated.
    feed_forward = function() {
      sum1 <- 0
      for (conn in self$input_connections) {
        if (is.null(conn$lower_node$get_value)) {
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
    add_input_connection = function(conn) {
      if (identical(conn$upper_node, self)) {
        self$input_connections <- append(self$input_connections, conn)
      }
      else{
        throw("The upper node is always the current node.")
      }
      invisible(self)
    },
    ## This function adjusts incoming weights as part of the back propagation
    ## process, taking into account the node error.  The learnrate moderates
    ## the degree of change applied to the weight from the errors.
    adjust_weights = function(learn_rate, halt_on_extremes) {
      for (conn in self$input_connections) {
        conn$add_weight(self$adjust_weight_(learn_rate,
                                            conn$lower_node$activate(),
                                            self$error))
        conn$weight_adjusted <- TRUE
        weight <- conn$get_weight()
        if (halt_on_extremes) {
          if (is.nan(weight)) {
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
    adjust_weight_ = function(learn_rate, activation_value, error) {
      return(learn_rate * activation_value * error)
    }
  )
)

## Create the CopyNode Class
CopyNode <-
  R6Class(
    classname = "CopyNode",
    inherit = Node,
    public = list(
      ## initialize all necessary class variables
      source_node_ = NULL,
      source_type_ = NULL,
      incoming_weight_ = NULL,
      existing_weight_ = NULL,

      ## assign variables
      initialize = function(){
        super$initialize()
        self$node_type <- NODE_COPY
        self$source_node_ <- NULL
        self$source_type_ <- NULL
        self$incoming_weight_ <- 1
        self$existing_weight_ <- 0
        self$set_activation_type(ACTIVATION_LINEAR)
      },
      
      set_source_node = function(node) {
        ## Sets the source of previous recurrent values
        self$source_node_ = node
        invisible(self)
      },
      ## return the source of previous recurrent values
      get_source_node = function() {
        self$source_node_
      },
      ## Transfers source node's value to copy node's value
      load_source_value = function() {
        if (self$source_type_ == 'a') {
          value <- self$source_node_$activate()
        }
        else if (self$source_type_ == 'v') {
          value <- self$source_node_$get_value()
        }
        else{
          throw("Invalid Source Type.")
        }

        self$value_ <-
          self$value_ * self$existing_weight_ + value * self$incoming_weight_
      },
      ## gets the type of source value to use
      get_source_type = function() {
        self$source_type_
      },
      # This function gets the value that will be multiplied times the incoming source value.
      get_incoming_weight = function() {
        self$incoming_weight_
      },
      ## This function gets the value that will be multiplied times the existing value.
      get_existing_weight = function() {
        self$existing_weight_
      },
      ## This function accepts parameters governing what the source information
      ## is used, and how the incoming and existing values are discounted.
      ##
      ## Source type can be either 'a' for the activation value or 'v' for the
      ## summed input value.
      ##
      ## By setting the existing weight to zero, and the incoming discount to
      ## 1.0. An Elman style update takes place.
      ##
      ## By setting the existing weight to some fraction of 1.0 such as .5, a
      ## Jordan style update can take place.
      source_update_config = function(source_type,
                                      incoming_weight,
                                      existing_weight) {
        
        if (source_type %in% c('a', 'v')) {
          self$source_type_ = source_type
        }
        else{
          throw("Invalid source type. Valid types are 'a' or 'v'.")
        }
        
        errormsg <-
          "The incoming weight must be numeric between 0 and 1."
        if (!is.numeric(incoming_weight)) {
          throw(errormsg)
        }
        if (!between(incoming_weight, 0.0, 1.0)) {
          throw(errormsg)
        }
        self$incoming_weight_ <- incoming_weight
        
        errormsg <-
          "The existing weight must be numeric between 0 and 1."
        if (!is.numeric(existing_weight)) {
          throw(errormsg)
        }
        if (!between(existing_weight, 0.0, 1.0)) {
          throw(errormsg)
        }
        self$existing_weight_ <- existing_weight
        invisible(self)
      }
    )
  )


## Create BiasNode
BiasNode <-
  R6Class(
    classname = "BiasNode",
    inherit = Node,
    public = list(
      ## initialize constants for use
      bias = NULL,
      activated_ = NULL,
      node_type = NULL,
      
      ## assign variables
      initialize = function(){
        super$initialize()
        self$node_type <- NODE_BIAS
        self$activated_ <- self$value_
        self$node_type <- NODE_BIAS
      },
      
      ## always returns activation which is 1
      activate = function(value = NULL) {
        return(1.0)
      },
      ## Also returns 1 since activation of bias node is always 1
      error_func = function(value = 1.0) {
        value = 1.0
        value
      }
    )
  )


## Create connection class
Connection <- R6Class(
  classname = "Connection",
  public = list(
    ##initialze all constants
    lower_node = NULL,
    upper_node = NULL,
    weight_ = NULL,
    weight_adjusted = NULL,
    
    initialize = function(lower_node, upper_node, weight = 0) {
      ## assign all necessary constants
      self$lower_node <- lower_node
      self$upper_node <- upper_node
      self$weight_ <- NULL
      self$weight_adjusted <- NULL
      self$set_weight(weight)
    },
    ## This function sets the weight of the connection, which relates to
    ## the impact that a lower node's activation will have on an upper node's
    ## value.
    set_weight = function(weight) {
      if (!is.numeric(weight)) {
      }
      else{
        self$weight_ <- weight
      }
      invisible(self)
    },
    ## This function adds to the weight of the connection, which is
    ## proportional to the impact that a lower node's activation will
    ## have on an upper node's value.
    add_weight = function(weight) {
      if (!is.numeric(weight)) {
        throw("Weight type is not numeric.")
      }
      else{
        self$weight_ = self$weight_ + weight
      }
    },
    ## This function sets the weight of the connection, which is relates to
    ## the impact that a lower node's activation will have on an upper node's
    ## value.
    get_weight = function() {
      weight <- self$weight_
      return(weight)
    }
  )
)