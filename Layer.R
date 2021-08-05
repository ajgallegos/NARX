library(R6)
library(R.oo)
library(tidyverse)

## initialize static constants
LAYER_TYPE_INPUT <- 'input'
LAYER_TYPE_HIDDEN <- 'hidden'
LAYER_TYPE_OUTPUT <- 'output'

## Create the layer class (only class in this file)
Layer <- R6Class(
  classname = "Layer",
  public = list(
    ## Initialize variable to be assigned
    nodes = NULL,
    layer_no = NULL,
    layer_type = NULL,
    default_activation_type = NULL,
    
    ## assign variables
    initialize = function(layer_no, layer_type) {
      self$nodes <- list()
      self$layer_no <- layer_no
      
      if (layer_type %in% c(LAYER_TYPE_INPUT, LAYER_TYPE_OUTPUT, LAYER_TYPE_HIDDEN)) {
        self$layer_type = layer_type
      }
      else {
        throw("Layer type must be 'input', 'hidden', or 'output'.")
      }
      
      if (layer_type == LAYER_TYPE_INPUT & layer_no != 0) {
        throw("The input layer must always be layer_no 0.")
      }
      
      if (self$layer_type == LAYER_TYPE_INPUT) {
        self$default_activation_type <- "linear"
      }
      else if (self$layer_type == LAYER_TYPE_OUTPUT) {
        self$default_activation_type <- "linear"
      }
      else {
        self$default_activation_type <- "sigmoid"
      }
      
      self$set_activation_type(self$default_activation_type)
    },
    ## This function returns the total nodes.  It can also return the total
    ## nodes of a particular type, such as 'copy'.
    total_nodes = function(node_type = NULL) {
      count <- 0
      if (!is.null(node_type)) {
        for (node in self$nodes) {
          if (node$node_type == node_type) {
            count <- count + 1
          }
        }
        return(count)
      }
      else {
        return(length(self$nodes))
      }
    },
    ## Looks for nodes that don't have an input connection
    ## Finish with help from cole
    unconnected_nodes = function() {
      index <- 1
      unconnected_list <- numeric()
      for (node in self$nodes) {
        if (length(node$input_connections) == 0) {
          unconnected_list[index] <- node$node_no
          index <- index + 1
        }
      }
      unconnected_list
    },
    ## This function returns the values for each node as a vector.
    values = function() {
      index <- 1
      val_list <- numeric(length(self$nodes))
      for (node in self$nodes) {
        val_list[index] <- node$get_value()
        index = index + 1
      }
      val_list
    },
    ## This function returns the activation values for each node as a list
    activations = function() {
      act_list <- list()
      index <- 1
      for (node in self$nodes) {
        act_list[index] <- node$activate()
        index = index + 1
      }
      return(act_list)
    },
    ## This function is a mechanism for setting the activation type
    ## for an entire layer.  If most nodes need to one specific type,
    ## this function can be used, then set whatever nodes individually
    ## after this use.
    set_activation_type = function(activation_type) {
      for (node in self$nodes) {
        if (node$node_type != 'bias') {
          node$set_activation_type(activation_type)
        }
      }
      invisible(self)
    },
    ## This function adds nodes in bulk for initialization.
    ## If an optional activation type is passed through, that will be set for
    ## the nodes.  Otherwise, the default activation type for the layer will
    ## be used.
    add_nodes = function(number_nodes,
                         node_type,
                         activation_type = NULL) {
      count <- 0
      while (count < number_nodes) {
        if (node_type == 'copy') {
          node <- CopyNode$new()
        }
        else {
          node <- Node$new(node_type)
        }
        
        if (!is.null(activation_type)) {
          node$set_activation_type(activation_type)
        }
        self$add_node(node)
        count <- count  + 1
      }
      invisible(self)
    },
    ## This function adds a node that has already been formed.  Since it can
    ## originate outside of the initialization process, the activation type is
    ## assumed to be set appropriately already.
    add_node = function(node) {
      node$node_no <- self$total_nodes()
      if (toString(node$node_type) != 'bias' || is.null(node$get_activation_type())) {
        node$set_activation_type(self$default_activation_type)
      }
      self$nodes <- append(self$nodes, node)
      invisible(self)
    },
    ## This function returns the node associated with the node_no.
    ## Although it would seem to be reasonable to look it up by
    ## position within the node list, because sparse nodes are supported,
    ## there might be a mis-match between node_no and position within the
    ## list.
    get_node = function(node_no) {
      for (node in self$nodes) {
        if (node$node_no == node_no) {
          return(node)
        }
      }
      return(FALSE)
      invisible(self)
    },
    ## This function returns all the nodes of a layer.  Optionally it can
    ## return all of the nodes of a particular type, such as 'copy'.
    get_nodes = function(node_type = NULL) {
      node_list <- list()
      if (is.null(node_type)) {
        for (node in self$nodes) {
          node_list <- append(node_list, node)
        }
      }
      else {
        for (node in self$nodes) {
          if (node$node_type == node_type) {
            node_list <- append(node_list, node)
          }
        }
      }
      node_list
    },
    ## This function accepts a lower layer within a network and for each node
    ## in that layer connects the node to nodes in the current layer.
    ## An exception is made for bias nodes. There is no reason to
    ## connect a bias node to a lower layer, since it always produces a 1.0
    ## for its value and activation.
    connect_layer = function(lower_layer, currentLayer) {
      for (node in currentLayer$nodes) {
        if (toString(node$node_type) != 'bias') {
          for (lower_node in lower_layer$nodes){
            conn <- Connection$new(lower_node, node)
            node$add_input_connection(conn)
          }
        }
      }
      invisible(self)
    },
    ## This takes a list of inputs that applied sequentially to
    ## each node in the input_layer
    load_inputs = function(inputs) {
      if (self$layer_type != LAYER_TYPE_INPUT) {
        throw("Inputs are only entered into the input layer")
      }
      for (i in 1:length(inputs)) {
        node <- self$nodes[[i]]
        if (node$node_type != LAYER_TYPE_INPUT) {
          throw("Attempting to load an input into a non-input node")
        }
        if (is.numeric(inputs[[i]])) {
          node$set_value(inputs[[i]])
        }
        else {
          throw("Invalid value. Must be numeric.")
        }
      }
      invisible(self)
    },
    ## This takes a list of targets that applied sequentially to
    ## each node in the output_layer
    load_targets = function(targets) {
      if (self$layer_type  != LAYER_TYPE_OUTPUT) {
        throw("Target values are only loaded to output layer.")
      }
      if (length(targets) != length(self$nodes)) {
        throw("Number of targets != Number of nodes")
      }
      for (i in 1:length(self$total_nodes())) {
        node <- self$nodes[[i]]
        if (is.numeric(targets[[i]])) {
          node$set_value(targets[i])
        }
        else {
          throw("Invalid value. Must be Numeric.")
        }
        node <- self$nodes[[i]]
        node$target <- target[i]
      }
      invisible(self)
    },
    ## This function builds random weights for all the input connections in
    ## the layer.
    randomize = function(random_constraint) {
      for (node in self$nodes) {
        node$randomize(random_constraint)
      }
      invisible(self)
    },
    ## This function loops through the nodes on the layer and causes each
    ## node to feedforward values from nodes below that node.
    feed_forward = function() {
      for (node in self$nodes) {
        if (node$node_type != 'bias') {
          node$feed_forward()
        }
      }
      invisible(self)
    },
    ## This function loops through the nodes on the layer and causes each
    ## node to feedforward values from nodes below that node.
    update_error = function(halt_on_extremes) {
      for (node in self$nodes) {
        node$update_error(halt_on_extremes)
      }
      invisible(self)
    },
    ## This function loops through the nodes causing each node to adjust the
    ## weights as a result of errors and the learning rate.
    adjust_weights = function(learnrate, halt_on_extremes) {
      for (node in self$nodes) {
        if (node$node_type != 'bias') {
          node$adjust_weights(learnrate, halt_on_extremes)
        }
      }
      invisible(self)
    },
    ## This function returns a list of the error with each node.
    get_errors = function() {
      error_list <- numeric()
      index <- 1
      for (node in self$nodes) {
        error_list[index] <- node$error
      }
      error_list
    },
    ## This function returns a list of the weights of input connections into
    ## each node in the layer.
    get_weights = function() {
      weights <- numeric()
      index <- 1
      for (conn in node$input_connections) {
        weights[index] <- conn$get_weight()
      }
      weights
    }
  )
)