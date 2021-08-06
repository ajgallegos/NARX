Recurrent <- R6Class(
  classname = "Recurrent",
  public = list(
    ## Create initializion class for necessary variables
    sourceType = NULL,
    activationType = NULL,
    incomingWeight = NULL,
    existingWeight = NULL,
    connectionType = NULL,
    copy_levels = NULL,
    copyNodesLayer = NULL,
    connectNodesLayer = NULL,
    
    initialize = function() {
      # This is the base class for recurrent modifications.  It is not intended to
      # be used directly.
      self$sourceType <- "a"
      self$activationType <- ACTIVATION_LINEAR
      self$incomingWeight <- 1
      self$existingWeight <- 0
      self$connectionType <- "m"
      self$copy_levels <- 1
      self$copyNodesLayer <- 1
      self$connectNodesLayer <- 2
    },
    
    applyConfig = function(neural_net) {
      # This function modifies the neural net that is passed in by taking the
      # parameters that have been set in this class.  By having _apply_config,
      # subclassed versions of apply_config can take multiple passes with less
      # code. This function actually does the work.
      if (!neural_net$isNeuralNet) {
        throw("neural net must be of NeuralNet class")
      }
      for (snode in self$get_source_nodes(neural_net)) {
        prev_copy_node <- NULL
        for (level in 1:self$copy_levels) {
          copyNode <- CopyNode$new()
          
          if (level == 1) {
            copyNode$set_source_node(snode)
          }
          else{
            copyNode$set_source_node(prev_copy_node)
          }
          
          copyNode$source_update_config(self$sourceType,
                                        self$incomingWeight,
                                        self$existingWeight)
          if (self$connectionType == "m") {
            self$fullyConnect(copyNode, self$getUpperNodes(neural_net))
          }
          else if (self$connectionType == "s") {
            self$fullyConnect(copyNode, self$getUpperNodes(neural_net))
          }
          else{
            throw("Invalid Connection Type")
          }
          neural_net$layers[[self$copyNodesLayer]]$add_node(copyNode)
          prev_copy_node <- copyNode
        }
      }
    },
    
    fullyConnect = function(lowerNode, upperNodes) {
      # This function creates connections to each of the upper nodes.
      # 
      # This is a separate function from the one in layers, because using this
      # version does not require ALL of the nodes on a layer to be used.
      for (upperNode in upperNodes) {
        upperNode$add_input_connection(Connection$new(lowerNode, upperNode))
      }
    },
    
    getSourceNodes = function(neural_net) {
      return(neural_net)
    },
    
    getUpperNodes = function(neural_net) {
      layer <- neural_net$layers[[self$connectNodesLayer]]
      retval <- list()
      index <- 1
      for (node in layer$nodes) {
        if (node$node_type != NODE_BIAS) {
          retval[[index]] <- node
        }
      }
      return(retval)
    }
  )
)


NARXRecurrent <-
  R6Class(
    classname = 'NARXRecurrent',
    inherit = Recurrent,
    public = list(
      node_type_ = NULL,
      output_values = NULL,
      input_values = NULL,
      # This function takes:
      #   the output order, or number of copy levels of
      # output values,
      # the weight to apply to the incoming values from output nodes,
      # the input order, or number of copy levels of input values,
      # the weight to apply to the incoming values from input nodes
      
      initialize = function(output_order,
                            incoming_weight_from_output,
                            input_order,
                            incoming_weight_from_input) {
        super$initialize()
        
        self$node_type_ <- NULL
        self$existingWeight <- 0
        
        self$output_values <-
          c(output_order, incoming_weight_from_output)
        self$input_values <-
          c(input_order, incoming_weight_from_input)
      },
      
      get_source_nodes = function(neural_net) {
        # This function returns either the output nodes or input nodes depending
        # upon self._node_type.
        if (self$node_type_ == NODE_OUTPUT) {
          len <- length(neural_net$layers)
          return(neural_net$layers[[len]]$get_nodes(self$node_type_))
        }
        else if (self$node_type_ == NODE_INPUT) {
          return(neural_net$layers[[1]]$get_nodes(self$node_type_))
        }
      },
      
      apply_config = function(neural_net) {
        # This function first applies any parameters related to the output nodes
        # and then any with the input nodes.
        if (self$output_values[1] > 0) {
          self$node_type_ <- NODE_OUTPUT
          self$copy_levels <- self$output_values[1]
          self$incomingWeight <- self$output_values[2]
          
          self$applyConfig(neural_net)
        }
        
        if (self$input_values[1] > 0) {
          self$node_type_ <- NODE_INPUT
          self$copy_levels <- self$input_values[1]
          self$incomingWeight <- self$input_values[2]
          
          self$applyConfig(neural_net)
        }
        
      }
    )
  )
