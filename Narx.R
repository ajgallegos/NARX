library(R6)
library(R.oo)

library(iterators)

NeuralNet <- R6Class(classname = "NeuralNet", 
  ## Declare fields
  ## Start all as private, maybe move to public later
  public = list(
    ##initialize variables
    # Because there are a number of parameters to specify, there are
    # no specific variables that are initialized within this.
    isNeuralNet = NULL,
    learnrate = NULL,
    randomConstraint = NULL,
    epochs = NULL,
    layers = NULL,
    dataRange = NULL,
    allInputs = NULL,
    allTargets = NULL,
    timeDelay = NULL,
    allResults = NULL,
    mse = NULL,
    mseAccum = NULL,
    validationTargetsActivations = NULL,
    testTargetsActivations = NULL,
    haltOnExtremes = NULL,
    inputLayer = NULL,
    outputLayer = NULL,
    
    ## Initialize the NeuralNet
    initialize = function() {
      self$isNeuralNet <- TRUE
      self$learnrate <- .1
      self$randomConstraint <- 1
      self$epochs <- 1
      self$layers <- list()
      self$dataRange <-
        list(
          "Learning" = list(NULL, NULL),
          "Validation" = list(NULL, NULL),
          "Test" = list(NULL, NULL)
        )
      
      self$allInputs <- numeric()
      self$allTargets <- numeric()
      self$timeDelay <- 0
      
      ## where all results are stored
      self$allResults <- numeric()
      
      ## holds mse for the test
      self$mse <- numeric()
      
      # holds accumulated mse for each epoch tested
      self$mseAccum <- list()
      self$validationTargetsActivations <- list()
      self$testTargetsActivations <- list()
      
      self$haltOnExtremes <- FALSE
      
      self$inputLayer <- NULL
      self$outputLayer <- NULL
    },
    
    init_layers = function(inputNodes,
                           totalHiddenNodesList,
                           outputNodes,
                           recurrentMods = NULL) {
      # This function initializes the layers.
      # The variables:
      #   
      #   * input_nodes: the number of nodes in the input layer
      # * total_hidden_nodes_list:  a list of numbers of nodes in the
      # hidden layer.  For example, [5, 3]
      # * output_nodes: the number of nodes in the output layer
      # 
      # The initial network is created, and then a series of modifications can
      # be made to enable recurrent features.  recurrent_mods are
      # configurations for modifications to the neural network that is created
      # within init_layers.
      # 
      # For example, if
      # init_layers(input_nodes, total_hidden_nodes_list, output_nodes,
      #             ElmanSimpleRecurrent())
      # was used, then the initial network structure of input, hidden, and
      # output nodes would be created.  After that, the additional copy or
      # context nodes that would automatically transfer values from the lowest
      # hidden layer would be added to the input layer.
      # 
      # More than one recurrent scheme can be applied, each one adding to the
      # existing network.
      ## Input layer
      self$layers <- list()
      layer <- Layer$new(length(self$layers), NODE_INPUT)
      layer$add_nodes(inputNodes, NODE_INPUT)
      bias.node <- BiasNode$new()
      layer$add_node(bias.node)
      self$layers <- append(self$layers, layer)
      self$inputLayer <- layer

      ## Hidden layer
      for (hiddenNode in totalHiddenNodesList) {
        layer <- Layer$new(length(self$layers), NODE_HIDDEN)
        layer$add_nodes(hiddenNode, NODE_HIDDEN)
        bias.node <- BiasNode$new()
        layer$add_node(bias.node)
        self$layers <- append(self$layers, layer)
      }
      
      ## Output layer
      layer <- Layer$new(length(self$layers), NODE_OUTPUT)
      layer$add_nodes(outputNodes, NODE_OUTPUT)
      
      self$layers <- append(self$layers, layer)
      self$outputLayer <- layer
      
      self$initConnections()
      
      if(!is.null(recurrentMods)){
        for (recurrentMod in recurrentMods) {
          recurrentMod$apply_config(self)
        }
      }
    },
    
    set_halt_on_extremes = function(halt) {
      # This function sets the flag as to whether the program should halt when
      # experiencing extremely positive or negative numbers.  This can happen
      # when using linear functions and data that may not be normalized.  Such
      # things as nan and inf can be experienced otherwise.  Not halting
      # instead, simply scales back the values to LARGEVALUE_LIMIT and
      # NEGVALUE_LIMIT. A satisfactory output from the network may be in doubt,
      # but at least it gives it the possibility.
      if (!(is(halt, "logical"))) {
        throw("Halt must be a True or False, Halt: ", halt)
      }
      else {
        self$haltOnExtremes <- halt
      }
    },
    
    get_halt_on_extremes = function() {
      # This function returns the True/False flag for halting on extremes.
      return(self$haltOnExtremes)
    },
    
    set_random_constraint = function(constraint) {
      # This fuction sets a value between 0 and 1 for limiting the random
      # weights upon initialization.  For example, .8 would limit weights to
      # -.8 through .8.
      if (!(is(constraint, "numeric")) | !between(constraint, 0, 1)) {
        throw("The constraint must be a float between 0.0 and 1.0, constraint: ",
              constraint)
      }
      else {
        self$randomConstraint <- constraint
      }
    },
    
    get_random_constraint = function() {
      # This function gets the random constraint used in weights
      # initialization.s
      return(self$randomConstraint)
    },
    
    set_epochs = function(epochs) {
      # This function sets the number of epochs or cycles through the learning
      # data.
      if (!(is(epochs, "numeric")) | (epochs <= 0)) {
        throw("The epochs must be an numeric, epochs: ", epochs)
      }
      else {
        self$epochs <- epochs
      }
    },
    
    get_epochs = function() {
      # This function gets the number of epochs that will run during learning.
      return(self$randomConstraint)
    },
    
    set_time_delay = function(timeDelay) {
      # This function sets a value for time delayed data.  For example, is the
      # time delay was 5, then input values would be taken 5 at a time.  Upon
      # the next increment the next input values would be 5, with 4 of the
      # previous values included, and one new value.
      if (!is(timeDelay, "numeric") | timeDelay < 0) {
        throw("Time delay must be an numeric greater than or equal to zero, time delay: ",
              timeDelay)
      }
      else {
        self$timeDelay <- timeDelay
      }
    },
    
    get_time_delay = function() {
      # This function gets the time delay to be used with timeseries data.
      return(self$timeDelay)
    },
    
    set_all_targets = function(allTargets) {
      # This function sets the targets.
      self$allTargets <- allTargets
    },
    
    set_all_inputs = function(allInputs) {
      # This function sets the inputs.  Inputs are basically treated as a
      # list.
      self$allInputs <- allInputs
    },
    
    set_learnrate = function(learnrate) {
      # This function sets the learn rate for the modeling.  It is used to
      # determine how much weight to associate with an error when learning.
      if (!is(learnrate, "numeric") | !between(learnrate, 0, 1)) {
        throw("Learnrate must be a numeric between 0.0 and 1.0, learnrate: ",
              learnrate)
      }
      else {
        self$learnrate <- learnrate
      }
    },
    
    get_learnrate = function() {
      # This function gets the learn rate for the modeling.  It is used to
      # determine how much weight to associate with an error when learning.
      return(self$learnrate)
    },
    
    set_data_range = function(dataType, startPosition, endPosition) {
      # This function sets the data positions by type
      if (!(is(startPosition, "numeric")) |
          !(is(endPosition, "numeric"))) {
        throw("start and end position must be an numeric")
      } else {
        self$dataRange[[dataType]] <- list(startPosition, endPosition)
      }
    },
    
    set_learn_range = function(startPosition, endPosition) {
      # This function sets the range within the data that is to used for
      # learning.
      self$set_data_range(
        dataType = "Learning",
        startPosition = startPosition,
        endPosition = endPosition
      )
      
    },
    
    get_learn_range = function() {
      # This function gets the range within the data that is to used for
      # learning.
      return(self$dataRange["Learning"])
    },
    
    check_time_delay = function(position) {
      # This function checks the position or index of the data and determines
      # whether the position is consistent with the time delay that has been
      # set.
      if (position - self$timeDelay < 0) {
        throw("Invalid start position with time delayed data")
      }
    },
    
    get_learn_data = function(randomTesting = FALSE) {
      # This function is a generator for learning data.  It is assumed that in
      # many cases, this function will be over-written with a situation
      # specific function.
      startPosition <- self$dataRange["Learning"][[1]][[1]]
      endPosition <- self$dataRange["Learning"][[1]][[2]]
      yield <- list()
      if (randomTesting) {
        counter <- 1
        for (i in self$get_randomized_position(startPosition, endPosition)) {
          timeDelay = self$get_time_delay()
          if (timeDelay > 0) {
            start <- i - timeDelay
            inputs <- self$allInputs[start:i + 1]
          } else {
            inputs <- self$allInputs[[i]]
            
          }
          targets <- self$allTargets[[i]]
          yield[[counter]] <- list(inputs, targets) ## Yield function in R
          counter <- counter + 1
  
        }
        return(yield)
      }
      else {
        i <- 1
        for (inputAndTargets in self$get_data(startPosition, endPosition)) {
          yield[[i]] <- c(inputAndTargets[1], inputAndTargets[2]) ## Substitute for yield function in R
          i <- i + 1
        }
        return(yield)
      }
    },
    
    get_validation_data = function() {
      # This function is a generator for validation data.  It is assumed that
      # in many cases, this function will be over-written with a situation
      # specific function.
      startPosition <- self$dataRange["Validation"][1]
      endPosition <- self$dataRange["Learning"][2]
      self$checkPositions(startPosition, endPosition)
      yield <- numeric()
      for (inputsAndTargets in self$get_data(startPosition, endPosition)) {
        yield <- append(yield, c(inputsAndTargets[1], inputsAndTargets[2]))
      }
      return(yield)
    },
    
    get_test_data = function() {
      # This function is a generator for testing data.  It is assumed that in
      # many cases, this function will be over-written with a situation
      # specific function.
      startPosition <- self$dataRange[["Test"]][[1]]
      endPosition <- self$dataRange[["Test"]][[2]]
      self$check_positions(startPosition, endPosition)
      yield <- list()
      index <- 1
      for (inputsAndTargets in self$get_data(startPosition, endPosition)) {
        if(!is.null(inputsAndTargets[[1]])){
          yield[[index]] <-list(inputsAndTargets[[1]], inputsAndTargets[[2]])
          index <- index + 1
        }
      }
      return(yield)
    },
    
    get_data = function(startPosition, endPosition) {
      # This function gets an input from the list of all inputs.
      i <- startPosition
      if (endPosition > length(self$allInputs)) {
        throw("endPosition is past end of allInputs, endPosition: ",
              endPosition)
      }
      yield <- list()
      while (i < endPosition) {
        timeDelay <- self$get_time_delay()
        if (timeDelay > 0) {
          start <- i - timeDelay
          inputs <- list()
          for (input in self$allInputs[start:(i + 1)]) {
            inputs[[i]] <-input[[1]]
          }
        } 
        else{
          inputs <- self$allInputs[[i]]
        }
        targets <- self$allTargets[[i]]
        yield[[i]] <- list(inputs, targets)
        i <- i + 1
      }
      return(yield)
    },
    
    get_randomized_position = function(startPosition, endPosition) {
      # This function accepts integers representing a starting and ending
      # position within a set of data and yields a position number in a random
      # fashion until all of the positions have been exhausted.
      order = c(startPosition:endPosition)
      randomOrder = sample(order)
      return(randomOrder)
    },
    
    check_positions = function(startPosition, endPosition) {
      # This function evaluates validates, somewhat, start and end positions
      # for data ranges.
      if (is.null(startPosition)) {
        throw("Start Position is not defined")
      }
      if (is.null(endPosition)) {
        throw("End Position is not defined")
      }
      if (startPosition > endPosition) {
        throw("Start Position cannot be greater than End Position")
      }
      self$check_time_delay(startPosition)
    },
    
    set_validation_range = function(startPosition, endPosition) {
      # This function sets the start position and ending position for the
      # validation range.  The first test period is often used to test the
      # current weights against data that is not within the learning period
      # after each epoch run.
      self$setDataRange("Validation", startPosition, endPosition)
    },
    
    get_validation_range = function() {
      # This function gets the start position and ending position for the
      # validation range.  The first test period is often used to test the
      # current weights against data that is not within the learning period
      # after each epoch run.
      return(self$dataRange["Validation"])
    },
    
    set_test_range = function(startPosition, endPosition) {
      # This function sets the start position and ending position for
      # the out-of-sample range.
      self$set_data_range("Test", startPosition, endPosition)
    },
    
    get_test_range = function() {
      # This function gets the start position and ending position for
      # the out-of-sample range.
      return(self$dataRange["Test"])
    },
    
    initConnections = function() {
      # Init connections sets up the linkages between layers.
      # 
      # This function connects all nodes, which is typically desirable
      # However, note that by substituting in a different process, a
      # sparse network can be achieved.  And, there is no restriction
      # to connecting layers in a non-traditional fashion such as skip-layer
      # connections.
      for (layer in self$layers[2:length(self$layers)]) {
        self$connectLayer(layer)
      }
    },
    
    connectLayer = function(layer) {
      # Generates connections to the lower layer.
      # 
      # If it is the input layer, then it's skipped
      #   It could raise an error, but it seems pointless.
      lowerLayerNo <- layer$layer_no 
      if (lowerLayerNo >= 1) {
        lowerLayer <- self$layers[[lowerLayerNo]]
        layer$connect_layer(lowerLayer, layer)
      }
  },
    
    randomize_network = function() {
      # This function randomizes the weights in all of the connections.
      for (layer in self$layers) {
        if (layer$layer_type != NODE_INPUT) {
          layer$randomize(self$randomConstraint)
        }
      }
    },
    
    learn = function(epochs = NULL,
                     showEpochResults = TRUE,
                     randomTesting = FALSE,
                     showSampleInterval = 0) {
      # This function performs the process of feeding into the network inputs
      # and targets, and computing the feedforward process.  After the
      # feedforward process runs, the actual values calculated by the output
      # are compared to the target values.  These errors are then used by the
      # back propagation process to adjust the weights for the next set of
      # inputs. If a recurrent netork structure is used, the stack of copy
      # levels is pushed with the latest set of hidden nodes.
      # 
      # Then, the next set of inputs is input.
      # 
      # When all of the inputs have been processed, resulting in the
      # completion of an epoch, if show_epoch_results=True, then the MSE will
      # be printed.
      # 
      # Finally, if random_testing=True, then the inputs will not be processed
      # sequentially.  Rather, the inputs will be sorted into a random order
      # and then input.  This is very useful for timeseries data to avoid
      # autocorrelations.
      if (!is.null(epochs)) {
        self$set_epochs(epochs = epochs)
      }
      self$mseAccum = list()
      for (epoch in 1:self$epochs) { 
        summedErrors <- 0
        count <- 0
        for (inputsAndTargets in self$get_learn_data(randomTesting)) {
          self$process_sample(inputsAndTargets[[1]], inputsAndTargets[[2]], TRUE)
          summedErrors <- summedErrors + self$calc_sample_error()
          count <- count + 1
          
          if (showSampleInterval > 0) {
            if (count %% showSampleInterval == 0) {
              print(
                paste("epoch ", epoch, "of ", self$epochs, ", sample: ", count, " errors: ",summedErrors)
              )
            }
          }
        }
        print(summedErrors)
        mse <- self$calcMse(summedErrors, count)
        if (showEpochResults) {
          print(paste("epoch: ", epoch, " MSE: ", mse))
        }
        self$mseAccum <- append(self$mseAccum, mse)
      }
    },
    
    validate = function(showSampleInterval = 0) {
      # This function loads and feedforwards the network with validation data.
      # Optionally, it can also store the actuals as well.
      return(self$evaluate("V", showSampleInterval))
    },
    
    test = function(showSampleInterval = 0) {
      # This function loads and feedforwards the network with test data.
      # Optionally, it can also store the actuals as well.
      return(self$evaluate("T", showSampleInterval))
    },
    
    evaluate = function(evalType, showSampleInterval) {
      # This function loads and feedforwards the network with data.
      # eval_type is either validation or test data ('t' or 'v')
      # Optionally, it can also store the actuals as well.
      if (evalType == "T") {
        evalList <- self$testTargetsActivations
        getData <- self$get_test_data
      } else if (evalType == "V") {
        evalList <- self$validationTargetsActivations
        getData <- self$get_validation_data
      }
      summedErrors <- 0
      count <- 0
      index <- 1
      for (inputsAndTargets in getData()) {
        if(!is.null(inputsAndTargets[[1]])){
          self$process_sample(inputsAndTargets[[1]], inputsAndTargets[[2]], learn = FALSE)
          summedErrors <- summedErrors + self$calc_sample_error()
          if(evalType == "T"){
            self$testTargetsActivations[[index]] <- list(inputsAndTargets[[2]], self$outputLayer$activations())
          } 
          else if(evalType == "V"){
            self$validationTargetsActivations[[index]] <- list(inputsAndTargets[[2]], self$outputLayer$activations())
          }
          index <- index + 1
          count <- count + 1
          if (showSampleInterval > 0) {
            if (count %% showSampleInterval == 0) {
              print(paste("sample: ", count, " errors: ", summedErrors))
            }
          }
        }
      }
      self$mse <- self$calcMse(summedErrors, count)
      return(self$mse)
    },
    
    calcMse = function(totalSummedErrors, count) {
      # This function calculates mean squared errors.
      mse <- (totalSummedErrors / count) / 2
      if (!is.numeric(mse)) {
        throw("Mean Squared Error is not a number")
      }
      return(mse)
    },
    
    process_sample = function(inputs, targets, learn = FALSE) {
      # Accepts inputs and targets, then forward and back propagations.  A
      # comparison is then made of the generated output with the target values.
      # 
      # Note that this is for an incremental learn, not the full set of inputs
      # and examples.
      self$inputLayer$load_inputs(inputs)
      if (length(targets) != 0) {
        self$outputLayer$load_targets(targets)
      }
      
      self$feedForward()
      
      if (length(targets) != 0 && learn == TRUE) {
        self$back_propagate()
      } 
      else if (length(targets) != 0) {
        self$update_error(TRUE)
      }
      self$copy_levels()
      invisible(self)
    },
    
    feedForward = function() {
      # This function starts with the first hidden layer and
      # gathers the values from the lower layer, applies the
      # connection weightings to those values, and activates the
      # nodes.  Then, the next layer up is selected and the process
      # is repeated; resulting in output values in the upper-most
      # layer.
      for (layer in self$layers[2:length(self$layers)]) {
        layer$feed_forward()
      }
    },
    
    back_propagate = function() {
      # Backpropagate the error through the network. Aside from the
      # initial compution of error at the output layer, the process takes the
      # top hidden layer, looks at the output connections reaching up to the
      # next layer, and carries the results down through each layer back to the
      # input layer.
      self$update_error(FALSE)
      self$adjust_weights()
    },
    
    update_error = function(toponly) {
      # This function goes through layers starting with the top hidden layer
      # and working its way down to the input layer.
      # 
      # At each layer, the errors are updated in the nodes from the errors and
      # weights in connections to nodes in the upper layer.
      if (toponly == FALSE) {
        self$zero_errors()
      }
      
      if (toponly == TRUE) {
        self$outputLayer$update_error(self$get_halt_on_extremes())
      } 
      else{
        for (layerNo in length(self$layers):1) {
          self$layers[[layerNo]]$update_error(self$get_halt_on_extremes())
        }
      }
      invisible(self)
    },
    
    zero_errors = function() {
      # This function sets the node errors to zero in preparation for back
      # propagation.
      for (layer in self$layers) {
        for (node in layer$nodes) {
          node$error <- 0
        }
      }
    },
    
    adjust_weights = function() {
      # This function goes through layers starting with the top hidden layer
      # and working its way down to the input layer.
      # 
      # At each layer, the weights are adjusted based upon the errors.
      loop <- c(1:length(self$layers))
      backLoop <- rev(loop) ## To loop backwards through the layer
      for (layerNo in backLoop) {
        layer <- self$layers[[layerNo]]
        layer$adjust_weights(self$learnrate, self$get_halt_on_extremes())
      }
    },
    
    calc_sample_error = function() {
      # The mean squared error (MSE) is a measure of how well the outputs
      # compared to the target values.
      total <- 0
      for (node in self$outputLayer$nodes) {
        total <- total + (node$error ^ 2)
      }
      return(total)
    },
    
    copy_levels = function() {
      # This function advances the copy node values, transferring the values
      # from the source node to the copy node.  In order to avoid stomping on
      # the values that are to be copies, it goes from highest node number to
      # lowest.
      # 
      # No provision is made at this point to exhaustively check precedence.
      for (layer in self$layers) {
        loop = c(1:layer$total_nodes())
        backLoop <- rev(loop) ## To loop backwards through the nodes
        ## Ask Alex if there's a better way to do this
        for (i in backLoop) {
          node <- layer$nodes[[i]]
          if (node$node_type == "copy") {
            node$load_source_value()
          }
        }
      }
    },
    
    output_values = function() {
      # This function outputs the values of the network.  It is meant to be
      # sufficiently complete that, once saved to a file, it could be loaded
      # back from that file completely to function.
      # 
      # To accommodate configparser, which is used as the file format, there is
      # a form of [category],
      # label = value
      # 
      # Since there are also no sub-categories possible, so the naming
      # structure is designed to take that into account. This accommodation
      # also leads to a couple design choices: Each layer is given a separate
      # category and a list of nodes follows.  Then each node has a separate
      # category identifying it by layer number and node number.  This can't be
      #   inferred from just knowing the number of nodes in the layer and
      #   sequentially reading, because if a sparse network is used, then the
      #   node numbers may be out of sync with the position of the node within of
      #   the layer.
      output <-
        paste(
          "[net]\n",
          "input_neurons = ",
          self$inputLayer$total_nodes(NODE_INPUT),
          "\n hidden_neurons = "
        )
      ## MISSING a couple of lines
      
      if (self$layers[2]$total_nodes(NODE_HIDDEN) > 0) {
        ## NEED to add here
      }
      output <- paste(output, "\n learnrate = ", self$learnrate)
      output <- paste(output, "\n epochs = ", self$epochs)
      output <- paste(output, "\n time delay = ", self$timeDelay)
      output <-
        paste(output, "\n halt on extremes = ", self$haltOnExtremes)
      output <-
        paste(output, "\n random constraint = ", self$randomConstraint)
      output <- paste(output, "\n\n")
      
      for (layer in self$layers) {
        output <- paste(output, "[layer ", layer$layer_no, "]\n")
        output <-
          paste(output, "[layer type ", layer$layer_type, "]\n")
        output <- paste(output, "nodes = ")
        for (node in layer$nodes) {
          output <- paste("\n node-", layer$layer_no, ":", node$node_no)
        }
        output <- paste(output, "\n\n")
        for (node in layer$nodes) {
          output <- paste(output, "[layer ", layer$layer_no, "]\n")
          output <-
            paste(output, "[layer type ", layer$layer_type, "]\n")
          if (node.is(CopyNode)) {
            snode <- node$get_source_node()
            output <-
              paste(output, "\n source node = ", self$node_id(snode))
            output <-
              paste(output, "\n source type = ", node$get_source_type())
            output <-
              paste(output,
                    "\n incoming weight = ",
                    node$get_incoming_weight())
            output <-
              paste(output,
                    "\n existing weight = ",
                    node$get_existing_weight())
            output <-
              paste(output,
                    "\n, activation type = ",
                    node$get_activation_type())
          } else{
            output <-
              paste(output,
                    "\n activation type = ",
                    node$get_activation_type())
          }
          output <- paste(output, "\n connections = \n")
          for (conn in node$input_connections) {
            lowerNode <- conn$lower_node
            node_id <- self$node_id(lowerNode)
            output <-
              paste(output, node_id, ", ", conn$get_weight(), "\n")
          }
          output <- paste(output, "\n")
        }
        output <- paste("\n")
      }
      return(output)
    },
    
    node_id = function(node) {
      # This function receives a node, and returns an text based id that
      # uniquely identifies it within the network.
      return(paste("node-", node$layer$layer_no, ":", node$node_no))
    },
    
    save = function(filename) {
      # This function saves the network structure to a file.
      sink(filename)
      cat(self$outputValues())
      sink()
    }
  )
)
