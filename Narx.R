library(R6)

NeuralNet <- R6Class(classname = "NeuralNet", list(
  ## Declare fields
  ## Start all as private, maybe move to public later
  private = list(
    learnrate <- 1,
    randomConstraint <- 1,
    epochs <- 1,
    layers <- list("Learning" = list(NULL, NULL), "Validation" = list(NULL, NULL), "Test" = list(NULL, NULL)),
    allInputs <- numeric(),
    allTargets <- numeric(),
    timeDelay <- numeric(), 
    allResults <- numeric(),
    mse <- numeric(),
    mseAccum <- numeric(), 
    validationTargetsActivations <- numeric(),
    testTargetsActivations <- numeric(),
    haltOnExtremes <- FALSE,
    inputLayer <- NULL,
    outputLayer <- NULL,
  ),
  
  ## init_layers function in NeuralNet.py file
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
  initialize = function(inputNodes, totalHiddenNodesList, outputNodes, recurrentMods) {
    self$layers = numeric()
    ## Need Alex's layer class
    layer = Layer$new(length(self$layers), NODE_INPUT) ## See what we're gonna do about node_input
    layer$addNodes(inputNodes, NODE_INPUT)
    layer$addNode(BiasNode()) ## BiasNode from nodes class
    self$layers <- append(self$layers, layer)
    inputLayer <- layer
    for(hiddenNode in totalHiddenNodesList){
      layer <- Layer$new(length(self$layers), NODE_HIDDEN)
      self$layers$addNodes(hid, NODE_HIDDEN)
      self$layers$addNode(BiasNOde())
      self$layers <- append(self$layers, layer)
    }
    self$layers <- Layer(length(self$layers), NODE_OUTPUT) ##From nodes class
    layer$add_nodes(outputNodes, NODE_OUTPUT) ##From nodes class
    self$layers <- append(self$layers, layer)
    output_layer <- layer
    self$initconnections()
    for(recurrentMod in recurrentMods){
      recurrentMod$apply_config() ## When we get to recurrent class
    }
  }
  
  setHaltOnExtremes = function(halt) {
    errMsg <- paste("Halt: ", halt, " -- Must be True or False")
  }
  
))