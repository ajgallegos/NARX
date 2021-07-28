library(R6)
library(R.oo)
library(iterators)

Recurrent <- R6Class(classname = "Recurrent", list(
  public = list(
    sourceType = "a",
    activationType = ACTIVATION_LINEAR,
    incomingWeight <- 1,
    existing_weight <- 0,
    connectionType <- "m",
    copyLevels <- 1,
    copyNodesLayer <- 0,
    connectNodesLayer <- 1,
    
    applyConifg = function(neural_net){
      if(!neural_net.is(NeuralNet)){
        throw("neural net must be of NeuralNet class")
      }
      for(snode in self$getSourceNodes(neuralNet)){
        prevCopyNode <- None
        for(level in c(1: self$copyLevels())){
          copyNode <- self$CopyNode()
          if(level == 0){
            copyNode$set_source_node(snode)
          }else{
            copyNode$set_source_node(prev_copy_node)
          }
          copyNode$source_update_config(
            self$sourceType, self$incomingWeight, self$existingWeight
          )
          if(self$connectionType == "m"){
            self$fullyConnect(copyNode, self$getUppernodes(neuralNet))
          }else if(self$connectionType == "s"){
            self$fullyConnect(copyNode, self$geUpperNodes(neuralNet))
          }else{
            throw("Invalid Connection Type")
          }
          neuralNet$layers[self$copyNodesLayer]$addNode(copyNode)
          prevCopyNode <- copyNode
        }
      }
    }
    
    fullyConnect = function(lowerNode, upperNodes){
      for(upperNode in upperNodes){
        upperNode$add_input_connection(Connection(lowerNode, upperNode))
      }
    }
    
    getSourceNodes = function(neuralNet){
      return(neuralNet)
    }
    
    getUpperNodes = function(neuralNet){
      layer <- neuralNet$layers[self$connectNodesLayer]
      retval <- numeric()
      for(node in layer$nodes){
        if(node$node_type != NODE_BIAS){
          retval <- append(retval, node)
        }
      }
      return(retval)
    }
  )
))

NeuralNet <- R6Class(classname = "NeuralNet", list(
  ## Declare fields
  ## Start all as private, maybe move to public later
  public = list(
    learnrate <- 1,
    randomConstraint <- 1,
    epochs <- 1,
    layers <- numeric(),
    dataRange <- list("Learning" = list(NULL, NULL), "Validation" = list(NULL, NULL), "Test" = list(NULL, NULL)),
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
        self$layers$addNode(BiasNode())
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
      if(!(is(halt, bool))) throw("Halt must be a True or False, Halt: ", halt)
      else self$haltOnExtremes <- halt
    }
    
    getHaltOnExtremes = function() {
      return(self$haltOnExtremes)
    }
    
    setRandomConstraint = function(constraint) {
      if(!(is(constraint, float)) | !(0.0 < constraint <= 1.0)) {
        throw("The constraint must be a float between 0.0 and 1.0, constraint: ", constraint)
      } else {
        self$randomConstraint <- constraint
      }
    }
    
    getRandomConstraint = function() {
      return(self$randomConstraint)
    }
    
    setEpochs = function(epochs) {
      if(!(is(epochs, int)) | (epochs <= 0)) {
        throw("The epochs must be an int, epochs: ", epochs)
      } else {
        self$epochs <- epochs
      }
    }
    
    getEpochs = function() {
      return(self$randomConstraint)
    }
    
    setTimeDelay = function(timeDelay) {
      if(!is(timeDelay, int) | timeDelay < 0){
        throw("Time delay must be an int greater than or equal to zero, time delay: ", timeDelay)
      }else{
        self$timeDelay <- timeDelay
      }
    }
    
    getTimeDelay = function(){
      return(self$timeDelay)
    }
    
    setAllTargets = function(allTargets) {
      self$allTargets <- allTargets
    }
    
    setAllInputs = function(allInputs) {
      self$allInputs <- allInputs
    }
    
    setLearnrate = function(learnrate) {
      if(!is(learnrate, float) | !(0.0 < learnrate <= 1.0)) {
        throw("Learnrate must be a float between 0.0 and 1.0, learnrate: ", learnrate)
      } else {
        self$learnrate <- learnrate
      }
    }
    
    getLearnrate = function() {
      return(self$learnrate)
    }
    
    setDataRange = function(dataType, startPosition, endPosition) {
      if(!(is(startPosition, int)) | !(is(endPosition, int))) {
        throw("start and end position must be an int")
      } else {
        self$dataRange$dataType <- c(startPosition, endPosition)
      }
    }
    
    setLearnRange = function(startPosition, endPosition) {
      self$setDataRange(dataType = "Learning", startPosition = startPosition, endPosition = endPosition)
      
    }
    
    getLearnRange = function() {
      return(self$dataRange["Learning"])
    }
    
    checkTimeDelay = function(position){
      if(position - self$timeDelay < 0) {
        throw("Invalid start position with time delayed data")
      }
    }
    
    getLearnData = function(randomTesting = FALSE){
      startPosition <- self$dataRange["Learning"][1]
      endPosition <- self$dataRange["Learning"][2]
      yield <- numeric()
      if(randomTesting){
        for(i in self$getRandomizedPosition(startPosition, endPosition)) {
          timeDelay = self$getTimeDelay()
          if (timeDelay > 0){
            start <- i - timeDelay
            inputs <- self$allInputs[start:i+1]
          } else {
            inputs <- self$allInputs[i]
          }
          targets <- self$allTargets[i]
          yield <- append(yield, c(inputs, targets)) ## Yield function in R
        }
        return(yield)
      } else {
        for(inputAndTargets in self$getData(startPosition, endPosition)){
          yield <- append(yield, c(inputsAndTargets[1], inputsAndTargets[2])) ## Substitute for yield function in R
        }
        return(yield)
      }
  }
    
  getValidationData = function(){
    startPosition <- self$dataRange["Validation"][1]
    endPosition <- self$dataRange["Learning"][2]
    self$checkPositions(startPosition, endPosition)
    yield <- numeric()
    for(inputsAndTargets in self$getData(startPosition, endPosition)){
      yield <- append(yield, c(inputsAndTargets[1], inputsAndTargets[2]))
    }
    return(yield)
  }
  
  getTestData = function(){
    startPosition <- self$dataRange["Test"][1]
    endPosition <- self$dataRange["Learning"][2]
    self$checkPositions(startPosition, endPosition)
    yield <- numeric()
    for(inputsAndTargets in self$getData(startPosition, endPosition)){
      yield <- append(yield, c(inputsAndTargets[1], inputsAndTargets[2]))
    }
    return(yield)
  }
  
  getData = function(startPosition, endPosition){
    i <- startPosition
    if(endPosition < length(self$allInputs)){
      throw("endPosition is past end of allInputs, endPosition: ", endPosition)
    }
    yield <- numeric()
    while(i < endPosition){
      timeDelay <- self$getTimeDelay()
      if(timeDelay > 0){
        start <- i - timeDelay
        inputs <- numeric()
        for(input in selfallInputs[start:i+1]){
          inputs <- append(inputs, input[1])
        }
      }else{
        inputs <- self$allinputs[i]
      }
      targets <- self$allTargets[i]
      yield <- append(yield, c(inputs, targets))
      i <- i + 1
    }
    return(yield)
  }
  
  getRandomizedPosition = function(startPosition, endPosition){
    order = c(startPosition: endPosition)
    randomOrder = sample(order)
    return(randomOrder)
  }
  
  checkPositions = function(startPosition, endPosition){
    if(startPosition.isNull){
      throw("Start Position is not defined")
    }
    if(endPosition.isNull){
      throw("End Position is not defined")
    }
    if(startPosition > endPosition){
      throw("Start Position cannot be greater than End Position")
    }
    self$checkTimeDelay(startPosition)
  }
  
  setValidationRange = function(startPosition, endPosition){
    self$setDataRange("Validation", startPosition, endPosition)
  }
  
  getValidationRange = function(){
    return(self$dataRange["Validation"])
  }
  
  setTestRange = function(startPosition, endPosition){
    self$setDataRange("Test", startPosition, endPosition)
  }
  
  getTestRange = function(){
    return(self$dataRange["Test"])
  }
  
  initConnections = function(self){
    for(layer in self$layers[2:length(self$layers)]){
      self$connectLayer(layer)
    }
  }
  
  connectLayer = function(layer){
    lowerLayerNo <- layer.layer_no
    if(lowerLayer >= 1){
      lowerLayer <- self$layers[lowerLayerNo]
      layer$connectLayer(lowerLayer)
    }
  }
  
  randomizeNetwork = function(){
    for(layer in self$layers){
      if(layer$layer_type != NODE_INPUT){
        layer$randomize(self$randomConstraint)
      }
    }
  }
  
  learn = function(epochs = NULL, showEpochResults = TRUE, randomTesting = FALSE,
                   showSampleInterval = 0){
    if(!(epochs.isNull)){
      self$setEpochs(epochs = epochs)
    }
    self$accumMse = numeric()
    for(epoch in range(self$epochs)){
      summedErrors <- 0
      count <- 0
      for(inputsAndTargets in self$getLearnData(randomTesting)){
        self$processSample(inputs, targets, learn=TRUE)
        summedErrors <- summedErrors + self$calcSampleError()
        count <- count + 1
        if(showSampleInterval > 0){
          if(count %% showSampleInterval == 0){
            print(paste("epoch ", epoch, "of ", self$epochs, ", sample: ", count, " errors: ", summedErrors))
          }
        }
      }
      mse <- self$calcMse(summedErrors, count)
      if(showEpochResults){
        print(paste("epoch: ", epoch, " MSE: ", mse))
      }
      self$accumeMse$append(mse)
    }
  }
  
  validate = function(showSampleInterval = 0){
    return(self$evaluate("V", showSampleInterval))
  }
  
  test = function(showSampleInterval = 0){
    return(self$evaluate("T", showSampleInterval))
  }
  
  evaluate = function(evalType, showSampleInterval){
    if(evalType == "T"){
      evalList <- self$testTargetsActivations
      getData <- self$getTestData
    }else if(evalType == "T"){
      evalList <- self$validationTargetsActivations
      getData <- self$getTestData
    }
    summedErrors <- 0
    count <- 0
    for(inputsAndTargets in getData()){
      self$processSample(inputsAndTargets[1], inputsAndTargets[2], learn = FALSE)
      summedErrors <- summedErrors + self$calcSampleError()
      evalList <- append(evalList, numeric(inputsAndTargets[2], self$outputLayer$activations()))
      if(showSampleInterval > 0){
        if(count %% showSampleInterval == 0){
          print(paste("sample: ", count, " errors: ", summedErrors))
        }
      }
    }
    self$mse <- self$calcMse(summedErrors, count)
    return(self$mse)
  }
  
  calcMse == function(totalSummedErrors, count){
    mse <- (totalSummederrors / count) / 2
    if(!is.numeric(mse)){
      throw("Mean Squared Error is not a number")
    }
    return(mse)
  }
  
  processSample = function(inputs, targets, learn = FALSE){
    self$inputLayer$loadInputs(inputs)
    if(targets){
      self$outputLayer$loadTargets(targets)
    }
    self$feedForward()
    if(!targets.isNull && !learn.isNull){
      self$backPropagate()
    }else if(!targets.isNull){
      self$updateError(toponly = TRUE)
    }
    self$copyLevels()
  }
  
  feedForward = function(){
    for(layer in self$layers[2:length(self$layers)]){
      layer$feed_forward()
    }
  }
  
  backPropagate = function(){
    self$updateError(toponly = FALSE)
    self$adjustWeights()
  }
  
  updateError = function(toponly){
    if(toponly == FALSE){
      self$zeroerrors()
    }
    if(toponly == TRUE){
      self$outputLayer$update_error(self$getHaltonExtremes())
    }else{
      for(layerNo in length(self$layers)){
        self$layers[layerNo]$update_error(self$getHaltOnExtremes())
      }
    }
  }
  
  zeroErrors = function(){
    for(layer in self$layers){
      for(node in layer$nodes){
        node$error <- 0
      }
    }
  }
  
  adjustWeights = function(){
    loop <- c(1: len(self$layers))
    backLoop <- rev(loop) ## To loop backwards through the layer
    for(layerNo in backLoop){
      layer <- self$layers[layerNo]
      layer$adjust_weights(self$learnrate, self$getHaltOnExtremes())
    }
  }
  
  calcSampleError = function(){
    total <- 0
    for(node in self$outputLayer$nodes){
      total <- total + (node$error ** 2)
    }
    return(total)
  }
  
  copyLevels = function()
    for(layer in self$layers){
      loop = c(1:layer$total_nodes())
      backLoop <- rev(loop) ## To loop backwards through the nodes
      ## Ask Alex if there's a better way to do this
      for(i in backLoop){
        node <- layer$nodes[i]
        if(node.is(copyNode)){
          node$load_source_value()
        }
      }
    }
  
  parseInputfileLayer = function(config, layerNo){
    layerId <- paste("layer ", layerNo)
    ## See what Alex thinks about reading in files
  }
  
  outputValues = function(){
    output <- paste("[net]\n", "input_neurons = ", self$inputLayer$total_nodes(NODE_INPUT), "\n hidden_neurons = ")
    ## MISSING a couple of lines
    
    if(self$layers[2]$total_nodes(NODE_HIDDEN) > 0){
      ## NEED to add here 
    }
    output <- paste(output, "\n learnrate = ", self$learnrate)
    output <- paste(output, "\n epochs = ", self$epochs)
    output <- paste(output, "\n time delay = ", self$timeDelay)
    output <- paste(output, "\n halt on extremes = ", self$haltOnExtremes)
    output <- paste(output, "\n random constraint = ", self$randomConstraint)
    output <- paste(output, "\n\n")
    
    for(layer in self$layers){
      output <- paste(output, "[layer ", layer$layer_no, "]\n")
      output <- paste(output, "[layer type ", layer$layer_type, "]\n")
      output <- paste(output, "nodes = ")
      for(node in layer$nodes){
        output <- paste("\n node-", layer$layer_no, ":", node$node_no)
      }
      output <- paste(output, "\n\n")
      for(node in layer$nodes){
        output <- paste(output, "[layer ", layer$layer_no, "]\n")
        output <- paste(output, "[layer type ", layer$layer_type, "]\n")
        if(node.is(CopyNode)){
          snode <- node$get_source_node()
          output <- paste(output, "\n source node = ", self$node_id(snode))
          output <- paste(output, "\n source type = ", node$get_source_type())
          output <- paste(output, "\n incoming weight = ", node$get_incoming_weight())
          output <- paste(output, "\n existing weight = ", node$get_existing_weight())
          output <- paste(output, "\n, activation type = ", node$get_activation_type())
        }else{
          output <- paste(output, "\n activation type = ", node$get_activation_type())
        }
        output <- paste(output, "\n connections = \n")
        for(conn in node$input_connections){
          lowerNode <- conn$lower_node
          node_id <- self$node_id(lowerNode)
          output <- paste(output, node_id, ", ", conn$get_weight(), "\n")
        }
        output <- paste(output, "\n")
      }
      output <- paste("\n")
    }
    return(output)
  }
  
  nodeId = function(node){
    return(paste("node-", node$layer$layer_no, ":", node$node_no))
  }
  
  save = function(filename){
    ## ASK Alex if this is the best way 
    sink(filename)
    cat(self$outputValues())
    sink()
  }
  )
))


