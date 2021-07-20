library(R6)
library(R.oo)

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
          yeild(inputs, targets)
        }
      }
    }
  )
))

neural = NeuralNet("NarxNet")
