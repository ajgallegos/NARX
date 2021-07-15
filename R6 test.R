## Create a test R6 object
library(R6)

## Creates the class of "Person"
Person <- R6Class("Person",
                  ## creates all public methods
                  public = list(
                    ## use initialize to set variables and set them as NULL beforehand
                    name = NULL,
                    hair = NULL,
                    ## anytime Person$new is called this will be run
                    initialize = function(name = NA, hair = NA) {
                      self$name <- name
                      self$hair <- hair
                      ## note: on initilization this will call a greet this is ok
                      self$greet()
                    },
                    ## function to set hair value
                    set_hair = function(val) {
                      self$hair <- val
                    },
                    ## function to print a greeting
                    greet = function() {
                      cat(paste0("Hello, my name is ", self$name, ".\n"))
                    }
                  )
)

## initialize Ann
Ann <- Person$new("Ann", "black")

## call the greet function for Ann
Ann$greet()

## Set Ann's new hair value and have her greet again
Ann$set_hair("red")
Ann$hair

## note: this also works but only for publics and in general should add functions to update values
Ann$hair <- "black"

## Can also create privates in classes
Queue <- R6Class("Queue",
                 ## Create public functions
                 public = list(
                   ## initializes by adding all given parameters to the queue
                   initialize = function(...) {
                     for (item in list(...)) {
                       self$add(item)
                     }
                   },
                   ## adds the the private queue the list and return invisible(self) for purposes 
                   ## of chainability
                   add = function(x) {
                     private$queue <- c(private$queue, list(x))
                     invisible(self)
                   },
                   ## if the queue is empty return NULL and if not remove teh first item and print the item removed
                   remove = function() {
                     if (private$length() == 0) return(NULL)
                     # Can use private$queue for explicit access
                     head <- private$queue[[1]]
                     private$queue <- private$queue[-1]
                     head
                   }
                 ),
                 ## the actual queue is stored here with the function length which is not able to be accessed directly
                 private = list(
                   queue = list(),
                   length = function() base::length(private$queue)
                 )
)

## Creates new Queue with items 5, 6, and "foo"
q <- Queue$new(5, 6, "foo")

## adds to the queue (does not return what was added or the new queue)
q$add("something")
q$add("else")

## removes first item from the queue
q$remove()

## not able to access
##q$length()
