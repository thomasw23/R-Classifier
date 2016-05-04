#'Report the Overall Classifier Results 
#'
#'\code{reportClassifierCalls} returns a data frame with 2 or more columns: 
#'1) classification; 2) score; 3+) additional scores. 
#'
#'This function is called to take individual classifier results and return 
#'an overall result for each sample tested. This function calls 
#'\code{\link{determineDiagnosis}}.
#'@param calls list of 3 elements containing individual classifier results.
#'@return data frame
#'@examples
#'\dontrun{ 
#'reportClassifierCalls(data)
#'}

reportClassifierCalls <- function(calls) {
  
  finalCalls <- data.frame(t(sapply(1:length(calls[[1]][,1]), function(x) {
    if(length(calls) == 1) {
      z <- c(as.character(calls[[1]][x,1]), calls[[1]][x,2])
    } else {
      l <- sapply(1:length(calls), function(x2) calls[[x2]][x,1])
      names(l) <- names(calls)
      y <- determineDiagnosis(l)
      z <- c(y, round(calls[[1]][x,2], digits=2), round(calls[[2]][x,2], digits=2))
    }
    return(z)
  })), stringsAsFactors=FALSE) 
  
  if(length(calls) == 1) {
    colnames(finalCalls)[2] <- c("score")
  } else {
    colnames(finalCalls)[2:3] <- paste0(sapply(names(calls), function(x) {
      unlist(strsplit(x, "v"))[1]})[1:2], "_score")
  }
  
  return(finalCalls)
  ## Output - finalCalls(data frame): 1 - id; 2 - diagnosis ; 3 - score; 4 - test 
}

#'Determine Diagnosis based on the Classifier Results 
#'
#'\code{determineDiagnosis} returns a 2-element vector: 1)classification; 
#'2) classification groups.
#'
#'This function is called by \code{\link{reportClassifierCalls}} to perform the logic that 
#'determines the diagnosis of a test sample based on the results from each of the 
#'three classifiers.
#'@param g character vector of 3-elements with the output from each of the classifiers.
#'@return vector
#'@examples 
#'g <- list(C=c(AvC="C", BvC="C", BvA="B"),
#'          A=c(AvC="A", BvC="C", BvA="A"),
#'          B=c(AvC="C", BvC="B", BvA="B"),
#'          A2=c(AvC="A", BvC="B", BvA="A"),
#'          B2=c(AvC="A", BvC="B", BvA="B"))
#'lapply(g, determineDiagnosis)

determineDiagnosis <- function(g) {
  grps <- sapply(names(g), function(x) strsplit(x, "v"))    
  if (g[2] == grps[[2]][2]) {
    if(g[1] == grps[[1]][2]) {
      final <- grps[[1]][2]
    } else {
      final <- grps[[1]][1]
    }
  } else {
    if (g[1] == grps[[1]][2]) {
      final <- grps[[2]][1] 
    } else {
      if (g[3] == grps[[1]][1]) {
        final <- grps[[1]][1]
      } else {
        final <- grps[[2]][1]
      }
    }
  }
  return(final)
}
