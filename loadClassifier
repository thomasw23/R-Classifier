#'Prepare Test Samples for Diagnosis by Trained Classifier 
#'
#'\code{\link{loadClassifier}} returns a list of the same length as the loaded classifier. If no cross-
#'validation is performed, each element is a data frame of the individual classifier's results 
#'and scores on each test sample. If a leave-one-out cross-validation is performed, each list
#'member is a list with a data.frame (same as above) and a numeric vector representing the order
#'in which the samples were removed during cross-validation. 
#'
#'This function is called to prepare properly formatted test data for classification. After loading the 
#'required dataset (txClassifierPAX), \code{\link{testClassifier}}
#'are called. This function is also called by \code{\link{trainClassifier}} and 
#'\code{\link{testDiagnosePatient}} to perform internal cross-validation on the classifier.
#'@param obj object of class "overall" or "single" which determines whether to perform
#'a leave-one-out cross-validation on the training data. 
#'@param data data frame of discretized sample data for testing.
#'@param dv logical specifying whether to discretize the test data (default: FALSE).
#'@param svm_cost numeric value specifying the cost for the svm model (default: 1)
#'@param svm_gamma numeric value specifying the gamma for the svm model (default: def)
#'@param ... additional arguments include: "classifierNameBase", "classifierName", 
#'"discretize_name", "discretize_name_base" 
#'@return list 
#'@rdname loadClassifier
#'@export
#'@examples 
#'\dontrun{  
#'loadClassifier(structure(list(), class="txClassifier"))
#'}

loadClassifier <- function(obj, ..., data, type, dv=FALSE, svm_cost=1, svm_gamma="def") {
  UseMethod("loadClassifier")
}

#'@rdname loadClassifier

loadClassifier.default <- function(obj, ..., data, type, dv=TRUE, svm_cost=1, svm_gamma="def") {
  dots <- list(...)
  class_name_args <- c("classifier_name_base", "classifier_name")
  do.call(testForExistingClassifier, 
          c(list(dv=dv, type=type), dots[names(dots) %in% class_name_args]))
}

#'Measure SVM Performance with Leave-One-Out Cross-Validation  
#'
#'This function takes a data frame containing discretized classifier values for
#'all training set samples and performs a leave-one-out cross-validation (w/ 
#'replacement and a specified number of iterations) on an svm model to determine
#'the accuracy of the classifier.
#'@rdname loadClassifier

loadClassifier.yesCV <- function(obj, ..., data, type, dv=TRUE, svm_cost=1, svm_gamma="def") {
  
  dots <- list(...)
  message(paste0("Expression values are discretized: ", dv))
  discretize_args <- c("discretize_name", "discretize_name_base")
  class_created <- NextMethod()
  class_file <- names(class_created)[1]
  s <- sample(1:nrow(data), nrow(data), replace=FALSE)
  s_names <- rownames(data[s,])
  output <- do.call(rbind, lapply(1:nrow(data), function(y) {
    data_sub <- data[s[y],]
    calls <- lapply(which(!is.na(class_created)), function(x) {
      do.call(testClassifier, c(list(obj=get(class_file)[[x]], train_data_name=names(get(class_file))[x], 
                                     data=data_sub, type=type, dv=dv, sub_name=s_names[y], 
                                     svm_cost=svm_cost, svm_gamma=svm_gamma), 
                                dots[names(dots) %in% discretize_args]))
    })
    names(calls) <- names(get(class_file))
    cbind(s_names[y], reportClassifierCalls(calls))
  }))
  list(output, s)
}

#'Apply Trained and Locked SVM Classifier on New Samples  
#'
#'This function takes a data frame containing discretized probed values for
#'all samples to test and predicts the phenotype using the trained svm classifier. 
#'In addition the score, on a scale from 0 to 100, is also reported.
#'@rdname loadClassifier

loadClassifier.noCV <- function(obj, ..., data, type, dv=TRUE, svm_cost=1, svm_gamma="def") {
  dots <- list(...)
  message(paste0("Expression values are discretized: ", dv))
  discretize_args <- c("discretize_name", "discretize_name_base")
  class_created <- NextMethod()
  class_file <- names(class_created)[1]
  calls <- lapply(which(!is.na(class_created)), function(x) {
    do.call(testClassifier, c(list(obj=get(class_file)[[x]], train_data_name=names(get(class_file))[x], 
                                   data=data, type=type, dv=dv,svm_cost=svm_cost, svm_gamma=svm_gamma), 
                              dots[names(dots) %in% discretize_args]))
  })
  names(calls) <- names(get(class_file))
  return(calls)
}
