loadModule(module = "PROJECTOR_MODULE", what = TRUE)

#' @name projector
#' @useDynLib projector
#' @importFrom Rcpp evalCpp loadModule cpp_object_initializer sourceCpp
#' @importFrom RcppParallel RcppParallelLibs
#' @import methods
"_PACKAGE"
