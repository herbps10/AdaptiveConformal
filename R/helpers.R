initialize_parameters <- function(parameters, defaults, acceptable) {
  for(n in names(defaults)) {
    if(is.null(parameters[[n]])) {
      parameters[[n]] <- defaults[[n]]
    }

    if(is.function(parameters[[n]])) {
      if(!(".Primitive(\"is.function\")" %in% as.character(acceptable[[n]]))) {
        stop(paste0("Parameter ", n, " cannot be a function."))
      }
    }
    else {
      if(!is.null(acceptable[[n]]) && !(parameters[[n]] %in% acceptable[[n]])) {
        stop(paste0("Parameter ", n, " must be one of ", acceptable[[n]]))
      }
    }
  }

  parameters
}

