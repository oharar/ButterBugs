FormatModel <- function(filename, tag) {
  proc.mod <- scan(filename, what="character", sep="\n")
  # Extract variable names (need to parse correctly)
  vars.str <- proc.mod[grep(".tag", proc.mod, fixed=TRUE)]
  # str <- unlist(strsplit(vars.str, "[ \\(\\)]"))
  str <- unlist(strsplit(vars.str, "[]\\[ \\:\\(\\)]"))
  # Don't need all of these (yet!)
  vars.tag <- unique(str[grep(".tag", str)])
  vars.index <- unique(str[grep(".index", str)])
  vars.univ <- unique(str[grep(".univ", str)])
  
  # Extract indices
  # Some of this replicates pulling out other code
  inds <- proc.mod[grep(".index", proc.mod, fixed=TRUE)]
  str <- strsplit(inds, '[ :)]')[[1]]
  max.indices <- str[grep('\\.index', str)]
  # Add the tag
  max.indices <- gsub(".index", paste(".", tag,sep=""), max.indices, fixed=TRUE)
  tag.indices <- gsub("\\(","",str[grep('\\.tag', str)])
  vars.tag <- gsub(".tag", paste(".", tag,sep=""), vars.tag, fixed=TRUE)
  vars.index <- gsub(".index", paste(".", tag,sep=""), vars.index, fixed=TRUE)
  
  proc.mod <- paste(proc.mod, collapse="\n")
  proc.mod <- gsub('\\}$', "", gsub('model[[:space:]]\\{',"",proc.mod))
  proc.mod <- gsub(".tag", paste(".", tag,sep=""), proc.mod, fixed=TRUE)
  proc.mod <- gsub(".index", paste(".", tag,sep=""), proc.mod, fixed=TRUE)
  mod <- list(model=proc.mod, variables=vars.tag, universals=vars.univ, Indices=tag.indices, maxIndices=max.indices)
  mod
}

# Function to get object model and data, and format them
GetModel <- function(obj) {
  #  obj <- procobj
  #  obj <- Obslist[[1]]
  # Read in model
  obj$model <- FormatModel(filename=obj$modelfile, tag=obj$tag)
  
  # Check which variables in the data are in the model
  # Add data to be sent to BUGS
  # This should probably be put in a function
  #  NameInModel.tag <- names(obj$data)[sapply(paste(names(obj$data), ".tag", sep=""), function(str, strgs) length(grep(str, strgs))>0, strgs=obj$model$variables)]
  #  NameInModel.univ <- names(obj$data)[sapply(paste(names(obj$data), ".univ", sep=""), function(str, strgs) length(grep(str, strgs))>0, strgs=obj$model$variables)]
  NameInModel.tag <- names(obj$data)[sapply(names(obj$data), function(str, strgs) length(grep(str, strgs))>0, strgs=obj$model$variables)]
  NameInModel.univ <- names(obj$data)[sapply(paste(names(obj$data), obj$tag, sep="."), function(str, strgs) length(grep(str, strgs))>0, strgs=obj$model$universals)]
  NameInModel <- c(NameInModel.tag, NameInModel.univ)
  UseNames <- names(obj$data)%in%NameInModel
  if(sum(UseNames)==0) warning("No data being used")
  if(sum(UseNames)==1) {
    obj$DataToModel <- list(obj$data[,UseNames])
  } else {
    obj$DataToModel <- as.list(obj$data[,UseNames])
  }
  names(obj$DataToModel) <- names(obj$data)[UseNames]
  
  # Need to sort this out, to get the correct elngths and make sure that variables being used as indices are included.
  # add correct index lengths
  #  I'm sure this can be improved a lot - it feels a bit messy
  # Extract indices and their limits
  mod.split <- strsplit(obj$model$model, "\n", fixed=TRUE)[[1]]
  mod.ind <- mod.split[grep('for[[:blank:]]*\\(', mod.split)]
  ind <- gsub(' in.*',"", gsub('^.*\\(', "", mod.ind))
  Max.ind <- gsub('\\).*',"", gsub('^.*:', "", mod.ind))
  
  VarsWithIndex <- sapply(NameInModel, function(name, vars) vars[grep(paste("^",name,sep=""), vars)], vars=c(obj$model$variables, obj$model$universals))
  
  IndexofVarsWithIndex <- sapply(VarsWithIndex, function(var, mod) {
    #    var <- VarsWithIndex[1]; mod=gsub(".univ", paste(".",obj$tag,sep=""),obj$model$model)
    # this might not always work...
    fullvar <- gsub(paste('.*', var, '\\[', sep=""), paste(var, '\\[', sep=""), mod)
    fullvar <- gsub('\\].*', "\\]", fullvar)
    
    strs <- strsplit(fullvar, '[]\\[]')[[1]]
    strs[strs!=var]
  }, mod=gsub(".univ", paste(".",obj$tag,sep=""),obj$model$model))
  
  Lengths <- sapply(NameInModel, function(name, lst) length(lst[[name]]), lst=obj$DataToModel)
  
  WhMaxInd <- sapply(IndexofVarsWithIndex, function(iVi, ind) which(iVi==ind), ind=ind)
  
  if(any(tapply(Lengths, WhMaxInd, function(len) any(len!=len[1])))) stop("Some variables have different lengths but the same index")
  
  obj$DataToModel[[obj$model$maxIndices]] <- Lengths[unique(WhMaxInd)]
  names(obj$DataToModel[[obj$model$maxIndices]]) <- NULL
  
  obj
}
