# We need 1 process model and at least one observation model
#  Each model must be a syntactically correct BUGS model: the 'model {' and '}' at the beginning & end will be stripped out
#  The model here is for occupancy, which is called z.
#  
#  Almost all other variables should end in either ".tag", ".index" or ".univ".
#  'tag' and 'index' will be replaced by a unique tag
#  '.univ' signifies a variable that can appear in >1 module (=universal).
#  The range of an index should have a '.index' suffix
#       (actually the index should probably be called index, and the range something else. Another design decision for later)

# Function to combine obs & process models
# For the moment I'm just providing data & model as file names, but might want to do this differently later

# ProcObj is a list with the following components:
#   - datafile: file with data
#   - modelfile: file with model
#   - tag: unique tag

# ObsObjList is a list of lists, each with the following components:
#   - data: data frame containing data.
#   - modelfile: file with model
#   - tag: unique tag

setwd("Gentians")

procdata=read.csv("GentiansSite.csv")
obsdata=read.csv("GentiansObs.csv")
# Make wetness in obs model different to the one in the process model
 obsdata$wetness2=procdata$wetness[obsdata$Site]

procobj=list(
  data=procdata,
  modelfile="ProcessModel.bug",
  tag="proc"
)

Obslist=list(
  list(
    data=obsdata,
    modelfile="ObservationModel.bug",
    tag="survey"
  )
)
  


# Need to parse and edit vars correctly!
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
    tag.indices <- gsub("\\(","",str[grep('\\.tag', str)])

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
  # Read in processmodel
  obj$model <- FormatModel(filename=obj$modelfile, tag=obj$tag)
  
  # Check which variables in the data are in the model
  # Add data to be sent to BUGS
  # This should probably be put in a function
  NameInModel.tag <- names(obj$data)[sapply(paste(names(obj$data), ".tag", sep=""), function(str, strgs) length(grep(str, strgs))>0, strgs=obj$model$variables)]
  NameInModel.univ <- names(obj$data)[sapply(paste(names(obj$data), ".univ", sep=""), function(str, strgs) length(grep(str, strgs))>0, strgs=obj$model$variables)]
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

  VarsWithIndex <- sapply(NameInModel, function(name, vars) vars[grep(name, vars)], vars=obj$model$variables)
  IndexofVarsWithIndex <- sapply(VarsWithIndex, function(ind, inds) {
    strs <- strsplit(ind, '[]\\[]')[[1]]
    strs[strs%in%inds]
  }, inds=obj$model$Indices)
    
  Lengths <- sapply(NameInModel, function(name, lst) length(lst[[name]]), lst=obj$DataToModel)
    names(Lengths) <- sapply(names(Lengths), function(names) IndexofVarsWithIndex[grep(names,  VarsWithIndex, fixed=TRUE)])
  
  Len <- sapply(unique(names(Lengths)), function(ind, lens) {
    ll <- lens[names(lens)==ind]
    if(any(ll!=ll[1])) stop("Not all variables with same index have same length")
    len <- ll[1]
    names(len) <- NULL
    len
  }, lens=Lengths)
  
  obj$DataToModel[[obj$model$maxIndices]] <- Len[which(obj$model$Indices==names(Len))]
  names(obj$DataToModel[[obj$model$maxIndices]]) <- NULL
  
# rename indices with tag
  names(obj$DataToModel) <- gsub(".index", paste(".",obj$tag, sep=""), names(obj$DataToModel))
  
  obj
}


# Check tags are unique: the stop() is because I expect this to end up in a function
tags <- c(procobj$tag, unlist(lapply(Obslist, function(lst) lst$tag)))
if(length(tags)>length(unique(tags))) stop("Not all tags are unique")


# Read in process model
ProcObj <- GetModel(procobj)
# Read in observation models
ObsObjs <- lapply(Obslist, GetModel)
  

AllModels <- paste("model {", ProcObj$model$model, lapply(ObsObjs, function(lst) lst$model$model), "}",sep="\n")
cat(AllModels, file="GentianAll.txt")

AllData <- unlist(list(ProcObj$DataToModel, unlist(lapply(ObsObjs, function(lst) lst$DataToModel), recursive=FALSE)), recursive=FALSE)

bugsData(AllData, "GentianAllData.txt")

# The moment of truth....
modelCheck("GentianAll.txt")
modelData("GentianAllData.txt")
modelCompile(1)
