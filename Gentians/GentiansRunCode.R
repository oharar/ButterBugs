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
library(BRugs)
source("GentiansFunctions.R")

procdata=read.csv("GentiansSite.csv")
obsdata=read.csv("GentiansObs.csv")
# Make wetness in obs model different to the one in the process model
 obsdata$wetness2=procdata$wetness[obsdata$site]

procobj=list(
  data=procdata,
  modelfile="ProcessModel.bug",
  tag="occ" # "proc"
)

Obslist=list(
  list(
    data=obsdata,
    modelfile="ObservationModel.bug",
    tag="p" # "survey"
  )
)
  
# Check tags are unique: the stop() is because I expect this to end up in a function
tags <- c(procobj$tag, unlist(lapply(Obslist, function(lst) lst$tag)))
if(length(tags)>length(unique(tags))) stop("Not all tags are unique")

# Read in process model
ProcObj <- GetModel(procobj)
# Read in observation model(s)
ObsObjs <- lapply(Obslist, GetModel)

# Paste models together & save the code
AllModels <- paste("model {", ProcObj$model$model, lapply(ObsObjs, function(lst) lst$model$model), "}",sep="\n")
cat(AllModels, file="GentianAll.txt")

# Merge data
AllData <- unlist(list(ProcObj$DataToModel, unlist(lapply(ObsObjs, function(lst) lst$DataToModel), recursive=FALSE)), recursive=FALSE)

# Add tags to variable names: have to check the model to see that they're right
names(AllData) <- sapply(names(AllData), function(Name, Model, tags) {
#  Name <- names(AllData)[3]
  if(any(sapply(paste('\\.', tags,"$", sep=""), function(tag, nm) grepl(tag, nm), nm=Name))) {
    fullvar <- Name
  } else {
    fullvar <- gsub(paste('.*', Name, '\\.', sep=""), paste(Name, '\\.', sep=""), Model)
    fullvar <- gsub('\\[.*', "", fullvar)
    var.tag=paste(Name, tags, sep=".")
    if(any(!fullvar%in%var.tag))  stop("No unique name for variable")
  }
  fullvar
}, Model=AllModels, tags=c("univ", ProcObj$tag, unlist(lapply(ObsObjs, function(lst) lst$tag))))

# manually add z's where sp observed at least once
AllData$z.univ <- c(ifelse(tapply(AllData$y.p, list(AllData$site.p), function(vec) any(vec>0)), 1,NA))
  
bugsData(AllData, "GentianAllData.txt")

# Get the variables: all & then the alpha's & beta's
AllVars <- unique(c(
  ProcObj$model$variables, unlist(lapply(ObsObjs, function(lst) lst$model$variables)),
  ProcObj$model$universals, unlist(lapply(ObsObjs, function(lst) lst$model$universals))
  ))
MonitorVars <- AllVars[grep("alpha|beta", AllVars)]

# The moment of truth....
modelCheck("GentianAll.txt")
modelData("GentianAllData.txt")
modelCompile(2)
modelGenInits()

modelUpdate(10000)
samplesSet(MonitorVars)
modelUpdate(10000)

samplesStats('*')
samplesHistory('*')
samplesDensity('*')


