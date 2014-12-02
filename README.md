ButterBugs
==========

## Models for analysing distribution/abundance data with several types of data.

This repository contains the following projects:

- Gentians: a proof-of-concept attempt to get something working

As you can see, it's early days


## Introduction

This is a repository for code for a project to iniitally develop models for combining data on butterfly abindances/occurrences. The project was initiated in Frankfut in November 2014: we'll see how it goes.

The aim is to combine different types of observation into a single statistical model, for example BMS (Butefly Monitoring Survey) data, which is high quality, but only available on a few sites, and incidental reports that ar spatially extensive but not controlled.

The plan is to get a set of submodels for different types of data, as well as process models for the actual distribution/dynamics.

The data is ususally collected for several species, so we have the choice of teh following types of data:
 - single species (with information on other species as covariates)
 - multiple single species (single secies process models, observation models carry information across species, e.g. with site-specific parameters
 - multiple species (full community modesl for the process)
 
 The models could be in discrete or continuous space: discrete being typial (i.e. on grids, although this is not necessary. Continuous space is being explored now, using ideas from point processes. 

## Initial Plans

After initial discussions, it was decided to start with single species models, and then move on to multiple single species, and to do this in discrete space. This is simply because these models are better developed at the moment.

In practice, the model code will be written in the BUGS language, as this gives us the flexibility to use a variety of models. 

We would start with single species models, and aim for mutliple single species models.

At some point we will need to iclude phenology.
