library(ncdf4)
library(fields)
library(stringr)

rm(list=ls())
source("/home/mgarvik/extract_files/function_extract_data.R")
load("/home/mgarvik/extract_files/models_list.rdata")

### Inputs

Model=TAS_MODELS[,1]
Model_version=TAS_MODELS[,2]

Freq="30Ayr"

Run="r1i1p1"

Exercise="historical"

Period="197601-200512"

for(i in 1:length(Model)){
  CDO_TAS_INTERPOLATE(1976,2005,Model=Model[i]) 
}