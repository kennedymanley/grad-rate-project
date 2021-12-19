# run all steps of the analysis pipeline

#the data cannot be directly downloaded from the internet so it is detailed in the 
#download file how to access the link to the data and extract the data file from the 
#downloaded zip file. I included the download code here so you may look at it, but
#it does not run. 
#source("code/0-download.R")

source("code/1-cleaning.R")
source("code/2-train-test-split.R")
source("code/3-exploration.R")
source("code/4-regression-modeling.R")
source("code/5-tree-modeling.R")
source("code/6-model-evaluation.R")
