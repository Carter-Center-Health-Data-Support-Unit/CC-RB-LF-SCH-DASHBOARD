rm(list = ls())

library(shiny)
library(tidyverse)

# call target
source("_targets.R")

#make target
tar_make()

# load data
tar_load(RB_pre_post_compiled) ### pre and post admin 2 level data
tar_load(RB_post201905_adm3)
