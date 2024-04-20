#==============================================================
# AUTORL - SIM (AUTOMATED REINFORCEMENT LEARNING - SIMULATION) 
# DEVELOPER: GLEICE KELLY BARBOSA SOUZA
# ADVISOR: ANDRÉ LUIZ CARVALHO OTTONI
# DATE: JULY 9, 2023
#==============================================================

rm(list=ls())

# Libraries
if (file.exists("base/libs.R")) source("base/libs.R") else stop("File 'base/libs.R' not found.")

# Files
if (file.exists("base/requirements.R")) source("base/requirements.R") else stop("File 'base/requirements.R' not found.")

# Application Execution
run_app()