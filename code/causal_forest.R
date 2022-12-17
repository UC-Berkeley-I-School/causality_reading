# Load Libraries and install them if needed

# packages from GitHub:
if (!"causalToolbox" %in% installed.packages()) {
  if (!require("devtools")) install.packages("devtools")
  devtools::install_github("forestry-labs/causalToolbox")
  }
library(causalToolbox)

# CRAN packages:
list.of.packages <- c("tidyverse", "ggplot2", "data.table")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.us.r-project.org")
lapply(list.of.packages, require, character.only = TRUE)



# data via Gerber and Green, "Field Experiments":
# "Rind and Bordia studied the tipping behavior of lunchtime patrons of an “upscale 
# Philadelphia restaurant” who were randomly assigned to four experimental groups.
# One factor was server sex (male or female), and a second factor was whether the 
# server draws a “happy face” on the back of the bill presented to customers.
# Download the data located at http://isps.research.yale.edu/FEDAI "
d <- fread("http://hdl.handle.net/10079/cd6be01a-a827-4312-a2fa-74329ce7f96d")

X_RF(
  feat = data.frame(d[ , .(female)]),
  tr   = d$happyface,
  yobs = d$tip
)


simulated_experiment <- simulate_causal_experiment(
  ntrain = 1000,
  ntest = 1000,
  dim = 10
)
feat <- simulated_experiment$feat_tr
tr <- simulated_experiment$W_tr
yobs <- simulated_experiment$Yobs_tr
feature_test <- simulated_experiment$feat_te

xl_rf <- X_RF(feat = feat, tr = tr, yobs = yobs)

EstimateCate(xl_rf, feature_test)
plot(
  x = simulated_experiment$tau_te, 
  y = EstimateCate(xl_rf, feature_test)
)


qqplot(
  x = simulated_experiment$tau_te, 
  y = EstimateCate(xl_rf, feature_test)
)