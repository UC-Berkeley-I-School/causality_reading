library(data.table)
library(causalToolbox)

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