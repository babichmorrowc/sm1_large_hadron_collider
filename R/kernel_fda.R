library(kfda)

# Train KFDA -------------------------------------------------------------------
kfda_drop_combo <- kfda(higgs_training_drop_combo[1:5000,]) # Error: cannot allocate vector of size 3192.5 Gb
