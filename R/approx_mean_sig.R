approx_mean_sig <- function(predictions, labels, weights, b_reg = 110) {
  true_pos <- sum(weights[predictions == "s" & labels == "s"])
  false_pos <- sum(weights[predictions == "s" & labels == "b"])
  ams <- sqrt(2 * ((true_pos + false_pos + b_reg) * log(1 + (true_pos / (false_pos + b_reg))) - true_pos))
  return(ams)
}

# Test AMS if we predicted all background
predictions <- rep("b", nrow(higgs_data_orig))
labels <- higgs_data_orig$Label
weights <- higgs_data_orig$Weight
approx_mean_sig(predictions, labels, weights) # give us 0
