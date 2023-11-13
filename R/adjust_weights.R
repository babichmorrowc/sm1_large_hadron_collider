adjust_weights <- function(complete_data, subset_data, unadjusted_weight_col = "Weight", label_col = "Label") {
  # Get total values from complete dataset
  total_signal_weights <- sum(complete_data[[unadjusted_weight_col]][complete_data[[label_col]] == "s"])
  total_background_weights <- sum(complete_data[[unadjusted_weight_col]][complete_data[[label_col]] == "b"])
  # Get total values from subset dataset
  subset_signal_weights <- sum(subset_data[[unadjusted_weight_col]][subset_data[[label_col]] == "s"])
  subset_background_weights <- sum(subset_data[[unadjusted_weight_col]][subset_data[[label_col]] == "b"])
  
  adjusted_weights <- subset_data[[unadjusted_weight_col]] *
    case_when(subset_data[[label_col]] == "s" ~ total_signal_weights / subset_signal_weights,
              subset_data[[label_col]] == "b" ~ total_background_weights / subset_background_weights)
  return(adjusted_weights)
    
}

# Test that adjust_weights is working as expected on the Kaggle weights:
kaggle_adjust_weights_t <- adjust_weights(complete_data = higgs_data,
                                        subset_data = higgs_data[higgs_data$KaggleSet == "t",],
                                        unadjusted_weight_col = "Weight",
                                        label_col = "Label")
all.equal(kaggle_adjust_weights_t, higgs_data$KaggleWeight[higgs_data$KaggleSet == "t"])
kaggle_adjust_weights_b <- adjust_weights(complete_data = higgs_data,
                                          subset_data = higgs_data[higgs_data$KaggleSet == "b",],
                                          unadjusted_weight_col = "Weight",
                                          label_col = "Label")
all.equal(kaggle_adjust_weights_b, higgs_data$KaggleWeight[higgs_data$KaggleSet == "b"])
