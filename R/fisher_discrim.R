fisher_discrim <- function(data, label_col = "Label", class_pos, class_neg) {
  X_pos <- data %>% 
    filter(get(label_col) == class_pos) %>% 
    select(-{{label_col}})
  X_neg <- data %>% 
    filter(get(label_col) == class_neg) %>% 
    select(-{{label_col}})
  n_pos <- nrow(X_pos)
  n_neg <- nrow(X_neg)
  mu_pos <- colMeans(X_pos)
  mu_neg <- colMeans(X_neg)
  mu_diff <- matrix(mu_pos - mu_neg)
  cov_pos <- cov(X_pos)
  cov_neg <- cov(X_neg)
  
  S_w <- n_pos*cov_pos + n_neg*cov_neg
  w <- solve(S_w) %*% mu_diff
  return(w)
}
