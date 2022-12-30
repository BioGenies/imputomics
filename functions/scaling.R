scale_df <- function(x) {
  scaled <- scale(as.matrix(x), center = TRUE, scale = TRUE)
  scaled_df <- data.frame(scaled)

  attr(scaled_df, "scaled:scale") <- attr(scaled, "scaled:scale")
  attr(scaled_df, "scaled:center") <- attr(scaled, "scaled:center")

  scaled_df
}

unscale_df <- function(x) {
  if(!all(c("scaled:center", "scaled:scale") %in% names(attributes(x))))
    stop("Make sure that the object has attributes 'scaled:center' and 'scaled:scale'")
  
  data.frame(t(t(as.matrix(x)) * attr(x, "scaled:scale") + attr(x, "scaled:center")))
}
