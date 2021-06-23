# MPLUS FUNCTIONS

cor_mean_table <- function(model){
  means <- model$sampstat$means 
  rownames(means)[1] <- "Mean" 
  covariances <- model$sampstat$covariances 
  variances <- diag(covariances) 
  SD <- sqrt(variances)
  correlations <- model$sampstat$correlations 
  correlations.mean.sd <- rbind(correlations, means, SD) 
  correlations.mean.sd <- round(as.data.frame(correlations.mean.sd), 2)
  correlations.mean.sd 
}

new_param <- function(x) {
  df <- x$parameters$unstandardized[grep("New.Additional.Parameters", 
                                   x$parameters$unstandardized$paramHeader),
                              c(2,3,4,6) ]
  df$filename <- x$summaries$ Filename
  df <- df[, c(5, 1:4)]
  return(df)
}

get_wald <- function(x) {
  options(scipen = 999)
  df <- cbind(x$summaries$ Observations,
              x$summaries$ WaldChiSq_Value,
              x$summaries$ WaldChiSq_DF,
              x$summaries$ WaldChiSq_PValue)
  df <- as.data.frame(df)
  colnames(df) <- c("n", "chi-square", "df", "p")
  rownames(df) <- x$summaries$ Filename
  return(df)
}

getMode <- function(df) {
  ux <- na.omit(unique(df))
  ux[which.max(tabulate(match(df, ux)))]
}

se <- function(x, na.rm=FALSE) {
  if (na.rm) x <- na.omit(x)
  sqrt(var(x)/length(x))
}


trace(grDevices::png, quote({
  if (missing(type) && missing(antialias)) {
    type <- "cairo-png"
    antialias <- "subpixel"
  }
}), print = FALSE)
