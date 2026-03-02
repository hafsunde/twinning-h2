library(tidyverse)
library(showtext)

clrs.drk <- c("#d90b56","#3b90c5", "#6ec132", "#bc644c", "#6d35b3", "#e8a830")


SEtoCI <- function(estimates, std_errors, conf_level = 0.95, is_correlation = FALSE) {
  # Calculate the z-value for the desired confidence level
  z_value <- qnorm((1 + conf_level) / 2)
  
  if (is_correlation) {
    # Apply Fisher's z-transformation for correlations
    z_estimates <- atanh(estimates)  # Fisher's z-transform
    lower_bound_z <- z_estimates - z_value * std_errors
    upper_bound_z <- z_estimates + z_value * std_errors
    
    # Back-transform from Fisher's z to correlation scale
    lower_bound <- tanh(lower_bound_z)
    upper_bound <- tanh(upper_bound_z)
  } else {
    # For non-correlation estimates, use standard calculation
    lower_bound <- estimates - z_value * std_errors
    upper_bound <- estimates + z_value * std_errors
  }
  
  # Return the confidence intervals as a data frame
  data.frame(Lower = lower_bound, Upper = upper_bound)
}

#font_import(paths = c("N:/durable/file-import/p1074-hansfsu-group/Open_Sans/Static"), prompt = F)
font_add(regular = c("09_functions/OpenSans-Regular.ttf"), 
         bold    = c("09_functions/OpenSans-Bold.ttf"), 
         italic  = c("09_functions/OpenSans-Italic.ttf"),
         family = "Open Sans")
showtext_auto()

#library(extrafont)
# import fonts - only once
#font_import()
# load fonts - every session
#loadfonts(device = "win", quiet = TRUE)
#loadfonts(device = "pdf", quiet = TRUE)

convert_edu_level_to_years <- Vectorize(function(edu_level) {
  return(ifelse(edu_level == 0, 0,
                ifelse(edu_level == 1, 7,
                       ifelse(edu_level == 2, 10,
                              ifelse(edu_level == 3, 12,
                                     ifelse(edu_level == 4, 13,
                                            ifelse(edu_level == 5, 14,
                                                   ifelse(edu_level == 6, 17,
                                                          ifelse(edu_level == 7, 19,
                                                                 ifelse(edu_level == 8, 20, NA))))))))))
})

# Create a function to convert edu_level to education group
convert_edu_level_to_group <- Vectorize(function(edu_level) {
  return(ifelse(edu_level %in% c(0,1,2,3), "Basic Education",
                ifelse(edu_level %in% c(4, 5), "High School",
                       ifelse(edu_level %in% c(6), "Bachelor (or equiv)",
                              ifelse(edu_level %in% c(7, 8), "Master (or equiv)", NA)))))
})







# Custom ggplot theme with transparent background and option for foreground
custom_theme <- function(Color = "black", BG = "transparent", ...) {
  theme_classic(...) +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.background = element_rect(fill = BG, color=Color, linewidth=1), # transparent panel bg
          plot.background = element_rect(fill = BG, color = NA), # transparent plot bg
          legend.background = element_rect(fill = BG), # transparent legend bg
          legend.box.background = element_rect(fill = BG, color = BG),
          legend.title = element_text(hjust = .5),
          # legend.margin = margin(r = 10, l = 5, t = 5, b = 5),
          text = element_text(color = Color),
          axis.text = element_text(color = Color),
          title = element_text(color = Color),
          axis.ticks = element_line(color = Color),
          line = element_line(color = Color),
          axis.line = element_blank(),
          rect = element_rect(fill = Color, colour = Color))
}


numformat <- function(value, nsmall=2, decimals.for.integers=F) {
  
  sapply(value, function(x) {
    if (is.na(x)) {
      return(NA)
    }
    
    if (x==0) {
      if(decimals.for.integers & nsmall != 0) {
        return(paste0("0.", paste(rep(0,nsmall), collapse = "")))
      } else {
        return("0")
      }
    }  else if (x %% 1 == 0) {
      # If x is an integer, return it without decimals
      formatted_value <- format(round(x), nsmall = nsmall*decimals.for.integers, big.mark = ",")
    } else {
      # If x is not an integer, format it as needed
      formatted_value <- format(round(x, nsmall), nsmall = nsmall, big.mark = ",")
    }
    
    # Remove leading zero for numbers between 0 and 1
    sub("^(-?)0", "\\1", formatted_value)
  })
}




cor_fastCI <- function(vector1, vector2, confidence_level = 0.95, missing_handling = "pairwise.complete.obs") {
  if(all(vector1 == 0) | all(vector2 == 0)) {
    return(c(Correlation = 0, CI_Lower = NA, CI_Upper = NA))
  }
  
  # Calculate correlation with missing value handling
  correlation <- cor(vector1, vector2, use = missing_handling)
  
  # If no valid correlation can be calculated, return NA
  if(is.na(correlation)) {
    return(c(Correlation = NA, CI_Lower = NA, CI_Upper = NA))
  }
  
  # Fisher Transformation
  fisher_z <- atanh(correlation)
  
  # Standard error
  # Adjust the sample size based on the actual pairs used
  actual_pairs <- sum(!is.na(vector1) & !is.na(vector2))
  stderr <- 1 / sqrt(actual_pairs - 3)
  
  # Confidence Interval
  z_critical <- qnorm((1 + confidence_level) / 2)
  ci_lower <- tanh(fisher_z - z_critical * stderr)
  ci_upper <- tanh(fisher_z + z_critical * stderr)
  
  # Return a named vector
  c(Correlation = correlation, CI_Lower = ci_lower, CI_Upper = ci_upper, N=actual_pairs)
}

bootstrap_correlation <- function(data, col1, col2, n_boot = 1000, conf.level = 0.95, use_parallel = FALSE) {
  # Convert to matrix for faster computation
  data_matrix <- na.omit(as.matrix(data[, c(col1, col2)]))
  
  # Function to perform bootstrap for one iteration
  one_boot <- function(data) {
    sampled_indices <- sample(nrow(data), replace = TRUE)
    return(cor(data[sampled_indices, ])[1,2])
  }
  
  # Check if parallel processing is to be used
  if (use_parallel) {
    require(parallel)
    cl <- makeCluster(detectCores() - 1) # Leave one core free
    clusterExport(cl, varlist = c("data_matrix", "one_boot"), envir = environment())
    boot_results <- parSapply(cl, 1:n_boot, function(i) one_boot(data_matrix))
    stopCluster(cl)
  } else {
    boot_results <- sapply(1:n_boot, function(i) one_boot(data_matrix))
  }
  
  # Compute confidence intervals
  lower_bound <- quantile(boot_results, (1 - conf.level) / 2)
  upper_bound <- quantile(boot_results, 1 - (1 - conf.level) / 2)
  
  result <- c(Correlation = mean(boot_results),
              CI_Lower = unname(lower_bound),
              CI_Upper = unname(upper_bound),
              N = nrow(data_matrix))
  
  return(result)
}







