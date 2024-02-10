# Load Gini gini_data
gini_data<- read.csv("/Users/brandonhendrickson/Documents/Github_Projects/M_guttatus_TraitInequality/Data/csv/gini_data.csv")

if (!require(nortest)) {
    install.packages("nortest")
}

if (!require(car)) {
    install.packages("car")
}

if (!require(readr)) {
    install.packages("readr")
}

if (!require(Rmisc)) {
    install.packages("Rmisc")
}

if (!require(psych)) {
    install.packages("psych")
}

if (!require(fitdistrplus)) {
    install.packages("fitdistrplus")
}
library(nortest)
library(car)
library(readr)
library(Rmisc)
library(psych)
library(fitdistrplus)

# Create a list of the column names from column 7 to the end
gini_traits <- colnames(gini_results)[7:ncol(gini_results)]

# Describe mean, standard deviation, variance, range, and kurtosis
for (trait in gini_traits) {
    summary <- describe(gini_data[[trait]])
    print(paste("Summary for", trait, ":"))
    print(paste("Mean:", summary$mean))
    print(paste("Standard deviation:", summary$sd))
    print(paste("Kurtosis:", summary$kurtosis))
}

# Determine normality of variables using Anderson-Darling Test
for (trait in gini_traits) {
    ad_test_result <- ad.test(gini_data[[trait]])
    print(paste("Anderson-Darling test for", trait, ":"))
    print(paste("Trait Name:", trait))
    print(ad_test_result)
}

# Define the distributions and their starting parameters
distributions <- list(
    "norm" = NULL,
    "gamma" = list(shape = 1, rate = 1),
    "exp" = list(rate = 1),
    "logis" = list(location = 0, scale = 1),
    "geom" = list(prob = 0.5)
)

# Initialize an empty dataframe to store the results
summary_df <- data.frame(Trait = character(), Best_Model = character(), stringsAsFactors = FALSE)

for (trait in gini_traits) {
    print(paste("Trait:", trait))
    
    # Remove NA values from the current trait
    trait_data <- na.omit(gini_data[[trait]])
    
    # Check if the data is continuous or discrete
    is_discrete <- all(trait_data == round(trait_data))
    
    # Check if the data is greater than 0
    is_greater_than_zero <- all(trait_data > 0)
    
    # Fit the distributions to the data and store the results in a list
    fit_list <- lapply(names(distributions), function(dist) {
        if ((dist == "geom" && !is_discrete) || (dist %in% c("gamma", "exp") && !is_greater_than_zero)) {
            return(NULL)
        }
        fitdist(as.numeric(trait_data), dist, start = distributions[[dist]])
    })

    # Remove NULL elements from the list
    fit_list <- fit_list[!sapply(fit_list, is.null)]
    
    # Compare the fits
    compare_dist <- gofstat(fit_list)

    # Find the best model based on AIC
    best_model <- which.min(compare_dist$aic)
    
    # Remove any string with a digit followed by "-mle-" from the best model name
    best_model_name <- sub("\\d-mle-", "", names(best_model))
    
    # Add the result to the summary dataframe
    summary_df <- rbind(summary_df, data.frame(Trait = trait, Best_Model = best_model_name, stringsAsFactors = FALSE))
}

# Print the summary dataframe
print(summary_df)

write.csv(summary_df, file = "/Users/brandonhendrickson/Documents/Github_Projects/M_guttatus_TraitInequality/Data/csv/gini_distributions.csv", row.names = FALSE)