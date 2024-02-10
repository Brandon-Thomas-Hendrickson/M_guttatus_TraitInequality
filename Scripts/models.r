# Load Gini data
gini_data <- read.csv("/Users/brandonhendrickson/Documents/Github_Projects/M_guttatus_TraitInequality/Data/csv/gini_data.csv")
gini_distribution <- read.csv("/Users/brandonhendrickson/Documents/Github_Projects/M_guttatus_TraitInequality/Data/csv/gini_distributions.csv")

# Load the necessary packages
library(dplyr)
library(ggplot2)

# Create a list of the column names from column 7 to the end
gini_traits <- colnames(gini_results)[7:ncol(gini_results)]

# Initialize an empty list to store the models
models <- list()

# Loop over each row in the gini_distribution dataframe
for (i in 1:nrow(gini_distribution)) {
    # Get the trait and the best model
    trait <- gini_distribution$Trait[i]
    best_model <- gini_distribution$Best_Model[i]
    
    # Check if the trait is a column in the gini_data
    if (!trait %in% names(gini_data)) next

    # Remove NA and 0 values from the current trait
    trait_data <- gini_data[!is.na(gini_data[[trait]]) & gini_data[[trait]] != 0,]

    # Create the GLM
    model <- if (best_model == "norm") {
        glm(formula(paste(trait, "~ Final_Density")), family = gaussian(), data = trait_data)
    } else if (best_model == "logis") {
        glm(formula(paste("log(", trait, ")", "~ Final_Density")), family = gaussian(), data = trait_data)
    } else {
        next
    }
    
    # Store the model in the list
    models[[trait]] <- model
}

# Initialize an empty dataframe to store the results
results_df <- data.frame(Predictor = character(), Trait = character(), Estimate = numeric(), Pr_F = numeric(), F_value = numeric(), Sum_Sq = numeric(), Mean_Sq = numeric(), stringsAsFactors = FALSE)

# Loop over each model in the models list
for (trait in names(models)) {
    # Get the model
    model <- models[[trait]]
    
    # Get the ANOVA table
    anova_table <- summary(aov(model))
    
    # Loop over each coefficient and row in the ANOVA table
    for (i in 2:nrow(anova_table[[1]])) {
        # Get the estimate
        estimate <- coef(model)[i][[1]]
        
        # Get the "Pr(>F)", "F value", "Sum Sq", and "Mean Sq"
        pr_f <- anova_table[[1]][i-1, "Pr(>F)"]
        f_value <- anova_table[[1]][i-1, "F value"]
        sum_sq <- anova_table[[1]][i-1, "Sum Sq"]
        mean_sq <- anova_table[[1]][i - 1, "Mean Sq"]
        predictor <- rownames(anova_table[[1]])[i-1]
        
        # Add the result to the results dataframe
        results_df <- rbind(results_df, data.frame(Predictor = predictor, Trait = trait, Estimate = estimate, Pr_F = pr_f, F_value = f_value, Sum_Sq = sum_sq, Mean_Sq = mean_sq, stringsAsFactors = FALSE))
    }
}

write.csv(results_df, file = "/Users/brandonhendrickson/Documents/Github_Projects/M_guttatus_TraitInequality/Data/csv/glm_results.csv", row.names = FALSE)
