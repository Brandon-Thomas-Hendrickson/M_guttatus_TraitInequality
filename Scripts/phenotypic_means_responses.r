# Create the trait List
trait_list <- c("yield","flower_number_1", "D2Flowering", "ff_height", "total_node_number", "prop_early_growth", "final_height","flower_area", "leaf_area","Huber_Volume")

# Load modelframe
data <- read.csv("/Users/brandonhendrickson/Documents/Github_Projects/M_guttatus_PhenotypicSelection/Data/csv/raw_data.csv")

# Remove all rows with "" in F_LF
data <- data[data$F_LF != "", ]

# Initialize an empty data frame
results <- data.frame(Trait = character(), Fitness = character(), Estimate = numeric(), P_Value = numeric(), R_Squared = numeric(), F_Statistic = numeric(), SE = numeric())

for (trait in trait_list) {
    for (fitness in unique(data$F_LF)) {
        mean <- lm(data[data$F_LF == fitness, trait] ~ data[data$F_LF == fitness, "Final_Density"], data = data[data$F_LF == fitness, ])
        mean_list_FD[[paste(fitness, trait, sep = "_")]] <- mean

        # Extract the model estimate for the trait
        estimate <- coef(mean)[2]

        # Extract the model summary
        summary_mean <- summary(mean)

        # Extract the p-value, Sum Sq, F statistic, and R2
        p_value <- summary_mean$coefficients[2, 4]
        SE <- summary_mean$coefficients[2, 2]
        r_squared <- summary_mean$r.squared

        # Add the result into the data frame
        results <- rbind(results, data.frame(Trait = trait, Fitness = as.character(fitness), Estimate = estimate, P_Value = p_value, SE = sum_sq, F_Statistic = f_statistic, R_Squared = r_squared))
    }
}

# Save the data
write.csv(results, "/Users/brandonhendrickson/Documents/Github_Projects/M_guttatus_PhenotypicSelection/Data/csv/phenotypic_means_responses.csv", row.names = FALSE)
