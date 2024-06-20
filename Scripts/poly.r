####################################################
## Fitting Different Polynomial Equations to Data ##
####################################################

# Load dataframe
data <- read.csv("/Users/brandonhendrickson/Documents/Github_Projects/M_guttatus_PhenotypicSelection/Data/csv/standardized_data.csv")

# Create the trait List
trait_list <- c("flower_number_1_std", "D2Flowering_std", "pollen_avg_std", "ff_height_std", "total_node_number_std", "prop_early_growth_std", "final_height_std", "veg_weight_std", "leaf_area_std")

# Remove NA from yield
data <- data[!is.na(data$yield), ]

#Make Density a Factor
data$Final_Density <- as.factor(data$Final_Density)

# Initialize an empty data frame
results <- data.frame(Trait = character(), Model = character(), AIC = numeric(), Density = numeric(), stringsAsFactors = FALSE)

# Run the polynomial regression analysis
for (density in unique(data$Final_Density)) {
    for (trait in trait_list) {
        # Subset data for current trait and density, and remove rows with NA in trait column
        data_subset <- na.omit(data[data$Final_Density == density, c(trait, "yield")])

        for (degree in 1:2) {
            model <- lm(yield ~ poly(data_subset[[trait]], degree), data = data_subset)
            aic <- AIC(model)
            results <- rbind(results, data.frame(Trait = trait, Model = degree, AIC = aic, Density = density, stringsAsFactors = FALSE))
        }
    }
}

# Find the best model for each trait
best_models <- results %>%
    group_by(Trait, Density) %>%
    filter(AIC == min(AIC))

# Convert Model to Factor
best_models$Model <- as.factor(best_models$Model)

# Create a list to store the plots
plots <- list()

# Create a plot for each trait
for (trait in unique(best_models$Trait)) {
    # Subset the data for the current trait
    data_subset <- best_models[best_models$Trait == trait,]
    
    # Create the plot
    p <- ggplot(data_subset, aes(x = Density, y = Model)) +
        geom_point() +
        labs(x = "Density", y = "Model", title = trait) +
        theme_light() +
        theme(plot.title = element_text(hjust = 0.5))
    
    # Add the plot to the list
    plots[[trait]] <- p
}

# Arrange the plots into a single graph
combined_plot <- ggarrange(plotlist = plots, ncol = 3, nrow = 3)

