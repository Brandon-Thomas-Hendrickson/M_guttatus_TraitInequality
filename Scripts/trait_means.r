# Load the ineq package
library(dplyr)
library(ggplot2)

# Load data
data <- read.csv("/Users/brandonhendrickson/Documents/Github_Projects/M_guttatus_TraitInequality/Data/csv/raw_data.csv")

trait_list <- list("yield","flower_number_1","D2Flowering","flower_node","pollen_avg","ff_height","ff_node","total_node_number","prop_early_growth","lowervolume","midrvolume","upvolume","Huber_Volume","Smalian_Volume","Newton_Volume","final_height","veg_weight","repro_weight","total_weight","leaf_area")

# Initialize an empty data frame to store the results
mean_results <- data.frame()

# Calculate the average trait for each tray and position
for (trait in trait_list) {
    # Skip if the trait is not a column in the data
    if (!trait %in% names(data)) next

    # Calculate the mean for the current trait
    mean_index <- data %>%
        group_by(Tray, POS) %>%
        summarise(!!paste0("Mean_", trait) := mean(get(trait)))

    # If mean_results is empty, copy mean_index
    if (nrow(mean_results) == 0) {
        mean_results <- mean_index
    } else {
        # Otherwise, join the new mean index to the existing results
        mean_results <- full_join(mean_results, mean_index, by = c("Tray", "POS"))
    }
}

data_smalls <- data[, c(1, 3, 6)]
data_smalls$F_LF <- "Average"

mean_results <- merge(data_smalls, mean_results, by = c("Tray", "POS"))

mean_results <- unique(mean_results)

trait_list <- list("yield","flower_number_1","D2Flowering","flower_node","pollen_avg","ff_height","ff_node","total_node_number","prop_early_growth","lowervolume","midrvolume","upvolume","Huber_Volume","Smalian_Volume","Newton_Volume","final_height","veg_weight","repro_weight","total_weight","leaf_area")

# Create data frame of F and LF
data_fits <- data[data$F_LF == "F" | data$F_LF == "LF", ]

data_fits <- data_fits[,c(-2,-4,-5,-7,-8,-17,-18,-19,-33,-34,-35)]

# Rename columns
data_fits <- data_fits %>%
    rename_with(~ifelse(.x %in% trait_list, paste0("Mean_", .x), .x), everything())
trait_list <- list("Mean_yield","Mean_flower_number_1","Mean_D2Flowering","Mean_flower_node","Mean_pollen_avg","Mean_ff_height","Mean_ff_node","Mean_total_node_number","Mean_prop_early_growth","Mean_lowervolume","Mean_midrvolume","Mean_upvolume","Mean_Huber_Volume","Mean_Smalian_Volume","Mean_Newton_Volume","Mean_final_height","Mean_veg_weight","Mean_repro_weight","Mean_total_weight","Mean_leaf_area")

# Combine dataframes
data_big <- rbind(mean_results, data_fits)

data_big<-data_big[!(data_big$F_LF == "LF" & data_big$Final_Density == 1),]
data_big<-data_big[!(data_big$F_LF == "Average" & data_big$Final_Density == 2),]
data_big<-data_big[!is.na(data_big$F_LF),]

# Initialize an empty data frame to store the results
mean_se_results <- data.frame()

# Calculate the mean and SE for each trait
for (trait in trait_list) {
    # Skip if the trait is not a column in the data
    if (!trait %in% names(data_big)) next
    
    # Calculate the mean and SE for the current trait
    mean_se <- data_big %>%
        group_by(Final_Density, F_LF) %>%
        summarise(mean = mean(get(trait), na.rm = TRUE),
                  se = sd(get(trait), na.rm = TRUE) / sqrt(n()))
    
    # Add the trait name to the results
    mean_se$trait <- trait
    
    # Bind the results to the existing data frame
    mean_se_results <- rbind(mean_se_results, mean_se)
}

# Initialize an empty list to store the plots
plot_list <- list()
# Create a named list where the names are the traits and the values are the trait names
trait_name <- c("Yield","# Flowers","Flowering Date","Node of First Flower","Pollen Avg","Height @ Flower","Node @ Flower","# Nodes","Growth","Lower Volume","Mid Volume","Upper Volume","Huber Volume","Smalian Volume","Newton Volume","Final Height","Biomass","Repro Weight","Total Weight","Leaf Area")
names(trait_name) <- unique(mean_se_results$trait)

# Create a ggplot for each trait
for (trait in names(trait_name)) {
    # Filter the data for the current trait
    trait_data <- mean_se_results[mean_se_results$trait == trait, ]

    # Create a ggplot for the current trait
    p <- ggplot(trait_data, aes(x = Final_Density, y = mean, color = F_LF)) +
        geom_line() +
        geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.1) +
        labs(
            title = NULL,
            x = "Final Density",
            y = trait_name[trait]  # Use the corresponding trait name here
        ) +
        scale_color_manual(
            name = "F_LF",
            breaks = c("F", "LF", "Average"),
            labels = c("F", "LF", "Average"),
            values = c("F" = "red", "LF" = "black", "Average" = "blue")
        ) +
        theme_light() +
        theme(
            legend.position = "none",
            text = element_text(size = 12, family = "Times New Roman"),
            axis.text.x = element_text(size = 12, family = "Times New Roman"),
            axis.text.y = element_text(size = 12, family = "Times New Roman")
        )
        # Add the plot to the list
    plot_list[[trait]] <- p
}

# Print the plots
for (p in plot_list) {
    print(p)
}

# Combine the plots using ggarrange()
combined_plot <- ggarrange(plotlist = plot_list, ncol = 2, nrow = 2)

# Print the combined plot
print(combined_plot)

ggsave(paste("/Users/brandonhendrickson/Documents/Github_Projects/M_guttatus_TraitInequality/Plots/mean_", trait, ".png", sep = ""), plot = p)

