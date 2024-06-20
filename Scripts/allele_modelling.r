# Load Libraries
library(ggplot2)
library(ggpubr)
library(tibble)
library(sjPlot)
library(dplyr)
library(tidyverse)

#Load in Data 
data <- read.csv("/Users/brandonhendrickson/Documents/Github_Projects/M_guttatus_PhenotypicSelection/Data/csv/phenotype_selection.csv")

# Define the function
change_in_allele_frequency <- function(P, s, generations) {
    for (i in 1:generations) {
        P <- P + s * P * (1 - P)
    }
    return(P)
}

# Initialize an empty dataframe to store the results
results <- data.frame(Trait = character(), Density = numeric(), Estimate = numeric(), Generations = integer(), Final_Allele_Frequency = numeric(), stringsAsFactors = FALSE)
P=0.5
# Loop over the "Estimate" column
for (i in 1:nrow(data)) {
    # Get the selection coefficient from the "Estimate" column
    s <- data$Estimate[i]
    
    # Loop over the list of generations
    for (generations in seq(1,1000,10)) {
        # Calculate the final allele frequency
        P_final <- change_in_allele_frequency(P, s, generations)
        
        # Add the results to the dataframe
        results <- rbind(results, data.frame(Trait = data$Trait[i], Density = data$Density[i], Estimate = s, Generations = generations, Final_Allele_Frequency = P_final, stringsAsFactors = FALSE))
    }
}

results <- results[results$Density >= 3,]
# Write Data to CSV
write.csv(results, "/Users/brandonhendrickson/Documents/Github_Projects/M_guttatus_PhenotypicSelection/Data/csv/allele_frequency.csv", row.names = FALSE)

# Initialize a new dataframe to store the marked rows
marked_rows <- data.frame()

# Loop over the list of dataframes
for (trait in names(results)) {
    # Loop over the unique densities
    for (density in unique(results$Density)) {
        # Subset the data for the current density
        data_density <- subset(results, Density == density)
        
        # Initialize a new column Passes_0.99 with FALSE
        data_density$Passes_0.99 <- FALSE
            # Loop over the rows of data_density
            for (i in 2:nrow(data_density)) {
                if (data_density$Trait[i] == "D2Flowering_std") {
                    if (data_density$Final_Allele_Frequency[i] < 0.01 && data_density$Final_Allele_Frequency[i - 1] > 0.01) {
                        # Mark this row
                        data_density$Passes_0.99[i] <- TRUE
                    }
                } else {
                    # If the current row is greater than 0.99 and the previous row is less than 0.99
                    if (data_density$Final_Allele_Frequency[i] > 0.99 && data_density$Final_Allele_Frequency[i - 1] < 0.99) {
                        # Mark this row
                        data_density$Passes_0.99[i] <- TRUE
                    }
                }
            }
        # Add the marked rows to the new dataframe
        marked_rows <- rbind(marked_rows, data_density)
    }
}


# Split the results dataframe by Trait
results_by_trait <- split(marked_rows, marked_rows$Trait)

# Assuming results_by_trait is your list of dataframes
gen <- data.frame()

# Loop over the list of dataframes
for (trait in names(results_by_trait)) {
    # Subset the data where Passes_0.99 is TRUE
    data_subset <- subset(results_by_trait[[trait]], Passes_0.99 == TRUE)

    # Add the subsetted data to the new dataframe
    gen <- rbind(gen, data_subset)
}

#Write out generation times for Fixation
write.csv(gen, "/Users/brandonhendrickson/Documents/Github_Projects/M_guttatus_PhenotypicSelection/Data/csv/generations.csv", row.names = FALSE)

p1 <- ggplot(results_by_trait[["D2Flowering_std"]], aes(x = Generations, y = Final_Allele_Frequency, group = factor(Density), color = factor(Density))) +
    geom_line() +
    theme_light() +
    labs(title = paste("Flowering Date"), x = NULL, y = "Allele Frequency") +
    scale_color_manual(name = "Density", values = c("#0ac9db", "blue", "purple", "red", "magenta")) +
    theme(legend.position = "none", , plot.title = element_text(hjust = 0.5)) +
    geom_vline(aes(xintercept = gen[gen$Trait == "D2Flowering_std" & gen$Density == 2, "Generations"][1]), linetype = "dashed", color = "#0e0325") +
    geom_vline(aes(xintercept = gen[gen$Trait == "D2Flowering_std" & gen$Density == 3, "Generations"][1]), linetype = "dashed", color = "#0ac9db") +
    geom_vline(aes(xintercept = gen[gen$Trait == "D2Flowering_std" & gen$Density == 4, "Generations"][1]), linetype = "dashed", color = "blue") +
    geom_vline(aes(xintercept = gen[gen$Trait == "D2Flowering_std" & gen$Density == 5, "Generations"][1]), linetype = "dashed", color = "purple") +
    geom_vline(aes(xintercept = gen[gen$Trait == "D2Flowering_std" & gen$Density == 6, "Generations"][1]), linetype = "dashed", color = "red") +
    geom_vline(aes(xintercept = gen[gen$Trait == "D2Flowering_std" & gen$Density == 7, "Generations"][1]), linetype = "dashed", color = "magenta")


p2 <- ggplot(results_by_trait[["ff_height_std"]], aes(x = Generations, y = Final_Allele_Frequency, group = factor(Density), color = factor(Density))) +
    geom_line() + theme_light() + 
    labs(title = paste("Height @ Flower"), x = NULL, y = NULL) +
    scale_color_manual(name = "Density", values = c("#0ac9db", "blue", "purple", "red", "magenta")) +
    theme(legend.position = "none",, plot.title = element_text(hjust = 0.5)) +
    geom_vline(aes(xintercept = gen[gen$Trait == "ff_height_std" & gen$Density == 2, "Generations"][1]), linetype = "dashed", color = "#0e0325") +
    geom_vline(aes(xintercept = gen[gen$Trait == "ff_height_std" & gen$Density == 3, "Generations"][1]), linetype = "dashed", color = "#0ac9db") +
    geom_vline(aes(xintercept = gen[gen$Trait == "ff_height_std" & gen$Density == 4, "Generations"][1]), linetype = "dashed", color = "blue") +
    geom_vline(aes(xintercept = gen[gen$Trait == "ff_height_std" & gen$Density == 5, "Generations"][1]), linetype = "dashed", color = "purple") +
    geom_vline(aes(xintercept = gen[gen$Trait == "ff_height_std" & gen$Density == 6, "Generations"][1]), linetype = "dashed", color = "red") +
    geom_vline(aes(xintercept = gen[gen$Trait == "ff_height_std" & gen$Density == 7, "Generations"][1]), linetype = "dashed", color = "magenta")


p3 <- ggplot(results_by_trait[["final_height_std"]], aes(x = Generations, y = Final_Allele_Frequency, group = factor(Density), color = factor(Density))) +
    geom_line() + theme_light() + 
    labs(title = paste("Final Height"), x = NULL, y = NULL) +
    scale_color_manual(name = "Density", values = c("#0ac9db", "blue", "purple", "red", "magenta")) +
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
    geom_vline(aes(xintercept = gen[gen$Trait == "final_height_std" & gen$Density == 2, "Generations"][1]), linetype = "dashed", color = "#0e0325") +
    geom_vline(aes(xintercept = gen[gen$Trait == "final_height_std" & gen$Density == 3, "Generations"][1]), linetype = "dashed", color = "#0ac9db") +
    geom_vline(aes(xintercept = gen[gen$Trait == "final_height_std" & gen$Density == 4, "Generations"][1]), linetype = "dashed", color = "blue") +
    geom_vline(aes(xintercept = gen[gen$Trait == "final_height_std" & gen$Density == 5, "Generations"][1]), linetype = "dashed", color = "purple") +
    geom_vline(aes(xintercept = gen[gen$Trait == "final_height_std" & gen$Density == 6, "Generations"][1]), linetype = "dashed", color = "red") +
    geom_vline(aes(xintercept = gen[gen$Trait == "final_height_std" & gen$Density == 7, "Generations"][1]), linetype = "dashed", color = "magenta")


p4 <- ggplot(results_by_trait[["flower_number_1_std"]], aes(x = Generations, y = Final_Allele_Frequency, group = factor(Density), color = factor(Density))) +
    geom_line() + theme_light() + 
    labs(title = paste("Flower Number"), x = NULL, y = "Allele Frequency") +
    scale_color_manual(name = "Density", values = c("#0ac9db", "blue", "purple", "red", "magenta")) +
    theme(legend.position = "none",, plot.title = element_text(hjust = 0.5)) +
    geom_vline(aes(xintercept = gen[gen$Trait == "flower_number_1_std" & gen$Density == 2, "Generations"][1]), linetype = "dashed", color = "#0e0325") +
    geom_vline(aes(xintercept = gen[gen$Trait == "flower_number_1_std" & gen$Density == 3, "Generations"][1]), linetype = "dashed", color = "#0ac9db") +
    geom_vline(aes(xintercept = gen[gen$Trait == "flower_number_1_std" & gen$Density == 4, "Generations"][1]), linetype = "dashed", color = "blue") +
    geom_vline(aes(xintercept = gen[gen$Trait == "flower_number_1_std" & gen$Density == 5, "Generations"][1]), linetype = "dashed", color = "purple") +
    geom_vline(aes(xintercept = gen[gen$Trait == "flower_number_1_std" & gen$Density == 6, "Generations"][1]), linetype = "dashed", color = "red") +
    geom_vline(aes(xintercept = gen[gen$Trait == "flower_number_1_std" & gen$Density == 7, "Generations"][1]), linetype = "dashed", color = "magenta")


p5 <- ggplot(results_by_trait[["leaf_area_std"]], aes(x = Generations, y = Final_Allele_Frequency, group = factor(Density), color = factor(Density))) +
    geom_line() + theme_light() + 
    labs(title = paste("Leaf Area"), x = NULL, y = NULL) +
    scale_color_manual(name = "Density", values = c("#0ac9db", "blue", "purple", "red", "magenta")) +
    theme(legend.position = "none",, plot.title = element_text(hjust = 0.5)) +
    geom_vline(aes(xintercept = gen[gen$Trait == "leaf_area_std" & gen$Density == 2, "Generations"][1]), linetype = "dashed", color = "#0e0325") +
    geom_vline(aes(xintercept = gen[gen$Trait == "leaf_area_std" & gen$Density == 3, "Generations"][1]), linetype = "dashed", color = "#0ac9db") +
    geom_vline(aes(xintercept = gen[gen$Trait == "leaf_area_std" & gen$Density == 4, "Generations"][1]), linetype = "dashed", color = "blue") +
    geom_vline(aes(xintercept = gen[gen$Trait == "leaf_area_std" & gen$Density == 5, "Generations"][1]), linetype = "dashed", color = "purple") +
    geom_vline(aes(xintercept = gen[gen$Trait == "leaf_area_std" & gen$Density == 6, "Generations"][1]), linetype = "dashed", color = "red") +
    geom_vline(aes(xintercept = gen[gen$Trait == "leaf_area_std" & gen$Density == 7, "Generations"][1]), linetype = "dashed", color = "magenta")

p6 <- ggplot(results_by_trait[["Huber_Volume_std"]], aes(x = Generations, y = Final_Allele_Frequency, group = factor(Density), color = factor(Density))) +
    geom_line() + theme_light() + 
    labs(title = paste("Volume"), x = NULL, y = NULL) +
    scale_color_manual(name = "Density", values = c("#0ac9db", "blue", "purple", "red", "magenta")) +
    theme(legend.position = "none",, plot.title = element_text(hjust = 0.5)) +
    geom_vline(aes(xintercept = gen[gen$Trait == "Huber_Volume_std" & gen$Density == 2, "Generations"][1]), linetype = "dashed", color = "#0e0325") +
    geom_vline(aes(xintercept = gen[gen$Trait == "Huber_Volume_std" & gen$Density == 3, "Generations"][1]), linetype = "dashed", color = "#0ac9db") +
    geom_vline(aes(xintercept = gen[gen$Trait == "Huber_Volume_std" & gen$Density == 4, "Generations"][1]), linetype = "dashed", color = "blue") +
    geom_vline(aes(xintercept = gen[gen$Trait == "Huber_Volume_std" & gen$Density == 5, "Generations"][1]), linetype = "dashed", color = "purple") +
    geom_vline(aes(xintercept = gen[gen$Trait == "Huber_Volume_std" & gen$Density == 6, "Generations"][1]), linetype = "dashed", color = "red") +
    geom_vline(aes(xintercept = gen[gen$Trait == "Huber_Volume_std" & gen$Density == 7, "Generations"][1]), linetype = "dashed", color = "magenta")


p7 <- ggplot(results_by_trait[["prop_early_growth_std"]], aes(x = Generations, y = Final_Allele_Frequency, group = factor(Density), color = factor(Density))) +
    geom_line() + theme_light() + 
    labs(title = paste("Early Growth"), x = "Generations", y = "Allele Frequency") +
    scale_color_manual(name = "Density", values = c("#0ac9db", "blue", "purple", "red", "magenta")) +
    theme(legend.position = "none",, plot.title = element_text(hjust = 0.5)) +
    geom_vline(aes(xintercept = gen[gen$Trait == "prop_early_growth_std" & gen$Density == 2, "Generations"][1]), linetype = "dashed", color = "#0e0325") +
    geom_vline(aes(xintercept = gen[gen$Trait == "prop_early_growth_std" & gen$Density == 3, "Generations"][1]), linetype = "dashed", color = "#0ac9db") +
    geom_vline(aes(xintercept = gen[gen$Trait == "prop_early_growth_std" & gen$Density == 4, "Generations"][1]), linetype = "dashed", color = "blue") +
    geom_vline(aes(xintercept = gen[gen$Trait == "prop_early_growth_std" & gen$Density == 5, "Generations"][1]), linetype = "dashed", color = "purple") +
    geom_vline(aes(xintercept = gen[gen$Trait == "prop_early_growth_std" & gen$Density == 6, "Generations"][1]), linetype = "dashed", color = "red") +
    geom_vline(aes(xintercept = gen[gen$Trait == "prop_early_growth_std" & gen$Density == 7, "Generations"][1]), linetype = "dashed", color = "magenta")


p8 <- ggplot(results_by_trait[["total_node_number_std"]], aes(x = Generations, y = Final_Allele_Frequency, group = factor(Density), color = factor(Density))) +
    geom_line() + theme_light() + 
    labs(title = paste("Node Number"), x = "Generations", y = NULL) +
    scale_color_manual(name = "Density", values = c("#0ac9db", "blue", "purple", "red", "magenta")) +
    theme(legend.position = "none",, plot.title = element_text(hjust = 0.5)) +
    geom_vline(aes(xintercept = gen[gen$Trait == "total_node_number_std" & gen$Density == 2, "Generations"][1]), linetype = "dashed", color = "#0e0325") +
    geom_vline(aes(xintercept = gen[gen$Trait == "total_node_number_std" & gen$Density == 3, "Generations"][1]), linetype = "dashed", color = "#0ac9db") +
    geom_vline(aes(xintercept = gen[gen$Trait == "total_node_number_std" & gen$Density == 4, "Generations"][1]), linetype = "dashed", color = "blue") +
    geom_vline(aes(xintercept = gen[gen$Trait == "total_node_number_std" & gen$Density == 5, "Generations"][1]), linetype = "dashed", color = "purple") +
    geom_vline(aes(xintercept = gen[gen$Trait == "total_node_number_std" & gen$Density == 6, "Generations"][1]), linetype = "dashed", color = "red") +
    geom_vline(aes(xintercept = gen[gen$Trait == "total_node_number_std" & gen$Density == 7, "Generations"][1]), linetype = "dashed", color = "magenta")

p9 <- ggplot(results_by_trait[["flower_area_std"]], aes(x = Generations, y = Final_Allele_Frequency, group = factor(Density), color = factor(Density))) +
    geom_line() + theme_light() + 
    labs(title = paste("Flower Area"), x = "Generations", y = NULL) +
    scale_color_manual(values = c("#0ac9db", "blue", "purple", "red", "magenta")) +
    theme(legend.position = c(0.8,0.5), plot.title = element_text(hjust = 0.5), legend.title = element_blank()) +
    geom_vline(aes(xintercept = gen[gen$Trait == "flower_area_std" & gen$Density == 2, "Generations"][1]), linetype = "dashed", color = "#0e0325") +
    geom_vline(aes(xintercept = gen[gen$Trait == "flower_area_std" & gen$Density == 3, "Generations"][1]), linetype = "dashed", color = "#0ac9db") +
    geom_vline(aes(xintercept = gen[gen$Trait == "flower_area_std" & gen$Density == 4, "Generations"][1]), linetype = "dashed", color = "blue") +
    geom_vline(aes(xintercept = gen[gen$Trait == "flower_area_std" & gen$Density == 5, "Generations"][1]), linetype = "dashed", color = "purple") +
    geom_vline(aes(xintercept = gen[gen$Trait == "flower_area_std" & gen$Density == 6, "Generations"][1]), linetype = "dashed", color = "red") +
    geom_vline(aes(xintercept = gen[gen$Trait == "flower_area_std" & gen$Density == 7, "Generations"][1]), linetype = "dashed", color = "magenta")# Arrange the plots into one graphic


combined_plot <- ggarrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, ncol = 3, nrow = 3)

ggsave(filename = "allele_frequency.pdf", plot = combined_plot, device = cairo_pdf, width = 8, height = 6)
##################################################################################################
## Modify function to include selection coefficient for the Least Fit, Most Fit, and All Plants ##
##################################################################################################

#Load in Data 
data <- read.csv("/Users/brandonhendrickson/Documents/Github_Projects/M_guttatus_PhenotypicSelection/Data/csv/phenotype_selection.csv")

# Define the function
change_in_allele_frequency <- function(P, s1, s2, s3, w1, w2, w3, generations) {
    for (i in 1:generations) {
        s <- w1 * s1 + w2 * s2 + w3 * s3  # Calculate the weighted sum of the s values
        P <- P + s * P * (1 - P)
    }
    return(P)
}

P = 0.5
# Initialize an empty dataframe to store the results
results_sub <- data.frame(Trait = character(), Density = numeric(), Estimate = numeric(), Estimate_F = numeric(), Estimate_LF = numeric(), Generations = integer(), Final_Allele_Frequency = numeric(), stringsAsFactors = FALSE)

# Loop over the rows of the data dataframe
for (i in 1:nrow(data)) {
    # Only proceed if the Density value is greater than 3
    if (data$Density[i] >= 3) {
        # Get the s values from the Estimate, Estimate_F, and Estimate_LF columns
        s1 <- data$Estimate[i]
        s2 <- data$Estimate_F[i]
        s3 <- data$Estimate_LF[i]

        # Calculate the weights
        density <- data$Density[i]
        w1 <- (density - 2) / density
        w2 <- 1 / density
        w3 <- 1 / density

        # Loop over the list of generations
        for (generations in seq(1, 1000, 10)) {
            # Calculate the final allele frequency
            P_final <- change_in_allele_frequency(P, s1, s2, s3, w1, w2, w3, generations)

            # Add the results to the dataframe
            results_sub <- rbind(results_sub, data.frame(Trait = data$Trait[i], Density = density, Estimate = s1, Estimate_F = s2, Estimate_LF = s3, Generations = generations, Final_Allele_Frequency = P_final, stringsAsFactors = FALSE))
        }
    } else if (data$Density[i] == 2) {
        # Get the s values from the Estimate, Estimate_F, and Estimate_LF columns
        s1 <- 0
        s2 <- data$Estimate_F[i]
        s3 <- data$Estimate_LF[i]

        # Calculate the weights
        density <- data$Density[i]
        w1 <- 0
        w2 <- 1 / density
        w3 <- 1 / density

        # Loop over the list of generations
        for (generations in seq(1, 1000, 10)) {
            # Calculate the final allele frequency
            P_final <- change_in_allele_frequency(P, s1, s2, s3, w1, w2, w3, generations)

            # Add the results to the dataframe
            results_sub <- rbind(results_sub, data.frame(Trait = data$Trait[i], Density = density, Estimate = s1, Estimate_F = s2, Estimate_LF = s3, Generations = generations, Final_Allele_Frequency = P_final, stringsAsFactors = FALSE))
        }
    }
}

#Write out simulation results
write.csv(results_sub, "/Users/brandonhendrickson/Documents/Github_Projects/M_guttatus_PhenotypicSelection/Data/csv/allele_frequency_stratified.csv", row.names = FALSE)

# Initialize a new dataframe to store the marked rows
marked_rows_sub <- data.frame()

# Loop over the list of dataframes
for (trait in names(results_sub)) {
    # Loop over the unique densities
    for (density in unique(results_sub$Density)) {
        # Subset the data for the current density
        data_density <- subset(results_sub, Density == density)
        
        # Initialize a new column Passes_0.99 with FALSE
        data_density$Passes_0.99 <- FALSE
            # Loop over the rows of data_density
            for (i in 2:nrow(data_density)) {
                if (data_density$Trait[i] == "D2Flowering_std") {
                    if (data_density$Final_Allele_Frequency[i] < 0.01 && data_density$Final_Allele_Frequency[i - 1] > 0.01) {
                        # Mark this row
                        data_density$Passes_0.99[i] <- TRUE
                    }
                } else {
                    # If the current row is greater than 0.99 and the previous row is less than 0.99
                    if (data_density$Final_Allele_Frequency[i] > 0.99 && data_density$Final_Allele_Frequency[i - 1] < 0.99) {
                        # Mark this row
                        data_density$Passes_0.99[i] <- TRUE
                    }
                }
            }
        # Add the marked rows to the new dataframe
        marked_rows_sub <- rbind(marked_rows_sub, data_density)
    }
}

# Initialize an empty list to store the plots
plots <- list()

# Split the results dataframe by Trait
results_by_trait_sub <- split(marked_rows_sub, marked_rows_sub$Trait)

# Assuming results_by_trait_sub is your list of dataframes
gen <- data.frame()

# Loop over the list of dataframes
for (trait in names(results_by_trait_sub)) {
    # Subset the data where Passes_0.99 is TRUE
    data_subset <- subset(results_by_trait_sub[[trait]], Passes_0.99 == TRUE)

    # Add the subsetted data to the new dataframe
    gen <- rbind(gen, data_subset)
}

#Write out generation times for Fixation
write.csv(gen, "/Users/brandonhendrickson/Documents/Github_Projects/M_guttatus_PhenotypicSelection/Data/csv/generations_stratified.csv", row.names = FALSE)

#Make Graphs
p1 <- ggplot(results_by_trait_sub[["D2Flowering_std"]], aes(x = Generations, y = Final_Allele_Frequency, group = factor(Density), color = factor(Density))) +
    geom_line() +
    theme_light() +
    labs(title = paste("Flowering Date"), x = NULL, y = "Allele Frequency") +
    scale_color_manual(name = "Density", values = c("#0e0325","#0ac9db", "blue", "purple", "red", "magenta")) +
    theme(legend.position = "none", , plot.title = element_text(hjust = 0.5)) +
    geom_vline(aes(xintercept = gen[gen$Trait == "D2Flowering_std" & gen$Density == 2, "Generations"][1]), linetype = "dashed", color = "#0e0325") +
    geom_vline(aes(xintercept = gen[gen$Trait == "D2Flowering_std" & gen$Density == 3, "Generations"][1]), linetype = "dashed", color = "#0ac9db") +
    geom_vline(aes(xintercept = gen[gen$Trait == "D2Flowering_std" & gen$Density == 4, "Generations"][1]), linetype = "dashed", color = "blue") +
    geom_vline(aes(xintercept = gen[gen$Trait == "D2Flowering_std" & gen$Density == 5, "Generations"][1]), linetype = "dashed", color = "purple") +
    geom_vline(aes(xintercept = gen[gen$Trait == "D2Flowering_std" & gen$Density == 6, "Generations"][1]), linetype = "dashed", color = "red") +
    geom_vline(aes(xintercept = gen[gen$Trait == "D2Flowering_std" & gen$Density == 7, "Generations"][1]), linetype = "dashed", color = "magenta")


p2 <- ggplot(results_by_trait_sub[["ff_height_std"]], aes(x = Generations, y = Final_Allele_Frequency, group = factor(Density), color = factor(Density))) +
    geom_line() + theme_light() + 
    labs(title = paste("Height @ Flower"), x = NULL, y = NULL) +
    scale_color_manual(name = "Density", values = c("#0e0325","#0ac9db", "blue", "purple", "red", "magenta")) +
    theme(legend.position = "none",, plot.title = element_text(hjust = 0.5)) +
    geom_vline(aes(xintercept = gen[gen$Trait == "ff_height_std" & gen$Density == 2, "Generations"][1]), linetype = "dashed", color = "#0e0325") +
    geom_vline(aes(xintercept = gen[gen$Trait == "ff_height_std" & gen$Density == 3, "Generations"][1]), linetype = "dashed", color = "#0ac9db") +
    geom_vline(aes(xintercept = gen[gen$Trait == "ff_height_std" & gen$Density == 4, "Generations"][1]), linetype = "dashed", color = "blue") +
    geom_vline(aes(xintercept = gen[gen$Trait == "ff_height_std" & gen$Density == 5, "Generations"][1]), linetype = "dashed", color = "purple") +
    geom_vline(aes(xintercept = gen[gen$Trait == "ff_height_std" & gen$Density == 6, "Generations"][1]), linetype = "dashed", color = "red") +
    geom_vline(aes(xintercept = gen[gen$Trait == "ff_height_std" & gen$Density == 7, "Generations"][1]), linetype = "dashed", color = "magenta")


p3 <- ggplot(results_by_trait_sub[["final_height_std"]], aes(x = Generations, y = Final_Allele_Frequency, group = factor(Density), color = factor(Density))) +
    geom_line() + theme_light() + 
    labs(title = paste("Final Height"), x = NULL, y = NULL) +
    scale_color_manual(name = "Density", values = c("#0e0325","#0ac9db", "blue", "purple", "red", "magenta")) +
    theme(legend.position = "none",, plot.title = element_text(hjust = 0.5)) +
    geom_vline(aes(xintercept = gen[gen$Trait == "final_height_std" & gen$Density == 2, "Generations"][1]), linetype = "dashed", color = "#0e0325") +
    geom_vline(aes(xintercept = gen[gen$Trait == "final_height_std" & gen$Density == 3, "Generations"][1]), linetype = "dashed", color = "#0ac9db") +
    geom_vline(aes(xintercept = gen[gen$Trait == "final_height_std" & gen$Density == 4, "Generations"][1]), linetype = "dashed", color = "blue") +
    geom_vline(aes(xintercept = gen[gen$Trait == "final_height_std" & gen$Density == 5, "Generations"][1]), linetype = "dashed", color = "purple") +
    geom_vline(aes(xintercept = gen[gen$Trait == "final_height_std" & gen$Density == 6, "Generations"][1]), linetype = "dashed", color = "red") +
    geom_vline(aes(xintercept = gen[gen$Trait == "final_height_std" & gen$Density == 7, "Generations"][1]), linetype = "dashed", color = "magenta")


p4 <- ggplot(results_by_trait_sub[["flower_number_1_std"]], aes(x = Generations, y = Final_Allele_Frequency, group = factor(Density), color = factor(Density))) +
    geom_line() + theme_light() + 
    labs(title = paste("Flower Number"), x = NULL, y = "Allele Frequency") +
    scale_color_manual(name = "Density", values = c("#0e0325","#0ac9db", "blue", "purple", "red", "magenta")) +
    theme(legend.position = "none",, plot.title = element_text(hjust = 0.5)) +
    geom_vline(aes(xintercept = gen[gen$Trait == "flower_number_1_std" & gen$Density == 2, "Generations"][1]), linetype = "dashed", color = "#0e0325") +
    geom_vline(aes(xintercept = gen[gen$Trait == "flower_number_1_std" & gen$Density == 3, "Generations"][1]), linetype = "dashed", color = "#0ac9db") +
    geom_vline(aes(xintercept = gen[gen$Trait == "flower_number_1_std" & gen$Density == 4, "Generations"][1]), linetype = "dashed", color = "blue") +
    geom_vline(aes(xintercept = gen[gen$Trait == "flower_number_1_std" & gen$Density == 5, "Generations"][1]), linetype = "dashed", color = "purple") +
    geom_vline(aes(xintercept = gen[gen$Trait == "flower_number_1_std" & gen$Density == 6, "Generations"][1]), linetype = "dashed", color = "red") +
    geom_vline(aes(xintercept = gen[gen$Trait == "flower_number_1_std" & gen$Density == 7, "Generations"][1]), linetype = "dashed", color = "magenta")


p5 <- ggplot(results_by_trait_sub[["leaf_area_std"]], aes(x = Generations, y = Final_Allele_Frequency, group = factor(Density), color = factor(Density))) +
    geom_line() + theme_light() + 
    labs(title = paste("Leaf Area"), x = NULL, y = NULL) +
    scale_color_manual(name = "Density", values = c("#0e0325","#0ac9db", "blue", "purple", "red", "magenta")) +
    theme(legend.position = "none",, plot.title = element_text(hjust = 0.5)) +
    geom_vline(aes(xintercept = gen[gen$Trait == "leaf_area_std" & gen$Density == 2, "Generations"][1]), linetype = "dashed", color = "#0e0325") +
    geom_vline(aes(xintercept = gen[gen$Trait == "leaf_area_std" & gen$Density == 3, "Generations"][1]), linetype = "dashed", color = "#0ac9db") +
    geom_vline(aes(xintercept = gen[gen$Trait == "leaf_area_std" & gen$Density == 4, "Generations"][1]), linetype = "dashed", color = "blue") +
    geom_vline(aes(xintercept = gen[gen$Trait == "leaf_area_std" & gen$Density == 5, "Generations"][1]), linetype = "dashed", color = "purple") +
    geom_vline(aes(xintercept = gen[gen$Trait == "leaf_area_std" & gen$Density == 6, "Generations"][1]), linetype = "dashed", color = "red") +
    geom_vline(aes(xintercept = gen[gen$Trait == "leaf_area_std" & gen$Density == 7, "Generations"][1]), linetype = "dashed", color = "magenta")


p6 <- ggplot(results_by_trait_sub[["Huber_Volume_std"]], aes(x = Generations, y = Final_Allele_Frequency, group = factor(Density), color = factor(Density))) +
    geom_line() + theme_light() + 
    labs(title = paste("Volume"), x = NULL, y = NULL) +
    scale_color_manual(name = "Density", values = c("#0e0325","#0ac9db", "blue", "purple", "red", "magenta")) +
    theme(legend.position = "none",, plot.title = element_text(hjust = 0.5)) +
    geom_vline(aes(xintercept = gen[gen$Trait == "Huber_Volume_std" & gen$Density == 2, "Generations"][1]), linetype = "dashed", color = "#0e0325") +
    geom_vline(aes(xintercept = gen[gen$Trait == "Huber_Volume_std" & gen$Density == 3, "Generations"][1]), linetype = "dashed", color = "#0ac9db") +
    geom_vline(aes(xintercept = gen[gen$Trait == "Huber_Volume_std" & gen$Density == 4, "Generations"][1]), linetype = "dashed", color = "blue") +
    geom_vline(aes(xintercept = gen[gen$Trait == "Huber_Volume_std" & gen$Density == 5, "Generations"][1]), linetype = "dashed", color = "purple") +
    geom_vline(aes(xintercept = gen[gen$Trait == "Huber_Volume_std" & gen$Density == 6, "Generations"][1]), linetype = "dashed", color = "red") +
    geom_vline(aes(xintercept = gen[gen$Trait == "Huber_Volume_std" & gen$Density == 7, "Generations"][1]), linetype = "dashed", color = "magenta")


p7 <- ggplot(results_by_trait_sub[["prop_early_growth_std"]], aes(x = Generations, y = Final_Allele_Frequency, group = factor(Density), color = factor(Density))) +
    geom_line() + theme_light() + 
    labs(title = paste("Early Growth"), x = "Generations", y = "Allele Frequency") +
    scale_color_manual(name = "Density", values = c("#0e0325","#0ac9db", "blue", "purple", "red", "magenta")) +
    theme(legend.position = "none",, plot.title = element_text(hjust = 0.5)) +
    geom_vline(aes(xintercept = gen[gen$Trait == "prop_early_growth_std" & gen$Density == 2, "Generations"][1]), linetype = "dashed", color = "#0e0325") +
    geom_vline(aes(xintercept = gen[gen$Trait == "prop_early_growth_std" & gen$Density == 3, "Generations"][1]), linetype = "dashed", color = "#0ac9db") +
    geom_vline(aes(xintercept = gen[gen$Trait == "prop_early_growth_std" & gen$Density == 4, "Generations"][1]), linetype = "dashed", color = "blue") +
    geom_vline(aes(xintercept = gen[gen$Trait == "prop_early_growth_std" & gen$Density == 5, "Generations"][1]), linetype = "dashed", color = "purple") +
    geom_vline(aes(xintercept = gen[gen$Trait == "prop_early_growth_std" & gen$Density == 6, "Generations"][1]), linetype = "dashed", color = "red") +
    geom_vline(aes(xintercept = gen[gen$Trait == "prop_early_growth_std" & gen$Density == 7, "Generations"][1]), linetype = "dashed", color = "magenta")


p8 <- ggplot(results_by_trait_sub[["total_node_number_std"]], aes(x = Generations, y = Final_Allele_Frequency, group = factor(Density), color = factor(Density))) +
    geom_line() + theme_light() + 
    labs(title = paste("Node Number"), x = "Generations", y = NULL) +
    scale_color_manual(name = "Density", values = c("#0e0325","#0ac9db", "blue", "purple", "red", "magenta")) +
    theme(legend.position = "none",, plot.title = element_text(hjust = 0.5)) +
    geom_vline(aes(xintercept = gen[gen$Trait == "total_node_number_std" & gen$Density == 2, "Generations"][1]), linetype = "dashed", color = "#0e0325") +
    geom_vline(aes(xintercept = gen[gen$Trait == "total_node_number_std" & gen$Density == 3, "Generations"][1]), linetype = "dashed", color = "#0ac9db") +
    geom_vline(aes(xintercept = gen[gen$Trait == "total_node_number_std" & gen$Density == 4, "Generations"][1]), linetype = "dashed", color = "blue") +
    geom_vline(aes(xintercept = gen[gen$Trait == "total_node_number_std" & gen$Density == 5, "Generations"][1]), linetype = "dashed", color = "purple") +
    geom_vline(aes(xintercept = gen[gen$Trait == "total_node_number_std" & gen$Density == 6, "Generations"][1]), linetype = "dashed", color = "red") +
    geom_vline(aes(xintercept = gen[gen$Trait == "total_node_number_std" & gen$Density == 7, "Generations"][1]), linetype = "dashed", color = "magenta")

p9 <- ggplot(results_by_trait_sub[["flower_area_std"]], aes(x = Generations, y = Final_Allele_Frequency, group = factor(Density), color = factor(Density))) +
    geom_line() + theme_light() + 
    labs(title = paste("Flower Area"), x = "Generations", y = NULL) +
    scale_color_manual(name = "Density", values = c("#0e0325","#0ac9db", "blue", "purple", "red", "magenta")) +
    theme(legend.position = c(0.8,0.5), plot.title = element_text(hjust = 0.5), legend.title = element_blank()) +
    geom_vline(aes(xintercept = gen[gen$Trait == "flower_area_std" & gen$Density == 2, "Generations"][1]), linetype = "dashed", color = "#0e0325") +
    geom_vline(aes(xintercept = gen[gen$Trait == "flower_area_std" & gen$Density == 3, "Generations"][1]), linetype = "dashed", color = "#0ac9db") +
    geom_vline(aes(xintercept = gen[gen$Trait == "flower_area_std" & gen$Density == 4, "Generations"][1]), linetype = "dashed", color = "blue") +
    geom_vline(aes(xintercept = gen[gen$Trait == "flower_area_std" & gen$Density == 5, "Generations"][1]), linetype = "dashed", color = "purple") +
    geom_vline(aes(xintercept = gen[gen$Trait == "flower_area_std" & gen$Density == 6, "Generations"][1]), linetype = "dashed", color = "red") +
    geom_vline(aes(xintercept = gen[gen$Trait == "flower_area_std" & gen$Density == 7, "Generations"][1]), linetype = "dashed", color = "magenta")
# Arrange the plots into one graphic
combined_plot <- ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,p9, ncol = 3, nrow = 3)

ggsave(filename = "allele_frequency_stratified.pdf", plot = combined_plot, device = cairo_pdf, width = 8, height = 6)
###################################################################################
## Modify to Weight the Selection Coefficients by the Number Proportion of Yield ##
###################################################################################

# Load dataframe
data <- read.csv("/Users/brandonhendrickson/Documents/Github_Projects/M_guttatus_PhenotypicSelection/Data/csv/standardized_data.csv")

# Calculate the average yield for each density and fitness group
yield_avg <- aggregate(yield ~ Final_Density + F_LF, data = data, FUN = mean)

# Remove Densities of 1 from dataframe
yield_avg <- yield_avg[yield_avg$Final_Density != 1, ]


# Reshape the data
yield_avg_wide <- yield_avg %>%
    spread(key = F_LF, value = yield)

# Calculate the total yield per row
yield_avg_wide$total_yield = rowSums(yield_avg_wide[, c("F", "LF", "Other")], na.rm = TRUE)

# Calculate the proportion of yield for each category
yield_avg_wide$F_prop = yield_avg_wide$F / yield_avg_wide$total_yield
yield_avg_wide$LF_prop = yield_avg_wide$LF / yield_avg_wide$total_yield
yield_avg_wide$Other_prop = yield_avg_wide$Other / yield_avg_wide$total_yield

# Calculate the row averages
yield_avg_wide$Avg <- rowMeans(yield_avg_wide[, c("F", "LF", "Other")], na.rm = TRUE)

yield_avg_wide$F_m <- yield_avg_wide$F / ((yield_avg_wide$F + yield_avg_wide$LF + (yield_avg_wide$Other * (yield_avg_wide$Final_Density - 2))) / yield_avg_wide$Final_Density)
yield_avg_wide$LF_m<-yield_avg_wide$LF/((yield_avg_wide$F + yield_avg_wide$LF + (yield_avg_wide$Other * (yield_avg_wide$Final_Density-2)))/yield_avg_wide$Final_Density)
yield_avg_wide$Other_m <- yield_avg_wide$Other / ((yield_avg_wide$F + yield_avg_wide$LF + (yield_avg_wide$Other * (yield_avg_wide$Final_Density - 2))) / yield_avg_wide$Final_Density)


# Change column name of Final_Density
colnames(yield_avg_wide)[1] <- "Density"

#Load in Selection Data
data <- read.csv("/Users/brandonhendrickson/Documents/Github_Projects/M_guttatus_PhenotypicSelection/Data/csv/phenotype_selection.csv")

# Merge the two dataframes
data <- merge(data, yield_avg_wide, by = "Density")

# Define the function
change_in_allele_frequency <- function(P, s1, s2, s3, w1, w2, w3, generations) {
    for (i in 1:generations) {
        s <- w1 * s1 + w2 * s2 + w3 * s3  # Calculate the weighted sum of the s values
        P <- P + s * P * (1 - P)
    }
    return(P)
}

P = 0.5
# Initialize an empty dataframe to store the results
results_yield <- data.frame(Trait = character(), Density = numeric(), Estimate = numeric(), Estimate_F = numeric(), Estimate_LF = numeric(), Generations = integer(), Final_Allele_Frequency = numeric(), stringsAsFactors = FALSE)

# Loop over the rows of the data dataframe
for (i in 1:nrow(data)) {
    # Only proceed if the Density value is greater than 3
    if (data$Density[i] >= 3) {
        # Get the s values from the Estimate, Estimate_F, and Estimate_LF columns
        s1 <- data$Estimate[i]
        s2 <- data$Estimate_F[i]
        s3 <- data$Estimate_LF[i]

        # Add Yield
        w1 <- data$Other_prop[i]
        w2 <- data$F_prop[i]
        w3 <- data$LF_prop[i]

        # Loop over the list of generations
        for (generations in seq(1, 1000, 10)) {
            # Calculate the final allele frequency
            P_final <- change_in_allele_frequency(P, s1, s2, s3, w1, w2, w3, generations)

            # Add the results to the dataframe
            results_yield <- rbind(results_yield, data.frame(Trait = data$Trait[i], Density = data$Density[i], Estimate = s1, Estimate_F = s2, Estimate_LF = s3, Generations = generations, Final_Allele_Frequency = P_final, stringsAsFactors = FALSE))
        }
    } else if (data$Density[i] == 2) {
        # Get the s values from the Estimate, Estimate_F, and Estimate_LF columns
        s1 <- 0
        s2 <- data$Estimate_F[i]
        s3 <- data$Estimate_LF[i]

        # Calculate the weights
        w1 <- 0
        w2 <- data$F_prop[i]
        w3 <- data$LF_prop[i]

        # Loop over the list of generations
        for (generations in seq(1, 1000, 10)) {
            # Calculate the final allele frequency
            P_final <- change_in_allele_frequency(P, s1, s2, s3, w1, w2, w3,generations)

            # Add the results to the dataframe
            results_yield <- rbind(results_yield, data.frame(Trait = data$Trait[i], Density = data$Density[i], Estimate = s1, Estimate_F = s2, Estimate_LF = s3, Generations = generations, Final_Allele_Frequency = P_final, stringsAsFactors = FALSE))
        }
    }
}


#Write out simulation results
write.csv(results_yield, "/Users/brandonhendrickson/Documents/Github_Projects/M_guttatus_PhenotypicSelection/Data/csv/allele_frequency_yield.csv", row.names = FALSE)

# Initialize a new dataframe to store the marked rows
marked_rows_yield <- data.frame()

# Loop over the list of dataframes
for (trait in names(results_yield)) {
    # Loop over the unique densities
    for (density in unique(results_yield$Density)) {
        # Subset the data for the current density
        data_density <- subset(results_yield, Density == density)
        
        # Initialize a new column Passes_0.99 with FALSE
        data_density$Passes_0.99 <- FALSE
            # Loop over the rows of data_density
            for (i in 2:nrow(data_density)) {
                if (data_density$Trait[i] == "D2Flowering_std") {
                    if (data_density$Final_Allele_Frequency[i] < 0.01 && data_density$Final_Allele_Frequency[i - 1] > 0.01) {
                        # Mark this row
                        data_density$Passes_0.99[i] <- TRUE
                    }
                } else {
                    # If the current row is greater than 0.99 and the previous row is less than 0.99
                    if (data_density$Final_Allele_Frequency[i] > 0.99 && data_density$Final_Allele_Frequency[i - 1] < 0.99) {
                        # Mark this row
                        data_density$Passes_0.99[i] <- TRUE
                    }
                }
            }
        # Add the marked rows to the new dataframe
        marked_rows_yield <- rbind(marked_rows_yield, data_density)
    }
}

# Initialize an empty list to store the plots
plots <- list()

# Split the results dataframe by Trait
results_by_trait_yield <- split(marked_rows_yield, marked_rows_yield$Trait)

# Assuming results_by_trait_yield is your list of dataframes
gen <- data.frame()

# Loop over the list of dataframes
for (trait in names(results_by_trait_yield)) {
    # Subset the data where Passes_0.99 is TRUE
    data_subset <- subset(results_by_trait_yield[[trait]], Passes_0.99 == TRUE)

    # Add the subsetted data to the new dataframe
    gen <- rbind(gen, data_subset)
}

#Write out generation times for Fixation
write.csv(gen, "/Users/brandonhendrickson/Documents/Github_Projects/M_guttatus_PhenotypicSelection/Data/csv/generations_yield.csv", row.names = FALSE)

#Make Graphs
p1 <- ggplot(results_by_trait_yield[["D2Flowering_std"]], aes(x = Generations, y = Final_Allele_Frequency, group = factor(Density), color = factor(Density))) +
    geom_line() +
    theme_light() +
    labs(title = paste("Flowering Date"), x = NULL, y = "Allele Frequency") +
    scale_color_manual(name = "Density", values = c("#0e0325","#0ac9db", "blue", "purple", "red", "magenta")) +
    theme(legend.position = "none", , plot.title = element_text(hjust = 0.5)) +
    geom_vline(aes(xintercept = gen[gen$Trait == "D2Flowering_std" & gen$Density == 2, "Generations"][1]), linetype = "dashed", color = "#0e0325") +
    geom_vline(aes(xintercept = gen[gen$Trait == "D2Flowering_std" & gen$Density == 3, "Generations"][1]), linetype = "dashed", color = "#0ac9db") +
    geom_vline(aes(xintercept = gen[gen$Trait == "D2Flowering_std" & gen$Density == 4, "Generations"][1]), linetype = "dashed", color = "blue") +
    geom_vline(aes(xintercept = gen[gen$Trait == "D2Flowering_std" & gen$Density == 5, "Generations"][1]), linetype = "dashed", color = "purple") +
    geom_vline(aes(xintercept = gen[gen$Trait == "D2Flowering_std" & gen$Density == 6, "Generations"][1]), linetype = "dashed", color = "red") +
    geom_vline(aes(xintercept = gen[gen$Trait == "D2Flowering_std" & gen$Density == 7, "Generations"][1]), linetype = "dashed", color = "magenta")


p2 <- ggplot(results_by_trait_yield[["ff_height_std"]], aes(x = Generations, y = Final_Allele_Frequency, group = factor(Density), color = factor(Density))) +
    geom_line() + theme_light() + 
    labs(title = paste("Height @ Flower"), x = NULL, y = NULL) +
    scale_color_manual(name = "Density", values = c("#0e0325","#0ac9db", "blue", "purple", "red", "magenta")) +
    theme(legend.position = "none",, plot.title = element_text(hjust = 0.5)) +
    geom_vline(aes(xintercept = gen[gen$Trait == "ff_height_std" & gen$Density == 2, "Generations"][1]), linetype = "dashed", color = "#0e0325") +
    geom_vline(aes(xintercept = gen[gen$Trait == "ff_height_std" & gen$Density == 3, "Generations"][1]), linetype = "dashed", color = "#0ac9db") +
    geom_vline(aes(xintercept = gen[gen$Trait == "ff_height_std" & gen$Density == 4, "Generations"][1]), linetype = "dashed", color = "blue") +
    geom_vline(aes(xintercept = gen[gen$Trait == "ff_height_std" & gen$Density == 5, "Generations"][1]), linetype = "dashed", color = "purple") +
    geom_vline(aes(xintercept = gen[gen$Trait == "ff_height_std" & gen$Density == 6, "Generations"][1]), linetype = "dashed", color = "red") +
    geom_vline(aes(xintercept = gen[gen$Trait == "ff_height_std" & gen$Density == 7, "Generations"][1]), linetype = "dashed", color = "magenta")


p3 <- ggplot(results_by_trait_yield[["final_height_std"]], aes(x = Generations, y = Final_Allele_Frequency, group = factor(Density), color = factor(Density))) +
    geom_line() + theme_light() + 
    labs(title = paste("Final Height"), x = NULL, y = NULL) +
    scale_color_manual(name = "Density", values = c("#0e0325","#0ac9db", "blue", "purple", "red", "magenta")) +
    theme(legend.position = "none",, plot.title = element_text(hjust = 0.5)) +
    geom_vline(aes(xintercept = gen[gen$Trait == "final_height_std" & gen$Density == 2, "Generations"][1]), linetype = "dashed", color = "#0e0325") +
    geom_vline(aes(xintercept = gen[gen$Trait == "final_height_std" & gen$Density == 3, "Generations"][1]), linetype = "dashed", color = "#0ac9db") +
    geom_vline(aes(xintercept = gen[gen$Trait == "final_height_std" & gen$Density == 4, "Generations"][1]), linetype = "dashed", color = "blue") +
    geom_vline(aes(xintercept = gen[gen$Trait == "final_height_std" & gen$Density == 5, "Generations"][1]), linetype = "dashed", color = "purple") +
    geom_vline(aes(xintercept = gen[gen$Trait == "final_height_std" & gen$Density == 6, "Generations"][1]), linetype = "dashed", color = "red") +
    geom_vline(aes(xintercept = gen[gen$Trait == "final_height_std" & gen$Density == 7, "Generations"][1]), linetype = "dashed", color = "magenta")


p4 <- ggplot(results_by_trait_yield[["flower_number_1_std"]], aes(x = Generations, y = Final_Allele_Frequency, group = factor(Density), color = factor(Density))) +
    geom_line() + theme_light() + 
    labs(title = paste("Flower Number"), x = NULL, y = "Allele Frequency") +
    scale_color_manual(name = "Density", values = c("#0e0325","#0ac9db", "blue", "purple", "red", "magenta")) +
    theme(legend.position = "none",, plot.title = element_text(hjust = 0.5)) +
    geom_vline(aes(xintercept = gen[gen$Trait == "flower_number_1_std" & gen$Density == 2, "Generations"][1]), linetype = "dashed", color = "#0e0325") +
    geom_vline(aes(xintercept = gen[gen$Trait == "flower_number_1_std" & gen$Density == 3, "Generations"][1]), linetype = "dashed", color = "#0ac9db") +
    geom_vline(aes(xintercept = gen[gen$Trait == "flower_number_1_std" & gen$Density == 4, "Generations"][1]), linetype = "dashed", color = "blue") +
    geom_vline(aes(xintercept = gen[gen$Trait == "flower_number_1_std" & gen$Density == 5, "Generations"][1]), linetype = "dashed", color = "purple") +
    geom_vline(aes(xintercept = gen[gen$Trait == "flower_number_1_std" & gen$Density == 6, "Generations"][1]), linetype = "dashed", color = "red") +
    geom_vline(aes(xintercept = gen[gen$Trait == "flower_number_1_std" & gen$Density == 7, "Generations"][1]), linetype = "dashed", color = "magenta")


p5 <- ggplot(results_by_trait_yield[["leaf_area_std"]], aes(x = Generations, y = Final_Allele_Frequency, group = factor(Density), color = factor(Density))) +
    geom_line() + theme_light() + 
    labs(title = paste("Leaf Area"), x = NULL, y = NULL) +
    scale_color_manual(name = "Density", values = c("#0e0325","#0ac9db", "blue", "purple", "red", "magenta")) +
    theme(legend.position = "none",, plot.title = element_text(hjust = 0.5)) +
    geom_vline(aes(xintercept = gen[gen$Trait == "leaf_area_std" & gen$Density == 2, "Generations"][1]), linetype = "dashed", color = "#0e0325") +
    geom_vline(aes(xintercept = gen[gen$Trait == "leaf_area_std" & gen$Density == 3, "Generations"][1]), linetype = "dashed", color = "#0ac9db") +
    geom_vline(aes(xintercept = gen[gen$Trait == "leaf_area_std" & gen$Density == 4, "Generations"][1]), linetype = "dashed", color = "blue") +
    geom_vline(aes(xintercept = gen[gen$Trait == "leaf_area_std" & gen$Density == 5, "Generations"][1]), linetype = "dashed", color = "purple") +
    geom_vline(aes(xintercept = gen[gen$Trait == "leaf_area_std" & gen$Density == 6, "Generations"][1]), linetype = "dashed", color = "red") +
    geom_vline(aes(xintercept = gen[gen$Trait == "leaf_area_std" & gen$Density == 7, "Generations"][1]), linetype = "dashed", color = "magenta")


p6 <- ggplot(results_by_trait_yield[["Huber_Volume_std"]], aes(x = Generations, y = Final_Allele_Frequency, group = factor(Density), color = factor(Density))) +
    geom_line() + theme_light() + 
    labs(title = paste("Volume"), x = NULL, y = NULL) +
    scale_color_manual(name = "Density", values = c("#0e0325","#0ac9db", "blue", "purple", "red", "magenta")) +
    theme(legend.position = "none",, plot.title = element_text(hjust = 0.5)) +
    geom_vline(aes(xintercept = gen[gen$Trait == "Huber_Volume_std" & gen$Density == 2, "Generations"][1]), linetype = "dashed", color = "#0e0325") +
    geom_vline(aes(xintercept = gen[gen$Trait == "Huber_Volume_std" & gen$Density == 3, "Generations"][1]), linetype = "dashed", color = "#0ac9db") +
    geom_vline(aes(xintercept = gen[gen$Trait == "Huber_Volume_std" & gen$Density == 4, "Generations"][1]), linetype = "dashed", color = "blue") +
    geom_vline(aes(xintercept = gen[gen$Trait == "Huber_Volume_std" & gen$Density == 5, "Generations"][1]), linetype = "dashed", color = "purple") +
    geom_vline(aes(xintercept = gen[gen$Trait == "Huber_Volume_std" & gen$Density == 6, "Generations"][1]), linetype = "dashed", color = "red") +
    geom_vline(aes(xintercept = gen[gen$Trait == "Huber_Volume_std" & gen$Density == 7, "Generations"][1]), linetype = "dashed", color = "magenta")


p7 <- ggplot(results_by_trait_yield[["prop_early_growth_std"]], aes(x = Generations, y = Final_Allele_Frequency, group = factor(Density), color = factor(Density))) +
    geom_line() + theme_light() + 
    labs(title = paste("Early Growth"), x = "Generations", y = "Allele Frequency") +
    scale_color_manual(name = "Density", values = c("#0e0325","#0ac9db", "blue", "purple", "red", "magenta")) +
    theme(legend.position = "none",, plot.title = element_text(hjust = 0.5)) +
    geom_vline(aes(xintercept = gen[gen$Trait == "prop_early_growth_std" & gen$Density == 2, "Generations"][1]), linetype = "dashed", color = "#0e0325") +
    geom_vline(aes(xintercept = gen[gen$Trait == "prop_early_growth_std" & gen$Density == 3, "Generations"][1]), linetype = "dashed", color = "#0ac9db") +
    geom_vline(aes(xintercept = gen[gen$Trait == "prop_early_growth_std" & gen$Density == 4, "Generations"][1]), linetype = "dashed", color = "blue") +
    geom_vline(aes(xintercept = gen[gen$Trait == "prop_early_growth_std" & gen$Density == 5, "Generations"][1]), linetype = "dashed", color = "purple") +
    geom_vline(aes(xintercept = gen[gen$Trait == "prop_early_growth_std" & gen$Density == 6, "Generations"][1]), linetype = "dashed", color = "red") +
    geom_vline(aes(xintercept = gen[gen$Trait == "prop_early_growth_std" & gen$Density == 7, "Generations"][1]), linetype = "dashed", color = "magenta")


p8 <- ggplot(results_by_trait_yield[["total_node_number_std"]], aes(x = Generations, y = Final_Allele_Frequency, group = factor(Density), color = factor(Density))) +
    geom_line() + theme_light() + 
    labs(title = paste("Node Number"), x = "Generations", y = NULL) +
    scale_color_manual(name = "Density", values = c("#0e0325","#0ac9db", "blue", "purple", "red", "magenta")) +
    theme(legend.position = "none",, plot.title = element_text(hjust = 0.5)) +
    geom_vline(aes(xintercept = gen[gen$Trait == "total_node_number_std" & gen$Density == 2, "Generations"][1]), linetype = "dashed", color = "#0e0325") +
    geom_vline(aes(xintercept = gen[gen$Trait == "total_node_number_std" & gen$Density == 3, "Generations"][1]), linetype = "dashed", color = "#0ac9db") +
    geom_vline(aes(xintercept = gen[gen$Trait == "total_node_number_std" & gen$Density == 4, "Generations"][1]), linetype = "dashed", color = "blue") +
    geom_vline(aes(xintercept = gen[gen$Trait == "total_node_number_std" & gen$Density == 5, "Generations"][1]), linetype = "dashed", color = "purple") +
    geom_vline(aes(xintercept = gen[gen$Trait == "total_node_number_std" & gen$Density == 6, "Generations"][1]), linetype = "dashed", color = "red") +
    geom_vline(aes(xintercept = gen[gen$Trait == "total_node_number_std" & gen$Density == 7, "Generations"][1]), linetype = "dashed", color = "magenta")

p9 <- ggplot(results_by_trait_yield[["flower_area_std"]], aes(x = Generations, y = Final_Allele_Frequency, group = factor(Density), color = factor(Density))) +
    geom_line() + theme_light() + 
    labs(title = paste("Flower Area"), x = "Generations", y = NULL) +
    scale_color_manual(name = "Density", values = c("#0e0325","#0ac9db", "blue", "purple", "red", "magenta")) +
    theme(legend.position = c(0.8,0.5), plot.title = element_text(hjust = 0.5), legend.title = element_blank()) +
    geom_vline(aes(xintercept = gen[gen$Trait == "flower_area_std" & gen$Density == 2, "Generations"][1]), linetype = "dashed", color = "#0e0325") +
    geom_vline(aes(xintercept = gen[gen$Trait == "flower_area_std" & gen$Density == 3, "Generations"][1]), linetype = "dashed", color = "#0ac9db") +
    geom_vline(aes(xintercept = gen[gen$Trait == "flower_area_std" & gen$Density == 4, "Generations"][1]), linetype = "dashed", color = "blue") +
    geom_vline(aes(xintercept = gen[gen$Trait == "flower_area_std" & gen$Density == 5, "Generations"][1]), linetype = "dashed", color = "purple") +
    geom_vline(aes(xintercept = gen[gen$Trait == "flower_area_std" & gen$Density == 6, "Generations"][1]), linetype = "dashed", color = "red") +
    geom_vline(aes(xintercept = gen[gen$Trait == "flower_area_std" & gen$Density == 7, "Generations"][1]), linetype = "dashed", color = "magenta")
# Arrange the plots into one graphic
combined_plot <- ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,p9, ncol = 3, nrow = 3)

ggsave(filename = "allele_frequency_yield.pdf", plot = combined_plot, device = cairo_pdf, width = 8, height = 6)
#################################################################
## Compare Fixation Times between Stratified and All Selection ##
#################################################################

# Load in data
all <- read.csv("/Users/brandonhendrickson/Documents/Github_Projects/M_guttatus_PhenotypicSelection/Data/csv/generations.csv")
strat <- read.csv("/Users/brandonhendrickson/Documents/Github_Projects/M_guttatus_PhenotypicSelection/Data/csv/generations_stratified.csv")
yield <- read.csv("/Users/brandonhendrickson/Documents/Github_Projects/M_guttatus_PhenotypicSelection/Data/csv/generations_yield.csv")

trait_list <- c("flower_number_1_std", "D2Flowering_std", "ff_height_std", "total_node_number_std", "prop_early_growth_std", "final_height_std","flower_area_std", "leaf_area_std","Huber_Volume_std")

# Add Type to the two datasets
all$Type <- "All"
strat$Type <- "Stratified"
yield$Type <- "Yield"

# Add Two Empty Columns to All
all$Estimate_F <- NA
all$Estimate_LF <- NA

# Combine the two datasets
combined <- rbind(all, strat, yield)

combined<-unique(combined)

# Save out combined data
write.csv(combined, "/Users/brandonhendrickson/Documents/Github_Projects/M_guttatus_PhenotypicSelection/Data/csv/generations_combined.csv", row.names = FALSE)

# Run a simple anova to compare the two datasets
anova_df <- data.frame(Predictors = character(), Df = numeric(), Sum_Sq = numeric(), F_value = numeric(), Pr = numeric(), Response = character(), stringsAsFactors = FALSE)
for(trait in trait_list){
    anova <- anova(lm(Generations ~ Type + Density + Type * Density, data = combined[combined$Trait == trait, ]))
    anova$Predictors <- rownames(anova)
    anova <- anova[,c(6,1,2,4,5)]
    anova$Response <- ifelse(anova$Predictors == "Type", trait, NA)
    anova_df <- rbind(anova_df,anova)
}

anova_df <- anova_df %>%
    mutate(Response = case_when(
        Response == trait_list[2] ~ "Flowering Date",
        Response == trait_list[3] ~ "Height at Flowering",
        Response == trait_list[6] ~ "Final Height",
        Response == trait_list[1] ~ "# Flowers",
        Response == trait_list[8] ~ "Leaf Area",
        Response == trait_list[5] ~ "Early Growth",
        Response == trait_list[4] ~ "Node Number",
        Response == trait_list[7] ~ "Flower Area",
        TRUE ~ Response
    ))

anova_df <- anova_df[, c(6, 1, 2, 3, 4, 5)]

# Create a dataframe of the anova results
tab_df(anova_df,file = "/Users/brandonhendrickson/Documents/Github_Projects/M_guttatus_PhenotypicSelection/Results/anova_results_SIM.doc")

p <- ggplot(combined, aes(x = Trait, y = Generations, fill = Type)) +
    geom_boxplot() +
    theme_light(base_size = 12, base_family = "Times New Roman") +
    labs(x = NULL, y = "Generations", title = NULL) +
    scale_x_discrete(labels = c("Flowering\nDate", "Height at\nFlowering", "Final\nHeight", "# Flowers", "Leaf\nArea", "Early\nGrowth", "Node\nNumber", "Flower Area","Volume")) +
    theme(legend.position = c(0.85, 0.8), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), legend.text = element_text(size = 12)) +
    scale_fill_manual(values = c("#7171ef", "#c477f3", "#f265a4"), labels = c("Average Selection", "Proportional Weighting", "Yield Weighting"))
# Save the plot#5555f1
ggsave("/Users/brandonhendrickson/Documents/Github_Projects/M_guttatus_PhenotypicSelection/Results/fixation_comparison.pdf", p, width = 10, height = 6, units = "in", dpi = 300, device = cairo_pdf)