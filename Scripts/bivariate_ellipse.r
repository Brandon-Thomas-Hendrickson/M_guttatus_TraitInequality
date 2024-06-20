# Install the package
install.packages(c("ggplot2","geepack", "readr", "sjPlot", "MASS", "stats","emmeans","wesanderson","ggpubr","purrr","ellipse"))

# Load the package
library(readr)
library(sjPlot)
library(MASS)
library(stats)
library(emmeans)
library(wesanderson)
library(ggpubr)
library(ggplot2)
library(purrr)
library(ellipse)

##############################################################
## Phenotypic Selection Analysis of Traits by Final Density ##
##############################################################

# Load dataframe
data <- read.csv("/Users/brandonhendrickson/Documents/Github_Projects/M_guttatus_PhenotypicSelection/Data/csv/standardized_data.csv")

# Create the trait List
trait_list <- c("flower_number_1_std", "D2Flowering_std", "pollen_avg_std", "ff_height_std", "total_node_number_std", "prop_early_growth_std", "final_height_std", "veg_weight_std", "leaf_area_std")

# Loop through the traits in the trait list
for (i in seq_along(trait_list)) {
    trait <- trait_list[i]
    
    # Filter the data for the current trait, "yield", and "F_LF" and remove rows with missing values
    data_clean <- na.omit(data[, c(trait, "yield", "F_LF")])
    
    # Group the data by "F_LF"
    grouped_data <- group_by(data_clean, F_LF)
    
    # Get the unique values of "F_LF"
    F_LF_values <- unique(data_clean$F_LF)
    
    # Initialize an empty list to store the ellipse data frames for each "F_LF" value
    ellipse_list[[i]] <- list()
    
    # Loop over the "F_LF" values
    for (j in seq_along(F_LF_values)) {
        # Filter the data for the current "F_LF" value
        group_data <- filter(grouped_data, F_LF == F_LF_values[j])
        group_data <- group_data[,c(1,2)]
        # Check that there are at least two observations in the group
        if (nrow(group_data) < 2) {
            next
        }
        
        # Calculate the mean vector and covariance matrix
        mu <- colMeans(group_data)
        Sigma <- cov(group_data)
        
        # Generate the ellipse with confidence level 0.50
        el <- ellipse(Sigma, centre = mu, level = 0.50)
        
        # Convert the ellipse to a data frame
        el_df <- as.data.frame(el)
        
        # Store the ellipse data frame in the list
        ellipse_list[[i]][[j]] <- el_df
    }
}


# Plot the ellipses for each trait
for (i in seq_along(trait_list)) {
    trait <- trait_list[i]
    
    # Create a new plot
    plot <- ggplot() +
        geom_polygon(
            data = ellipse_list[[i]][[1]],
            aes(x = ellipse_list[[i]][[1]][[1]], y = ellipse_list[[i]][[1]][[2]]),
            color = "red",
            fill = "red",
            alpha = 0.3,
            linetype = 1
        ) +
        geom_polygon(
            data = ellipse_list[[i]][[2]],
            aes(x = ellipse_list[[i]][[2]][[1]], y = ellipse_list[[i]][[2]][[2]]),
            color = "black",
            fill = "black",
            alpha = 0.3,
            linetype = 1
        ) +
        geom_polygon(
            data = ellipse_list[[i]][[3]],
            aes(x = ellipse_list[[i]][[3]][[1]], y = ellipse_list[[i]][[3]][[2]]),
            color = "blue",
            fill = "blue",
            alpha = 0.3,
            linetype = 1
        ) +
        labs(x = trait, y = "yield", title = NULL) +
        theme_light() +
        theme(
            text = element_text(family = "Times New Roman", size = 12),
            axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12)
        )
        # Save the plot as a PNG file
    ggsave(filename = paste0(trait, ".png"), plot = plot, width = 6, height = 6, units = "in", dpi = 300)
    }

# Loop through the traits in the trait list
for (i in seq_along(trait_list)) {
    trait <- trait_list[i]

    # Filter the data for the current trait, "yield", "F_LF" and "Final_Density" and remove rows with missing values
    data_clean <- na.omit(data[, c(trait, "yield", "F_LF", "Final_Density")])

    # Get the unique values of "F_LF" and "Final_Density"
    F_LF_values <- unique(data_clean$F_LF)
    Final_Density_values <- unique(data_clean$Final_Density)

    # Initialize an empty list to store the ellipse data frames for each "F_LF" value
    ellipse_list[[i]] <- vector("list", length(F_LF_values))

    # Loop over the "F_LF" values
    for (j in seq_along(F_LF_values)) {
        # Initialize an empty list to store the ellipse data frames for each "Final_Density" value
        ellipse_list[[i]][[j]] <- vector("list", length(Final_Density_values))

        # Loop over the "Final_Density" values
        for (k in seq_along(Final_Density_values)) {
            # Skip if "Final_Density" value is not above two
            if (Final_Density_values[k] <= 2) {
                next
            }

            # Filter the data for the current "F_LF" value and "Final_Density" value
            group_data <- filter(data_clean, F_LF == F_LF_values[j] & Final_Density == Final_Density_values[k])
            group_data <- group_data[, c(1, 2)]

            # Check that there are at least two observations in the group
            if (nrow(group_data) < 2) {
                next
            }

            # Calculate the mean vector and covariance matrix
            mu <- colMeans(group_data)
            Sigma <- cov(group_data)

            # Generate the ellipse with confidence level 0.50
            el <- ellipse(Sigma, centre = mu, level = 0.50)

            # Convert the ellipse to a data frame
            el_df <- as.data.frame(el)

            # Store the ellipse data frame in the list
            ellipse_list[[i]][[j]][[k]] <- el_df
        }
    }
}

# Define a list of labels for the traits
trait_labels <- list("# Flowers", "Flowering Date", "Pollen Number", "Height @ Flower", "Node Number", "Early Growth", "Final Height", "Veg Weight", "Leaf Area")

# Plot the ellipses for each trait
for (i in seq_along(trait_list)) {
    trait <- trait_list[i]
    
    # Create a new plot
    plot <- ggplot() +
        geom_polygon(
            data = ellipse_list[[i]][[1]],
            aes(x = ellipse_list[[i]][[1]][[1]], y = ellipse_list[[i]][[1]][[2]]),
            color = "red",
            fill = "red",
            alpha = 0.3,
            linetype = 1
        ) +
        geom_polygon(
            data = ellipse_list[[i]][[2]],
            aes(x = ellipse_list[[i]][[2]][[1]], y = ellipse_list[[i]][[2]][[2]]),
            color = "black",
            fill = "black",
            alpha = 0.3,
            linetype = 1
        ) +
        geom_polygon(
            data = ellipse_list[[i]][[3]],
            aes(x = ellipse_list[[i]][[3]][[1]], y = ellipse_list[[i]][[3]][[2]]),
            color = "blue",
            fill = "blue",
            alpha = 0.3,
            linetype = 1
        ) +
        labs(x = trait_labels[[i]], y = "yield", title = NULL) +
        theme_light() +
        theme(
            text = element_text(family = "Times New Roman", size = 12),
            axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12)
        )
        # Save the plot as a PNG file
    ggsave(filename = paste0(trait, ".png"), plot = plot, width = 6, height = 6, units = "in", dpi = 300)
    }

# Define a list of labels for the traits
trait_labels <- list("# Flowers", "Flowering Date", "Pollen Number", "Height @ Flower", "Node Number", "Early Growth", "Final Height", "Veg Weight", "Leaf Area")

# Plot the ellipses for each trait
for (i in seq_along(trait_list)) {
    trait <- trait_list[i]
    for(j in seq_along(Final_Density_values)){
    # Create a new plot
    plot <- ggplot() +
        geom_polygon(
            data = ellipse_list[[i]][[1]][[j]],
            aes(x = ellipse_list[[i]][[1]][[j]][[1]], y = ellipse_list[[i]][[1]][[j]][[2]]),
            color = "red",
            fill = "red",
            alpha = 0.3,
            linetype = 1
        ) +
        geom_polygon(
            data = ellipse_list[[i]][[2]][[j]],
            aes(x = ellipse_list[[i]][[2]][[j]][[1]], y = ellipse_list[[i]][[2]][[j]][[2]]),
            color = "black",
            fill = "black",
            alpha = 0.3,
            linetype = 1
        ) +
        geom_polygon(
            data = ellipse_list[[i]][[3]][[j]],
            aes(x = ellipse_list[[i]][[3]][[j]][[1]], y = ellipse_list[[i]][[3]][[j]][[2]]),
            color = "blue",
            fill = "blue",
            alpha = 0.3,
            linetype = 1
        ) +
        labs(x = trait_labels[[i]], y = "yield", title = NULL) +
        theme_light() +
        theme(
            text = element_text(family = "Times New Roman", size = 12),
            axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12)
        )
    # Save the plot as a PNG file
    ggsave(filename = paste0(trait, j,".png"), plot = plot, width = 6, height = 6, units = "in", dpi = 300)
}
}