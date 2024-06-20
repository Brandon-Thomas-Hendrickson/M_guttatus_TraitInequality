# Load the necessary libraries
library(dplyr)

#####################
## Standardize All ##
#####################

# Create the trait List
trait_list <- c("flower_number_1", "D2Flowering", "ff_height", "total_node_number", "prop_early_growth", "final_height","flower_area", "leaf_area","Huber_Volume")

# Load modelframe
data <- read.csv("/Users/brandonhendrickson/Documents/Github_Projects/M_guttatus_PhenotypicSelection/Data/csv/raw_data.csv")

# Change Final Density to Factor
data$Final_Density <- as.factor(data$Final_Density)

# Modify the function to standardize a single trait
standardize <- function(data, trait) {
    data <- data %>%
        group_by(Final_Density) %>%
        mutate(!!paste0(trait, "_std") := ( .data[[trait]] - mean(.data[[trait]], na.rm = TRUE)) / sd(.data[[trait]], na.rm = TRUE))
    return(data)
}

# Apply the function to all elements in the trait list
for (trait in trait_list) {
    data <- standardize(data, trait)
}

# Save the data
write.csv(data, "/Users/brandonhendrickson/Documents/Github_Projects/M_guttatus_PhenotypicSelection/Data/csv/standardized_data.csv", row.names = FALSE)

####################################
## Standardize Fittest Plant Only ##
####################################

# Load modelframe
data <- read.csv("/Users/brandonhendrickson/Documents/Github_Projects/M_guttatus_PhenotypicSelection/Data/csv/raw_data.csv")

# Create the trait List
trait_list <- c("flower_number_1", "D2Flowering", "ff_height", "total_node_number", "prop_early_growth", "final_height","flower_area", "leaf_area","Huber_Volume")

# Change Final Density to Factor
data$Final_Density <- as.factor(data$Final_Density)

data <- data[data$F_LF == "F",]

# Modify the function to standardize a single trait
standardize <- function(data, trait) {
    data <- data %>%
        group_by(Final_Density) %>%
        mutate(!!paste0(trait, "_std") := ( .data[[trait]] - mean(.data[[trait]], na.rm = TRUE)) / sd(.data[[trait]], na.rm = TRUE))
    return(data)
}

# Apply the function to all elements in the trait list
for (trait in trait_list) {
    data <- standardize(data, trait)
}

# Save data_F
write.csv(data, "/Users/brandonhendrickson/Documents/Github_Projects/M_guttatus_PhenotypicSelection/Data/csv/standardized_data_F.csv", row.names = FALSE)

######################################
## Standardize Least Fit Plant Only ##
######################################

# Load modelframe
data <- read.csv("/Users/brandonhendrickson/Documents/Github_Projects/M_guttatus_PhenotypicSelection/Data/csv/raw_data.csv")

# Create the trait List
trait_list <- c("flower_number_1", "D2Flowering", "ff_height", "total_node_number", "prop_early_growth", "final_height","flower_area", "leaf_area","Huber_Volume")

# Change Final Density to Factor
data$Final_Density <- as.factor(data$Final_Density)

data <- data[data$F_LF == "LF",]

# Modify the function to standardize a single trait
standardize <- function(data, trait) {
    data <- data %>%
        group_by(Final_Density) %>%
        mutate(!!paste0(trait, "_std") := ( .data[[trait]] - mean(.data[[trait]], na.rm = TRUE)) / sd(.data[[trait]], na.rm = TRUE))
    return(data)
}

# Apply the function to all elements in the trait list
for (trait in trait_list) {
    data <- standardize(data, trait)
}

# Save data_F
write.csv(data, "/Users/brandonhendrickson/Documents/Github_Projects/M_guttatus_PhenotypicSelection/Data/csv/standardized_data_LF.csv", row.names = FALSE)
