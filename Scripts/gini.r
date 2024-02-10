# Install the necessary packages if not already installed
if (!require(dplyr)) install.packages('dplyr')
if (!require(ineq)) install.packages('ineq')

# Load the ineq package
library(dplyr)
library(ineq)

# Load data
data <- read.csv("/Users/brandonhendrickson/Documents/Github_Projects/M_guttatus_TraitInequality/Data/csv/raw_data.csv")

trait_list <- list("yield","flower_number_1","D2Flowering","flower_node","pollen_avg","ff_height","ff_node","total_node_number","prop_early_growth","lowervolume","midrvolume","upvolume","Huber_Volume","Smalian_Volume","Newton_Volume","final_height","veg_weight","repro_weight","total_weight","leaf_area")

# Initialize an empty data frame to store the results
gini_results <- data.frame()

# Loop over each trait in the trait list
for (trait in trait_list) {
  # Skip if the trait is not a column in the data
  if (!trait %in% names(data)) next
  
  # Calculate the Gini index for the current trait
  gini_index <- data %>%
    group_by(Tray, POS) %>%
    summarise(!!paste0("Gini_Index_", trait) := Gini(get(trait)))
  
  # If gini_results is empty, copy gini_index
  if (nrow(gini_results) == 0) {
    gini_results <- gini_index
  } else {
    # Otherwise, join the new Gini index to the existing results
    gini_results <- full_join(gini_results, gini_index, by = c("Tray", "POS"))
  }
}

# Print the Gini index for each trait
print(gini_results)

#Subset data to only include the columns we want
data_smalls <- data[, c(1, 2, 3, 5, 6, 7, 8)]

# Make Gini data frame
gini_data <- merge(data_smalls, gini_results, by = c("Tray", "POS"))

# Write the Gini data frame to a CSV file
write.csv(gini_data, file = "/Users/brandonhendrickson/Documents/Github_Projects/M_guttatus_TraitInequality/Data/csv/gini_data.csv", row.names = FALSE)
