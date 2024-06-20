# Load the ineq package
library(dplyr)
library(ineq)
library(purrr)
library(splines)

# Load Data
data <- read.csv("/Users/brandonhendrickson/Documents/Github_Projects/M_guttatus_TraitInequality/Data/csv/mortality_raw.csv")

# Create trait list 
trait_list <- list("yield","flower_number_1","D2Flowering","flower_node","pollen_avg","ff_height","ff_node","total_node_number","prop_early_growth","lowervolume","midrvolume","upvolume","Huber_Volume","Smalian_Volume","Newton_Volume","final_height","veg_weight","repro_weight","total_weight","leaf_area")

# Caculate the Gini index for the current trait
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

# Mered Dataframe
gini_data <- merge(data, gini_results, by = c("Tray", "POS"))

for (trait in trait_list) {
    gini_data <- gini_data %>%
        group_by(POS, Tray) %>%
        arrange(!!sym(trait)) %>%
        mutate(!!paste("cumulative_yield", trait, sep = "_") := cumsum(yield)) %>%
        relocate(!!paste("cumulative_yield", trait, sep = "_"), .before = 4)
    gini_data <- gini_data %>%
        group_by(POS, Tray) %>%
        mutate(!!paste("delta_yield", trait, sep = "_") := c(0, diff(!!sym(paste("cumulative_yield", trait, sep = "_")))) / max(!!sym(paste("cumulative_yield", trait, sep = "_")))) %>%
        relocate(!!paste("delta_yield", trait, sep = "_"), .before = 4)
    gini_data <- gini_data %>%
        group_by(POS, Tray) %>%
        mutate(!!paste("yield_left", trait, sep = "_") := max(!!sym(paste("cumulative_yield", trait, sep = "_"))) - cumsum(yield)) %>%
        relocate(!!paste("yield_left", trait, sep = "_"), .before = 4)
}


# Create a Column that store the cumulative proportion (ie. simulated mortality) of the row number for each POS and Tray
gini_data <- gini_data %>%
    group_by(POS, Tray) %>%
    mutate(cumulative_proportion = row_number() / n()) %>%
    relocate(cumulative_proportion, .before = 4)

trait_list <- list("yield_left_yield","yield_left_flower_number_1","yield_left_D2Flowering","yield_left_flower_node","yield_left_pollen_avg","yield_left_ff_height","yield_left_ff_node","yield_left_total_node_number","yield_left_prop_early_growth","yield_left_lowervolume","yield_left_midrvolume","yield_left_upvolume","yield_left_Huber_Volume","yield_left_Smalian_Volume","yield_left_Newton_Volume","yield_left_final_height","yield_left_veg_weight","yield_left_repro_weight","yield_left_total_weight","yield_left_leaf_area")

# Visualize how delta yield changes with cumulative proportion

# Calculate the median of Gini_Index_flower_number_1

median_gini_index <- median(gini_data$Gini_Index_final_height, na.rm = TRUE)

p_fh <- gini_data %>%
    filter(Final_Density == 5) %>%
    mutate(Gini_Category = ifelse(Gini_Index_final_height > median_gini_index, "Above Median", "Below Median")) %>%
    ggplot(aes(x = cumulative_proportion, y = yield_left_final_height, linetype = Gini_Category)) +
    geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = FALSE, color = "black") +
    labs(
        title = NULL,
        x = "Cumulative Proportion",
        y = "Yield: Final Height"
    ) +
    theme_minimal() +
    theme(legend.position = "none") 
    
median_gini_index <- median(gini_data$Gini_Index_veg_weight, na.rm = TRUE)

p_vw <- gini_data %>%
    filter(Final_Density == 5) %>%
    mutate(Gini_Category = ifelse(Gini_Index_veg_weight > median_gini_index, "Above Median", "Below Median")) %>%
    ggplot(aes(x = cumulative_proportion, y = yield_left_veg_weight, linetype = Gini_Category)) +
    geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = FALSE, color = "black") +
    labs(
        title = NULL,
        x = "Cumulative Proportion",
        y = "Yield: Biomass"
    ) +
    theme_minimal() +
    theme(legend.position = "none") 

ggarrange(p_fh, p_vw, ncol = 1, nrow = 2)

