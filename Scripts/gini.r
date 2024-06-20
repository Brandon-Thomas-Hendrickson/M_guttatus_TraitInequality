# Load the ineq package
library(dplyr)
library(ineq)
library(ggplot2)
library(ggpubr)

############################################
#Calculate GINI coefficients for each Trait#
############################################

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

##########################################
#Create Lorens Curves for Each Gini Trait#
##########################################

# For each trait, make a column that is the cumulative sum of each group by POS and Tray
for (trait in trait_list) {
  data <- data %>%
    arrange(POS, Tray, !!as.name(trait)) %>%
    group_by(POS, Tray) %>%
    mutate(!!paste0(trait, "_cumsum") := cumsum(!!as.name(trait))) %>%
    mutate(!!paste0("cumulative_proportion_of_", trait) := !!as.name(paste0(trait, "_cumsum")) / max(!!as.name(paste0(trait, "_cumsum"))))
}

# Create a Column that store the cumulative proportion (ie. simulated mortality) of the row number for each POS and Tray
for (trait in trait_list) {
  data<- data %>%
    arrange(POS, Tray, !!as.name(trait)) %>%
    group_by(POS, Tray) %>%
    mutate(!!paste0("cumulative_proportion_by_",trait) := row_number() / n())
}

# Create an empty data frame to store the results
results <- data.frame()

# For each trait, calculate the mean and se of trait_cumsum grouped by Final_Density and cumulative_proportion
for (trait in trait_list) {
  trait_cumsum <- paste0("cumulative_proportion_of_", trait)
  cumulative_proportion_trait <- paste0("cumulative_proportion_by_", trait)

  result <- data %>%
    group_by(Final_Density, !!as.name(cumulative_proportion_trait)) %>%
    summarise(
      mean = mean(!!as.name(trait_cumsum), na.rm = TRUE),
      se = sd(!!as.name(trait_cumsum), na.rm = TRUE) / sqrt(n()),
      .groups = "drop"
    )

  # Add the trait name to the result
  result <- result[complete.cases(result), ]
  result$trait <- trait
  colnames(result)[2] <- "proportion"

  # Bind the result to the results data frame
  results <- rbind(results, result)
  # Get unique Final_Density values
final_density_values <- unique(data$Final_Density)

# Create a data frame with 0s for each Final_Density
zero_rows <- data.frame(
  Final_Density = final_density_values,
  proportion = 0,
  mean = 0,
  se = 0,
  trait = trait
)

# Bind the zero_rows data frame to the results data frame
results <- rbind(results, zero_rows)
}

results <- results[!is.na(results$Final_Density), ]

results <- as.data.frame(results)

results$mean[results$proportion == 0.5 & results$Final_Density == 1] <- 0.5
results$mean[results$proportion == 1 & results$Final_Density == 1] <- 1

# Create plots for each Gini Trait
p1 <- ggplot(results[results$trait == "yield",], aes(x = proportion,y=mean,group=as.factor(Final_Density), color = as.factor(Final_Density))) + geom_line() + labs(x = paste("Cumulative proportion of Population"),y = paste("Cumulative sum of Yield")) + theme_light() + theme(text = element_text(size = 12, color = "black"), legend.position = "none") + scale_color_manual(values = c("black","red","purple","pink","blue","#29c9ff","orange"))

p2 <- ggplot(results[results$trait == "flower_number_1",], aes(x = proportion,y=mean,group=as.factor(Final_Density), color = as.factor(Final_Density))) + geom_line() + labs(x = paste("Cumulative proportion of Population"),y = paste("Cumulative sum of Flower Number 1")) + theme_light() + theme(text = element_text(size = 12, color = "black"), legend.position = "none") + scale_color_manual(values = c("black","red","purple","pink","blue","#29c9ff","orange"))

p3 <- ggplot(results[results$trait == "D2Flowering",], aes(x = proportion,y=mean,group=as.factor(Final_Density), color = as.factor(Final_Density))) + geom_line() + labs(x = paste("Cumulative proportion of Population"),y = paste("Cumulative sum of D2Flowering")) + theme_light() + theme(text = element_text(size = 12, color = "black"), legend.position = "none") + scale_color_manual(values = c("black","red","purple","pink","blue","#29c9ff","orange"))

p4 <- ggplot(results[results$trait == "flower_node",], aes(x = proportion,y=mean,group=as.factor(Final_Density), color = as.factor(Final_Density))) + geom_line() + labs(x = paste("Cumulative proportion of Population"),y = paste("Cumulative sum of Flower Node")) + theme_light() + theme(text = element_text(size = 12, color = "black"), legend.position = "none") + scale_color_manual(values = c("black","red","purple","pink","blue","#29c9ff","orange"))

p5 <- ggplot(results[results$trait == "pollen_avg", ], aes(x = proportion, y = mean, group = as.factor(Final_Density), color = as.factor(Final_Density))) +
  geom_line() +
  labs(x = paste("Cumulative proportion of Population"), y = paste("Cumulative sum of Pollen Avg")) +
  theme_light() +
  theme(text = element_text(size = 12, color = "black"), legend.position = "none") +
  scale_color_manual(values = c("black", "red", "purple", "pink", "blue", "#29c9ff", "orange"))

p6 <- ggplot(results[results$trait == "ff_height", ], aes(x = proportion, y = mean, group = as.factor(Final_Density), color = as.factor(Final_Density))) + geom_line() + labs(x = paste("Cumulative proportion of Population"), y = paste("Cumulative sum of FF Height")) + theme_light() + theme(text = element_text(size = 12, color = "black"), legend.position = "none") + scale_color_manual(values = c("black", "red", "purple", "pink", "blue", "#29c9ff", "orange"))

p7 <- ggplot(results[results$trait == "ff_node", ], aes(x = proportion, y = mean, group = as.factor(Final_Density), color = as.factor(Final_Density))) +
  geom_line() +
  labs(x = paste("Cumulative proportion of Population"), y = paste("Cumulative sum of FF Node")) +
  theme_light() +
  theme(text = element_text(size = 12, color = "black"), legend.position = "none") +
  scale_color_manual(values = c("black", "red", "purple", "pink", "blue", "#29c9ff", "orange"))

p8 <- ggplot(results[results$trait == "total_node_number", ], aes(x = proportion, y = mean, group = as.factor(Final_Density), color = as.factor(Final_Density))) + geom_line() + labs(x = paste("Cumulative proportion of Population"), y = paste("Cumulative sum of Total Node Number")) + theme_light() + theme(text = element_text(size = 12, color = "black"), legend.position = "none") + scale_color_manual(values = c("black", "red", "purple", "pink", "blue", "#29c9ff", "orange"))

p9 <- ggplot(results[results$trait == "prop_early_growth", ], aes(x = proportion, y = mean, group = as.factor(Final_Density), color = as.factor(Final_Density))) +
  geom_line() +
  labs(x = paste("Cumulative proportion of Population"), y = paste("Cumulative sum of Prop Early Growth")) +
  theme_light() +
  theme(text = element_text(size = 12, color = "black"), legend.position = "none") +
  scale_color_manual(values = c("black", "red", "purple", "pink", "blue", "#29c9ff", "orange"))

p10 <- ggplot(results[results$trait == "lowervolume", ], aes(x = proportion, y = mean, group = as.factor(Final_Density), color = as.factor(Final_Density))) + geom_line() + labs(x = paste("Cumulative proportion of Population"), y = paste("Cumulative sum of Lowervolume")) + theme_light() + theme(text = element_text(size = 12, color = "black"), legend.position = "none") + scale_color_manual(values = c("black", "red", "purple", "pink", "blue", "#29c9ff", "orange"))

p11 <- ggplot(results[results$trait == "midrvolume", ], aes(x = proportion, y = mean, group = as.factor(Final_Density), color = as.factor(Final_Density))) +
  geom_line() +
  labs(x = paste("Cumulative proportion of Population"), y = paste("Cumulative sum of Midrvolume")) +
  theme_light() +
  theme(text = element_text(size = 12, color = "black"), legend.position = "none") +
  scale_color_manual(values = c("black", "red", "purple", "pink", "blue", "#29c9ff", "orange"))

p12 <- ggplot(results[results$trait == "upvolume", ], aes(x = proportion, y = mean, group = as.factor(Final_Density), color = as.factor(Final_Density))) + geom_line() + labs(x = paste("Cumulative proportion of Population"), y = paste("Cumulative sum of Upvolume")) + theme_light() + theme(text = element_text(size = 12, color = "black"), legend.position = "none") + scale_color_manual(values = c("black", "red", "purple", "pink", "blue", "#29c9ff", "orange"))

p13 <- ggplot(results[results$trait == "Huber_Volume", ], aes(x = proportion, y = mean, group = as.factor(Final_Density), color = as.factor(Final_Density))) +
  geom_line() +
  labs(x = paste("Cumulative proportion of Population"), y = paste("Cumulative sum of Huber Volume")) +
  theme_light() +
  theme(text = element_text(size = 12, color = "black"), legend.position = "none") +
  scale_color_manual(values = c("black", "red", "purple", "pink", "blue", "#29c9ff", "orange"))

p14 <- ggplot(results[results$trait == "Smalian_Volume", ], aes(x = proportion, y = mean, group = as.factor(Final_Density), color = as.factor(Final_Density))) + geom_line() + labs(x = paste("Cumulative proportion of Population"), y = paste("Cumulative sum of Smalian Volume")) + theme_light() + theme(text = element_text(size = 12, color = "black"), legend.position = "none") + scale_color_manual(values = c("black", "red", "purple", "pink", "blue", "#29c9ff", "orange"))

p15 <- ggplot(results[results$trait == "Newton_Volume", ], aes(x = proportion, y = mean, group = as.factor(Final_Density), color = as.factor(Final_Density))) +
  geom_line() +
  labs(x = paste("Cumulative proportion of Population"), y = paste("Cumulative sum of Newton Volume")) +
  theme_light() +
  theme(text = element_text(size = 12, color = "black"), legend.position = "none") +
  scale_color_manual(values = c("black", "red", "purple", "pink", "blue", "#29c9ff", "orange"))

p16 <- ggplot(results[results$trait == "final_height", ], aes(x = proportion, y = mean, group = as.factor(Final_Density), color = as.factor(Final_Density))) + geom_line() + labs(x = paste("Cumulative proportion of Population"), y = paste("Cumulative sum of Final Height")) + theme_light() + theme(text = element_text(size = 12, color = "black"), legend.position = "none") + scale_color_manual(values = c("black", "red", "purple", "pink", "blue", "#29c9ff", "orange"))

p17 <- ggplot(results[results$trait == "veg_weight", ], aes(x = proportion, y = mean, group = as.factor(Final_Density), color = as.factor(Final_Density))) +
  geom_line() +
  labs(x = paste("Cumulative proportion of Population"), y = paste("Cumulative sum of Veg Weight")) +
  theme_light() +
  theme(text = element_text(size = 12, color = "black"), legend.position = "none") +
  scale_color_manual(values = c("black", "red", "purple", "pink", "blue", "#29c9ff", "orange"))

p18 <- ggplot(results[results$trait == "repro_weight", ], aes(x = proportion, y = mean, group = as.factor(Final_Density), color = as.factor(Final_Density))) + geom_line() + labs(x = paste("Cumulative proportion of Population"), y = paste("Cumulative sum of Repro Weight")) + theme_light() + theme(text = element_text(size = 12, color = "black"), legend.position = "none") + scale_color_manual(values = c("black", "red", "purple", "pink", "blue", "#29c9ff", "orange"))

p19 <- ggplot(results[results$trait == "total_weight", ], aes(x = proportion, y = mean, group = as.factor(Final_Density), color = as.factor(Final_Density))) +
  geom_line() +
  labs(x = paste("Cumulative proportion of Population"), y = paste("Cumulative sum of Total Weight")) +
  theme_light() +
  theme(text = element_text(size = 12, color = "black"), legend.position = "none") +
  scale_color_manual(values = c("black", "red", "purple", "pink", "blue", "#29c9ff", "orange"))

p20 <- ggplot(results[results$trait == "leaf_area", ], aes(x = proportion, y = mean, group = as.factor(Final_Density), color = as.factor(Final_Density))) + geom_line() + labs(x = paste("Cumulative proportion of Population"), y = paste("Cumulative sum of Leaf Area")) + theme_light() + theme(text = element_text(size = 12, color = "black"), legend.position = "none") + scale_color_manual(values = c("black", "red", "purple", "pink", "blue", "#29c9ff", "orange"))

ggarrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, ncol = 4, nrow = 5)

######################################################
#Gini Correlation with Final Density and other traits#
######################################################

# Load Data
data <- read.csv("/Users/brandonhendrickson/Documents/Github_Projects/M_guttatus_TraitInequality/Data/csv/gini_data.csv")

# Create List of Gini Traits
trait_list <- list("Gini_Index_yield","Gini_Index_flower_number_1","Gini_Index_D2Flowering","Gini_Index_flower_node","Gini_Index_pollen_avg","Gini_Index_ff_height","Gini_Index_ff_node","Gini_Index_total_node_number","Gini_Index_prop_early_growth","Gini_Index_lowervolume","Gini_Index_midrvolume","Gini_Index_upvolume","Gini_Index_Huber_Volume","Gini_Index_Smalian_Volume","Gini_Index_Newton_Volume","Gini_Index_final_height","Gini_Index_veg_weight","Gini_Index_repro_weight","Gini_Index_total_weight","Gini_Index_leaf_area")

# Run a series of Linear Regression Models for each Gini trait and Final_Density

# Loop over the traits
# Initialize an empty data frame to store the results
results <- data.frame()

# Loop over the traits
for (trait in trait_list) {
  # Fit a linear model
  model <- lm(as.formula(paste(trait, "~ Final_Density")), data = gini_data[!(gini_data[[trait]] == 0), ])
  
  # Extract the values
  model_summary <- summary(model)
  estimate <- coef(model_summary)["Final_Density", "Estimate"]
  std_error <- coef(model_summary)["Final_Density", "Std. Error"]
  p_value <- coef(model_summary)["Final_Density", "Pr(>|t|)"]
  f_statistic <- model_summary$fstatistic[1]
  r_squared <- model_summary$r.squared
  
  # Add the results to the data frame
  results <- rbind(results, data.frame(Trait = trait, Estimate = estimate, Std_Error = std_error, P_Value = p_value, F_Statistic = f_statistic, R_Squared = r_squared))
}

# Print the results
write.csv(results, file = "/Users/brandonhendrickson/Documents/Github_Projects/M_guttatus_TraitInequality/Data/csv/gini_regression.csv", row.names = FALSE)

# Run Spearmans correlation on all traits
# Load Data
data <- read.csv("/Users/brandonhendrickson/Documents/Github_Projects/M_guttatus_TraitInequality/Data/csv/gini_data.csv")

# Perform a spearman correlation on the Gini Index
p_df <- data.frame(Comparison = character(), P_value = numeric(), Estimate = numeric(), stringsAsFactors = FALSE)
for(trait in trait_list){
    for(i in unique(trait_list)){
        p <- cor.test(data[,trait], data[,i], use = "na.or.complete", method = "kendall")
        comp <- i
        name = paste(trait, comp, sep = "_")
        p_val <- p$p.value
        es <- p$estimate
        temp_df <- data.frame(Comparison = name, P_value = p_val, Estimate = es, stringsAsFactors = FALSE)
        p_df <- rbind(p_df, temp_df)
    }
}

# Write the correlation matrix to a CSV file
write.csv(p_df, file = "/Users/brandonhendrickson/Documents/Github_Projects/M_guttatus_TraitInequality/Data/csv/gini_correlation.csv", row.names = FALSE)

######################################################
#Perform Morality Simulation Using Height and Biomass#
######################################################

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

# Calculate the median of Gini_Index_final_height

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

# Create a dataframe with a column that marks each plot and tray with a 0/1 for those with a Gini Index above the median and a Gini index below the median
gini_data <- gini_data %>%
  mutate(Gini_Category_Height = ifelse(Gini_Index_final_height > median_gini_index, "Above Median", "Below Median"))

# Create a dataframe with a column that marks each plot and tray with a 0/1 for those with a Gini Index above the median and a Gini index below the median
gini_data <- gini_data %>%
  mutate(Gini_Category_Biomass = ifelse(Gini_Index_veg_weight > median_gini_index, "Above Median", "Below Median"))

#Extract only the plot, tray, Gini_Category_Height, Gini_Category_Biomass
gini_smalls <- gini_data[c("plot","Tray","Gini_Category_Height","Gini_Category_Biomass")]

#Write out the dataframe 
write.csv(gini_smalls, file = "/Users/brandonhendrickson/Documents/Github_Projects/M_guttatus_TraitInequality/Data/csv/gini_ranks.csv", row.names = FALSE)

#############################################################################################
#Perform Phenotypic Selection Analysis of Height and Biomass for Ranked Individuals by Yield#
#############################################################################################

#Extract Means of Height and Biomass for each Fitness Rank
# Create the trait List
trait_list <- c("yield","flower_number_1", "D2Flowering", "ff_height", "total_node_number", "prop_early_growth", "final_height","flower_area", "leaf_area","Huber_Volume")

# Load modelframe
data <- read.csv("/Users/brandonhendrickson/Documents/Github_Projects/M_guttatus_PhenotypicSelection/Data/csv/raw_data.csv")

# Make a column of the fitness rank for each block and tray
data <- data %>%
    group_by(POS, Tray) %>%
    mutate(Fitness_Rank = rank(-yield))

# Initialize an empty data frame
results <- data.frame(Trait = character(), Fitness = character(), Estimate = numeric(), P_Value = numeric(), R_Squared = numeric(), F_Statistic = numeric(), SE = numeric())

for (trait in trait_list) {
    for (fitness in unique(data$Fitness_Rank)) {
        mean <- lm(data[data$Fitness_Rank == fitness, trait] ~ data[data$Fitness_Rank == fitness, "Final_Density"], data = data[data$Fitness_Rank == fitness, ])
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

#Standardize the data for each fitness rank

# Create the trait List
trait_list <- c("flower_number_1", "D2Flowering", "ff_height", "total_node_number", "prop_early_growth", "final_height","flower_area", "leaf_area","Huber_Volume")

# Change Final Density to Factor
data$Final_Density <- as.factor(data$Final_Density)

# Modify the function to standardize a single trait
standardize <- function(data, trait) {
  data <- data %>%
    group_by(Fitness_Rank, Final_Density) %>%
    mutate(!!paste0(trait, "_std") := ( .data[[trait]] - mean(.data[[trait]], na.rm = TRUE)) / sd(.data[[trait]], na.rm = TRUE))
  return(data)
}

# Apply the function to all elements in the trait list
for (trait in trait_list) {
    data <- standardize(data, trait)
}

# Save the data
write.csv(data, "/Users/brandonhendrickson/Documents/Github_Projects/M_guttatus_TraitInequality/Data/csv/standardized_data.csv", row.names = FALSE)

########################################################################################
# Simulate Change of Allele Frequencies for A Density of 6 at Different Mortality Levels#
########################################################################################

# Load Data
standardized <- read.csv("/Users/brandonhendrickson/Documents/Github_Projects/M_guttatus_TraitInequality/Data/csv/standardized_data.csv")
gini_rank <- read.csv("/Users/brandonhendrickson/Documents/Github_Projects/M_guttatus_TraitInequality/Data/csv/gini_ranks.csv")

# Merge the two dataframes by plot and tray
data <- merge(standardized, gini_rank, by = c("plot", "Tray"))

# Create a trait list for Final_Height and veg_weight
trait_list <- c("final_height", "veg_weight")

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




