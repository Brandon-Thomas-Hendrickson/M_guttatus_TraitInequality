# Load the ineq package
library(dplyr)
library(ineq)
library(ggplot2)
library(ggpubr)
library(data.table)

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
gini_smalls <- gini_data[c("POS","Tray","Final_Density","Fitness_Rank","Gini_Category_Height","Gini_Category_Biomass")]

#Write out the dataframe 
write.csv(gini_smalls, file = "/Users/brandonhendrickson/Documents/Github_Projects/M_guttatus_TraitInequality/Data/csv/gini_ranks.csv", row.names = FALSE)

#############################################################################################
#Perform Phenotypic Selection Analysis of Height and Biomass for Ranked Individuals by Yield#
#############################################################################################

#Extract Means of Height and Biomass for each Fitness Rank
gini_data <- gini_data[!is.na(gini_data$Tray), ]
gini_data$Tray <- as.factor(gini_data$Tray)
# Make a column of the fitness rank for each block and tray
gini_data <- gini_data %>%
    group_by(POS, Tray) %>%
    mutate(Fitness_Rank = base::rank(-yield, ties.method = "first"))

#Standardize the gini_data for each fitness rank

# Create the trait List
trait_list <- c("flower_number_1", "D2Flowering", "ff_height", "total_node_number", "prop_early_growth", "final_height","flower_area", "leaf_area","Huber_Volume")

# Change Final Density to Factor
gini_data$Final_Density <- as.factor(gini_data$Final_Density)

# Modify the function to standardize a single trait
standardize <- function(gini_data, trait) {
  gini_data <- gini_data %>%
    group_by(Fitness_Rank, Final_Density) %>%
    mutate(!!paste0(trait, "_std") := (.data[[trait]] - mean(.data[[trait]], na.rm = TRUE)) / sd(.data[[trait]], na.rm = TRUE))
  return(gini_data)
}

# Apply the function to all elements in the trait list
for (trait in trait_list) {
    gini_data <- standardize(gini_data, trait)
}

# Create a gini_dataframe to store the results
results <- data.frame()
#Calculate Phenotypic Selection Coefficients for each trait and fitness_rank
for(trait in trait_list){
    for(i in 1:7){
        for (q in 1:i) {
            # Subset the gini_data for the current fitness rank
            gini_data_subset <- subset(gini_data, Fitness_Rank == q & Final_Density == i)
            # Fit a linear model
            model <- lm(as.formula(paste(trait, "_std ~ yield", sep = "")), data = gini_data_subset)

            # Extract the values
            model_summary <- summary(model)
            estimate <- coef(model_summary)["yield", "Estimate"]
            std_error <- coef(model_summary)["yield", "Std. Error"]
            p_value <- coef(model_summary)["yield", "Pr(>|t|)"]
            f_statistic <- model_summary$fstatistic[1]
            r_squared <- model_summary$r.squared

            # Add the results to the gini_data frame
            results <- rbind(results, data.frame(Trait = trait, Fitness_Rank = q, Final_Density = i, Estimate = estimate, Std_Error = std_error, P_Value = p_value, F_Statistic = f_statistic, R_Squared = r_squared))
        }
    }
}

rank <- summarySE(results, measurevar = "Estimate", groupvars = c("Trait", "Fitness_Rank"))

# Run Linear Regression to Determine if Estimates are sigificantly different between fitness_ranks
# Initialize an empty gini_dataframe to store trait names and their p-values
p_values_df <- data.frame(Trait = character(), P_Value = numeric(), stringsAsFactors = FALSE)

for(i in trait_list){
    model <- lm(as.formula(paste("Estimate ~ Fitness_Rank", sep = "")), data = rank[rank$Trait == i,])
    # Extract the p-value for the Fitness_Rank coefficient
    p_value <- summary(model)$coefficients["Fitness_Rank", 4]
    # Append the trait and p-value to the gini_dataframe
    p_values_df <- rbind(p_values_df, data.frame(Trait = i, P_Value = p_value))
}

# Print the gini_dataframe to see the p-values
print(p_values_df)

###############################################################
#Calculate Selection Coefficient for Ranked Height and Biomass#
###############################################################
#Extract Means of Height and Biomass for each Fitness Rank

gini_data <- gini_data[!is.na(gini_data$Tray), ]
gini_data$Tray <- as.factor(gini_data$Tray)
# Make a column of the fitness rank for each block and tray
gini_data <- gini_data %>%
    group_by(POS, Tray) %>%
    mutate(Height_Rank = base::rank(-final_height, ties.method = "first"))

gini_data <- gini_data %>%
    group_by(POS, Tray) %>%
    mutate(Veg_Rank = base::rank(-veg_weight, ties.method = "first"))
# Standardize the gini_data for each fitness rank

# Create the trait List
trait_list <- c("final_height")

# Change Final Density to Factor
gini_data$Final_Density <- as.factor(gini_data$Final_Density)

# Modify the function to standardize a single trait
standardize <- function(gini_data, trait) {
  gini_data <- gini_data %>%
    group_by(Height_Rank, Final_Density) %>%
    mutate(!!paste0(trait, "_std_by_Height") := (.data[[trait]] - mean(.data[[trait]], na.rm = TRUE)) / sd(.data[[trait]], na.rm = TRUE))
  return(gini_data)
}

# Apply the function to all elements in the trait list
for (trait in trait_list) {
    gini_data <- standardize(gini_data, trait)
}

standardize <- function(gini_data, trait) {
    gini_data <- gini_data %>%
        group_by(Veg_Rank, Final_Density) %>%
        mutate(!!paste0(trait, "_std_by_Biomass") := (.data[[trait]] - mean(.data[[trait]], na.rm = TRUE)) / sd(.data[[trait]], na.rm = TRUE))
    return(gini_data)
}

for (trait in trait_list) {
    gini_data <- standardize(gini_data, trait)
}

trait_list <- list("final_height")
# Create a gini_dataframe to store the results
results <- data.frame()
storeResults <- function() {
    model_summary <- summary(model)
    estimate <- coef(model_summary)["final_height_std_by_Height", "Estimate"]
    std_error <- coef(model_summary)["final_height_std_by_Height", "Std. Error"]
    p_value <- coef(model_summary)["final_height_std_by_Height", "Pr(>|t|)"]
    f_statistic <- model_summary$fstatistic[1]
    r_squared <- model_summary$r.squared
    # Add the results to the gini_data frame
    results <<- rbind(results, data.frame(Trait = trait, Gini_Category_Height = m, Height_Rank = q, Final_Density = i, Estimate = estimate, Std_Error = std_error, P_Value = p_value, F_Statistic = f_statistic, R_Squared = r_squared))
}
# Calculate Phenotypic Selection Coefficients for each trait and fitness_rank
for(trait in trait_list){
    for(i in 1:7){
        for (q in 1:i) {
            if (i > 1) {
                # Only apply Gini_Category_Height for Final_Density > 1
                for (m in unique(gini_data$Gini_Category_Height)) {
                    # Subset the gini_data for the current fitness rank and Gini_Category_Height
                    gini_data_subset <- subset(gini_data, Height_Rank == q & Final_Density == i & Gini_Category_Height == m)
                    # Fit a linear model
                    model <- lm(as.formula(paste("yield ~", trait,"_std_by_Height", sep = "")), data = gini_data_subset)
                    # Extract and store the values
                    storeResults()
                }
            } else {
                # For Final_Density == 1, ignore Gini_Category_Height
                gini_data_subset <- subset(gini_data, Height_Rank == q & Final_Density == i)
                # Fit a linear model
                model <- lm(as.formula(paste("yield ~", trait,"_std_by_Height", sep = "")), data = gini_data_subset)
                # Extract and store the values, setting Gini as NA or a placeholder
                m <- NA  # Or use a placeholder value that indicates this condition
                storeResults()
            }
        }
    }
}

trait_list <- list("veg_weight")
# Create a gini_dataframe to store the results_BIOMASS
results_BIOMASS <- data.frame()
storeresults_BIOMASS <- function() {
    model_summary <- summary(model)
    estimate <- coef(model_summary)["veg_weight_std_by_Biomass", "Estimate"]
    std_error <- coef(model_summary)["veg_weight_std_by_Biomass", "Std. Error"]
    p_value <- coef(model_summary)["veg_weight_std_by_Biomass", "Pr(>|t|)"]
    f_statistic <- model_summary$fstatistic[1]
    r_squared <- model_summary$r.squared
    # Add the results_BIOMASS to the gini_data frame
    results_BIOMASS <<- rbind(results_BIOMASS, data.frame(Trait = trait, Gini_Category_Biomass = m, Veg_Rank = q, Final_Density = i, Estimate = estimate, Std_Error = std_error, P_Value = p_value, F_Statistic = f_statistic, R_Squared = r_squared))
}
# Calculate Phenotypic Selection Coefficients for each trait and fitness_rank
for(trait in trait_list){
    for(i in 1:7){
        for (q in 1:i) {
            if (i > 1) {
                # Only apply Gini_Category_Height for Final_Density > 1
                for (m in unique(gini_data$Gini_Category_Biomass)) {
                    # Subset the gini_data for the current fitness rank and Gini_Category_Height
                    gini_data_subset <- subset(gini_data, Veg_Rank == q & Final_Density == i & Gini_Category_Biomass == m)
                    # Fit a linear model
                    model <- lm(as.formula(paste("yield ~", trait,"_std_by_Biomass", sep = "")), data = gini_data_subset)
                    # Extract and store the values
                    storeresults_BIOMASS()
                }
            } else {
                # For Final_Density == 1, ignore Gini_Category_Biomass
                gini_data_subset <- subset(gini_data, Veg_Rank == q & Final_Density == i)
                # Fit a linear model
                model <- lm(as.formula(paste("yield ~", trait,"_std_by_Biomass", sep = "")), data = gini_data_subset)
                # Extract and store the values, setting Gini as NA or a placeholder
                m <- NA  # Or use a placeholder value that indicates this condition
                storeresults_BIOMASS()
            }
        }
    }
}

checkpls <- summarySE(results, measurevar = "Estimate", groupvars = c("Gini_Category_Height", "Height_Rank"))
ggplot(checkpls, aes(x = Height_Rank, y = Estimate, color = Gini_Category_Height, group = Gini_Category_Height)) +
    geom_line() +
    geom_point()

results_BIOMASS <- results_BIOMASS[results_BIOMASS$Trait == "veg_weight",]
checkpls_BIOMASS <- summarySE(results_BIOMASS, measurevar = "Estimate", groupvars = c("Gini", "Veg_Rank"))
ggplot(checkpls, aes(x = Veg_Rank, y = Estimate, color = Gini, group = Gini)) +
    geom_line() +
    geom_point()

# Write the results to a CSV file
write.csv(results, file = "/Users/brandonhendrickson/Documents/Github_Projects/M_guttatus_PhenotypicSelection/Data/csv/phenotype_selection_Height.csv", row.names = FALSE)
write.csv(results_BIOMASS, file = "/Users/brandonhendrickson/Documents/Github_Projects/M_guttatus_PhenotypicSelection/Data/csv/phenotype_selection_BIOMASS.csv", row.names = FALSE)


allele_ready <- merge(gini_data, results, by = c("Final_Density", "Height_Rank", "Gini_Category_Height"))
allele_ready_BIOMASS <- merge(gini_data, results_BIOMASS, by = c("Final_Density", "Veg_Rank", "Gini_Category_Biomass"))



########################################################################################
# Simulate Change of Allele Frequencies for A Density of 6 at Different Mortality Levels#
########################################################################################


# Create a trait list for Final_Height and veg_weight
trait_list <- c("final_height")

# Calculate the average yield for each density and fitness group
yield_avg <- aggregate(yield ~ Final_Density + Height_Rank + Gini_Category_Height, data = allele_ready, FUN = mean)

# Remove Densities of 1 from dataframe
yield_avg <- yield_avg[yield_avg$Final_Density != 1, ]

# Reshape the data
yield_avg_wide <- yield_avg %>%
    spread(key = Height_Rank, value = yield)

# Calculate the total yield per row
yield_avg_wide$total_yield = rowSums(yield_avg_wide[, c("1","2","3","4","5","6","7")], na.rm = TRUE)

# Calculate the proportion of yield for each category
yield_avg_wide$Rank_1 = yield_avg_wide$`1` / yield_avg_wide$total_yield
yield_avg_wide$Rank_2 = yield_avg_wide$`2` / yield_avg_wide$total_yield
yield_avg_wide$Rank_3 = yield_avg_wide$`3` / yield_avg_wide$total_yield
yield_avg_wide$Rank_4 = yield_avg_wide$`4` / yield_avg_wide$total_yield
yield_avg_wide$Rank_5 = yield_avg_wide$`5` / yield_avg_wide$total_yield
yield_avg_wide$Rank_6 = yield_avg_wide$`6` / yield_avg_wide$total_yield
yield_avg_wide$Rank_7 = yield_avg_wide$`7` / yield_avg_wide$total_yield


# Calculate the row averages
yield_avg_wide$Avg <- rowMeans(yield_avg_wide[, c("1","2","3","4","5","6","7")], na.rm = TRUE)

# Merge the two dataframes
data <- merge(allele_ready, yield_avg_wide, by = c("Final_Density", "Gini_Category_Height"))
data_sub <- data[data$Final_Density == 6, ]
yp <- summarySE(data_sub, measurevar = "yield_proportion", groupvars = c("Final_Density", "Gini_Category_Height", "Height_Rank"))
es <- summarySE(data_sub, measurevar = "Estimate", groupvars = c("Final_Density", "Gini_Category_Height", "Height_Rank"))
es_yp <- merge(es, yp, by = c("Final_Density", "Gini_Category_Height", "Height_Rank"))
es_yp <- es_yp[, c(1, 2, 3, 5, 10)]

# Get the s values from the Estimate
        s1 <- es_yp[es_yp$Height_Rank == 1 & es_yp$Gini_Category_Height == "Above Median", "Estimate"]
        s2 <- es_yp[es_yp$Height_Rank == 2 & es_yp$Gini_Category_Height == "Above Median", "Estimate"]
        s3 <- es_yp[es_yp$Height_Rank == 3 & es_yp$Gini_Category_Height == "Above Median", "Estimate"]
        s4 <- es_yp[es_yp$Height_Rank == 4 & es_yp$Gini_Category_Height == "Above Median", "Estimate"]
        s5 <- es_yp[es_yp$Height_Rank == 5 & es_yp$Gini_Category_Height == "Above Median", "Estimate"]
        s6 <- es_yp[es_yp$Height_Rank == 6 & es_yp$Gini_Category_Height == "Above Median", "Estimate"]

        # Add Yield
        w1 <- es_yp[es_yp$Height_Rank == 1 & es_yp$Gini_Category_Height == "Above Median", "yield_proportion"]
        w2 <- es_yp[es_yp$Height_Rank == 2 & es_yp$Gini_Category_Height == "Above Median", "yield_proportion"]
        w3 <- es_yp[es_yp$Height_Rank == 3 & es_yp$Gini_Category_Height == "Above Median", "yield_proportion"]
        w4 <- es_yp[es_yp$Height_Rank == 4 & es_yp$Gini_Category_Height == "Above Median", "yield_proportion"]
        w5 <- es_yp[es_yp$Height_Rank == 5 & es_yp$Gini_Category_Height == "Above Median", "yield_proportion"]
        w6 <- es_yp[es_yp$Height_Rank == 6 & es_yp$Gini_Category_Height == "Above Median", "yield_proportion"]

# Define the function
change_in_allele_frequency <- function(P, s1, s2, s3, s4,s5,s6, w1, w2, w3, w4,w5,w6, generations) {
    for (i in 1:generations) {
        s <- w1 * s1 + w2 * s2 + w3 * s3 + w4 * s4 + w5 * s5 + w6 * s6 # Calculate the weighted sum of the s values
        P <- P + s * P * (1 - P)
    }
    return(P)
}

P = 0.5

results_yield <- data.frame(Trait = character(), Density = numeric(), Estimate = numeric(), Estimate_F = numeric(), Estimate_LF = numeric(), Generations = integer(), Final_Allele_Frequency = numeric(), stringsAsFactors = FALSE)
        # Loop over the list of generations
        for (generations in seq(1, 1000, 1)) {
            # Calculate the final allele frequency
            P_final <- change_in_allele_frequency(P, s1, s2, s3, s4,s5,s6,w1, w2, w3, w4,w5,w6, generations)

            # Add the results to the dataframe
            results_yield <- rbind(results_yield, data.frame(Generations = generations, Final_Allele_Frequency = P_final, stringsAsFactors = FALSE))
        }

# Get the s values from the Estimate
        s1 <- es_yp[es_yp$Height_Rank == 1 & es_yp$Gini_Category_Height == "Below Median", "Estimate"]
        s2 <- es_yp[es_yp$Height_Rank == 2 & es_yp$Gini_Category_Height == "Below Median", "Estimate"]
        s3 <- es_yp[es_yp$Height_Rank == 3 & es_yp$Gini_Category_Height == "Below Median", "Estimate"]
        s4 <- es_yp[es_yp$Height_Rank == 4 & es_yp$Gini_Category_Height == "Below Median", "Estimate"]
        s5 <- es_yp[es_yp$Height_Rank == 5 & es_yp$Gini_Category_Height == "Below Median", "Estimate"]
        s6 <- es_yp[es_yp$Height_Rank == 6 & es_yp$Gini_Category_Height == "Below Median", "Estimate"]

        # Add Yield
        w1 <- es_yp[es_yp$Height_Rank == 1 & es_yp$Gini_Category_Height == "Below Median", "yield_proportion"]
        w2 <- es_yp[es_yp$Height_Rank == 2 & es_yp$Gini_Category_Height == "Below Median", "yield_proportion"]
        w3 <- es_yp[es_yp$Height_Rank == 3 & es_yp$Gini_Category_Height == "Below Median", "yield_proportion"]
        w4 <- es_yp[es_yp$Height_Rank == 4 & es_yp$Gini_Category_Height == "Below Median", "yield_proportion"]
        w5 <- es_yp[es_yp$Height_Rank == 5 & es_yp$Gini_Category_Height == "Below Median", "yield_proportion"]
        w6 <- es_yp[es_yp$Height_Rank == 6 & es_yp$Gini_Category_Height == "Below Median", "yield_proportion"]

# Define the function
change_in_allele_frequency <- function(P, s1, s2, s3, s4,s5,s6, w1, w2, w3, w4,w5,w6, generations) {
    for (i in 1:generations) {
        s <- w1 * s1 + w2 * s2 + w3 * s3 + w4 * s4 + w5 * s5 + w6 * s6 # Calculate the weighted sum of the s values
        P <- P + s * P * (1 - P)
    }
    return(P)
}

P = 0.5

results_yield_Low <- data.frame(Trait = character(), Density = numeric(), Estimate = numeric(), Estimate_F = numeric(), Estimate_LF = numeric(), Generations = integer(), Final_Allele_Frequency = numeric(), stringsAsFactors = FALSE)
        # Loop over the list of generations
        for (generations in seq(1, 1000, 1)) {
            # Calculate the final allele frequency
            P_final <- change_in_allele_frequency(P, s1, s2, s3, s4, s5, s6, w1, w2, w3, w4, w5, w6, generations)

            # Add the results to the dataframe
            results_yield_Low <- rbind(results_yield_Low, data.frame(Generations = generations, Final_Allele_Frequency = P_final, stringsAsFactors = FALSE))
        }

results_yield$Gini_Category_Height <- "Above Median"
results_yield_Low$Gini_Category_Height <- "Below Median"

results_yield <- rbind(results_yield, results_yield_Low)

ggplot(results_yield,aes(x=Generations,y=Final_Allele_Frequency,color=Gini_Category_Height))+geom_line()

# Repeat with Mortality of 2 and Mortality of 4
yield_sub <- yield_avg[yield_avg$Final_Density == 6, ]
do2 <- yield_sub[yield_sub$Height_Rank < 5, ]
do4 <- yield_sub[yield_sub$Height_Rank < 3, ]

# Reshape the data
yield_avg_wide_do2 <- do2 %>%
    spread(key = Height_Rank, value = yield)

# Calculate the total yield per row
yield_avg_wide_do2$total_yield = rowSums(yield_avg_wide_do2[, c("1","2","3","4")], na.rm = TRUE)

# Calculate the proportion of yield for each category
yield_avg_wide_do2$Rank_1 = yield_avg_wide_do2$`1` / yield_avg_wide_do2$total_yield
yield_avg_wide_do2$Rank_2 = yield_avg_wide_do2$`2` / yield_avg_wide_do2$total_yield
yield_avg_wide_do2$Rank_3 = yield_avg_wide_do2$`3` / yield_avg_wide_do2$total_yield
yield_avg_wide_do2$Rank_4 = yield_avg_wide_do2$`4` / yield_avg_wide_do2$total_yield

# Reshape the data
yield_avg_wide_do4 <- do4 %>%
    spread(key = Height_Rank, value = yield)

# Calculate the total yield per row
yield_avg_wide_do4$total_yield = rowSums(yield_avg_wide_do4[, c("1","2")], na.rm = TRUE)

# Calculate the proportion of yield for each category
yield_avg_wide_do4$Rank_1 = yield_avg_wide_do4$`1` / yield_avg_wide_do4$total_yield
yield_avg_wide_do4$Rank_2 = yield_avg_wide_do4$`2` / yield_avg_wide_do4$total_yield

# Merge the two dataframes
data <- merge(allele_ready, do4, by = c("Final_Density", "Gini_Category_Height","Height_Rank"))
data_sub <- data[data$Final_Density == 6, ]
yp <- summarySE(data_sub, measurevar = "yield_proportion", groupvars = c("Final_Density", "Gini_Category_Height", "Height_Rank"))
es <- summarySE(data_sub, measurevar = "Estimate", groupvars = c("Final_Density", "Gini_Category_Height", "Height_Rank"))
es_yp <- merge(es, yp, by = c("Final_Density", "Gini_Category_Height", "Height_Rank"))
es_yp <- es_yp[, c(1, 2, 3, 5, 10)]

# Get the s values from the Estimate
        s1 <- es_yp[es_yp$Height_Rank == 1 & es_yp$Gini_Category_Height == "Above Median", "Estimate"]
        s2 <- es_yp[es_yp$Height_Rank == 2 & es_yp$Gini_Category_Height == "Above Median", "Estimate"]

        # Add Yield
        w1 <- es_yp[es_yp$Height_Rank == 1 & es_yp$Gini_Category_Height == "Above Median", "yield_proportion"]
        w2 <- es_yp[es_yp$Height_Rank == 2 & es_yp$Gini_Category_Height == "Above Median", "yield_proportion"]

# Define the function
change_in_allele_frequency <- function(P, s1, s2, w1, w2, generations) {
    for (i in 1:generations) {
        s <- w1 * s1 + w2 * s2 # Calculate the weighted sum of the s values
        P <- P + s * P * (1 - P)
    }
    return(P)
}

P = 0.5

results_yield_DO4 <- data.frame(Generations = integer(), Final_Allele_Frequency = numeric(), stringsAsFactors = FALSE)
        # Loop over the list of generations
        for (generations in seq(1, 1000, 1)) {
            # Calculate the final allele frequency
            P_final <- change_in_allele_frequency(P, s1, s2, w1, w2, generations)

            # Add the results to the dataframe
            results_yield_DO4 <- rbind(results_yield_DO4, data.frame(Generations = generations, Final_Allele_Frequency = P_final, stringsAsFactors = FALSE))
        }

# Get the s values from the Estimate
        s1 <- es_yp[es_yp$Height_Rank == 1 & es_yp$Gini_Category_Height == "Below Median", "Estimate"]
        s2 <- es_yp[es_yp$Height_Rank == 2 & es_yp$Gini_Category_Height == "Below Median", "Estimate"]

        # Add Yield
        w1 <- es_yp[es_yp$Height_Rank == 1 & es_yp$Gini_Category_Height == "Below Median", "yield_proportion"]
        w2 <- es_yp[es_yp$Height_Rank == 2 & es_yp$Gini_Category_Height == "Below Median", "yield_proportion"]

# Define the function
change_in_allele_frequency <- function(P, s1, s2, w1, w2,generations) {
    for (i in 1:generations) {
        s <- w1 * s1 + w2 * s2  # Calculate the weighted sum of the s values
        P <- P + s * P * (1 - P)
    }
    return(P)
}

P = 0.5

results_yield_DO4_Low <- data.frame(Generations = integer(), Final_Allele_Frequency = numeric(), stringsAsFactors = FALSE)
        # Loop over the list of generations
        for (generations in seq(1, 1000, 1)) {
            # Calculate the final allele frequency
            P_final <- change_in_allele_frequency(P, s1, s2, w1, w2, generations)

            # Add the results to the dataframe
            results_yield_DO4_Low <- rbind(results_yield_DO4_Low, data.frame(Generations = generations, Final_Allele_Frequency = P_final, stringsAsFactors = FALSE))
        }

results_yield_DO4$Gini_Category_Height <- "Above Median"
results_yield_DO4_Low$Gini_Category_Height <- "Below Median"

results_yield_DO4 <- rbind(results_yield_DO4, results_yield_DO4_Low)

ggplot(results_yield_DO4,aes(x=Generations,y=Final_Allele_Frequency,color=Gini_Category_Height))+geom_line()

# Repeat with Mortality of 2
# Merge the two dataframes
data <- merge(allele_ready, do2, by = c("Final_Density", "Gini_Category_Height","Height_Rank"))
data_sub <- data[data$Final_Density == 6, ]
yp <- summarySE(data_sub, measurevar = "yield_proportion", groupvars = c("Final_Density", "Gini_Category_Height", "Height_Rank"))
es <- summarySE(data_sub, measurevar = "Estimate", groupvars = c("Final_Density", "Gini_Category_Height", "Height_Rank"))
es_yp <- merge(es, yp, by = c("Final_Density", "Gini_Category_Height", "Height_Rank"))
es_yp <- es_yp[, c(1, 2, 3, 5, 10)]
# Get the s values from the Estimate
        s1 <- es_yp[es_yp$Height_Rank == 1 & es_yp$Gini_Category_Height == "Above Median", "Estimate"]
        s2 <- es_yp[es_yp$Height_Rank == 2 & es_yp$Gini_Category_Height == "Above Median", "Estimate"]
        s3 <- es_yp[es_yp$Height_Rank == 3 & es_yp$Gini_Category_Height == "Above Median", "Estimate"]
        s4 <- es_yp[es_yp$Height_Rank == 4 & es_yp$Gini_Category_Height == "Above Median", "Estimate"]
        # Add Yield
        w1 <- es_yp[es_yp$Height_Rank == 1 & es_yp$Gini_Category_Height == "Above Median", "yield_proportion"]
        w2 <- es_yp[es_yp$Height_Rank == 2 & es_yp$Gini_Category_Height == "Above Median", "yield_proportion"]
        w3 <- es_yp[es_yp$Height_Rank == 3 & es_yp$Gini_Category_Height == "Above Median", "yield_proportion"]
        w4 <- es_yp[es_yp$Height_Rank == 4 & es_yp$Gini_Category_Height == "Above Median", "yield_proportion"]

# Define the function
change_in_allele_frequency <- function(P, s1, s2, s3,s4,w1, w2, w3,w4,generations) {
    for (i in 1:generations) {
        s <- w1 * s1 + w2 * s2 +w3 * s3 + w4 * s4 # Calculate the weighted sum of the s values
        P <- P + s * P * (1 - P)
    }
    return(P)
}

P = 0.5

results_yield_DO2 <- data.frame(Generations = integer(), Final_Allele_Frequency = numeric(), stringsAsFactors = FALSE)
        # Loop over the list of generations
        for (generations in seq(1, 1000, 1)) {
            # Calculate the final allele frequency
            P_final <- change_in_allele_frequency(P, s1, s2, s3,s4,w1, w2, w3,w4,generations)

            # Add the results to the dataframe
            results_yield_DO2 <- rbind(results_yield_DO2, data.frame(Generations = generations, Final_Allele_Frequency = P_final, stringsAsFactors = FALSE))
        }

# Get the s values from the Estimate
        s1 <- es_yp[es_yp$Height_Rank == 1 & es_yp$Gini_Category_Height == "Below Median", "Estimate"]
        s2 <- es_yp[es_yp$Height_Rank == 2 & es_yp$Gini_Category_Height == "Below Median", "Estimate"]
        s3 <- es_yp[es_yp$Height_Rank == 3 & es_yp$Gini_Category_Height == "Below Median", "Estimate"]
        s4 <- es_yp[es_yp$Height_Rank == 4 & es_yp$Gini_Category_Height == "Below Median", "Estimate"]
        # Add Yield
        w1 <- es_yp[es_yp$Height_Rank == 1 & es_yp$Gini_Category_Height == "Below Median", "yield_proportion"]
        w2 <- es_yp[es_yp$Height_Rank == 2 & es_yp$Gini_Category_Height == "Below Median", "yield_proportion"]
        w3 <- es_yp[es_yp$Height_Rank == 3 & es_yp$Gini_Category_Height == "Below Median", "yield_proportion"]
        w4 <- es_yp[es_yp$Height_Rank == 4 & es_yp$Gini_Category_Height == "Below Median", "yield_proportion"]

# Define the function
change_in_allele_frequency <- function(P, s1, s2, s3,s4,w1, w2, w3,w4,generations) {
    for (i in 1:generations) {
        s <- w1 * s1 + w2 * s2  # Calculate the weighted sum of the s values
        P <- P + s * P * (1 - P)
    }
    return(P)
}

P = 0.5

results_yield_DO2_Low <- data.frame(Generations = integer(), Final_Allele_Frequency = numeric(), stringsAsFactors = FALSE)
        # Loop over the list of generations
        for (generations in seq(1, 1000, 1)) {
            # Calculate the final allele frequency
            P_final <- change_in_allele_frequency(P, s1, s2, s3,s4,w1, w2, w3,w4,generations)

            # Add the results to the dataframe
            results_yield_DO2_Low <- rbind(results_yield_DO2_Low, data.frame(Generations = generations, Final_Allele_Frequency = P_final, stringsAsFactors = FALSE))
        }

results_yield_DO2$Gini_Category_Height <- "Above Median"
results_yield_DO2_Low$Gini_Category_Height <- "Below Median"

results_yield_DO2 <- rbind(results_yield_DO2, results_yield_DO2_Low)

ggplot(results_yield_DO2,aes(x=Generations,y=Final_Allele_Frequency,color=Gini_Category_Height))+geom_line()

# Combine All Runs
results_yield_DO2$Mortality <- "Medium"
results_yield_DO4$Mortality <- "High"
results_yield$Mortality <- "Low"

results_yield <- rbind(results_yield_DO2, results_yield_DO4, results_yield)


# Initialize a new dataframe to store the marked rows
marked_rows_yield <- data.frame()

# Loop over the list of dataframes
for (ineq in unique(results_yield$Gini_Category_Height)) {
    # Loop over the unique densities
    for (mort in unique(results_yield$Mortality)) {
        # Subset the data for the current density
        data_density <- subset(results_yield, Gini_Category_Height == ineq & Mortality == mort)

        # Initialize a new column Passes_0.99 with FALSE
        data_density$Passes_0.99 <- FALSE
        # Loop over the rows of data_density
        for (i in 2:nrow(data_density)) {
            # If the current row is greater than 0.99 and the previous row is less than 0.99
            if (data_density$Final_Allele_Frequency[i] > 0.99 && data_density$Final_Allele_Frequency[i - 1] < 0.99) {
                # Mark this row
                data_density$Passes_0.99[i] <- TRUE
            }
        }
        # Add the marked rows to the new dataframe
        marked_rows_yield <- rbind(marked_rows_yield, data_density)
    }
}








