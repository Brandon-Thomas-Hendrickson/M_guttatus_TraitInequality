# Load Data
data <- read.csv("/Users/brandonhendrickson/Documents/Github_Projects/M_guttatus_TraitInequality/Data/csv/gini_data.csv")

# Create List of Gini Traits
trait_list <- list("Gini_Index_yield","Gini_Index_flower_number_1","Gini_Index_D2Flowering","Gini_Index_flower_node","Gini_Index_pollen_avg","Gini_Index_ff_height","Gini_Index_ff_node","Gini_Index_total_node_number","Gini_Index_prop_early_growth","Gini_Index_lowervolume","Gini_Index_midrvolume","Gini_Index_upvolume","Gini_Index_Huber_Volume","Gini_Index_Smalian_Volume","Gini_Index_Newton_Volume","Gini_Index_final_height","Gini_Index_veg_weight","Gini_Index_repro_weight","Gini_Index_total_weight","Gini_Index_leaf_area")

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



