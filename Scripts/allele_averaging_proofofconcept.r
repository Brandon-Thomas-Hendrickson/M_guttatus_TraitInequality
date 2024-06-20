
# Define the function
change_in_allele_frequency <- function(P,s21,s22,s23,s31,s32,s33,s41,s42,s43,s51,s52,s53,s61,s62,s63,s71,s72,s73,w21,w22,w23,w31,w32,w33,w41,w42,w43,w51,w52,w53,w61,w62,w63,w71,w72,w73,yield21,yield22,yield23,yield31,yield32,yield33,yield41,yield42,yield43,yield51,yield52,yield53,yield61,yield62,yield63,yield71,yield72,yield73,generations) {
    for (i in 1:generations) {
        s2 <- w21 * s21 * yield21 + w22 * s22 * yield22 + w23 * s23 * yield23 # Calculate the weighted sum of the s values
        s3 <- w31 * s31 * yield31 + w32 * s32 * yield32 + w33 * s33 * yield33 
        s4 <- w41 * s41 * yield41 + w42 * s42 * yield42 + w43 * s43 * yield43
        s5 <- w51 * s51 * yield51 + w52 * s52 * yield52 + w53 * s53 * yield53
        s6 <- w61 * s61 * yield61 + w62 * s62 * yield62 + w63 * s63 * yield63 
        s7 <- w71 * s71 * yield71 + w72 * s72 * yield72 + w73 * s73 * yield73
        s <- (s2 + s3 + s4 + s5 + s6 + s7) / 6 # Calculate the total s value
        P <- P + s * P * (1 - P)
    }
    return(P)
}

P = 0.5
# Initialize an empty dataframe to store the results
results_yield_metapop <- data.frame(Trait = character(), Generations = integer(), Final_Allele_Frequency = numeric(), stringsAsFactors = FALSE)
for(trait in unique(data$Trait)){
for (generations in seq(1, 1000, 10)) {
            # Calculate the final allele frequency
            # Define weights for each sub-class in each density class
w21 <- 1/2; w22 <- 1/2; w23 <- 0 
w31 <- 1/3; w32 <- 1/3; w33 <- 1/3
w41 <- 1/4; w42 <- 1/4; w43 <- 2/4
w51 <- 1/5; w52 <- 1/5; w53 <- 3/5
w61 <- 1/6; w62 <- 1/6; w63 <- 4/6
w71 <- 1/7; w72 <- 1/7; w73 <- 5/7

# Define selection coefficients for each sub-class in each density class
s21 <- data[data$Trait == trait & data$Density == 2,"Estimate_F"]; s22 <- data[data$Trait == trait & data$Density == 2,"Estimate_LF"]; s23 <- data[data$Trait == trait & data$Density == 2,"Estimate"]
s31 <- data[data$Trait == trait & data$Density == 3,"Estimate_F"]; s32 <- data[data$Trait == trait & data$Density == 3,"Estimate_LF"]; s33 <- data[data$Trait == trait & data$Density == 3,"Estimate"]
s41 <- data[data$Trait == trait & data$Density == 4,"Estimate_F"]; s42 <- data[data$Trait == trait & data$Density == 4,"Estimate_LF"]; s43 <- data[data$Trait == trait & data$Density == 4,"Estimate"]
s51 <- data[data$Trait == trait & data$Density == 5,"Estimate_F"]; s52 <- data[data$Trait == trait & data$Density == 5,"Estimate_LF"]; s53 <- data[data$Trait == trait & data$Density == 5,"Estimate"]
s61 <- data[data$Trait == trait & data$Density == 6,"Estimate_F"]; s62 <- data[data$Trait == trait & data$Density == 6,"Estimate_LF"]; s63 <- data[data$Trait == trait & data$Density == 6,"Estimate"]
s71 <- data[data$Trait == trait & data$Density == 7,"Estimate_F"]; s72 <- data[data$Trait == trait & data$Density == 7,"Estimate_LF"]; s73 <- data[data$Trait == trait & data$Density == 7,"Estimate"]

# Define yields for each sub-class in each density class
yield21 <- data[data$Trait == trait & data$Density == 2,"F_m"]; yield22 <- data[data$Trait == trait & data$Density == 2,"LF_m"]; yield23 <- data[data$Trait == trait & data$Density == 2,"Other_m"]
yield31 <- data[data$Trait == trait & data$Density == 3,"F_m"]; yield32 <- data[data$Trait == trait & data$Density == 3,"LF_m"]; yield33 <- data[data$Trait == trait & data$Density == 3,"Other_m"]
yield41 <- data[data$Trait == trait & data$Density == 4,"F_m"]; yield42 <- data[data$Trait == trait & data$Density == 4,"LF_m"]; yield43 <- data[data$Trait == trait & data$Density == 4,"Other_m"]
yield51 <- data[data$Trait == trait & data$Density == 5,"F_m"]; yield52 <- data[data$Trait == trait & data$Density == 5,"LF_m"]; yield53 <- data[data$Trait == trait & data$Density == 5,"Other_m"]
yield61 <- data[data$Trait == trait & data$Density == 6,"F_m"]; yield62 <- data[data$Trait == trait & data$Density == 6,"LF_m"]; yield63 <- data[data$Trait == trait & data$Density == 6,"Other_m"]
yield71 <- data[data$Trait == trait & data$Density == 7,"F_m"]; yield72 <- data[data$Trait == trait & data$Density == 7,"LF_m"]; yield73 <- data[data$Trait == trait & data$Density == 7,"Other_m"]

            P_final <- change_in_allele_frequency(P,s21,s22,s23,s31,s32,s33,s41,s42,s43,s51,s52,s53,s61,s62,s63,s71,s72,s73,w21,w22,w23,w31,w32,w33,w41,w42,w43,w51,w52,w53,w61,w62,w63,w71,w72,w73,yield21,yield22,yield23,yield31,yield32,yield33,yield41,yield42,yield43,yield51,yield52,yield53,yield61,yield62,yield63,yield71,yield72,yield73,generations)

            # Add the results to the dataframe
            results_yield_metapop<- rbind(results_yield_metapop, data.frame(Trait = trait, Generations = generations, Final_Allele_Frequency = P_final, stringsAsFactors = FALSE))
        }
}


# Initialize a new dataframe to store the marked rows
marked_rows_yield_metapop <- data.frame()

# Loop over the list of dataframes
for (trait in names(results_yield_metapop)) {     
        # Initialize a new column Passes_0.99 with FALSE
        results_yield_metapop$Passes_0.99 <- FALSE
            # Loop over the rows of results_yield_metapop
            for (i in 2:nrow(results_yield_metapop)) {
                if (results_yield_metapop$Trait[i] == "D2Flowering_std") {
                    if (results_yield_metapop$Final_Allele_Frequency[i] < 0.01 && results_yield_metapop$Final_Allele_Frequency[i - 1] > 0.01) {
                        # Mark this row
                        results_yield_metapop$Passes_0.99[i] <- TRUE
                    }
                } else {
                    # If the current row is greater than 0.99 and the previous row is less than 0.99
                    if (results_yield_metapop$Final_Allele_Frequency[i] > 0.99 && results_yield_metapop$Final_Allele_Frequency[i - 1] < 0.99) {
                        # Mark this row
                        results_yield_metapop$Passes_0.99[i] <- TRUE
                    }
                }
            }
        # Add the marked rows to the new dataframe
        marked_rows_yield_metapop <- rbind(marked_rows_yield_metapop, results_yield_metapop)
    }

# Initialize an empty list to store the plots
plots <- list()

# Split the results dataframe by Trait
results_by_trait_yield_metapop <- split(marked_rows_yield_metapop, marked_rows_yield_metapop$Trait)

# Assuming results_by_trait_yield_metapop is your list of dataframes
gen <- data.frame()

# Loop over the list of dataframes
for (trait in names(results_by_trait_yield_metapop)) {
    # Subset the data where Passes_0.99 is TRUE
    data_subset <- subset(results_by_trait_yield_metapop[[trait]], Passes_0.99 == TRUE)

    # Add the subsetted data to the new dataframe
    gen <- rbind(gen, data_subset)
}

gen <- unique(gen)