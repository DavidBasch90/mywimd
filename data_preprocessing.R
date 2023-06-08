# Function for removing large NA variables and converting -9999 to NA. The total Wales figures
# are also removed. The data is also converted to wide format. By default, the function 
# removes variables related to childhood obesity, burglaries, criminal damage, fire 
# incidents, and theft.
#
# Inputs
#
# df: The raw WIMD dataframe from StatsWales

clean_wimd_data <- function(df) {
    
    na_codes <- c("OBCH", "BURG", "CRDG", "FIRE", "THEF")
    
    cleaned <- df %>%
        select(LSOA_Code, Indicator_Code, Data) %>% 
        mutate(
            Data = case_when(
                Data == -9999.0 ~ as.numeric(NA),
                TRUE ~ Data
            )
        ) %>%
        pivot_wider(names_from = Indicator_Code, values_from = Data) %>%
        filter(LSOA_Code != "Wales") %>%
        column_to_rownames("LSOA_Code") %>%
        select(-all_of(na_codes))
    
    cleaned
}

# Function for imputing missing values from the cleaned WIMD data. The function uses the mice
# package to impute missing values multiple times, and takes the average of those imputed
# values, 
#
# Inputs
# 
# df: The cleaned WIMD dataframe, output from clean_wimd_data()

impute_missing_wimd_values <- function(df) {
    
    set.seed(234)
    
    wimd_imputed <- mice(df, printFlag = FALSE, method = "pmm")
    
    new_data <- complete(wimd_imputed, "long")
    
    imputed_df <- new_data %>% 
        group_by(.id) %>%
        summarise(across(DIG:FLRS, mean)) %>%
        select(-.id) %>%
        mutate(
            LSOA_Code = rownames(df)
        ) 
    
    imputed_df
}

# Function for processing travel variables with principal component analysis.
# Only the first principal component is used. 
# 
# Inputs

# df: Cleaned WIMD dataframe with missing values imputed (PCA does not work with 
# missing values)
# keep_travel_vars: Should travel variables be kept? Should be left as FALSE if 
# data is being used for clustering. 

run_travel_pca <- function(df, keep_travel_vars = F) {
    
    # Isolate travel variables for PCA
    travel_codes <- c("PRFS", "PRGP", "PRLI", "PRPE", "PRPH", "PRPO", "PRPS",
                      "PRSF", "PRSS", "PUFS", "PUGP", "PULI", "PUPH", "PUPO", 
                      "PUPS", "PUSF", "PUSS")
    
    travel_df <- df %>% select(LSOA_Code, all_of(travel_codes))
    
    # Run PCA on travel variables
    set.seed(123)
    
    pca_rec <- recipe(~., data = travel_df) %>%
        update_role(LSOA_Code, new_role = "id") %>%
        step_normalize(all_predictors()) %>%
        step_pca(all_predictors())
    
    pca_prep <- prep(pca_rec)
    
    juiced_pca <- juice(pca_prep)
    
    # Bring together
    
    pc1 <- tibble(
        LSOA_Code = juiced_pca$LSOA_Code,
        PCA_Travel = juiced_pca$PC1
    )
    
    new_df <- df %>%
        left_join(pc1, by = c("LSOA_Code" = "LSOA_Code"))
    
    # Optionally keep travel variables
    
    if(keep_travel_vars == F) {
        new_df <- new_df %>%
            select(-all_of(travel_codes))
    }
    
    # Output new dataframe
    new_df
    
}

# Function for quickly scaling data with the dplyr `across()` function. The base
# scale function only works with matrices. 

scale2 <- function(x, na.rm = TRUE) (x - mean(x, na.rm = na.rm)) / sd(x, na.rm)
