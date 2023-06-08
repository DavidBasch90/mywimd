# Script for running the full pre-processing pipeline for WIMD 2019 data

run_travel_pca <- function(df, keep_travel_vars = FALSE) {
    
    # Get indicators
    
    generic_pattern <- c("return ", " time to a", "\\(minutes\\)")
    
    indicator_lookup <- df %>% 
        select(Indicator_Code, Indicator_ItemName_ENG) %>% 
        rename("indicator_code" = "Indicator_Code",
               "indicator_name" = "Indicator_ItemName_ENG") %>%
        mutate(
            indicator_name = str_remove_all(indicator_name, str_c(generic_pattern, collapse="|"))
        ) %>%
        unique()
    
    
    # Remove variables with large number of NA values
    # and convert -9999 (NA coded on StatsWales) to a numeric NA
    
    na_codes <- c("OBCH", "BURG", "CRDG", "FIRE", "THEF")
    
    wimd_wide <- df %>% 
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
        select(-na_codes)
    
    # Run multiple imputation and take average
    
    
    set.seed(234)
    wimd_imputed <- mice(wimd_wide, printFlag = FALSE)
    
    new_data <- complete(wimd_imputed, "long")
    
    wimd_wide <- new_data %>% 
        group_by(.id) %>%
        summarise(across(DIG:FLRS, mean)) %>%
        select(-.id) %>%
        mutate(
            LSOA_Code = rownames(wimd_wide)
        ) 
    
    # Get travel variable codes
    travel_codes <- indicator_lookup %>%
        filter(str_detect(indicator_name, "travel"))
    
    # Create dataframe of travel columns from wimd_wide
    travel_variables <- wimd_wide %>%
        select(LSOA_Code, travel_codes$indicator_code)
    
    # Run PCA on travel variables
    set.seed(123)
    
    pca_rec <- recipe(~., data = travel_variables) %>%
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
    
    wimd_wide <- wimd_wide %>%
        left_join(pc1, by = c("LSOA_Code" = "LSOA_Code"))
    
    # Optionally keep travel variables
    
    if(keep_travel_vars == F) {
        wimd_wide <- wimd_wide %>%
            select(-travel_codes$indicator_code) 
    }
    
    wimd_wide
    
}


run_full_pca <- function(df) {
    
    # Codes with NA
    
    na_codes <- c("OBCH", "BURG", "CRDG", "FIRE", "THEF")
    
    wimd_wide <- df %>% 
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
        select(-na_codes)
    
    
    # Run multiple imputation and take average
    
    
    set.seed(234)
    wimd_imputed <- mice(wimd_wide)
    
    new_data <- complete(wimd_imputed, "long")
    
    wimd_wide <- new_data %>% 
        group_by(.id) %>%
        summarise(across(DIG:FLRS, mean)) %>%
        select(-.id) %>%
        mutate(
            LSOA_Code = rownames(wimd_wide)
        ) 
    
    # Run PCA
    
    set.seed(123)
    
    pca_rec <- recipe(~., data = wimd_wide) %>%
        update_role(LSOA_Code, new_role = "id") %>%
        step_normalize(all_predictors()) %>%
        step_pca(all_predictors(), num_comp = 20)
    
    pca_prep <- prep(pca_rec)
    
    juiced_pca <- juice(pca_prep)
    
    tidied_pca <- tidy(pca_prep, 2)
    
    
    model_list <- list(
        prep = pca_prep,
        tidied = tidied_pca,
        juiced = juiced_pca
    
    )
    
    model_list
}

scale2 <- function(x, na.rm = TRUE) (x - mean(x, na.rm = na.rm)) / sd(x, na.rm)

plot_validation_measures <- function(model_object) {
    
    conn_df <- as_tibble(measures(model_object, "Connectivity")) %>%
        pivot_longer(everything(), names_to = "model", values_to = "value") %>%
        separate(model, c("clusters", "model")) %>%
        mutate(measure = "Connectivity (to be minimised)")
    
    dunn_df <- as_tibble(measures(model_object, "Dunn")) %>%
        pivot_longer(everything(), names_to = "model", values_to = "value") %>%
        separate(model, c("clusters", "model")) %>%
        mutate(measure = "Dunn (to be maximised)")
    
    silhouette_df <- as_tibble(measures(model_object, "Silhouette")) %>%
        pivot_longer(everything(), names_to = "model", values_to = "value") %>%
        separate(model, c("clusters", "model")) %>%
        mutate(measure = "Silhouette (to be maximised)")
    
    
    # Bring metrics together and plot
    validation_measures <- rbind(conn_df, dunn_df, silhouette_df)
    
    
    validation_measures %>%
        mutate(
            clusters = as.numeric(clusters)
        ) %>%
        ggplot(aes(x = clusters, y = value, col = model, group = model)) +
        geom_line(size = 1.5, alpha = 0.5) +
        scale_x_continuous(breaks = 2:12) +
        theme(
            legend.title = element_text(size = 10),
            legend.text = element_text(size = 12),
            legend.key.height = unit(2,"cm")
        ) +
        facet_wrap(~measure, scales = "free_y")
    
}

extract_validation_measures <- function(model_object) {
    
    conn_df <- as_tibble(measures(model_object, "Connectivity")) %>%
        pivot_longer(everything(), names_to = "model", values_to = "value") %>%
        separate(model, c("clusters", "model")) %>%
        mutate(measure = "Connectivity (to be minimised)")
    
    dunn_df <- as_tibble(measures(model_object, "Dunn")) %>%
        pivot_longer(everything(), names_to = "model", values_to = "value") %>%
        separate(model, c("clusters", "model")) %>%
        mutate(measure = "Dunn (to be maximised)")
    
    silhouette_df <- as_tibble(measures(model_object, "Silhouette")) %>%
        pivot_longer(everything(), names_to = "model", values_to = "value") %>%
        separate(model, c("clusters", "model")) %>%
        mutate(measure = "Silhouette (to be maximised)")
    
    
    # Bring metrics together and plot
    validation_measures <- rbind(conn_df, dunn_df, silhouette_df)
    
    validation_measures
    
}

plot_stability_measures <- function(stability_object){
    
    apn_df <- as_tibble(measures(stability_object, "APN")) %>%
        pivot_longer(everything(), names_to = "model", values_to = "value") %>%
        separate(model, c("clusters", "model")) %>%
        mutate(measure = "APN (minimise)")
    
    ad_df <- as_tibble(measures(stability_object, "AD")) %>%
        pivot_longer(everything(), names_to = "model", values_to = "value") %>%
        separate(model, c("clusters", "model")) %>%
        mutate(measure = "AD (minimise)")
    
    adm_df <- as_tibble(measures(stability_object, "ADM")) %>%
        pivot_longer(everything(), names_to = "model", values_to = "value") %>%
        separate(model, c("clusters", "model")) %>%
        mutate(measure = "ADM (minimise)")
    
    fom_df <- as_tibble(measures(stability_object, "FOM")) %>%
        pivot_longer(everything(), names_to = "model", values_to = "value") %>%
        separate(model, c("clusters", "model")) %>%
        mutate(measure = "FOM (minimised)")
    
    stability_measures <- rbind(apn_df, ad_df, adm_df, fom_df)
    
    
    
    
    stability_measures %>%
        ggplot(aes(x = model, y = value, fill = model)) +
        geom_col(size = 1.5, alpha = 0.5, show.legend = F) +
        theme(
            legend.title = element_text(size = 10),
            legend.text = element_text(size = 10),
            legend.key.height = unit(2,"cm")
        ) +
        facet_wrap(~measure, scales = "free_y")
    
}
