run_bootstrapped_rf_model <- function(data, cl_method) {
    
    # Prepare data
    model_df <- data %>%
        filter(method == cl_method) %>%
        mutate(first_cluster = factor(first_cluster)) %>%
        select(-second_cluster, -method)
    
    # Create bootstrap resamples
    set.seed(123)
    wimd_boot <- bootstraps(model_df)
    
    # Define recipe
    wimd_rec <- recipe(first_cluster ~ ., data = model_df) %>%
        update_role(LSOA_Code, new_role = "Id") %>%
        step_zv(all_predictors()) 
    
    # Model specification
    wimd_spec <- rand_forest(trees = 1000) %>%
        set_mode("classification") %>%
        set_engine("ranger")
    
    # Define workflow
    wimd_wf <- workflow() %>%
        add_recipe(wimd_rec) %>%
        add_model(wimd_spec)
    
    # Fit workflow to bootstrap resamples
    set.seed(234)
    wimd_results <- fit_resamples(
        wimd_wf,
        resamples = wimd_boot,
        control = control_resamples(save_pred = T)
    )
    
}


get_importance_scores <- function(data, cl_method) {
    
    # Prepare data
    model_df <- data %>%
        filter(method == cl_method) %>%
        mutate(first_cluster = factor(first_cluster)) %>%
        select(-second_cluster, -method)
    
    # Define recipe
    wimd_rec <- recipe(first_cluster ~ ., data = model_df) %>%
        update_role(LSOA_Code, new_role = "Id") %>%
        step_zv(all_predictors()) 
    
    # Model specification
    wimd_spec <- rand_forest(trees = 1000) %>%
        set_mode("classification") %>%
        set_engine("ranger")
    
    # Prep data
    wimd_prep <- prep(wimd_rec)
    
    # Run model and extract vi scores
    set.seed(234)
    wimd_spec %>%
        set_engine("ranger", importance = "permutation") %>%
        fit(
            first_cluster ~ .,
            data = juice(wimd_prep) %>%
                select(-LSOA_Code)
        ) %>%
        vip(geom = "point")
    
}

run_subcluster_bootstrapped_rf_model <- function(data, cl_method, cluster_num) {
    
    # Prepare data
    model_df <- data %>%
        filter(method == cl_method, first_cluster == cluster_num) %>%
        mutate(second_cluster = factor(second_cluster)) %>%
        select(-first_cluster, -method)
    
    # Create bootstrap resamples
    set.seed(123)
    wimd_boot <- bootstraps(model_df)
    
    # Define recipe
    wimd_rec <- recipe(second_cluster ~ ., data = model_df) %>%
        update_role(LSOA_Code, new_role = "Id") %>%
        step_zv(all_predictors()) 
    
    # Model specification
    wimd_spec <- rand_forest(trees = 1000) %>%
        set_mode("classification") %>%
        set_engine("ranger")
    
    # Define workflow
    wimd_wf <- workflow() %>%
        add_recipe(wimd_rec) %>%
        add_model(wimd_spec)
    
    # Fit workflow to bootstrap resamples
    set.seed(234)
    wimd_results <- fit_resamples(
        wimd_wf,
        resamples = wimd_boot,
        control = control_resamples(save_pred = T)
    )
    
}

get_subcluster_importance_scores <- function(data, cl_method, cluster_num) {
    
    # Prepare data
    model_df <- data %>%
        filter(method == cl_method, first_cluster == cluster_num) %>%
        mutate(second_cluster = factor(second_cluster)) %>%
        select(-first_cluster, -method)
    
    # Define recipe
    wimd_rec <- recipe(second_cluster ~ ., data = model_df) %>%
        update_role(LSOA_Code, new_role = "Id") %>%
        step_zv(all_predictors()) 
    
    # Model specification
    wimd_spec <- rand_forest(trees = 1000) %>%
        set_mode("classification") %>%
        set_engine("ranger")
    
    # Prep data
    wimd_prep <- prep(wimd_rec)
    
    # Run model and extract vi scores
    set.seed(234)
    wimd_spec %>%
        set_engine("ranger", importance = "permutation") %>%
        fit(
            second_cluster ~ .,
            data = juice(wimd_prep) %>%
                select(-LSOA_Code)
        ) %>%
        vip(geom = "point")
    
}