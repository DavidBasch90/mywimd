# This is code for producing sublcusterings

library(tidyverse, warn.conflicts = F)
library(tidymodels)
library(here)
library(cluster)
library(dendextend)
library(factoextra)
library(mice)
library(clValid)
# WIMD 2019 dataset saved from discovery phase
wimd_2019 <- read_csv(here("datasets", "external_data", "wimd_2019.csv"))


# R code for data prep, including PCA 
source(here("scripts", "clustering_functions.r"))

wimd_25 <- wimd_2019 %>%
    run_travel_pca() %>%
    select(LSOA_Code, DIG:PCA_Travel) %>%
    mutate(across(DIG:PCA_Travel, scale2)) %>%
    column_to_rownames("LSOA_Code") %>%
    select(-c(LLTI, AIQP2, HQUA))

dist_matrix <- dist(wimd_25)

####----PAM

# Two-tiered PAM models

## 2 high-level clusters

set.seed(123)
pam_model <- pam(dist_matrix, k = 2)

pam_labels_2 <- tibble(
    lsoa = names(pam_model$clustering),
    first_cluster = pam_model$clustering
)
# function for cluster assignments
get_subcluster_assignments <- function(data, labels, cluster_num, k_num) {
    lsoas <- labels %>%
        filter(first_cluster == cluster_num) %>%
        pull(lsoa)
    
    
    filtered_data <- data %>%
        rownames_to_column("LSOA_Code") %>%
        filter(LSOA_Code %in% lsoas) %>%
        column_to_rownames("LSOA_Code")
    
    dist_matrix <- dist(filtered_data)
    
    set.seed(123)
    pam_model <- pam(dist_matrix, k = k_num)
    
    second_clust_labels <- tibble(
        lsoa = names(pam_model$clustering),
        second_cluster = pam_model$clustering
    )
    
    second_clust_labels
    
}

# join to high-level clusters
pam_labels_two_clusters <- pam_labels_2 %>%
    inner_join(
        rbind(
            get_subcluster_assignments(wimd_25, pam_labels_2, 1, 3),
            get_subcluster_assignments(wimd_25, pam_labels_2, 2, 3) 
        ),
        by = "lsoa"
    ) %>%
    mutate(
        method = "PAM_two_main_clusters"
    )


## 3 high-level clusters

set.seed(123)
pam_model <- pam(dist_matrix, k = 3)

pam_labels_3 <- tibble(
    lsoa = names(pam_model$clustering),
    first_cluster = pam_model$clustering
)

# join to high-level clusters
pam_labels_three_clusters <- pam_labels_3 %>%
    inner_join(
        rbind(
            get_subcluster_assignments(wimd_25, pam_labels_3, 1, 2),
            get_subcluster_assignments(wimd_25, pam_labels_3, 2, 3),
            get_subcluster_assignments(wimd_25, pam_labels_3, 3, 2) 
        ),
        by = "lsoa"
    ) %>%
    mutate(
        method = "PAM_three_main_clusters"
    )


pam_df <- rbind(pam_labels_two_clusters, pam_labels_three_clusters)

#### ---- Write out PAM cluster assignments

write_csv(pam_df, file = here("datasets", "cluster_assignments", "pam_clusterings.csv"))


#### ------ Model extraction

extract_subclustering_model <- function(data, labels, cluster_num, k_num) {
    lsoas <- labels %>%
        filter(first_cluster == cluster_num) %>%
        pull(lsoa)
    
    
    filtered_data <- data %>%
        rownames_to_column("LSOA_Code") %>%
        filter(LSOA_Code %in% lsoas) %>%
        column_to_rownames("LSOA_Code")
    
    dist_matrix <- dist(filtered_data)
    
    set.seed(123)
    pam_model <- pam(dist_matrix, k = k_num)
    
    pam_model
    
}

cl1_model <- extract_subclustering_model(wimd_25, pam_labels_2, 1, 3)
cl2_model <- extract_subclustering_model(wimd_25, pam_labels_2, 2, 3)

#saveRDS(cl1_model, file = here("models", "pam_cl1_model.rds"))
#saveRDS(cl2_model, file = here("models", "pam_cl2_model.rds"))

