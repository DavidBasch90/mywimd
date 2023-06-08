library(corrr)
library(here)
library(tidyverse)
library(mice)
library(tidymodels)

wimd_2019 <- read_csv(here("datasets", "external_data", "wimd_2019.csv"))

source(here("scripts", "clustering_functions.R"))

wimd_processed <- wimd_2019 %>% 
    run_travel_pca() %>%
    relocate(LSOA_Code) %>%
    mutate(across(DIG:PCA_Travel, scale2)) %>%
    select(-LLTI, -AIQP2) 


wimd_processed %>%
    select(-LSOA_Code) %>%
    correlate() %>%
    shave() %>%
    pivot_longer(DIG:PCA_Travel) %>%
    mutate(pair = paste0(term, "-", name)) %>%
    filter(!is.na(value)) %>%
    slice_max(abs(value), n = 10) %>%
    ggplot(aes(x = abs(value), y = pair, fill = term)) +
    geom_col(show.legend = F)    

