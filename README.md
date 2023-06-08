## Scripts

This folder holds R scripts that contain:

* useful functions for the WIMD project 
* code for producing cluster assignments
* code for checking correlations

### clustering_functions

Contains functions that facilitate clustering:

* run_travel_pca: takes the WIMD dataset as input and reduces the dimensionality of travel variables
to one dimension
* run_full_pca: takes the WIMD dataset as input, runs PCA on the whole dataset, and outputs a list object containing principal components 1 to 20
* plot_validation_measures: plots the connectivity, dunn index, and silhouette of a cluster model object
* extract_validation_measures: produces a tibble of validation measures from a cluster model object
* plot_stability_measures: plots stability measures (average proportion non-overlap, average distance, average distance from mean, figure of merit) from a cluster model object
* scale2: function for scaling data, can be applied column-wise (the base function scale() doesn't do this)

### modelling_functions

Contains functions for running bootstrapped random forest models (used to check whether cluster groups can be predicted), and running and extracting variable importance scores from a random forest model (not run on bootstrap samples).

* run_bootstrapped_rf_model: Run bootstrapped random forest model on main cluster assignments. 
* get_importance_scores: Extract importance scores from main cluster classification model. 
* run_subcluster_bootstrapped_rf_model: Run bootstrapped random forest model on  sub-cluster assignments.
* get_subcluster_importance_scores: Extract importance scores from sub-cluster classification model. 

### pam_cluster_assignments

Contains the code used to produce the final lookups for the analysis of the PAM clusters. The lookup 
contains LSOA assignments for the two different PAM models (two main clusters and three main clusters)

### correlation_checks

Contains code for checking correlations between indicators. We used this to check whether any variables 
could be removed that were duplicating information held in other variables. 