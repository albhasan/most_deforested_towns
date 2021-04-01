# Fit 100 models and estimate for estimating the RMS

library(tidymodels)
library(doParallel)
library(ensurer)
library(readr)
library(vip)
library(magrittr)


raw_tb <- "./data/samples/data_tb.rds" %>%
    readRDS()  %>%
    # NOTE: Convert deforestation from kilometers to square meters.
    dplyr::mutate(def = def * 1000000) %>%
    # NOTE: Apply a logarithmic transformation to deforestation
    dplyr::mutate(log_def = log(def)) %>%
    (function(x) {
        print("Raw data deforestation statistics...")
        x %>%
            dplyr::group_by(year) %>%
            dplyr::summarize(total = dplyr::n(),
                             def_mean = mean(def),
                             def_sd   = sd(def),
                             def_min  = min(def, na.rm = TRUE),
                             def_max  = max(def, na.rm = TRUE)) %>%
            print(n = Inf)
        return(x) 
    })

data_tb <- raw_tb %>%
    dplyr::filter(year %in% c(2019, 2020),
                  def > 0) %>%
    ensurer::ensure_that(nrow(.) > 0,
                err_desc = "No data found!")


out_dir <- file.path("./results/cross-validation")

for (i in 1:100) {
    print(sprintf("Processing iteration... %s", i))
    performance_file <- file.path(out_dir, 
                                  paste0("performance_test_", i, ".rds"))  
    if (file.exists(performance_file))
        next
    
    #---- Data split ----
    data_split <- rsample::initial_split(data_tb)
    data_train <- rsample::training(data_split)
    data_test  <- rsample::testing(data_split)
    data_folds <- rsample::bootstraps(data_train)
    
    #---- Recipe ----
    ranger_recipe <- recipes::recipe(formula = log_def ~ .,
                                     data = data_tb) %>%
        recipes::update_role(id, UF, CD_GEOCMU, NM_MUNICIP, 
                             longitude, latitude, 
                             def, slope, year,
                             new_role = "other")
    
    #---- Model ----
    ranger_spec <- parsnip::rand_forest(mtry = tune::tune(),
                                        min_n = tune::tune(),
                                        trees = 2000) %>%
        parsnip::set_mode(mode = "regression") %>%
        parsnip::set_engine("ranger")
    
    #---- Workflow ----
    
    ranger_workflow <- workflows::workflow() %>%
        workflows::add_recipe(ranger_recipe) %>%
        workflows::add_model(ranger_spec)
    
    #---- Tunning ----
    
    doParallel::registerDoParallel()
    ranger_tune <- tune::tune_grid(ranger_workflow,
                                   resamples = data_folds,
                                   grid = 11)
    
    #---- Finalize ----
    param_final <- ranger_tune %>%
        select_best(metric = "rmse")
    saveRDS(param_final, 
            file = file.path(out_dir, paste0("param_final_", i, ".rds")))
    ranger_workflow <- ranger_workflow %>%
        finalize_workflow(param_final)
    
    #---- Evaluate ----
    data_fit <- ranger_workflow %>%
        tune::last_fit(data_split)
    
    test_performance <- data_fit %>%
        tune::collect_metrics()
    test_performance %>%
        dplyr::mutate(experiment = i) %>%
        saveRDS(file = performance_file)
}