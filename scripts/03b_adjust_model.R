# Adjust a model using all the training samples

library(tidymodels)
library(doParallel)
library(ensurer)
library(readr)
library(vip)
library(magrittr)



#---- Read data ----

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



#---- Data split ----

data_train <- data_tb
set.seed(42)
data_folds <- rsample::bootstraps(data_train)



#---- Recipe ----

ranger_recipe <- recipes::recipe(formula = log_def ~ .,
                                 data = data_tb) %>%
    recipes::update_role(id, UF, CD_GEOCMU, NM_MUNICIP, 
                         longitude, latitude, 
                         def, slope, year,
                         new_role = "other")
print("Variables...")
ranger_recipe %>%
    summary() %>%
    dplyr::arrange(dplyr::desc(role), variable) %>%
    print(n = Inf)



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
tune::show_best(ranger_tune, metric = "rmse")
tune::show_best(ranger_tune, metric = "rsq")
autoplot(ranger_tune)



#---- Finalize ----

param_final <- ranger_tune %>%
    select_best(metric = "rmse")

ranger_workflow <- ranger_workflow %>%
    finalize_workflow(param_final)
print("Final model's hyper-parameters...")
print(ranger_workflow)



#---- Evaluate ----

# NOTE: We're using ALL the samples for training. 



#---- Importance of variables ----

imp_spec <- ranger_spec %>%
    finalize_model(select_best(ranger_tune,
                               metric = "rmse")) %>%
    set_engine("ranger",
               importance = "permutation")

workflow_fit <-  workflow() %>%
    add_recipe(ranger_recipe) %>%
    add_model(imp_spec) %>%
    fit(data_train) %>%
    pull_workflow_fit()
workflow_fit %>%
    magrittr::extract2("fit") %>%
    magrittr::extract2("variable.importance") %>%
    as.list() %>%
    as_tibble() %>%
    write_csv(file = "./results/variable_importance.csv")

workflow_fit %>%
    vip(aesthetics = list(alpha = 0.8,
                          fill = "midnightblue"))
ggsave("./results/feature_importance.png")



#---- Fitting and using ----

final_model <- parsnip::fit(ranger_workflow,
                            data_tb)
saveRDS(final_model,
        file = "./results/final_model.rds")

# Save the prediction on the original data.
fake_new_data <- ranger_recipe %>%
    recipes::prep(raw_tb) %>%
    recipes::juice()
fake_pred <- predict(final_model, fake_new_data)
new_data_tb <- raw_tb %>%
    dplyr::bind_cols(fake_pred) %>%
    dplyr::mutate(pred_def = exp(.pred)) %>%
    (function(x) {
        saveRDS(x, file = "./results/new_data_tb.rds")
        readr::write_csv(x, file = "./results/new_data_tb.csv")
        return(x)
    })
