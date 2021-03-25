library(tidymodels)
library(doParallel)
library(ensurer)
library(readr)
library(vip)
library(magrittr)



raw_tb <- "./data/samples/data_tb.rds" %>%
    readRDS()  %>%
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
    #------------------------
    # # NOTE: Only for testing code!
    # dplyr::group_by(year) %>%
    # dplyr::sample_n(1000) %>%
    # dplyr::ungroup() %>%
    #------------------------
    ensurer::ensure_that(nrow(.) > 0,
                err_desc = "No data found!")



#---- Data split ----

set.seed(123)
data_split <- rsample::initial_split(data_tb)
data_train <- rsample::training(data_split)
data_test  <- rsample::testing(data_split)

set.seed(456)
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
#tune::show_best(ranger_tune, metric = "rmse")
#tune::show_best(ranger_tune, metric = "rsq")
#autoplot(ranger_tune)



#---- Finalize ----

param_final <- ranger_tune %>%
    select_best(metric = "rmse")

ranger_workflow <- ranger_workflow %>%
    finalize_workflow(param_final)
print("Final model's hyper-parameters...")
print(ranger_workflow)


#---- Evaluate ----

data_fit <- ranger_workflow %>%
    tune::last_fit(data_split)

test_performance <- data_fit %>%
    tune::collect_metrics()
print("Performance...")
print(test_performance, n = Inf)
saveRDS(test_performance, 
        file = "./results/test_performance.rds") 

test_predictions <- data_fit %>%
    tune::collect_predictions()
saveRDS(test_predictions,
        file = "./results/test_predictions.rds")
write_csv(test_predictions,
          file = "./results/test_predictions.csv")

test_predictions %>%
    ggplot2::ggplot(ggplot2::aes(log_def, 
                                 .pred)) +
    ggplot2::geom_abline(lty = 2,
                color = "gray50") +
    ggplot2::geom_point(alpha = 0.5,
               color = "midnightblue") +
    ggplot2::coord_fixed() +
    ggplot2::labs(title = sprintf("Test data (n = %s)", 
                                  nrow(test_predictions))) +
    ggplot2::xlab("ln(Deforestation)") +
    ggplot2::ylab("Prediction")
ggplot2::ggsave("./results/log_def_vs_prediction.png")
    

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
