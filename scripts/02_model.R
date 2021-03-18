library(tidymodels)
library(doParallel)
library(ensurer)
library(readr)
library(vip)

data_tb <- "./data/samples/data_tb.rds" %>%
    readRDS()  %>%
    #sample_n(1000) %>%
    filter(def_2019 > 0)  %>% 
    dplyr::mutate(log_def_2019 = log(def_2019)) %>%
    ensure_that(nrow(.) > 0,
                err_desc = "No data found!")



#---- Data split ----

set.seed(123)
data_split <- rsample::initial_split(data_tb)
data_train <- rsample::training(data_split)
data_test  <- rsample::testing(data_split)

set.seed(234)
data_folds <- rsample::bootstraps(data_train)



#---- Recipe ----

ranger_recipe <- recipes::recipe(formula = log_def_2019 ~ .,
                                 data = data_tb) %>%
    recipes::update_role(longitude, latitude, 
                         new_role = "position") %>%
    recipes::update_role(id, UF, CD_GEOCMU, NM_MUNICIP, 
                         longitude, latitude, def_2019,
                         new_role = "other")



#---- Model ----

ranger_spec <- parsnip::rand_forest(mtry = tune::tune(),
                                    min_n = tune::tune(),
                                    trees = 2000) %>%
                                    #trees = 200) %>%
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
#autoplot(ranger_tune)



#---- Finalize ----

param_final <- ranger_tune %>%
    select_best(metric = "rmse")

ranger_workflow <- ranger_workflow %>%
    finalize_workflow(param_final)



#---- Evaluate ----

data_fit <- ranger_workflow %>%
    tune::last_fit(data_split)

test_performance <- data_fit %>%
    tune::collect_metrics()
saveRDS(test_performance, 
        file = "test_performance.rds") 

test_predictions <- data_fit %>%
    tune::collect_predictions()
saveRDS(test_predictions,
        file = "test_predictions.rds")

test_predictions %>%
    ggplot2::ggplot(ggplot2::aes(log_def_2019, 
                                 .pred)) +
    ggplot2::geom_abline(lty = 2,
                color = "gray50") +
    ggplot2::geom_point(alpha = 0.5,
               color = "midnightblue") +
    ggplot2::coord_fixed() +
    ggplot2::labs(title = sprintf("Test data (n = %s)", 
                                  nrow(test_predictions))) +
    ggplot2::xlab("ln(Deforestation 2019)") +
    ggplot2::ylab("Prediction")
ggplot2::ggsave("log_def2019_vs_prediction.png")
    

#---- Importance of variables ----

imp_spec <- ranger_spec %>%
    finalize_model(select_best(ranger_tune,
                               metric = "rmse")) %>%
    set_engine("ranger",
               importance = "permutation")

workflow() %>%
    add_recipe(ranger_recipe) %>%
    add_model(imp_spec) %>%
    fit(data_train) %>%
    pull_workflow_fit() %>%
    vip(aesthetics = list(alpha = 0.8,
                          fill = "midnightblue"))
ggsave("feature_importance.png")



#---- Fitting and using ----

final_model <- parsnip::fit(ranger_workflow,
                            data_tb)
saveRDS(final_model,
        file = "final_model.rds")

# Save the prediction on the original data.
fake_new_data <- ranger_recipe %>%
    recipes::prep(data_tb) %>%
    recipes::juice()
fake_pred <- predict(final_model, fake_new_data)
new_data_tb <- data_tb %>%
    dplyr::bind_cols(fake_pred) %>%
    dplyr::mutate(pred_def_2019 = exp(.pred)) %>%
    (function(x) {
        saveRDS(x, file = "new_data_tb.rds")
        readr::write_csv(x, file = "new_data_tb.csv")
        return(x)
    })