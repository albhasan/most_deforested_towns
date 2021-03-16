library(tidymodels)
library(doParallel)
library(vip)

data_tb <- "./data/samples/data_tb.rds" %>%
    readRDS()  #%>%
    #filter(def_2019 > 0)  %>% 
    #sample_n(1000)


set.seed(123)
data_split <- initial_split(data_tb)
data_train <- training(data_split)
data_test  <- testing(data_split)

set.seed(234)
data_folds <- bootstraps(data_train)

ranger_recipe <- recipe(formula = def_2019 ~ .,
                        data = data_tb) %>%
    update_role(longitude, latitude, 
                new_role = "position") %>%
    update_role(id, UF, CD_GEOCMU, NM_MUNICIP, 
                new_role = "ids") 
    #%>% step_log(all_numeric())

ranger_spec <- rand_forest(mtry = tune(),
                           min_n = tune(),
                           trees = 1000) %>%
    set_mode("regression") %>%
    set_engine("ranger")

ranger_workflow <- workflow() %>%
    add_recipe(ranger_recipe) %>%
    add_model(ranger_spec)

doParallel::registerDoParallel()
ranger_tune <- tune_grid(ranger_workflow,
                         resamples = data_folds,
                         grid = 11)

show_best(ranger_tune, metric = "rmse")
show_best(ranger_tune, metric = "rsq")

autoplot(ranger_tune)

final_rf <- ranger_workflow %>%
    finalize_workflow(select_best(ranger_tune))

data_fit <- last_fit(final_rf, data_split)
data_fit

collect_metrics(data_fit)

predictions <- collect_predictions(data_fit)
saveRDS(predictions, "predictions.rds")

predictions %>%
    ggplot(aes(def_2019, .pred)) +
    geom_abline(lty = 2,
                color = "gray50") +
    geom_point(alpha = 0.5,
               color = "midnightblue") +
    coord_fixed()
ggsave("pred_vs_def2019.png")

imp_spec <- ranger_spec %>%
    finalize_model(select_best(ranger_tune)) %>%
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
