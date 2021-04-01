# Estimate the mean RMSE of experiments.

library(dplyr)

cv_dir <- "./results/cross-validation"
stopifnot(dir.exists(cv_dir))

crossvalidation_tb <- cv_dir %>%
    list.files(pattern = "performance_test*.",
               full.names = TRUE) %>%
    tibble::as_tibble() %>%
    dplyr::rename(file_path = value) %>%
    dplyr::mutate(data = purrr::map(file_path, readRDS)) %>%
    dplyr::pull(data) %>%
    dplyr::bind_rows()
crossvalidation_tb %>%
    readr::write_csv(file = "./results/crossvalidation_tb.csv")

crossvalidation_tb %>%
    dplyr::group_by(.metric) %>%
    dplyr::summarize(mean = mean(.estimate))
