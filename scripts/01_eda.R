library(ensurer)
library(ggplot2)
library(janitor)
library(pillar)
library(readxl)
library(sf)
library(skimr)
library(tidyverse)

source("./scripts/00_util.R")

point_shp <- "./data/samples/Random_Forest.shp"
town_shp  <- "./data/towns/br_munic.shp"
xlx_file  <- "./data/samples/Random_Forest.xlsx"
stopifnot(file.exists(point_shp))
stopifnot(file.exists(town_shp))
stopifnot(file.exists(xlx_file))

#---- Read the data ----

town_sf <- town_shp %>%
    sf::read_sf() %>%
    sf::st_transform(crs = 4326)

point_tb <- point_shp %>%
    sf::read_sf() %>%
    sf::st_transform(crs = 4326) %>%
    add_coords() %>%
    dplyr::select(ID, longitude, latitude) %>%
    ensurer::ensure_that(length(unique(.$ID)) == nrow(.),
                         err_desc = "Duplicated ids in shp!") %>%
    sf::st_join(y = town_sf) %>%
    sf::st_set_geometry(NULL) %>%
    tibble::as_tibble() %>%
    dplyr::rename(id = ID)

sheet_names <- readxl::excel_sheets(xlx_file)
data_ls <- list()
for (my_sheet in sheet_names) {
    data_ls[[my_sheet]] <- xlx_file %>%
        readxl::read_excel(sheet = my_sheet, 
                           .name_repair = janitor::make_clean_names) %>%
        tibble::as_tibble() %>%
        magrittr::set_colnames(value = c("id", "def_lastyear", "def_cum_2year", 
                                         "def_cum_4year", "dist_road", 
                                         "dist_hydr", "dist_rodhyd", 
                                         "dist_1per", "dist_2per", 
                                         "heat_lastyear", "slope", "prot_area", 
                                         "x",  "def")) %>%
        dplyr::select_if(all_na) %>%
        dplyr::mutate(year = as.integer(my_sheet)) %>%
        ensurer::ensure_that(length(unique(.$id)) == nrow(.),
                             err_desc = sprintf("Duplicated ids in data %s!", 
                                                my_sheet))
}

data_tb <- data_ls %>%
    dplyr::bind_rows() %>%   
    dplyr::left_join(point_tb, by = "id")

# Plot the variables
plot_tb <- data_tb %>%
    dplyr::select(-id, -longitude, -latitude, -NM_MUNICIP, -CD_GEOCMU, -UF) %>%
    tidyr::pivot_longer(cols = !tidyselect::matches("^year$"))

plot_tb %>%
    dplyr::mutate(name = stringr::str_c(name, "_", year)) %>%
    dplyr::select(-year) %>%
    ggplot2::ggplot(ggplot2::aes(x = value)) + 
    ggplot2::facet_wrap(~name, scales = "free_x") + 
    ggplot2::geom_histogram()
ggsave(filename = "./data/samples/histogram.png",
       width = 297,
       height = 210,
       units = "mm")

plot_tb %>%
    dplyr::filter(name == "def") %>%
    dplyr::mutate(name = stringr::str_c("ln_", name, "_", year),
                  log_value = log(value)) %>%
    dplyr::select(-year) %>%
    ggplot2::ggplot(ggplot2::aes(x = log_value)) + 
    ggplot2::geom_histogram(aes(y = ..density..)) +
    ggplot2::geom_density(alpha=.2, fill="#FF6666") +
    ggplot2::facet_wrap(~name, scales = "free_x")
ggplot2::ggsave(filename = "./data/samples/histogram_ln10.png",
       width = 297,
       height = 210,
       units = "mm")

# This correlogram shows correlation coefficients for all pairs of variables 
# (with more intense colors for more extreme correlations), and correlations 
# not significantly different from 0 are represented by a white box
corrplot2(
    data = data_tb %>%
        dplyr::select(-id, -longitude, -latitude) %>%
        dplyr::select(where(is.numeric)) %>%
        dplyr::filter(def > 0,
                      year == 2019) %>%
        dplyr::select(-year) %>%
        dplyr::mutate(log_def = log(def)),
    method = "pearson",
    sig.level = 0.05,
    order = "original",
    diag = FALSE,
    type = "upper",
    tl.srt = 75
)
corrplot2(
    data = data_tb %>%
        dplyr::select(-id, -longitude, -latitude) %>%
        dplyr::select(where(is.numeric)) %>%
        dplyr::filter(def > 0,
                      year == 2020) %>%
        dplyr::select(-year) %>%
        dplyr::mutate(log_def = log(def)),
    method = "pearson",
    sig.level = 0.05,
    order = "original",
    diag = FALSE,
    type = "upper",
    tl.srt = 75
)

# Save data
saveRDS(data_tb, 
        file = "./data/samples/data_tb.rds")
            