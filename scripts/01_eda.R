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

data_tb <- xlx_file %>%
    readxl::read_excel(.name_repair = janitor::make_clean_names) %>% 
    dplyr::rename(
        "id"            = "id",
        "def_lastyear"  = "desmatamento_ano_anterior_km2",
        "def_cum_2year" = "desmatamento_acumulado_2_anos_anteriores_km2",
        "def_cum_4year" = "desmatamento_acumulado_4_anos_anteriores_km2",
        "dist_road"     = "distancia_rodovia_km",
        "dist_hydr"     = "distancia_hidrovia_km",
        "dist_rodhyd"   = "distancia_rodovia_hidrovia_km",
        "dist_1per"     = "distancia_a_ponto_de_grade_1_percent_desmatamentono_ano_anterior_km",
        "dist_2per"     = "distancia_a_ponto_de_grade_2_percent_desmatamento_acumulado_em_2_anos_km",
        "heat_lastyear" = "focos_de_calor_ano_anterior",
        "slope"         = "declividade",
        "prot_area"     = "area_pa",
        "x"             = "x",
        "def_2019"      = "desmatamento_2019_km2"
    ) %>%
    dplyr::select_if(all_na) %>%
    ensurer::ensure_that(length(unique(.$id)) == nrow(.),
                         err_desc = "Duplicated ids in data!") %>%
    dplyr::left_join(point_tb, 
                     by = "id")

# Plot the predicted variable with and wihout a lograithic transformation.
data_tb %>%
    dplyr::select(-id, -longitude, -latitude, -NM_MUNICIP, -CD_GEOCMU, -UF) %>%
    tidyr::pivot_longer(cols = everything()) %>%
    ggplot2::ggplot(ggplot2::aes(x = value)) + 
    ggplot2::facet_wrap(~name, scales = "free_x") + 
    ggplot2::geom_histogram()
ggsave(filename = "./data/samples/histogram.png",
       width = 297,
       height = 210,
       units = "mm")

data_tb %>%
    dplyr::select(-id, -longitude, -latitude, -NM_MUNICIP, -CD_GEOCMU, -UF) %>%
    tidyr::pivot_longer(cols = everything()) %>%
    dplyr::mutate(name = stringr::str_c("log10_", name),
           log_value = log10(value)) %>%
    ggplot2::ggplot(ggplot2::aes(x = log_value)) + 
    ggplot2::geom_histogram(aes(y = ..density..)) +
    ggplot2::geom_density(alpha=.2, fill="#FF6666") +
    ggplot2::facet_wrap(~name, scales = "free_x")
ggplot2::ggsave(filename = "./data/samples/histogram_log10.png",
       width = 297,
       height = 210,
       units = "mm")

data_tb %>%
    ggplot2::ggplot(ggplot2::aes(x = log10(def_2019))) + 
    ggplot2::geom_histogram(aes(y = ..density..))+
    ggplot2::geom_density(alpha=.2, fill="#FF6666")  

# This correlogram shows correlation coefficients for all pairs of variables 
# (with more intense colors for more extreme correlations), and correlations 
# not significantly different from 0 are represented by a white box
corr_plot <- corrplot2(
    data = data_tb %>%
        dplyr::select(-id, -longitude, -latitude) %>%
        dplyr::select(where(is.numeric)),
    method = "pearson",
    sig.level = 0.05,
    order = "original",
    diag = FALSE,
    type = "upper",
    tl.srt = 75
)

# The same plot, taking the log10 of the outcome variable.
corrplot2(
    data = data_tb %>%
        dplyr::select(-id, -longitude, -latitude) %>%
        dplyr::select(where(is.numeric)) %>%
        dplyr::filter(def_2019      > 0) %>%
        dplyr::mutate(log_def_2019      = log10(def_2019)) %>%
        dplyr::select(-def_2019),
    method = "pearson",
    sig.level = 0.05,
    order = "original",
    diag = FALSE,
    type = "upper",
    tl.srt = 75
)

# The same plot, taking the log10 of the variables.
corrplot2(
    data = data_tb %>%
        dplyr::select(-id, -longitude, -latitude) %>%
        dplyr::select(where(is.numeric)) %>%
        dplyr::filter(
           def_lastyear  > 0,
           def_cum_2year > 0,
           def_cum_4year > 0,
           dist_road     > 0,
           dist_hydr     > 0,
           dist_rodhyd   > 0,
           dist_1per     > 0,
           dist_2per     > 0,
           heat_lastyear > 0,
           def_2019      > 0) %>%
        dplyr::mutate(def_lastyear  = log10(def_lastyear), 
               def_cum_2year = log10(def_cum_2year),
               def_cum_4year = log10(def_cum_4year),
               dist_road     = log10(dist_road),
               dist_hydr     = log10(dist_hydr),
               dist_rodhyd   = log10(dist_rodhyd),
               dist_1per     = log10(dist_1per),
               dist_2per     = log10(dist_2per),
               heat_lastyear = log10(heat_lastyear),
               def_2019      = log10(def_2019)),
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
            