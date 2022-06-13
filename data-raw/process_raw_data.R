library(magrittr)

ds1 <- readr::read_delim("data-raw/test_data.txt") %>%
  tidyr::pivot_longer(cols=everything()) %>%
  dplyr::mutate(log10_value = log10(value)) %>%
  dplyr::group_by(name) %>%
  dplyr::mutate(linear_outlier = rstatix::is_outlier(value)) %>%
  dplyr::mutate(log10_outlier = rstatix::is_outlier(log10_value)) %>%
  dplyr::ungroup()

ds2 <- readr::read_delim("data-raw/ds2.csv") %>%
  dplyr::mutate(difference = Treatment-Placebo) %>%
  tidyr::pivot_longer(cols=c(Treatment, Placebo)) %>%
  dplyr::mutate(log10_value = log10(value)) %>%
  dplyr::mutate(linear_outlier = FALSE) %>%
  dplyr::mutate(log10_outlier = FALSE)

saveRDS(ds1, file = "data/ds1.rds")
saveRDS(ds2, file = "data/ds2.rds")
