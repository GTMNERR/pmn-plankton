library(tidyverse)
library(here)
library(janitor)
library(readxl)
library(cowplot)

# ----01a load in data----
dat <- readxl::read_xlsx(here::here('data', 'GTMNERR_Plankton_IK.xlsx'), sheet = "raw") %>%
  janitor::clean_names()
codes <- readxl::read_xlsx(here::here('data', 'GTMPlankton_SpeciesCodes_IK.xlsx'), sheet = "SpeciesCodes") %>%
  janitor::clean_names()

# ----01b make sure species codes are read as characters and not numeric----
dat <- dat %>% dplyr::mutate(sp_code = as.character(sp_code))
codes <- codes %>%
  dplyr::mutate(sp_code = as.character(code)) %>%
  dplyr::select(-code)

# ----02 combine data so species description and group comes from 'codes'----
dat2 <- codes %>%
  dplyr::select(-notes) %>% # remove unnecessary columns
  dplyr::right_join(dat, by = "sp_code") %>%
  dplyr::select(-col_met) # remove unnecessary columns

# ----03 calculate number of days between collection and identification----
dat2 <- dat2 %>% dplyr::mutate(proc_time = id_date - col_date)

# ----04 pull out single species counts and calculate density----
single_density <- dat2 %>%
  dplyr::filter(al_notes == 1) %>%
  dplyr::rename(total_count = total,
                total_volume = vol) %>%
  dplyr::mutate(density = total_count / total_volume) %>%
  dplyr::select(site, col_date, total_volume, sp_code, total_count, density)

# ----05 pull out diversity counts al_notes 0 & 2, pivot wider, add zeros----
diver_density <- dat2 %>%
  dplyr::filter(al_notes != 1) %>%
  dplyr::group_by(site, col_date, sp_code, al) %>%
  dplyr::summarise(total_volume = sum(vol),
                   total_count = sum(total)) %>%
  tidyr::pivot_wider(names_from = sp_code,
                     values_from = total_count,
                     id_cols = c("site", "col_date", "al", "total_volume")) %>%
  dplyr::ungroup()
# build in zeros into NAs
# this is to make sure that volumes of the full sample are used in the diversity counts, including species that were not counted in both aliquots (but present). Data sheets are only detecting and counting presence
diver_density[is.na(diver_density)] <- 0

# ----06 summarise everything in diversity, remove aliquot, pivot longer, calculate density----
diver_density2 <- diver_density %>%
  dplyr::group_by(site, col_date) %>%
  dplyr::summarise_all(sum) %>%
  dplyr::select(-al) %>%
  tidyr::pivot_longer(-c(site, col_date, total_volume), #this keeps these columns in the data
                      names_to = "sp_code",
                      values_to = "total_count") %>%
  dplyr::mutate(density = total_count / total_volume) %>%
  dplyr::ungroup()

# ----07 combine with single species dataframe----
density <- bind_rows(diver_density2, single_density)

# ----08 clean up density data and combine with code----
density_full <- density %>%
  dplyr::right_join(codes, by = "sp_code") %>%
  dplyr::select(-notes, -total_volume, -total_count) %>%
  dplyr::filter(!is.na(site)) # remove NA rows in site - not sure why this happened

# ----09 cleanup dataframes by removing from environment----
rm(diver_density, diver_density2, single_density, density)

# ----10 summarize site by group----
density_full %>%
  dplyr::group_by(site, group) %>%
  dplyr::summarise(mean = mean(density, na.rm = TRUE),
            sd = sd(density, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  ggplot() + # make plot!
  geom_bar(aes(x = group, y = mean, fill = group),
           stat = "identity",
           position = "stack") +
  facet_wrap(.~site, scales = "free_y") +
  scale_x_discrete(name = "Plankton Group") +
  scale_y_continuous(name = "Average density (#cells/mL)") +
  scale_fill_discrete(name = "Plankton Group") +
  cowplot::theme_cowplot() +
  theme(axis.text.x = element_blank(),
        legend.position = "bottom")
ggsave(here::here('output', 'density_group_site_bar.pdf'), height = 8.5, width = 11, dpi = 120)

write.csv(density_full, here::here('output', 'densities.csv'))

# ----11 lake middle----
View(density_full %>%
  dplyr::filter(site == "LM") %>%
  dplyr::group_by(species_description) %>%
  dplyr::summarise(mean = mean(density, na.rm = TRUE),
                   sd = sd(density, na.rm = TRUE)) %>%
  dplyr::ungroup()
)
# lake middle timeseries
density_full %>%
  dplyr::filter(site == c("LM", "GR")) %>%
  dplyr::group_by(col_date, site) %>%
  dplyr::summarise(sum = sum(density)) %>%
  ggplot() +
  geom_point(aes(x = col_date, y = log(sum), color = site)) +
  geom_line(aes(x = col_date, y = log(sum), color = site))
