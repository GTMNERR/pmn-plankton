# load-data ---------------------------------------------------------------

dat <- readxl::read_xlsx(here::here('data', 'SWMP_Plankton_2020_SrayPRIMERXFORM_042621v2.xlsx'),
                         sheet = 'RawData-2020') %>%
      janitor::clean_names()

codes <- readxl::read_xlsx(here::here('data', 'SWMP_Plankton_2020_SrayPRIMERXFORM_042621v2.xlsx'),
                           sheet = 'species_codes_skd') %>%
        janitor::clean_names()

dplyr::glimpse(dat)

# check columns
unique(dat$vol)
## need to remove '??' entry from `vol` and then reassign to numeric
unique(dat$tot_mag)
## need to lower 'x' in magnification entries

dat2 <- dat %>%
        select(-set_date, -col_met, -id_date, -id_name) %>%
        filter(vol != '??' & tot_mag != '400x') %>% # remove '??' volumes and '400x' entries
        mutate(tot_mag = tolower(tot_mag), # standardize entries
               vol = as.numeric(vol), # set volume col to numeric
               sp_code = as.character(sp_code), # set species codes to character, since identifier
               ) %>%
        filter(al_notes == 0) # keep the al_notes with `0`, these are diversity counts