# calculate densities -----------------------------------------------------

# get total sample volume for every site-collection_date
total_sample_volumes <- dat2 %>%
  group_by(site, col_date, al, vol) %>%
  summarise(count = n()) %>%
  select(-count) %>%
  ungroup() %>%
  group_by(site, col_date) %>%
  summarise(tot_sam_vol = sum(vol),
            count = n()) %>%
  ungroup() %>%
  mutate(sitedate = paste0(site, col_date)) %>%
  select(-count)

# sum total species counts for each of the samples
total_species_counts <- dat2 %>%
  group_by(site, col_date, sp_code) %>%
  summarise(sp_counts_sample = sum(total),
            count = n()) %>%  # counts are for if they were in multiple aliquots
  ungroup() %>%
  mutate(sitedate = paste0(site, col_date)) %>%
  select(-count)

# merge total_sample_volumes with total_species_counts for density calculations
sp_counts_per_samplevol <- left_join(total_species_counts, total_sample_volumes,
                                     by = "sitedate") %>%
  select(-site.y, -col_date.y) %>%
  rename(site = site.x,
         col_date = col_date.x)

sp_den_per_sample <- sp_counts_per_samplevol %>%
  mutate(density = sp_counts_sample/tot_sam_vol,
         year = year(col_date),
         month = month(col_date, label = TRUE),
         ym = paste0(year, '-', month),
         ID = paste0(sp_code, '_', col_date, '_', site))

den_primer <- sp_den_per_sample %>%
  select(site, ym, sp_code, density) %>%
  tidyr::pivot_wider(names_from = sp_code,
                     values_from = 'density')

# replace all NAs with blanks
# this is a new dataframe because it will make everything factors
# this is JUST to export the data into a csv without NAs
export <- sapply(den_primer, as.character)
export[is.na(export)] <- " "
export <- as.data.frame(export)
write.csv(export, here::here('output', 'data', 'densities_species.csv'))

code_ready <- codes %>%
  select(code, species_description, hi_lvl_cat, hi_lvl_desc) %>%
  rename(sp_code = code,
         sp_desc = species_description,
         hi_lvl_code = hi_lvl_cat) %>%
  mutate(sp_code = as.character(sp_code),
         hi_lvl_code = as.character(hi_lvl_code))

code_flip <- as.data.frame(t(code_ready)) %>%
  row_to_names(row_number = 1) %>%
  write_csv(here::here('output', 'data', 'tags.csv'))
