# Build dashboard datasets




# Sections:
# 1. Get Combined Indicator Data
# 2. Clean line, leaflet, and dumbbell data
# 3. Load and clean cases per 100K and positivity rate data



pacman::p_load(swatches, covidcastR, extrafont, dplyr, glue)


###############################
# Get Combined Indiator data
###############################


# census bureau's tiger-line msa shapefiles
us_msa_tiles <- sf::read_sf(glue("{rprojroot::find_rstudio_root_file()}/data/shapefiles/msa-2019/tl_2019_us_cbsa.shp"))

# leaflet requires EPSG:4326
msa_tiles <- sf::st_transform(us_msa_tiles, 4326)
ind_msa_tiles <- msa_tiles %>% 
      filter(stringr::str_detect(NAME, "IN")) %>% 
      janitor::clean_names() %>% 
      mutate(# remove state abbrev from msa names
            name = stringr::str_remove_all(name, "([A-Z][A-Z]-)*"),
            name = stringr::str_remove_all(name, ", [A-Z]*"))

# get historic combined indicator values I have saved
combined_index_complete <- readr::read_csv(glue("{rprojroot::find_rstudio_root_file()}/data/covidcast-msa-complete.csv"))

# date of data I have
ci_comp <- combined_index_complete %>% 
      filter(time_value == max(time_value)) %>%
      slice(1) %>% 
      pull(time_value)

# get data update
c <- 0
while (TRUE) {
      combined_index_yday <- try({
            # try dates from today until ci_comp date reached
            try_date <- lubridate::today() - c
            
            ind_msa_tiles %>% 
                  # cbsafp is the msa id
                  select(name, cbsafp) %>% 
                  group_by(name) %>% 
                  tidyr::nest() %>% 
                  # get the combined indicator value for each msa if one is available
                  mutate(data = purrr::map(data, ~suppressMessages(
                        covidcast_signal(
                              data_source = "indicator-combination",
                              signal = "nmf_day_doc_fbc_fbs_ght",
                              start_day = try_date,
                              end_day = try_date,
                              geo_type = "msa",
                              geo_values = .x$cbsafp)
                  ))) %>% 
                  tidyr::unnest(cols = "data") %>% 
                  ungroup() %>% 
                  select(-geo_value, -direction, -sample_size)
            
      }, silent = TRUE)
      # stay in loop until I get some new data or ci_comp date reached
      if (class(combined_index_yday) != "try-error"){
            break
      } else if (try_date <= ci_comp) {
            break
      } else {
            c <- c + 1
      }
}


ci_yday <- combined_index_yday %>% 
      filter(time_value == max(time_value)) %>% 
      slice(1) %>% 
      pull(time_value)

# compare dates first before adding new data
if (ci_yday != ci_comp) {
      combined_index_all <- combined_index_complete %>%
            bind_rows(combined_index_yday)
      
      readr::write_csv(combined_index_all, glue("{rprojroot::find_rstudio_root_file()}/data/covidcast-msa-complete.csv"))
} else {
      combined_index_all <- combined_index_complete
}



###########################################
# Clean line, leaflet, and dumbbell data
###########################################


# colorNumeric creates a color function where input is a vector with values in the domain and outputs hex colors. 30 hex values.

# combined indicator values don't get much higher than 3.00. Anything higher will have the darkest value.
pal <- leaflet::colorNumeric("YlOrRd",
                             domain = seq(0.00, 3.00, by = 0.10))

# Clean names
ci_clean <- combined_index_all %>% 
      mutate(# remove state abbrev from msa names
            name = stringr::str_remove_all(name, "([A-Z][A-Z]-)*"),
            name = stringr::str_remove_all(name, ", [A-Z]*")
      ) %>% 
      select(-issue, -lag)

# leaflet map: add popup text, styling
ci_leaf <- ci_clean %>%
      # create popup text
      mutate(value = round(value, 2),
             color = pal(value),
             # white map background so darkening the palette some
             color = unclass(prismatic::clr_darken(color,
                                                   shift = 0.20)),
             # dark-red background for high values needs white text
             value_text = ifelse(value >= 2,
                                 glue("<b style='background-color:{color}; font-family:Roboto; font-size:15px; color:white'>{value}</b>"),
                                 glue("<b style='background-color:{color}; font-family:Roboto; font-size:15px'>{value}</b>")),
             popup = glue("<b style= 'font-family:Roboto; font-size:15px'>{name}</br>Combined Indicator</b>: {value_text}")) 

# dumbbell: add 95% CI, styling
ci_db <- ci_clean %>% 
      mutate(upper = round(value + (1.96 * stderr), 2),
             lower = round(value - (1.96 * stderr), 2),
             value = round(value, 2),
             upper_col = unclass(prismatic::clr_darken(pal(upper),
                                                       shift = 0.20)),
             lower_col = unclass(prismatic::clr_darken(pal(lower),
                                                       shift = 0.20)),
             # contrast text against label color
             text_col = ifelse(value > 2, 'white', 'black'))

ci_clean_line <- ci_clean %>% 
   select(name, time_value, value) %>% 
   mutate(value = round(value, 2))

ci_clean_db <- ci_db %>% 
      select(name, time_value, value, text_col, lower, upper, lower_col, upper_col) %>% 
      filter(time_value == max(time_value)) %>%
      mutate(name = forcats::as_factor(name) %>%
                   forcats::fct_reorder(value))
ci_clean_leaf <- ci_leaf %>% 
      select(name, time_value, color, popup) %>% 
      filter(time_value == max(time_value)) %>%
      left_join(ind_msa_tiles, by = "name") %>% 
      sf::st_as_sf()

readr::write_rds(ci_clean_line, glue("{rprojroot::find_rstudio_root_file()}/data/dash-ci-line.rds"))
readr::write_rds(ci_clean_leaf, glue("{rprojroot::find_rstudio_root_file()}/data/dash-ci-leaf.rds"))
readr::write_rds(ci_clean_db, glue("{rprojroot::find_rstudio_root_file()}/data/dash-ci-db.rds"))



##########################################################
# Load and clean cases per 100k and positivity rate data
##########################################################


# Cases per 100K population and MSA positivity rates (current and hisoric)
case_pos_current <- readr::read_csv(glue("{rprojroot::find_rstudio_root_file()}/data/msa-cases100-posrate-current.csv"))
case_pos_hist <- readr::read_csv(glue("{rprojroot::find_rstudio_root_file()}/data/msa-cases100-posrate-historic.csv"))

# cases per 100k palette
moody <- swatches::read_palette(glue("{rprojroot::find_rstudio_root_file()}/palettes/Moody Blooms.ase"))[c(1,4,5,8)]
# positive test rate palette function
pos_pal <- leaflet::colorNumeric("YlOrRd",
                                 domain = seq(0.00, 0.20, by = 0.05),
                                 na.color = "#800026")

# color palettes
posrate_col <- case_pos_current %>% 
   select(msa, pos_rate) %>% 
   filter(!is.na(pos_rate)) %>%
   mutate(pos_color = pos_pal(pos_rate),
          pos_color_light = unclass(prismatic::clr_lighten(pos_color, shift = .40))) %>% 
   select(-pos_rate)

cases_col <- case_pos_current %>% 
   select(msa, cases_100k) %>% 
   mutate(cases_100k = round(cases_100k, 0),
          cases_color = case_when(cases_100k < 1 ~ moody[[1]],
                                  between(cases_100k, 1, 9) ~ moody[[2]],
                                  between(cases_100k, 10, 24) ~ moody[[3]],
                                  cases_100k >= 25 ~ moody[[4]]),
          cases_color = unclass(prismatic::clr_darken(cases_color, shift = .50)),
          cases_color_light = unclass(prismatic::clr_lighten(cases_color, shift = .60))) %>%  
   select(msa, cases_color, cases_color_light)


# cases per 100k and positivity rate trend data
pos_hist <- case_pos_hist %>%
      filter(!is.na(pos_rate)) %>%
      select(end_date = date, msa, pos_rate) %>%
      group_by(msa) %>%
      summarize(pos_list = mapply(function (end_date, pos_rate)
      {list(endDate = end_date, posRate = pos_rate)},
      end_date, pos_rate,
      SIMPLIFY = FALSE)) %>% 
      tidyr::nest() %>%
      mutate(data = purrr::map(data, ~as.list(.x))) %>%
      rename(posList = data)

cases_hist <- case_pos_hist %>%  
      select(date, msa, cases_100k) %>%
      mutate(cases_100k = round(cases_100k, 2)) %>% 
      group_by(msa) %>% 
      summarize(cases_list = mapply(function (date, cases)
      {list(date = date, cases = cases)},
      date, cases_100k,
      SIMPLIFY = FALSE)) %>% 
      tidyr::nest() %>%
      mutate(data = purrr::map(data, ~as.list(.x))) %>%
      rename(casesList = data)

# current cases per 100K and positivity rates
current_dat <- case_pos_current %>%
      select(msa, cases_100k, pos_rate) %>% 
      mutate(cases_100k = round(cases_100k, 0))


# combine everything into a single tbl
react_dat <- purrr::reduce(list(current_dat, cases_hist, pos_hist,
                                cases_col, posrate_col),
                           left_join, by = "msa") %>% 
   arrange(desc(cases_100k))

readr::write_rds(react_dat, glue("{rprojroot::find_rstudio_root_file()}/data/dash-case-pos.rds"))


# # Get the data dates
# cases_current_date <- case_pos_current %>% 
#    slice(n()) %>% 
#    select(cases_date = date)
# indmich_pos_current_date <- case_pos_current %>% 
#    filter(!stringr::str_detect(msa, "Chicago")) %>% 
#    slice(n()) %>% 
#    pull(date)
# chi_pos_current_date <- case_pos_current %>% 
#    filter(stringr::str_detect(msa, "Chicago")) %>% 
#    pull(date)
