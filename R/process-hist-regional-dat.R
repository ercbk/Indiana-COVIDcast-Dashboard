# Processes historic test data

# Similiar to process-regional-dat.R

# Notes:
# 1. Make sure if historic data gets updated, then I have updated my previous calculations
# 2. It takes a few minutes to download the complete cases_100k dataset, so probably run this once a week.


# Sections:
# 1. Set-up
# 2. Clean MSA counties and tests data
# 3. Find week with latest data for IN, MI, and WI
# 4. Make everything look like the clean Illinois df
# 5. Calc positive test rates




##################
# Set-up
##################


pacman::p_load(dplyr, glue)


# the beautiful author of totalcensus has the counties that make-up each msa. Hard to find in the wild.
data(dict_cbsa, package = "totalcensus")

# Needed to convert Illinois's weekly, county-level, cases-per-100K data into weekly cases data
ill_county_pop <- readr::read_csv(glue("{rprojroot::find_rstudio_root_file()}/data/states/ill-county-pop-2019.csv")) %>% 
   mutate(name = stringr::str_to_title(name),
          name = recode(name,
                        "De Kalb" = "Dekalb",
                        "De Witt" = "Dewitt"))

# Has a spec_tbl class (for some reason). I'd rather not have it b/c some pkgs can be finicky.
ill_tests <- readr::read_csv(glue("{rprojroot::find_rstudio_root_file()}/data/states/illinois-tests-complete.csv")) %>% 
   as_tibble()
mich_tests <- readr::read_csv(glue("{rprojroot::find_rstudio_root_file()}/data/states/mich-tests-complete.csv"))
wisc_tests <- readr::read_csv(glue("{rprojroot::find_rstudio_root_file()}/data/states/wisc-tests-complete.csv"), col_types = "Dcdd")
ind_tests <- readr::read_csv(glue("{rprojroot::find_rstudio_root_file()}/data/states/ind-tests-complete.csv"))


us_msa_tiles <- sf::read_sf(glue("{rprojroot::find_rstudio_root_file()}/data/shapefiles/msa-2019/tl_2019_us_cbsa.shp"))
# leaflet requires EPSG:4326
msa_tiles <- sf::st_transform(us_msa_tiles, 4326)
ind_msa_tiles <- msa_tiles %>% 
   filter(stringr::str_detect(NAME, "IN")) %>% 
   janitor::clean_names() %>% 
   mutate(# remove state abbrev from msa names
      name = stringr::str_remove_all(name, "([A-Z][A-Z]-)*"),
      name = stringr::str_remove_all(name, ", [A-Z]*"))



############################################
# Get cases per 100k data for each MSA
############################################


old_prop_date <- readr::read_csv(glue("{rprojroot::find_rstudio_root_file()}/data/msa-cases100-posrate-current.csv")) %>%
   # should only have 1 date, but I may want to do a ts or something in the future
   filter(date == max(date)) %>% 
   slice(n()) %>% 
   pull(date)

# Number of new confirmed COVID-19 cases per 100,000 population, 7-day avg
c <- 0
while (TRUE) {
   prop_cases_hist <- try({
      # try dates from today until old data date reached
      try_date <- lubridate::today() - c
      
      ind_msa_tiles %>% 
         # cbsafp is the msa id
         select(name, cbsafp) %>% 
         group_by(name) %>% 
         tidyr::nest() %>% 
         # get the cases per 100K value for each msa if one is available
         mutate(data = purrr::map(data, ~suppressMessages(
            covidcast::covidcast_signal(
               data_source = "indicator-combination",
               signal = "confirmed_7dav_incidence_prop",
               start_day = as.Date("2020-04-02"),
               end_day = try_date,
               geo_type = "msa",
               geo_values = .x$cbsafp)
         ))) %>% 
         tidyr::unnest(cols = "data") %>% 
         ungroup() %>% 
         rename(msa = name, date = time_value, cases_100k = value) %>% 
         select(date, msa, geo_value, cases_100k)
   }, silent = TRUE)
   
   # stay in loop until I get some new data or old data date reached
   if (class(prop_cases_hist) != "try-error"){
      break
   } else if (try_date <= old_prop_date) {
      break
   } else {
      c <- c + 1
      # give the api a sec
      Sys.sleep(1)
   }
}



######################################
# Clean MSA counties and tests
######################################


covidcast_msa_counties <- dict_cbsa %>% 
   janitor::clean_names() %>% 
   # filter for cbsa ids for MSAs that covidcast uses 
   filter(cbsa %in% prop_cases_hist$geo_value) %>%
   # get all county values into the same format
   mutate(county = stringr::str_replace_all(county,
                                            pattern = " County",
                                            replacement = ""),
          county = stringr::str_to_title(county),
          # counties like st. joseph are a problem
          county = stringr::str_replace_all(county,
                                            pattern = "\\.",
                                            replacement = ""),
          # remove state abbrevs
          msa = stringr::str_remove(cbsa_title,
                                    pattern = "(, [A-Z]*$)|(, [A-Z]*-[A-Z]*$)|(, [A-Z]*-[A-Z]*-[A-Z]*$)"),
          fips = paste0(state, county_2)) %>% 
   select(cbsa, msa, state_full, county, fips)


# Illinois
ill_msa_counties <- covidcast_msa_counties %>% 
   filter(state_full == "Illinois")

# only counties with a covidcast msa
# extract cases per 100K population, weekly tests
# calculate weekly cases using 2019 county population data
ill_tests_clean <- ill_tests %>% 
   janitor::clean_names() %>% 
   mutate(county = stringr::str_to_title(county)) %>% 
   filter(county %in% ill_msa_counties$county) %>% 
   mutate(cases_100k = stringr::str_extract(new_cases_per_100_000,
                                            pattern = "^[0-9]*") %>% as.numeric(),
          test_positivity_percent = stringr::str_remove_all(test_positivity_percent, "warning"),
          weekly_tests = stringr::str_extract(test_positivity_percent,
                                              pattern = "([0-9]*,[0-9]*$)|([0-9][0-9][0-9]$)"),
          weekly_tests = stringr::str_remove_all(weekly_tests, ",") %>% 
             as.numeric(.)) %>%
   left_join(ill_county_pop, by = c("county" = "name")) %>% 
   mutate(weekly_positives = round((cases_100k * `2019`)/100000),
          state = "Illinois") %>% 
   select(start_date, end_date, county, weekly_positives, weekly_tests, state)


# needed for function below and partially filtering other states' data
ill_date_range <- ill_tests_clean %>% 
   summarize(first_date = min(start_date),
             last_date = max(end_date))


# Filter counties included in Indiana MSAs
# Indiana
ind_msa_counties <- covidcast_msa_counties %>% 
   filter(state_full == "Indiana")
ind_county_tests <- ind_tests %>% 
   mutate(county = stringr::str_to_title(county),
          county = recode(county, "La Porte" = "Laporte")) %>%
   filter(county %in% ind_msa_counties$county)


# Wisconsin
wisc_msa_counties <- covidcast_msa_counties %>% 
   filter(state_full == "Wisconsin")
# wisc data is cumulative, so need to calc daily
wisc_county_tests <- wisc_tests %>% 
   filter(county %in% wisc_msa_counties$county) %>% 
   arrange(date) %>% 
   mutate(daily_positives = tsibble::difference(positive),
          daily_negatives = tsibble::difference(negative),
          daily_tests = daily_positives + daily_negatives)


# Michigan
mich_msa_counties <- covidcast_msa_counties %>% 
   filter(state_full == "Michigan")
mich_county_tests <- mich_tests %>% 
   filter(county %in% mich_msa_counties$county)



#################################################
# Find week with latest data for IN, MI, and WI
#################################################


# There are some Wisc and Ind counties in the Chicago MSA, so the Chicago MSA pos-rate will have data from different weeks.
state_tests <- list("Indiana" = ind_county_tests,
                    "Michigan" = mich_county_tests,
                    "Wisconsin" = wisc_county_tests)


# Illinois doesn't publish data nearly as quickly as other states, so I want the other MSAs, that don't involve Illinois counties to have more up-to-date pos rates.
# Determine latest week that each state has test data
determine_end_date <- function(dat) {
   c <- 0
   # illinois is laggard, so that's starting week
   ill_date <- ill_date_range$last_date[[1]]
   # using a string function for detection so need a string date
   dat2 <- dat %>% 
      mutate(date = as.character(date))
   # adds weeks to illinois end date until it doesn't find data for that week or hits today
   while(TRUE) {
      new_date <- as.character(ill_date + c)
      result <- sjmisc::str_contains(dat2$date, new_date)
      if (result == FALSE) {
         end_date <- as.Date(new_date) - 7
         return(end_date)
      } else if (as.Date(new_date) == lubridate::today()) {
         end_date <- as.Date(new_date)
         return(end_date)
      } else {
         c <- c + 7
      }
   }
}

states_end_date <- purrr::map_dfr(state_tests, ~determine_end_date(.x)) %>% 
   tidyr::pivot_longer(cols = everything(),
                       names_to = "state",
                       values_to = "end_date") %>% 
   # these 3 states' data date will be same to stay somewhat consistent, so take min date 
   summarize(min_date = min(end_date)) %>% 
   pull(min_date)

states_start_date <- purrr::map_dfr(state_tests, ~min(.x$date)) %>% 
   tidyr::pivot_longer(cols = everything(),
                       names_to = "state",
                       values_to = "start_date") %>% 
   # these 3 states' data date will be same to stay somewhat consistent, so take max date 
   summarize(max_date = max(start_date)) %>% 
   pull(max_date)



###################################################
# Make everything look like the clean Illinois df
###################################################

# Indiana
# historic values; same as clean set except filter using state_start_date
# ind_tests_clean_hist <- ind_county_tests %>% 
#    filter(between(date,
#                   states_start_date,
#                   states_end_date)) %>% 
#    group_by(county) %>% 
#    arrange(date) %>% 
#    # illinois's "week" variable is off by 3 days
#    mutate(week = lubridate::week(date),
#           # default changes NAs to the final week
#           week = lead(week, n = 3, default = max(week))) %>% 
#    group_by(week) %>% 
#    # start_date = 1 date of that week, end_date = last date of that week
#    mutate(start_date = min(date),
#           end_date = max(date)) %>%
#    ungroup() %>% 
#    select(week, start_date, end_date, county,
#           daily_positives = positives,
#           daily_tests = num_tests) %>% 
#    # grouping by all vars keeps them in final df; some not necessary for calc
#    group_by(week, start_date, end_date, county) %>% 
#    summarize(weekly_positives = sum(daily_positives),
#              weekly_tests = sum(daily_tests),
#              state = "Indiana") %>% 
#    ungroup()

ind_tests_clean_hist <- ind_county_tests %>% 
   filter(between(date,
                  states_start_date,
                  states_end_date)) %>% 
   group_by(county) %>% 
   arrange(date) %>%
   mutate(weekly_positives = slider::slide_int(positives,
                                               .f = sum,
                                               .before = 6,
                                               .step = 7,
                                               .complete = T),
          weekly_tests = slider::slide_int(num_tests,
                                           .f = sum,
                                           .before = 6,
                                           .step = 7,
                                           .complete = T),
          start_date = date - 6,
          state = "Indiana") %>% 
   tidyr::drop_na() %>% 
   rename(end_date = date) %>% 
   ungroup() %>% 
   select(-positives, -num_tests) %>% 
   relocate(start_date, .before = end_date)

# Wisconsin
# wisc_tests_clean_hist <- wisc_county_tests %>% 
#    filter(between(date,
#                   states_start_date,
#                   states_end_date)) %>% 
#    group_by(county) %>% 
#    arrange(date) %>% 
#    # illinois's "week" variable is off by 3 days
#    mutate(week = lubridate::week(date),
#           # default changes NAs to the final week
#           week = lead(week, n = 3, default = max(week))) %>% 
#    group_by(week) %>% 
#    # start_date = 1 date of that week, end_date = last date of that week
#    mutate(start_date = min(date),
#           end_date = max(date)) %>%
#    ungroup() %>% 
#    select(week, start_date, end_date, county,
#           daily_positives,daily_tests) %>% 
#    # grouping by all vars keeps them in final df; some not necessary for calc
#    group_by(week, start_date, end_date, county) %>% 
#    summarize(weekly_positives = sum(daily_positives),
#              weekly_tests = sum(daily_tests),
#              state = "Wisconsin") %>% 
#    ungroup()

wisc_tests_clean_hist <- wisc_county_tests %>% 
   filter(between(date,
                  states_start_date,
                  states_end_date)) %>% 
   group_by(county) %>% 
   arrange(date) %>%
   mutate(weekly_positives = slider::slide_int(daily_positives,
                                               .f = sum,
                                               .before = 6,
                                               .step = 7,
                                               .complete = T),
          weekly_tests = slider::slide_int(daily_tests,
                                           .f = sum,
                                           .before = 6,
                                           .step = 7,
                                           .complete = T),
          start_date = date - 6,
          state = "Wisconsin") %>% 
   tidyr::drop_na() %>% 
   rename(end_date = date) %>% 
   ungroup() %>% 
   select(-positive, -negative,
          -daily_positives, -daily_negatives,
          -daily_tests) %>% 
   relocate(start_date, .before = end_date)

# Michigan
# mich_tests_clean_hist <- mich_county_tests %>%
#    filter(between(date,
#                   states_start_date,
#                   states_end_date)) %>%
#    group_by(county) %>% 
#    arrange(date) %>%
#    # illinois's "week" variable is off by 3 days
#    mutate(week = lubridate::week(date),
#           # default changes NAs to the final week
#           week = lead(week, n = 3, default = max(week))) %>% 
#    group_by(week) %>% 
#    # start_date = 1 date of that week, end_date = last date of that week
#    mutate(start_date = min(date),
#           end_date = max(date)) %>%
#    ungroup() %>% 
#    select(week, start_date, end_date, county,
#           daily_positives = positive,
#           daily_tests = total) %>% 
#    # grouping by all vars keeps them in final df; some not necessary for calc
#    group_by(week, start_date, end_date, county) %>% 
#    summarize(weekly_positives = sum(daily_positives),
#              weekly_tests = sum(daily_tests),
#              state = "Michigan") %>% 
#    ungroup()

mich_tests_clean_hist <- mich_county_tests %>% 
   filter(between(date,
                  states_start_date,
                  states_end_date)) %>% 
   group_by(county) %>% 
   arrange(date) %>%
   mutate(weekly_positives = slider::slide_int(positive,
                                               .f = sum,
                                               .before = 6,
                                               .step = 7,
                                               .complete = T),
          weekly_tests = slider::slide_int(total,
                                           .f = sum,
                                           .before = 6,
                                           .step = 7,
                                           .complete = T),
          start_date = date - 6,
          state = "Michigan") %>% 
   tidyr::drop_na() %>% 
   rename(end_date = date) %>% 
   ungroup() %>% 
   select(-positive, -negative, -total) %>% 
   relocate(start_date, .before = end_date)



##########################################
# Calc positive test rates
##########################################


# illinois has no historic data
voltron_hist <- purrr::reduce(list(ill_tests_clean, ind_tests_clean_hist,
                                   wisc_tests_clean_hist, mich_tests_clean_hist),
                              bind_rows) %>% 
   left_join(covidcast_msa_counties, by = c("county", "state" = "state_full"))

# Calc Chicago separately
chicago_msa_posrates <- voltron_hist %>% 
   filter(stringr::str_detect(msa, "Chicago"),
          start_date >= ill_date_range$first_date[[1]],
          end_date <= ill_date_range$last_date[[1]]) %>% 
   group_by(start_date, end_date, msa) %>%
   summarize(pos_rate = sum(weekly_positives) / sum(weekly_tests)) %>% 
   ungroup()

region_msa_posrates_hist <- voltron_hist %>% 
   # no test data available in Ohio and Kentucky so no pos_rate
   filter(!msa %in% c("Cincinnati", "Evansville"),
          !stringr::str_detect(msa, "Chicago"),
          !stringr::str_detect(msa, "Louisville")) %>% 
   group_by(start_date, end_date, msa) %>%
   # calc indiana/Michigan msa pos_rates
   summarize(pos_rate = sum(weekly_positives) / sum(weekly_tests)) %>% 
   # chicago historic data has same date range, so use same set
   bind_rows(chicago_msa_posrates) %>% 
   ungroup() %>% 
   # choosing this date so it joins up cleanly with historic prop cases df
   filter(start_date >= as.Date("2020-03-29"))

map2_hist <- prop_cases_hist %>% 
   full_join(region_msa_posrates_hist, by = c("date" = "end_date", "msa")) %>% 
   mutate(data_date = lubridate::today(),
          # replacing a couple obvious typos
          cases_100k = case_when(msa == "Michigan City-La Porte" & date == as.Date("2020-08-01") |
                                 msa == "Michigan City-La Porte" & date == as.Date("2020-08-08") ~ 10,
                                 TRUE ~ cases_100k))

readr::write_csv(map2_hist, glue("{rprojroot::find_rstudio_root_file()}/data/msa-cases100-posrate-historic.csv"))
