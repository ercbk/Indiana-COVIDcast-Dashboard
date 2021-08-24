# Build dashboard datasets

# replaced on 2021-08-23


# Sections:
# Load and clean cases per 100K and positivity rate data



pacman::p_load(swatches, covidcast, extrafont, dplyr, glue)



#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Load and clean cases per 100k and positivity rate data ----
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


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
          cases_color_light = unclass(prismatic::clr_lighten(cases_color, shift = .50))) %>%  
   select(msa, cases_color, cases_color_light)


# cases per 100k and positivity rate trend data
# pos_list is a list column of pos_list = list(list(endDate=dateval1, posRate = posval1), list(endDate=dateval2, posRate=posval2), ...) for each MSA
# required format for arrays in javascript
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


# Get the data dates Cases per 100K, Positive Test Rate (chicago, everybody else)
cases_current_date <- case_pos_current %>%
   slice(n()) %>%
   select(cases_date = date)

indmich_pos_current_date <- case_pos_current %>%
   filter(!stringr::str_detect(msa, "Chicago"),
          !is.na(end_date)) %>%
   slice(n()) %>%
   select(other_pos_date = end_date)

chi_pos_current_date <- case_pos_current %>%
   filter(stringr::str_detect(msa, "Chicago"),
          !is.na(end_date)) %>%
   select(chi_pos_date = end_date)

cases_pos_data_dates <- purrr::reduce(list(cases_current_date, indmich_pos_current_date, chi_pos_current_date),
                                      bind_cols)

readr::write_rds(cases_pos_data_dates, glue("{rprojroot::find_rstudio_root_file()}/data/dash-case-pos-dates.rds"))

