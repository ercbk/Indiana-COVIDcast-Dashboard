# Scrapes and claws state testing data then save it to data/states dir

# Notes
# 1. Haven't solved the RSelenium-on-github-runner problem yet, but I'll go ahead and make this script publicly available since it's vital to running the project



setwd("~/R/Projects/Indiana-COVIDcast-Dashboard")

# text me if there's an error
options(error = function() { 
      library(RPushbullet)
      pbPost("note", "Error", geterrmessage())
      if(!interactive()) stop(geterrmessage())
})

pacman::p_load(RSelenium, glue, dplyr, rvest)


windows_tasks <- installr::get_tasklist()
java_pid <- windows_tasks %>% 
      filter(stringr::str_detect(`Image Name`, "java.exe")) %>% 
      pull(PID)

chrome_pid <- windows_tasks %>% 
      filter(stringr::str_detect(`Image Name`, "chromedriver.exe")) %>% 
      pull(PID)

tools::pskill(pid = java_pid)
tools::pskill(pid = chrome_pid)




#@@@@@@@@@@@@@@@@@
# Illinois ----
#@@@@@@@@@@@@@@@@@


driver <- rsDriver(browser = c("chrome"), chromever = "92.0.4515.107")

# chrome browser
chrome <- driver$client
ill_url <- "http://www.dph.illinois.gov/countymetrics"
# go to illinois county website
chrome$navigate(url = ill_url)
Sys.sleep(10)

# element with table data
tbl_elt <- chrome$findElement(using = "id", "detailedData")
# get html code
tbl_html <- tbl_elt$getPageSource()[[1]]

# use rvest to process the table
page <- read_html(tbl_html)
tbl_node <- html_node(page, "table") 
# cols: CLI = covid like illness, ED = emergency department
ill_table <- html_table(tbl_node, header = T)

# pull the text that says which week this data is for
wkelt <- chrome$findElement(using = "id", "mmwrWeekNumber")
week_number <- as.numeric(wkelt$getElementText()[[1]])
# pull start date of the week
startelt <- chrome$findElement(using = "id", "priorPeriodDate")
start_date <- lubridate::mdy(startelt$getElementText()[[1]])
# pull end date of the week
endelt <- chrome$findElement(using = "id", "lastupdateDate")
end_date <- lubridate::mdy(endelt$getElementText()[[1]])

# add dates to scraped table
ill_test <- ill_table %>% 
      mutate(week = week_number,
             start_date = start_date,
             end_date = end_date,
             `Number of Deaths` = stringr::str_extract(`Number of Deaths`, "^[0-9]*"),
             `Number of Deaths` = as.integer(`Number of Deaths`)) %>% 
      select(week, start_date, end_date, everything())


ill_test_comp <- readr::read_csv(glue("{rprojroot::find_rstudio_root_file()}/data/states/illinois-tests-complete.csv"),
                                 col_types = "nDDcccccic")

ill_test_wk <- ill_test %>% 
      slice(n()) %>% 
      pull(week)
ill_comp_wk <- ill_test_comp %>% 
      slice(n()) %>% 
      pull(week)

# make sure data is new before adding it complete dataset
if (ill_test_wk != ill_comp_wk) {
      
      ill_test_comp <- ill_test_comp %>% 
            bind_rows(ill_test)
      Sys.sleep(5)
      readr::write_csv(ill_test_comp, glue("{rprojroot::find_rstudio_root_file()}/data/states/illinois-tests-complete.csv"))
}



#@@@@@@@@@@@@@@@@
# Michigan ----
#@@@@@@@@@@@@@@@@


mich_url <- "https://www.michigan.gov/coronavirus/0,9753,7-406-98163_98173---,00.html"

# extract the link for the data off the webpage
chrome$navigate(url = mich_url)
Sys.sleep(10)
mich_tests_elt <- chrome$findElement(using = "css selector", "#comp_115341 > ul > li > span > span > span.shortdesc > p:nth-child(7) > a")
mich_tests_link <- mich_tests_elt$getElementAttribute(attrName = "href")[[1]]

# extract date from the link
mich_tests_date <- stringr::str_extract(mich_tests_link, pattern = "[0-9]*-[0-9]*-[0-9]*")

# use date in part of the filename
mich_tests_dest <- glue::glue("{rprojroot::find_rstudio_root_file()}/data/states/mich-tests-{mich_tests_date}.xlsx")
# wb needed for Excel files
download.file(mich_tests_link, destfile = mich_tests_dest, mode = "wb")
# filter only covid test; clean
mich_tests_new <- readxl::read_xlsx(mich_tests_dest) %>%
      janitor::clean_names() %>% 
      select(date = message_date, everything()) %>%
      mutate(date = lubridate::as_date(date))

readr::write_csv(mich_tests_new, glue("{rprojroot::find_rstudio_root_file()}/data/states/mich-tests-complete.csv"))


mich_dat_files <- tibble::tibble(paths = fs::dir_ls(glue::glue("{rprojroot::find_rstudio_root_file()}/data/states"))) %>% 
   filter(stringr::str_detect(paths, "mich"),
          !stringr::str_detect(paths, "complete")) %>% 
   mutate(
      chart = stringr::str_extract(paths,
                                   pattern = "[a-z]*-[a-z]*-[a-z]*"),
      date = stringr::str_extract(paths,
                                  pattern = "[0-9]{4}-[0-9]{2}-[0-9]{2}") %>%
         as.Date())

# clean-up data files older than a week
mich_dat_files %>% 
   add_count() %>% 
   filter(n > 7) %>%
   filter(date == min(date)) %>% 
   pull(paths) %>% 
   fs::file_delete(.)



#@@@@@@@@@@@@@@@@@
# Wisconsin ----
#@@@@@@@@@@@@@@@@@


wisc_url <- "https://data.dhsgis.wi.gov/datasets/covid-19-historical-data-by-county-1/explore"

chrome$navigate(url = wisc_url)
Sys.sleep(10)

# opens file download menu in the side panel
wisc_dl_panel_button <- chrome$findElement("css selector", "#ember189 > div > button:nth-child(3)")
wisc_dl_panel_button$clickElement()
Sys.sleep(10)

# Have to run a JS script to access elements in a shadow dom (#shadow-root)
# clicks button to dl file into downloads folder
chrome$executeScript("document.querySelector('hub-download-card').shadowRoot.querySelector('calcite-card').querySelector('calcite-dropdown').querySelector('calcite-dropdown-group').querySelector('calcite-dropdown-item:nth-child(2)').click()")
Sys.sleep(60)

wisc_csv_filename <- "COVID-19_Historical_Data_by_County.csv"
download_location <- file.path(Sys.getenv("USERPROFILE"), "Downloads")
wisc_tests_new <- readr::read_csv(file.path(download_location, wisc_csv_filename))

wisc_tests_clean <- wisc_tests_new %>%
      janitor::clean_names() %>%
      select(date, geo, county = name, negative, positive) %>%
      filter(geo == "County") %>%
      mutate(date = lubridate::as_date(date)) %>%
      select(-geo)

# clean-up
rm(wisc_tests_new)
# have to bob and weave around some permission/filename bs that won't let me delete the file in the current session (unlink didn't work)
file.rename(file.path(download_location, wisc_csv_filename),file.path(download_location, "fu.csv"))
Sys.sleep(10)
fs::file_delete(file.path(download_location, "fu.csv"))

readr::write_csv(wisc_tests_clean, glue("{rprojroot::find_rstudio_root_file()}/data/states/wisc-tests-complete.csv"))




#@@@@@@@@@@@@@@@
# Indiana ----
#@@@@@@@@@@@@@@@


ind_test_dat <- readr::read_csv("https://hub.mph.in.gov/datastore/dump/afaa225d-ac4e-4e80-9190-f6800c366b58?bom=True")

ind_test_clean <- ind_test_dat %>% 
      janitor::clean_names() %>% 
      select(date, county = county_name, 
             positives = covid_count, num_tests = covid_tests_administrated) %>% 
      mutate(date = lubridate::as_date(date))

readr::write_csv(ind_test_clean, glue("{rprojroot::find_rstudio_root_file()}/data/states/ind-tests-complete.csv"))



# close browser
chrome$close()

# think this may be for another method of using RSelenium
driver$server$stop()

windows_tasks <- installr::get_tasklist()
java_pid <- windows_tasks %>% 
      filter(stringr::str_detect(`Image Name`, "java.exe")) %>% 
      pull(PID)

chrome_pid <- windows_tasks %>% 
      filter(stringr::str_detect(`Image Name`, "chromedriver.exe")) %>% 
      pull(PID)

tools::pskill(pid = java_pid)
tools::pskill(pid = chrome_pid)
