# Scrapes and claws state testing data then save it to data/states dir

# Notes
# 1. Haven't solved the RSelenium-on-github-runner problem yet, but I'll go ahead and make this script publicly available since it's vital to running the project
# 2. {readr} should be at >= version 2.0.2; there was a complication when they implemented {vroom} that prevented certain characters being written





#@@@@@@@@@@@@@@
# Set-Up ----
#@@@@@@@@@@@@@@


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




# Get installed stable Google Chrome version ...
if (xfun::is_unix()) {
      
      chrome_driver_version <-
            system2(command = ifelse(xfun::is_macos(),
                                     "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
                                     "google-chrome-stable"),
                    args = "--version",
                    stdout = TRUE,
                    stderr = TRUE) %>%
            stringr::str_extract(pattern = "(?<=Chrome )(\\d+\\.){3}")
      
      ## on Windows a plattform-specific bug prevents us from calling the Google Chrome binary directly to get its version number
      ## cf. https://bugs.chromium.org/p/chromium/issues/detail?id=158372
} else if (xfun::is_windows()) {
      
      chrome_driver_version <-
            system2(command = "wmic",
                    args = 'datafile where name="C:\\\\Program Files (x86)\\\\Google\\\\Chrome\\\\Application\\\\chrome.exe" get Version /value',
                    stdout = TRUE,
                    stderr = TRUE) %>%
            stringr::str_extract(pattern = "(?<=Version=)(\\d+\\.){3}")
      
} else rlang::abort(message = "Your OS couldn't be determined (Linux, macOS, Windows) or is not supported.")

# ... and determine most recent ChromeDriver version matching it
chrome_driver_version %<>%
      magrittr::extract(!is.na(.)) %>%
      stringr::str_replace_all(pattern = "\\.",
                               replacement = "\\\\.") %>%
      paste0("^",  .) %>%
      stringr::str_subset(string =
                                binman::list_versions(appname = "chromedriver") %>%
                                dplyr::last()) %>%
      as.numeric_version() %>%
      max() %>%
      as.character()


driver <- rsDriver(browser = c("chrome"), chromever = chrome_driver_version)

# chrome browser
chrome <- driver$client




#@@@@@@@@@@@@@@@@@
# Illinois ----
#@@@@@@@@@@@@@@@@@


ill_url <- "http://www.dph.illinois.gov/countymetrics"
# go to illinois county website
chrome$navigate(url = ill_url)
Sys.sleep(10)

## (old) scrape html table ----
# # element with table data
# tbl_elt <- chrome$findElement(using = "id", "detailedData")
# # get html code
# tbl_html <- tbl_elt$getPageSource()[[1]]
# 
# # use rvest to process the table
# page <- read_html(tbl_html)
# tbl_node <- html_node(page, "table") 
# # cols: CLI = covid like illness, ED = emergency department
# ill_table <- html_table(tbl_node, header = T)

## (old) html table d/l ----
# ill_dl_button <- chrome$findElement(using = "css", "#data-table-detailedData > div > div.data-table__search > button")
# ill_dl_button$clickElement()
# Sys.sleep(5)
# 
# ill_csv_filename <- "County Level Risk Metrics.csv"
# ill_download_location <- file.path(Sys.getenv("USERPROFILE"), "Downloads")
# ill_file_path <- file.path(ill_download_location, ill_csv_filename)
# ill_tests <- readr::read_csv(ill_file_path)
# 
# # file needs to be unlinked from environment in order to delete it
# ill_safe_unlink <- purrr::safely(unlink)
# ill_safe_unlink(ill_tests)


## scrape paginated html table ----
# element with table data
tbl_elt <- chrome$findElement(using = "id", "detailedData")
# element for the the "Next" page button of the table
ill_next_button <- chrome$findElement(using = "css", "#detailedData_next")

ill_table_full <- tibble(
      County = character(),
      `New Cases per 100,000` = character(),
      `Test Positivity %` = character(),
      `(%) CLI ED Visits, Adults` = character(),
      `Number of CLI Admissions` = character(),
      `Number of Deaths` = character(),
      `ICU (%) Available` = character()
)

# Scrape paginated html table until "Next" button throws error
while(TRUE) {
      
      # get html code
      tbl_html <- tbl_elt$getPageSource()[[1]]
      # use rvest to process the table
      page <- read_html(tbl_html)
      tbl_node <- html_node(page, "table")
      # cols: CLI = covid like illness, ED = emergency department
      ill_table <- html_table(tbl_node, header = T, convert = FALSE)
      ill_table_full <- ill_table_full %>% 
            bind_rows(ill_table)
      
      # Test if "Next" button is still there. If not, exit loop. If so, go to next page of table
      ill_next_butt_class <- try(unlist(chrome$findElement(using = "css", "#detailedData_next")$getElementAttribute("class")), silent = TRUE)
      
      if (ill_next_butt_class != "paginate_button next"){
            break
      } else {
            # Have to re-instantiate it again for some reason
            ill_next_button <- chrome$findElement(using = "css", "#detailedData_next")
            # Go to the next page of the table
            ill_next_button$clickElement()
            Sys.sleep(5)
      }
      
}


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
ill_tests_new <- ill_table_full %>% 
      mutate(week = week_number,
             start_date = start_date,
             end_date = end_date,
             `Number of Deaths` = stringr::str_extract(`Number of Deaths`, "^[0-9]*"),
             `Number of Deaths` = as.integer(`Number of Deaths`),
             `Test Positivity %` = stringr::str_remove_all(`Test Positivity %`, "\r|\r\n|\n"),
             `(%) CLI ED Visits, Adults` = stringr::str_replace_all(`(%) CLI ED Visits, Adults`, "\\*", "<5%")) %>% 
      select(week, start_date, end_date, everything())


ill_test_comp <- readr::read_csv(glue("{rprojroot::find_rstudio_root_file()}/data/states/illinois-tests-complete.csv"),
                                 col_types = "nDDcccccic", lazy = FALSE)
Sys.sleep(5)


ill_test_wk <- ill_tests_new %>% 
      slice(n()) %>% 
      pull(week)
ill_comp_wk <- ill_test_comp %>% 
      slice(n()) %>% 
      pull(week)

# make sure data is new before adding it complete dataset
if (ill_test_wk != ill_comp_wk) {
      
      ill_test_comp_fin <- ill_test_comp %>% 
            bind_rows(ill_tests_new)
      Sys.sleep(5)
      readr::write_csv(ill_test_comp_fin, glue("{rprojroot::find_rstudio_root_file()}/data/states/illinois-tests-complete.csv"))
}

# clean-up
# fs::file_delete(ill_file_path)



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

"COVID19-Historical-V2-CNTY.csv"
"https://dhsgis.page.link/j5nk"

wisc_url <- "https://data.dhsgis.wi.gov/datasets/covid-19-historical-data-by-county-v2/explore"

chrome$navigate(url = wisc_url)
Sys.sleep(10)

# opens file download menu in the side panel
wisc_dl_panel_button <- chrome$findElement("css selector", "#ember234 > div > button:nth-child(3)")
wisc_dl_panel_button$clickElement()
Sys.sleep(10)

# Have to run a JS script to access elements in a shadow dom (#shadow-root)
# clicks button to dl file into downloads folder
chrome$executeScript("document.querySelector('hub-download-card').shadowRoot.querySelector('calcite-card').querySelector('calcite-dropdown').querySelector('calcite-dropdown-group').querySelector('calcite-dropdown-item:nth-child(2)').click()")
Sys.sleep(30)

wisc_csv_filename <- "COVID-19_Historical_Data_by_County_V2.csv"
download_location <- file.path(Sys.getenv("USERPROFILE"), "Downloads")
wisc_file_path <- file.path(download_location, wisc_csv_filename)
wisc_tests_new <- readr::read_csv(wisc_file_path, lazy = FALSE)

# file needs to be unlinked from environment in order to delete it
safe_unlink <- purrr::safely(unlink)
safe_unlink(wisc_tests_new)

wisc_tests_clean <- wisc_tests_new %>%
      janitor::clean_names() %>%
      select(date = rpt_dt, county = geo_name, negative = neg_cum, positive = pos_cum_conf) %>%
      mutate(date = lubridate::as_date(date)) %>% 
      arrange(county, date)

# clean-up
fs::file_delete(wisc_file_path)

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

