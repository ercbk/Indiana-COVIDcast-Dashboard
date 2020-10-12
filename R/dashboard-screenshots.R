# Take some pictures of the dashboard charts

library(glue); library(dplyr)

# get latest html files
html_files <- tibble::tibble(paths = fs::dir_ls(glue::glue("{rprojroot::find_rstudio_root_file()}/images/dashboard"), regexp = "html")) %>% 
      mutate(
            chart = stringr::str_extract(paths,
                                         pattern = "[a-z]*-[a-z]*-[a-z]*"),
            date = stringr::str_extract(paths,
                                        pattern = "[0-9]{4}-[0-9]{2}-[0-9]{2}") %>%
                  as.Date()
      ) %>%
      group_by(chart) %>% 
      filter(date == max(date))

# html file path
dumbbell_path <- html_files %>% 
      filter(stringr::str_detect(chart, "dumbbell")) %>% 
      pull(paths)
dumbbell_date <- html_files %>% 
      filter(stringr::str_detect(chart, "dumbbell")) %>% 
      pull(date)

# convert html to png
webshot2::webshot(glue("{dumbbell_path}"),
                 file = glue("{rprojroot::find_rstudio_root_file()}/images/dashboard/covidcast-msa-dumbbell-{dumbbell_date}.png"),
                 delay = 5)


reacttab_path <- html_files %>% 
      filter(stringr::str_detect(chart, "reacttab")) %>% 
      pull(paths)
reacttab_date <- html_files %>% 
      filter(stringr::str_detect(chart, "reacttab")) %>% 
      pull(date)

webshot2::webshot(glue("{reacttab_path}"),
                 file = glue("{rprojroot::find_rstudio_root_file()}/images/dashboard/covidcast-msa-reacttab-{reacttab_date}.png"),
                 delay = 5)



# garbage collection

png_files <- tibble::tibble(paths = fs::dir_ls(glue::glue("{rprojroot::find_rstudio_root_file()}/images/dashboard"), regexp = "png")) %>% 
   mutate(
      chart = stringr::str_extract(paths,
                                   pattern = "[a-z]*-[a-z]*-[a-z]*"),
      date = stringr::str_extract(paths,
                                  pattern = "[0-9]{4}-[0-9]{2}-[0-9]{2}") %>%
         as.Date()
   )

# clean-up old pngs and extraneous html output
png_files %>% 
   group_by(chart) %>% 
   add_count() %>% 
   filter(n > 7) %>%
   filter(date == min(date)) %>% 
   pull(paths) %>% 
   fs::file_delete(.)


fs::file_delete(glue::glue("{rprojroot::find_rstudio_root_file()}/README.html"))


# create path to artifacts
dumbbell_files <- stringr::str_replace(dumbbell_path, ".html", "_files")
reacttab_files <- stringr::str_replace(reacttab_path, ".html", "_files")


# delete artifacts that were created when html files created
fs::dir_delete(c(dumbbell_files, reacttab_files))
# delete html files
fs::file_delete(c(dumbbell_path, reacttab_path))