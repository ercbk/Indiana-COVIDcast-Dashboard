
pacman::p_load(swatches, dplyr, glue, extrafont, htmltools, reactable, dataui)


map2_current <- readr::read_csv(glue("{rprojroot::find_rstudio_root_file()}/data/msa-cases100-posrate-current.csv"))
map2_hist <- readr::read_csv(glue("{rprojroot::find_rstudio_root_file()}/data/msa-cases100-posrate-historic.csv"))

moody <- swatches::read_palette(glue("{rprojroot::find_rstudio_root_file()}/palettes/Moody Blooms.ase"))[c(1,4,5,8)]
pal <- leaflet::colorNumeric("YlOrRd",
                             domain = seq(0.00, 0.20, by = 0.05),
                             na.color = "#800026")


# color palettes
posrate_col_df <- map2_current %>% 
      select(msa, pos_rate) %>% 
      filter(!is.na(pos_rate)) %>% 
      # slice(n()) %>% 
      mutate(pos_color = pal(pos_rate))

posrate_col <- map2_current %>% 
      select(msa, cases_100k) %>% 
      left_join(posrate_col_df, by = "msa") %>% 
      mutate(cases_100k = round(cases_100k, 2)) %>%
      arrange(desc(cases_100k)) %>% 
      # filter(!is.na(pos_color)) %>%
      pull(pos_color)

cases_col <- map2_current %>% 
      select(msa, cases_100k) %>% 
      # slice(n()) %>% 
      mutate(cases_100k = round(cases_100k, 2),
             cases_color = case_when(cases_100k < 1 ~ moody[[1]],
                                     between(cases_100k, 1, 9.99) ~ moody[[2]],
                                     between(cases_100k, 10, 24.99) ~ moody[[3]],
                                     cases_100k >= 25 ~ moody[[4]])) %>% 
      arrange(desc(cases_100k)) %>% 
      select(msa, cases_color)




# cases per 100k and positivity rate trend data
pos_hist <- map2_hist %>%
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

cases_hist <- map2_hist %>%  
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
current_dat <- map2_current %>%
      select(msa, cases_100k, pos_rate) %>% 
      mutate(cases_100k = round(cases_100k, 0))


# combine everything into a single tbl
react_dat <- purrr::reduce(list(current_dat, cases_hist, pos_hist, cases_col), left_join, by = "msa") %>% 
      # mutate(posList = tidyr::replace_na(posList, NA)) %>% 
      arrange(desc(cases_100k))

# sparkline column for cases per 100k trend
cases_spark <- function(class = NULL, ...) {
      colDef(
            name = "Cases per 100K Trend",
            cell = dui_for_reactable(
                  dui_sparkline(
                        data = htmlwidgets::JS("cellInfo.value.cases_list"),
                        valueAccessor = htmlwidgets::JS("(d) => d.cases[0]"),
                        renderTooltip = htmlwidgets::JS(
                              htmltools::HTML(
                                    "function (_ref) {
                   var datum = _ref.datum;
                   return React.createElement(
                       'div',
                       {style: {margin: 0, padding: 0}},
                       datum.date && React.createElement(
                         'div',
                         {style: {
                           backgroundColor: 'black', color: 'white',
                           padding: '4px 0px', margin: 0, textAlign: 'center'
                         }},
                         // good reference https://observablehq.com/@mbostock/date-formatting
                         //   for built-in JavaScript formatting
                         //   but need to convert string to Date with new Date()
                         new Date(datum.date).toLocaleString(undefined, {
                          'month': 'numeric',
                          'day': '2-digit'
                        })
                       ),
                       React.createElement(
                         'div',
                         {style: {fontWeight: 'bold', fontSize: '1.2em', padding: '6px 0'}},
                         datum.y ? datum.y.toLocaleString(undefined, {maximumFractionDigits: 0}) : '--'
                       )
                   );
                  }"
                              )),
                        components = list(
                              dui_sparklineseries(
                                    showLine = FALSE,
                                    showArea = TRUE,
                                    fill = htmlwidgets::JS("cellInfo.original.cases_color"),
                                    fillOpacity = 0.6                              ),
                              dui_sparkpointseries(
                                    points = list("max"),
                                    fill = htmlwidgets::JS("cellInfo.original.cases_color"),
                                    stroke = htmlwidgets::JS("cellInfo.original.cases_color"),
                                    renderLabel = htmlwidgets::JS("(d) => d.toFixed(0)"),
                                    labelPosition = "left",
                                    size = 3
                              )
                        )
                  )
            )
      )
}


posrate_spark <- function(class = NULL, ...){
      colDef(
            name = "Positive Test Rate Trend",
            cell = function(value, index) {
                  if(is.null(value)) return(dui_sparkline())
                  dui_sparkline(
                        data = value[[1]],
                        valueAccessor = htmlwidgets::JS("(d) => d.posRate"),
                        renderTooltip = htmlwidgets::JS(
                           htmltools::HTML(
                              "function (_ref) {
                                 var datum = _ref.datum;
                                 return React.createElement(
                                     'div',
                                     {style: {margin: 0, padding: 0}},
                                    datum.endDate && React.createElement(
                                       'div',
                                       {style: {
                                         backgroundColor: 'black', color: 'white',
                                         padding: '4px 0px', margin: 0, textAlign: 'center'
                                       }},
                                       new Date(datum.endDate).toLocaleString(undefined, {
                                         'month': 'numeric',
                                         'day': '2-digit'
                                       })
                                    ),
                                    React.createElement(
                                       'div',
                                       {style: {fontWeight: 'bold', fontSize: '1.2em', padding: '6px 0'}},
                                       datum.y ? datum.y.toLocaleString(undefined, {maximumFractionDigits: 0, style: 'percent'}) : '--'
                                    )
                                 );
                              }"
                           )
                        ),
                        components = list(
                              # dui_sparkpatternlines(
                              #    id = "band_pattern_misc",
                              #    height = 4,
                              #    width = 4,
                              #    stroke = posrate_col[index],
                              #    strokeWidth = 1,
                              #    orientation = list('diagonal')
                              # ),
                              # dui_sparkbandline(
                              #    band = list( from = list( y = 0 ), to = list( y = 0.05 ) ),
                              #    fill = "url(#band_pattern_misc)"
                              # ),
                              dui_sparklineseries(
                                    stroke = posrate_col[index]
                              )
                        )
                  )
            }
      )
}

react_tbl <- reactable(
      data = react_dat,
      borderless = TRUE,
      style = list(fontSize = "18px"),
      compact = TRUE,
      defaultPageSize = 5,
      defaultSortOrder = "desc",
      defaultSorted = "cases_100k",
      defaultColDef = colDef(
            align = "center",
            headerStyle = "align-self: flex-end; font-weight:normal;"
      ),
      rowStyle = list(
            alignItems = "center",
            # add back border here
            borderBottom = "1px solid lightgray"
      ),
      highlight = TRUE,
      columns = list(
            cases_color = colDef(show = FALSE),
            pos_color = colDef(show = FALSE),
            msa = colDef(
                  name = "Metropolitan Statistical Area"
            ),
            cases_100k = colDef(
                  name = "Cases per 100K",
            ),
            pos_rate = colDef(
                  name = "Positive Test Rate",
                  na = "–",
                  format = colFormat(percent = TRUE, digits = 1)
            ),
            casesList = cases_spark(),
            posList = posrate_spark()
            # posList = colDef(show = FALSE)
      )
) %>% 
      dui_add_reactable_dep()
react_tbl
