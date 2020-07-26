
pacman::p_load(swatches, dplyr, glue, extrafont, htmltools, reactable, dataui)


map2_current <- readr::read_csv(glue("{rprojroot::find_rstudio_root_file()}/data/msa-cases100-posrate-current.csv"))
map2_hist <- readr::read_csv(glue("{rprojroot::find_rstudio_root_file()}/data/msa-cases100-posrate-historic.csv"))

moody <- swatches::read_palette(glue("{rprojroot::find_rstudio_root_file()}/palettes/Moody Blooms.ase"))[c(1,4,5,8)]
pal <- leaflet::colorNumeric("YlOrRd",
                             domain = seq(0.00, 0.20, by = 0.05),
                             na.color = "#800026")


# color palettes
posrate_col <- map2_current %>% 
   select(msa, pos_rate) %>% 
   filter(!is.na(pos_rate)) %>%
   mutate(pos_color = pal(pos_rate),
          pos_color_light = unclass(prismatic::clr_lighten(pos_color, shift = .40))) %>% 
   select(-pos_rate)


cases_col <- map2_current %>% 
      select(msa, cases_100k) %>% 
      mutate(cases_100k = round(cases_100k, 2),
             cases_color = case_when(cases_100k < 1 ~ moody[[1]],
                                     between(cases_100k, 1, 9.99) ~ moody[[2]],
                                     between(cases_100k, 10, 24.99) ~ moody[[3]],
                                     cases_100k >= 25 ~ moody[[4]]),
             cases_color = unclass(prismatic::clr_darken(cases_color, shift = .50)),
             cases_color_light = unclass(prismatic::clr_lighten(cases_color, shift = .60))) %>% 
      arrange(desc(cases_100k)) %>% 
      select(msa, cases_color, cases_color_light)






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
react_dat <- purrr::reduce(list(current_dat, cases_hist, pos_hist,
                                cases_col, posrate_col),
                           left_join, by = "msa") %>% 
      arrange(desc(cases_100k))





# sparkline column specifications
cases_spark <- function(...) {
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
                     null,
                     datum.date && React.createElement(
                        'span',
                        {style: {
                             backgroundColor: 'black', color: 'white',
                             padding: '3px', margin: '0px 4px 0px 0px', textAlign: 'center'
                           }},
                        datum.date[0].split('-').slice(1).join('/')
                     ),
                     React.createElement(
                        'span',
                        {style: {
                        fontWeight: 'bold', fontSize: '1.1em',
                        padding: '2px'
                        }},
                        datum.y ? datum.y.toLocaleString(undefined, {maximumFractionDigits: 0}) : '--'
                     )
                   );
                  }"
               )
            ),
            components = list(
               dui_sparklineargradient(
                  id = htmlwidgets::JS("'cases' + cellInfo.original.msa.split(' ').join('-')"),
                  from = htmlwidgets::JS("cellInfo.original.cases_color"),
                  to = htmlwidgets::JS("cellInfo.original.cases_color_light"),
                  fromOffset = "40%"
               ),
               dui_sparklineseries(
                  showLine = FALSE,
                  showArea = TRUE,
                  fill = htmlwidgets::JS("'url(#cases' + cellInfo.original.msa.split(' ').join('-') + ')'"),
                  # stroke = htmlwidgets::JS("cellInfo.original.cases_color"),
                  fillOpacity = htmlwidgets::JS("(d, i) => (i === 24 ? 1 : 0.5)")
               ),
               dui_sparkpointseries(
                  points = list("max"),
                  fill = htmlwidgets::JS("cellInfo.original.cases_color"),
                  stroke = htmlwidgets::JS("cellInfo.original.cases_color_light"),
                  renderLabel = htmlwidgets::JS("(d) => React.createElement('tspan',{fontWeight: 'bold'},d.toFixed(0))"),
                  labelPosition = "left",
                  size = 3
               )
            )
         )
      )
   )
}



posrate_spark <- function(...){
   colDef(
      name = "Positive Test Rate Trend",
      cell = dui_for_reactable(
         dui_sparkline(
            data = htmlwidgets::JS("cellInfo.value.pos_list"),
            valueAccessor = htmlwidgets::JS("(d) => d.posRate[0]"),
            renderTooltip = htmlwidgets::JS(
               htmltools::HTML(
                  "function (_ref) {
                   var datum = _ref.datum;
                   return React.createElement(
                     'div',
                     null,
                     datum.endDate && React.createElement(
                        'span',
                        {style: {
                             backgroundColor: 'black', color: 'white',
                             padding: '3px', margin: '0px 4px 0px 0px', textAlign: 'center'
                           }},
                        datum.endDate[0].split('-').slice(1).join('/')
                     ),
                     React.createElement(
                        'span',
                        {style: {
                        fontWeight: 'bold', fontSize: '1.1em',
                        padding: '2px'
                        }},
                        datum.y ? datum.y.toLocaleString(undefined, {maximumFractionDigits: 1, style: 'percent'}) : '--'
                     )
                   );
                  }"
               )
            ),
            components = list(
               # dui_sparkpatternlines(
               #    id = htmlwidgets::JS("'pos' + cellInfo.original.msa.split(' ').join('-')"),
               #    height = 4,
               #    width = 4,
               #    stroke = htmlwidgets::JS("cellInfo.original.pos_color_light"),
               #    strokeWidth = 1,
               #    orientation = list('diagonal')
               # ),
               # dui_sparkbandline(
               #    band = list( from = list( y = 0 ), to = list( y = 0.05 ) ),
               #    fill = htmlwidgets::JS("'pos' + cellInfo.original.msa.split(' ').join('-')")
               # ),
               dui_sparkpatternlines(
                  id = "band_pattern_misc",
                  height = 4,
                  width = 4,
                  stroke = "#aaa",
                  strokeWidth = 1,
                  orientation = list('diagonal')
               ),
               dui_sparkbandline(
                  band = list( from = list( y = 0 ), to = list( y = 0.05 ) ),
                  fill = "url(#band_pattern_misc)"
               ),
               dui_sparklineseries(
                  stroke = htmlwidgets::JS("cellInfo.original.pos_color")
               ),
               dui_sparkpointseries(
                  points = list("max"),
                  fill = htmlwidgets::JS("cellInfo.original.pos_color"),
                  stroke = htmlwidgets::JS("cellInfo.original.pos_color_light"),
                  renderLabel = htmlwidgets::JS("d => React.createElement('tspan',{fontWeight: 'bold'},d.toLocaleString(undefined, {maximumFractionDigits: 1, style: 'percent'}))"),
                  labelPosition = htmlwidgets::JS("(d, i) => (i === 0 ? 'right' : 'left')"),
                  size = 3
               )
            )
         )
      )
   )
}

# cases_color_spec <- function(show = FALSE, ...){
#    colDef(
#       show = show,
#       cell = htmlwidgets::JS(
#          "function(cellInfo) {
#               return React.createElement(
#                 'svg',
#                 null,
#                 React.createElement(
#                   'rect',
#                  {style: {fill: cellInfo.value},height:100,width:100}
#                 )
#               )
#             }")
#    )
# }
# posrate_color_spec <- function(show = FALSE, ...){
#    colDef(
#       show = show,
#       cell = htmlwidgets::JS(
#          "function(cellInfo) {
#               return React.createElement(
#                 'svg',
#                 null,
#                 React.createElement(
#                   'rect',
#                  {style: {fill: cellInfo.value || '#ccc'},height:100,width:100}
#                 )
#               )
#             }")
#    )
# }

reactable(
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
         # cases_color = cases_color_spec(),
         # pos_color = posrate_color_spec(),
         cases_color = colDef(show = FALSE),
         pos_color = colDef(show = FALSE),
         cases_color_light = colDef(show = FALSE),
         pos_color_light = colDef(show = FALSE),
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
      )
) %>% 
      dui_add_reactable_dep()


