




pacman::p_load(swatches, dplyr, glue, extrafont, leaflet, leaflet.extras2, leaflet.esri, leafpop, reactable, dataui)

us_msa_tiles <- sf::read_sf(glue("{rprojroot::find_rstudio_root_file()}/data/shapefiles/msa-2019/tl_2019_us_cbsa.shp"))
# leaflet requires EPSG:4326
msa_tiles <- sf::st_transform(us_msa_tiles, 4326)
ind_msa_tiles <- msa_tiles %>% 
      filter(stringr::str_detect(NAME, "IN")) %>% 
      janitor::clean_names() %>% 
      mutate(# remove state abbrev from msa names
            name = stringr::str_remove_all(name, "([A-Z][A-Z]-)*"),
            name = stringr::str_remove_all(name, ", [A-Z]*"))

map2_current <- readr::read_csv(glue("{rprojroot::find_rstudio_root_file()}/data/msa-cases100-posrate-current.csv"))
map2_hist <- readr::read_csv(glue("{rprojroot::find_rstudio_root_file()}/data/msa-cases100-posrate-historic.csv"))

moody <- swatches::read_palette(glue("{rprojroot::find_rstudio_root_file()}/palettes/Moody Blooms.ase"))[c(1,4,5,8)]
pal <- colorNumeric("YlOrRd",
                    domain = seq(0.00, 0.20, by = 0.05),
                    na.color = "#800026")

posrate_col <- map2_hist %>% 
      select(pos_rate) %>% 
      filter(!is.na(pos_rate)) %>% 
      slice(n()) %>% 
      mutate(color = pal(pos_rate)#,
             #color = prismatic::clr_lighten(color, shift = .20)
             ) %>% 
      pull(color)
cases_col <- map2_hist %>% 
      select(cases_100k) %>% 
      slice(n()) %>% 
      mutate(cases_100k = round(cases_100k, 2),
             color = case_when(cases_100k < 1 ~ moody[[1]],
                               between(cases_100k, 1, 9.99) ~ moody[[2]],
                               between(cases_100k, 10, 24.99) ~ moody[[3]],
                               cases_100k >= 25 ~ moody[[4]])) %>% 
      pull(color)
spark_cols <- c(cases_col, posrate_col)


posrate_list <- map2_hist %>% 
      select(pos_rate) %>% 
      filter(!is.na(pos_rate)) %>% 
      pull(pos_rate) %>% 
      list()

spark_dat <- tibble(
      cases_100k = list(map2_hist$cases_100k),
      pos_rate = posrate_list
) %>% 
      tidyr::pivot_longer(cols = everything(),
                          names_to = "signal",
                          values_to = "value")

rt1 <- reactable(
      spark_dat,
      columns = list(
            value = colDef(
                  # use reactable very convenient conversion of htmlwidgets
                  #  we will focus on this in another article
                  #  more details on custom rendering
                  #  https://glin.github.io/reactable/articles/custom-rendering.html
                  cell = function(value, index) {
                        dui_sparkline(
                              data = value, # because we gave it a list use [[1]]
                              height = 80, # we will want to be specific here
                              components = list(
                                    dui_sparklineseries(
                                          showLine = FALSE,
                                          showArea = TRUE,
                                          fill = spark_cols[index]
                                    ),
                                    # interactivity
                                    dui_tooltip(components = list(
                                          dui_sparkverticalrefline(
                                                strokeDasharray = "4,4",
                                                stroke = gray.colors(10)[3]
                                          ),
                                          dui_sparkpointseries(
                                                stroke = spark_cols[index],
                                                fill = "#fff",
                                                renderLabel = htmlwidgets::JS("(d) => d.toFixed(2)")
                                          )
                                    ))
                              )
                              # end of interactivity
                        )
                        
                  }
            )
      ),
      theme = reactableTheme(
            color = "white",
            backgroundColor = "#252429"
      )
)
rt1





# leaflet map: add popup text, styling
map2_dat <- map2_current %>%
      rename(name = msa) %>%
      # create popup text
      mutate(cases_100k = round(cases_100k, 2),
             color = case_when(cases_100k < 1 ~ moody[[1]],
                               between(cases_100k, 1, 9.99) ~ moody[[2]],
                               between(cases_100k, 10, 24.99) ~ moody[[3]],
                               cases_100k >= 25 ~ moody[[4]]),
             # # dark map background so lighten the palette some
             # color = prismatic::clr_lighten(color,
             #                               shift = 0.20),
             # dark-red background for high values needs white text
             value_text = ifelse(cases_100k >= 25,
                                 glue("<b style='background-color:{color}; font-family:Roboto; font-size:15px; color:white'>{cases_100k}</b>"),
                                 glue("<b style='background-color:{color}; font-family:Roboto; font-size:15px'>{cases_100k}</b>")),
             label_html = glue("<b style= 'font-family:Roboto; font-size:15px'>{name}</br>Cases per 100,000 people</b>: {value_text}"),
             label_list = as.list(label_html)) %>% 
      left_join(ind_msa_tiles, by = "name") %>% 
      # converts tibble to a sf object for leaflet
      sf::st_as_sf()




# minzoom is the maximum you can zoomout; viceversa for maxzoom; 0 would be for zooming all the out
map2 <- leaflet(options = leafletOptions(minZoom = 6.5,
                                 maxZoom = 18,
                                 # remove caption
                                 attributionControl = FALSE)) %>%
      # {leaflet.esri}
      addEsriBasemapLayer(esriBasemapLayers$DarkGray, autoLabels = TRUE) %>%
      # sets starting point; coords for center of Indiana
      setView(lat = 40.2672, lng = 86.1349,
              zoom = 7) %>%
      # set panning range; if user tries to go beyond, it springs back
      setMaxBounds(lat1 = 37.62598, lng1 = -89.53418,
                   lat2 = 42.64689, lng2 = -83.05625) %>%
      addPolygons(data = map2_dat,
                  # weight is thickness of stroke
                  weight = 2, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.5,
                  color = ~color, popup = popupGraph(rt1, type = "html"),
                  label = purrr::map(map2_dat$label_list, htmltools::HTML),
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE))

map2





# # minzoom is the maximum you can zoomout; viceversa for maxzoom; 0 would be for zooming all the out
# leaflet(data = ci_shared_yday,
#         options = leafletOptions(minZoom = 6.5,
#                                  maxZoom = 18,
#                                  # remove caption
#                                  attributionControl = FALSE)) %>% 
#       # black and white basemap
#       addProviderTiles("Stamen.Toner") %>% 
#       # sets starting point; coords for center of Indiana
#       setView(lat = 40.2672, lng = 86.1349,
#               zoom = 7) %>% 
#       # set panning range; if user tries to go beyond, it springs back
#       setMaxBounds(lat1 = 37.62598, lng1 = -89.53418,
#                    lat2 = 42.64689, lng2 = -83.05625) %>% 
#       # add msa shapes
#       addPolygons(# weight is thickness of stroke
#             weight = 2, smoothFactor = 0.5,
#             opacity = 1.0, fillOpacity = 0.5,
#             color = ~color, popup = ~popup,
#             popupOptions = ,
#             # bringtofront makes highlight stroke standout more
#             highlightOptions = highlightOptions(color = "black", weight = 2,
#                                                 bringToFront = TRUE)) %>% 
#       addLegend("bottomleft", pal = pal_rev,
#                 opacity = 1, values = c(2.5, 0.5),
#                 # reverses direction of values in legend
#                 labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
#       ) %>% 
#       # {leaflet.extras}
#       addResetMapButton()
