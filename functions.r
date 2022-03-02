# ==========================================================================
# Package load or install and load 
# ==========================================================================

RR <- function(y, n){
  local = y/n
  regional = sum(y, na.rm = TRUE)/sum(n, na.rm = TRUE)
  RR = local/regional
  RR
}

latest_file <- function(path, keyword = NULL)
  list.files(path,full.names = T) %>%
    enframe(name = NULL) %>%
    bind_cols(pmap_df(., file.info)) %>%
    filter(mtime==max(mtime), grepl(keyword)) %>%
    pull(value)


options(scipen=10, width=system("tput cols", intern=TRUE), tigris_use_cache = TRUE) # avoid scientific notation

ipak <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg)) 
        install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
}

ipak_gh <- function(pkg){
    new.pkg <- pkg[!(sub('.*/', '', pkg) %in% installed.packages()[, "Package"])]
    if (length(new.pkg)) 
        remotes::install_github(new.pkg, dependencies = TRUE)
    sapply(sub('.*/', '', pkg), require, character.only = TRUE)
}

# Example
# ipak_gh(c("lmullen/gender", "mtennekes/tmap", "jalvesaq/colorout", "timathomas/neighborhood", "arthurgailes/rsegregation"))

# ==========================================================================
# Mapping 
# ==========================================================================

map_main <- function(data = NULL, title = NULL, zoom = TRUE, position = 'topright', basegroups, hidegroups = NULL, ...){
        leaflet(
            data, 
            options = leafletOptions(title = title, zoomControl = zoom)
            ) %>% 
        addLayersControl(
            position = position, 
            baseGroups = basegroups, 
            options = layersControlOptions(collapsed = FALSE, maxHeight = 'auto')) %>%
        hideGroup(hidegroups) %>%
        addMapPane(name = 'polygons', zIndex = 410) %>%
        addMapPane(name = 'maplabels', zIndex = 420) %>% 
        addProviderTiles('CartoDB.PositronNoLabels') %>%
        addProviderTiles("Stamen.TerrainLabels", 
                        options = leafletOptions(pane = 'maplabels'),
                        group = 'map labels') %>%
        addEasyButton(
            easyButton(
              icon='fa-crosshairs',
              title='My Location',
              onClick=JS('function(btn, map){ map.locate({setView: true}); }'))) %>%
        # Note: this function below allows leaflet to show only one legend per baseGroup layer. You will have to use the `className` option in the `addLegend` function. Note, you can have groups with no more than one space but when adding the group name to `className`, you will have to concatenate the two word group name into one and apend 'info legend xxx' to the front, where 'xxx' is your group name. 
        htmlwidgets::onRender("
            function(el, x) {
               var updateLegend = function () {
                    var selectedGroup = document.querySelectorAll('input:checked'0].nextSibling.innerText.substr(1);
                  var selectedClass = selectedGroup.replace(' ', '');
                    document.querySelectorAll('.legend').forEach(a =a.hidden=true);
                  document.querySelectorAll('.legend').forEach(l => {
                     if (l.classList.contains(selectedClass)) l.hidden=false;
                  });
               };
               updateLegend();
               this.on('baselayerchange', el => updateLegend());
            }"
        ) %>%
        leaflet.extras::addSearchOSM()
    }

map_polygons <- function(data, group, label, color, popup, position = 'topright',  legendpal, values, title = NULL, ...){
    df <- data
    addPolygons(
        data = df, 
        group = group,
        label = ~label,
        labelOptions = labelOptions(textsize = '12px'),
        fillOpacity = .5,
        color = ~color,
        stroke = TRUE,
        weight = 1,
        opacity = .3,
        highlightOptions = highlightOptions(
          color = '#ff4a4a',
          weight = 5,
          bringToFront = FALSE
          ),
        popup = ~popup,
        popupOptions = popupOptions(maxHeight = 215, closeOnClick = TRUE)
      ) %>%
      addLegend(
        data = df,
        position = 'topright',
        pal = legendpal,
        values = ~values,
        group = group,
        className = paste0('info legend ', str_replace(group, fixed(" "), ""),
        title = title
      ))
    }

# ==========================================================================
# Erase Water
# ==========================================================================

# rt_water <- function(s, c = NULL, a = 500000){
#     w <- tigris::area_water(s, c, class = "sf") %>% filter(AWATER > a)
#     t <- tigris::tracts(s, c, class = "sf")
#     e <- sf::st_difference(t, sf::st_union(sf::st_combine(w)))
# }

rm_water <- function(
    sf_df,
    s,
    c = tigris::list_counties(s)$county_code,
    a = 500000){
    # c <- tigris::fips_codes %>% filter(state == s) %>% pull(county_code)
    w <- tigris::area_water(state = s, county = c, class = "sf") %>% dplyr::filter(AWATER > a) %>%
         sf::st_transform(sf::st_crs(sf_df))
    e <- sf::st_difference(sf::st_make_valid(sf_df), sf::st_union(w)) %>%
      sf::st_collection_extract('POLYGON')
    rm(w)
    return(e)
}

rt_water(map_pan_df_tr, "CA", counties)
    # ==========================================================================
    # DO NOT RUN
    #        kc_water <- tigris::area_water("WA", "King", class = "sf")
    #        kc <- tigris::tracts("WA", "King", class = "sf")
    #
    #        kc_erase <- st_erase(kc, kc_water)
    #
    #        king_bg <- geo_join(kc_erase,
    #                            nt,
    #                            by = c("GEOID"))
    # END DO NOT RUN
    # ==========================================================================

#
# Erase water
# --------------------------------------------------------------------------

no_water <- function(df, st, size = 500000){
    county_water <- tigris::list_counties(st)$county_code

    water <- area_water(
      state = st,
      county = county_water,
      class = 'sf') %>%
      # Filter out small bodies of water
      filter(AWATER > size) %>%
      # Set projection
      st_transform(crs = st_crs(final_df))

    # Remove water
    # NOTE: the st_erase function sometimes converts the sf geometry from
    # a MULTIPOLYGON to a GEOMETRYCOLLECTION. Leaflet can't handle a GEOMETRYCOLLECTION
    # so you have to use `st_collection_extract('POLYGON') function
    # to convert the geometry back to a MULTIPOLYGON. To check if this
    # happened you can run st_geometry(object) to see what the output is.

    # Apply function to CA tracts
    nw <-
      st_difference(final_df, st_union(water)) %>%
      st_collection_extract('POLYGON')

    gc()
    return(nw)
}

# counties <- function(state, df)
#   counties(state) %>%
#   st_transform(st_crs(df)) %>%
#   .[df, ]  %>%
#   arrange(STATEFP, COUNTYFP) %>%

# st_geometry(counties) <- NULL

# state_water <- counties %>% pull(STATEFP)
# county_water <- counties %>% pull(COUNTYFP)

# water <-
# map2_dfr(state_water, county_water,
#   function(states = state_water, counties = county_water){
#     area_water(
#       state = states,
#       county = counties,
#       class = 'sf') %>%
#     filter(AWATER > 500000)
#     }) %>%
# st_transform(st_crs(bay5))

#
# Remove water & non-urban areas & simplify spatial features
# --------------------------------------------------------------------------