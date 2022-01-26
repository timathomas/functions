# ==========================================================================
# Package load or install and load 
# ==========================================================================

RR <- function(y, n){
  local = y/n
  regional = sum(y, na.rm = TRUE)/sum(n, na.rm = TRUE)
  RR = local/regional
  RR
}

latest_file <- function(path)
  list.files(path,full.names = T) %>%
    enframe(name = NULL) %>%
    bind_cols(pmap_df(., file.info)) %>%
    filter(mtime==max(mtime)) %>%
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
