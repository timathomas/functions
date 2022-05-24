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

ipak <- function(pkg, update = FALSE){
    # if(update = TRUE){
    #     # update.packages(ask = FALSE)
    # }
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg)) 
        install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
}

ipak_gh <- function(pkg, force = FALSE){
    new.pkg <- pkg[!(sub('.*/', '', pkg) %in% installed.packages()[, "Package"])]
    if (length(new.pkg)) 
        remotes::install_github(new.pkg, dependencies = TRUE, force = force)
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

    # rm_water(map_pan_df_tr, "CA", counties)
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

#
# BART partial dependency plot in ggplot
# --------------------------------------------------------------------------

pd_plotGGPLOT <- function (bart_machine, j, levs = c(0.05, seq(from = 0.1, to = 0.9,
                                              by = 0.1), 0.95), lower_ci = 0.025, upper_ci = 0.975, prop_data = 1)
{
  #check_serialization(bart_machine)
  if (class(j) == "integer") {
    j = as.numeric(j)
  }
  if (class(j) == "numeric" && (j < 1 || j > bart_machine$p)) {
    stop(paste("You must set j to a number between 1 and p =",
               bart_machine$p))
  }
  else if (class(j) == "character" && !(j %in% bart_machine$training_data_features)) {
    stop("j must be the name of one of the training features (see \"<bart_model>$training_data_features\")")
  }
  else if (!(class(j) == "numeric" || class(j) == "character")) {
    stop("j must be a column number or column name")
  }
  x_j = bart_machine$model_matrix_training_data[, j]
  if (length(unique(na.omit(x_j))) <= 1) {
    warning("There must be more than one unique value in this training feature. PD plot not generated.")
    return()
  }
  x_j_quants = unique(quantile(x_j, levs, na.rm = TRUE))
  if (length(unique(x_j_quants)) <= 1) {
    warning("There must be more than one unique value among the quantiles selected. PD plot not generated.")
    return()
  }
  n_pd_plot = round(bart_machine$n * prop_data)
  bart_predictions_by_quantile = array(NA, c(length(x_j_quants),
                                             n_pd_plot, bart_machine$num_iterations_after_burn_in))
  for (q in 1:length(x_j_quants)) {
    indices = sample(1:bart_machine$n, n_pd_plot)
    test_data = bart_machine$X[indices, ]
    test_data[, j] = rep(x_j_quants[q], n_pd_plot)
    bart_predictions_by_quantile[q, , ] = bart_machine_get_posterior(bart_machine,
                                                                     test_data)$y_hat_posterior_samples
    cat(".")
  }
  cat("\n")
  if (bart_machine$pred_type == "classification") {
    bart_predictions_by_quantile = qnorm(bart_predictions_by_quantile)
  }
  bart_avg_predictions_by_quantile_by_gibbs = array(NA, c(length(x_j_quants),
                                                          bart_machine$num_iterations_after_burn_in))
  for (q in 1:length(x_j_quants)) {
    for (g in 1:bart_machine$num_iterations_after_burn_in) {
      bart_avg_predictions_by_quantile_by_gibbs[q, g] = mean(bart_predictions_by_quantile[q,
                                                                                          , g])
    }
  }
  bart_avg_predictions_by_quantile = apply(bart_avg_predictions_by_quantile_by_gibbs,
                                           1, mean)
  bart_avg_predictions_lower = apply(bart_avg_predictions_by_quantile_by_gibbs,
                                     1, quantile, probs = lower_ci)
  bart_avg_predictions_upper = apply(bart_avg_predictions_by_quantile_by_gibbs,
                                     1, quantile, probs = upper_ci)
  var_name = ifelse(class(j) == "character", j, bart_machine$training_data_features[j])
  ylab_name = ifelse(bart_machine$pred_type == "classification",
                     "Partial Effect (Probits)", "Partial Effect")

  # GGPLOT
  gg_output <- ggplot() +
    geom_polygon(aes(c(x_j_quants,
                       rev(x_j_quants)),
                     c(bart_avg_predictions_upper,
                       rev(bart_avg_predictions_lower))),
                 fill = "gray87") +
    geom_line(aes(x_j_quants, bart_avg_predictions_lower), col = "black", linetype = "dashed") +
    #geom_point(aes(x_j_quants, bart_avg_predictions_lower), col = "black", shape = 21) +
    geom_line(aes(x_j_quants, bart_avg_predictions_upper), col = "black", linetype = "dashed") +
    #geom_point(aes(x_j_quants, bart_avg_predictions_upper), col = "black", shape = 21) +
    geom_line(aes(x_j_quants, bart_avg_predictions_by_quantile), lwd = 0.5) +
    geom_point(aes(x_j_quants, bart_avg_predictions_by_quantile), size = 0.8, shape = 21, fill = "black") +
    coord_cartesian(ylim = c(min(bart_avg_predictions_lower,
                                 bart_avg_predictions_upper),
                             max(bart_avg_predictions_lower,
                                 bart_avg_predictions_upper))) +
    ggthemes::theme_few() +
    scale_y_continuous(position = "right") +
    labs(x = paste(var_name, "plotted at specified quantiles"),
         y = ylab_name)

  return(gg_output)
}