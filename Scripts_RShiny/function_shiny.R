# Function to generate transparent colours in plots
transp <- function(col, alpha=.5){
  res <- apply(col2rgb(col),2, function(c) 
    rgb(c[1]/255, c[2]/255, c[3]/255, alpha))
  return(res)
}

## Function to generate first map in Predictors tab (incidence, cases, or changes since
# last week of data at the latest forecasts date)
figure_inc <- function(pred, nuts, age, map, pop, pop_age, obs, quant = .5, type = "incidence"){
  if(nuts == 1){
    # If nuts-2 level is selected, select the rows of pred corresponding to nuts-2 
    # 14-day forecasts, with the condition and quantile specified by the user.
    pred_f <- pred[pred$level == "nuts2_tot" & pred$type == "14_day_cases" &
                     pred$quant == paste0(quant * 100, "%") & pred$conditions == max(pred$conditions),]
    # If nuts-2 is selected, the way to extract region names depends on the selected country
    if(substr(colnames(pop), 1, 2)[1] == "FR"){
      group_reg <- substr(colnames(obs), 4, 6)
      group_map <- substr(map$key, start = 4, stop = 6)
    } else if(substr(colnames(pop), 1, 2)[1] == "CZ"){
      group_reg <- substr(colnames(obs), 4, 4)
      group_map <- substr(map$key, start = 4, stop = 4)
    } else if(substr(colnames(pop), 1, 2)[1] == "IT"){
      group_reg <- substr(colnames(obs), 1, 4)
      group_map <- substr(map$key, start = 1, stop = 4)
    }
    names(group_map) <- map$key
  } else if(nuts == 2){
    # If nuts-3 level is selected, select the rows of pred corresponding to nuts-3 
    # 14-day forecasts, with the condition and quantile specified by the user.
    pred_f <- pred[pred$level == "nuts3_tot" & pred$type == "14_day_cases" &
                     pred$quant == paste0(quant * 100, "%") & pred$conditions == max(pred$conditions),]
    group_reg <- sub("[.].*", "", colnames(obs))
    group_map <- NULL
  }
  # Compute number of inhabitants in each age group / region
  pop_per_entry <- pop[1,] * pop_age[1,]
  # If incidence map: divide the number of cases per region by the number of inhabitants
  if(type == "incidence"){
    # Aggregate pop_per_entry by nuts groups
    pop_per_region <- aggregate(pop_per_entry, list(group_reg), sum)
    # Extract number of inhabitant per region
    vec_pop <- pop_per_region$x
    # Set names as region name
    names(vec_pop) <- pop_per_region$Group.1
    incidence_per_region <- cbind.data.frame(
      region = pred_f$reg, 
      q_inc = pred_f$n_cases / vec_pop[pred_f$reg]
    )
  }
  # If cases map: Merge pred_f$reg and pred_f$n_cases
  if(type == "cases"){
    incidence_per_region <- cbind.data.frame(region = pred_f$reg, q_inc = pred_f$n_cases)
    title_map <- "Number of cases"
    
  }
  # If changes map: Compute the number of cases in the past week, and compare it
  # to pred_f$n_cases / 2
  if(type == "changes"){
    weekly_last_cases <- obs@observed[nrow(obs@observed) - seq(7),] %>% colSums()
    mat_aggreg_weekly <- aggregate(weekly_last_cases, list(group_reg), sum)
    aggreg_weekly <- mat_aggreg_weekly[,2]
    names(aggreg_weekly) <- mat_aggreg_weekly[,1]
    incidence_per_region <- cbind.data.frame(
      region = pred_f$reg, 
      q_inc = (pred_f$n_cases / 2 - aggreg_weekly[pred_f$reg]) * 100 / aggreg_weekly[pred_f$reg]
    )
    title_map <- "Change in cases compared to the last week of data"
  }
  
  # Define vector containing the incidence in each region
  vec_inc <- incidence_per_region$q_inc
  names(vec_inc) <- incidence_per_region$region
  
  if(is.null(group_map)){
    # If group_map is not specified, region names is matched to map$key
    if(any(!is.element(incidence_per_region$region, map$key))) 
      stop("If the aggregation names are not map$key, use the argument group_map to set the correspondence between map$key and group_reg")
    # Add incidence to map
    map$inc <- vec_inc[map$key]
  } else {
    # Add column "region" to map, which are then matched with incidence_per_region$region
    map$region <- group_map[map$key]
    # Group NUTS-3 regions into NUTS-2 areas
    map <- map %>% group_by(region) %>% summarise()
    
    map$inc <- vec_inc[map$region]
  }
  if(type == "incidence"){
    # Define breaks
    inc_breaks <- c(0, 50, 100, 500, 1000, 2500, max(c(10000, map$inc)))
    title_map <- "Incidence"
    # Add discrete column to map
    map$inc_cat <- cut(map$inc, breaks = inc_breaks, 
                       labels = c("0-50", "50-100", "100-500", 
                                  "500-1,000", "1,000-2,500", ">2,500"))
    lab_main <- "Average 14-day case incidence per 100,000 hab."
  } 
  if(type == "cases"){
    # Define breaks and add discrete column to map
    if(length(map$inc) < 20){
      inc_breaks <- c(0, 1e3, 1e4, 2.5e4, 5e4, 1e5, max(c(1e6, map$inc))) 
      map$inc_cat <- cut(map$inc, breaks = inc_breaks, 
                         labels = c("0-1,000", "1,000-10,000", "10,000-25,000", 
                                    "25,000-50,000", "50,000-100,000", ">100,000"))
    } else{
      inc_breaks <- c(0, 1e2, 1e3, 5e3, 1e4, 5e4, max(c(5e5, map$inc))) 
      map$inc_cat <- cut(map$inc, breaks = inc_breaks, 
                         labels = c("0-100", "100-1,000", "1,000-5,000", 
                                    "5,000-10,000", "10,000-50,000", ">50,000"))
    }
    lab_main <- "14-day average case number predicted"
  }
  if(type == "changes"){
    # Define breaks and add discrete column to map
    inc_breaks <- c(-100, -50, -10, 0, 10, 50, max(c(100, map$inc)))
    map$inc_cat <- cut(map$inc, breaks = inc_breaks, 
                       labels = c("-100% - -50%", "-50% - -10%", "-10% - 0%", 
                                  "0% - 10%", "10% - 50%", "50% - 100%"))
    lab_main <- "Average change in cases compared to the last week of data"
  }
  # Set colour scheme
  cols_vect <- c("#2c7bb6", "#67a9cf", "#e0f3f8", "#ffffbf", "#fee090", "#fdae61")
  
  # Create map
  map_inc <- ggplot(map) +  geom_sf(aes(fill = inc_cat)) +
    scale_fill_manual(na.translate = F, guide = guide_legend(),
                      values = cols_vect, name = lab_main, drop = FALSE) + 
    theme_classic(base_size = 10) + 
    theme(axis.text.x = element_blank(), axis.text.y = element_blank(),
          strip.background = element_blank(), strip.text = element_text(size = 10),  
          axis.ticks = element_blank(), axis.line = element_blank(), 
          plot.title = element_text(hjust = 0.5, size = 15), legend.position = "bottom") + 
    guides(fill = guide_legend(title.position="top", title.hjust = 0.5, byrow = T)) + 
    labs(title = title_map)
  
  # Plot map
  print(map_inc)
  
}

# Function to generate the risk of transmission / importation maps (Predictors tab)
plot_predictor <- function(predictor, group_reg, map, pop, pop_age, type, group_map = NULL){
  # Extract neighbourhood and endemic predictors in "predictor" object (from list_output)
  pred_auto <- predictor$ne.exppred[nrow(predictor$ne.exppred),]
  pred_end <- predictor$end.exppred[nrow(predictor$end.exppred),]
  # Compute the total number of inhabitants per region / age group
  pop_per_entry <- pop[1,] * pop_age[1,]
  
  # Aggregate the number of inhabitants per region
  pop_per_region <- aggregate(pop_per_entry, list(group_reg), sum)
  # Similarly, sum the number of importations in each region
  aggreg_pred_end <- aggregate(pred_end, list(group_reg), sum)
  
  # Compute the number of secondary cases per age group / region
  pred_auto_pop <- pred_auto * pop_per_entry
  # Aggregate by region
  aggreg_pred_auto <- aggregate(pred_auto_pop, list(group_reg), sum)
  colnames(aggreg_pred_auto) <- colnames(aggreg_pred_end) <- c("region", "predictor")
  # Divide by the overall number of inhabitants in each region (i.e. corresponds to the
  # average number of secondary cases for an inhabitant chosen at random)
  aggreg_pred_auto$predictor <- aggreg_pred_auto$predictor / pop_per_region$x
  
  
  # Define vectors containing each predictor in each region
  vec_pred_auto <- aggreg_pred_auto$predictor
  names(vec_pred_auto) <- aggreg_pred_auto$region
  vec_pred_end <- aggreg_pred_end$predictor
  names(vec_pred_end) <- aggreg_pred_end$region
  
  if(is.null(group_map)){
    # If group_map is not specified, region names is matched to map$key
    if(any(!is.element(aggreg_pred_auto$region, map$key))) 
      stop("If the aggregation names are not map$key, use the argument group_map to set the correspondence between map$key and group_reg")
    
    # Add them to map
    map$pred_auto <- vec_pred_auto[map$key]
    map$pred_end <- vec_pred_end[map$key]
  } else {
    # Add column "region" to map, which are then matched with aggreg_pred_auto$region
    map$region <- group_map[map$key]
    # Group NUTS-3 regions into NUTS-2 areas
    map <- map %>% group_by(region) %>% summarise()
    
    map$pred_auto <- vec_pred_auto[map$region]
    map$pred_end <- vec_pred_end[map$region]
  }
  # Compute percentage of the maximum value
  map$pred_auto <- map$pred_auto/max(map$pred_auto)
  map$pred_end <- map$pred_end/max(map$pred_end)
  # Generate discrete variable and add it to map
  pred_breaks <- c(0, 0.1, .25, .5, .75, .9, 1)
  map$pred_auto_cat <- cut(map$pred_auto, breaks = pred_breaks, 
                           labels = c("0-10%", "10-25%", "25-50%", "50-75%", "75-90%", "90-100%"))
  map$pred_end_cat <- cut(map$pred_end, breaks = pred_breaks, 
                          labels = c("0-10%", "10-25%", "25-50%", "50-75%", "75-90%", "90-100%"))
  
  # Define colour scheme
  cols_vect <- c("#2c7bb6", "#67a9cf", "#e0f3f8", "#ffffbf", "#fee090", "#fdae61")
  
  ## Generate maps
  map_auto <- ggplot(map) +  geom_sf(aes(fill = pred_auto_cat)) +
    scale_fill_manual(na.translate = F, guide = guide_legend(),
                      values = cols_vect, drop = FALSE,
                      name = "Percentage compared to the highest value in the country") + 
    theme_classic(base_size = 10) + 
    theme(axis.text.x = element_blank(), axis.text.y = element_blank(),
          strip.background = element_blank(), strip.text = element_text(size = 10),  
          axis.ticks = element_blank(), axis.line = element_blank(), 
          plot.title = element_text(hjust = 0.5, size = 15), legend.position = "bottom") + 
    guides(fill = guide_legend(title.position="top", title.hjust = 0.5, byrow = T)) + 
    labs(title = "Local risk of transmission")
  
  map_end <- ggplot(map) +  geom_sf(aes(fill = pred_end_cat)) +
    scale_fill_manual(na.translate = F, guide = guide_legend(),
                      values = cols_vect, drop = FALSE,
                      name = "Percentage compared to the highest value in the country") + 
    theme_classic(base_size = 10) + 
    theme(axis.text.x = element_blank(), axis.text.y = element_blank(),
          strip.background = element_blank(), strip.text = element_text(size = 10),  
          axis.ticks = element_blank(), axis.line = element_blank(), 
          plot.title = element_text(hjust = 0.5, size = 15), legend.position = "bottom") + 
    guides(fill = guide_legend(title.position="top", title.hjust = 0.5, byrow = T)) + 
    labs(title = "Local risk of importation")
  
  # Plot each map
  if(type == "ar") print(map_auto)
  if(type == "en") print(map_end)
}

# Function to generate interactive map showing the incidence in each region (Forecasts and Scenario tabs)
leaflet_inc <- function(pred, nuts, map, pop, pop_age, condition, type, quant = .5, region_selected = NULL){
  # Select the rows of pred that correspond to the condition specified by the user.
  pred <- pred[pred$conditions == condition, ]
  if(nuts == 1) {
    # If nuts-2 level is selected,choose the rows of pred corresponding to nuts-2 
    # 14-day forecasts, with the quantile specified by the user.
    pred_f <- pred[pred$level == "nuts2_tot" & pred$type == "14_day_cases" &
                     pred$quant == paste0(quant * 100, "%"),]
    # If nuts-2 is selected, the way to extract region names depends on the selected country
    if(substr(colnames(pop), 1, 2)[1] == "FR"){
      group_reg <- substr(colnames(pop), 4, 6)
      group_map <- substr(map$key, start = 4, stop = 6)
    } else if(substr(colnames(pop), 1, 2)[1] == "CZ"){
      group_reg <- substr(colnames(pop), 4, 4)
      group_map <- substr(map$key, start = 4, stop = 4)
    } else if(substr(colnames(pop), 1, 2)[1] == "IT"){
      group_reg <- substr(colnames(pop), 1, 4)
      group_map <- substr(map$key, start = 1, stop = 4)
    }
    names(group_map) <- map$key
  } else if(nuts == 2){
    # If nuts-3 level is selected, choose the rows of pred corresponding to nuts-3 
    # 14-day forecasts, with the quantile specified by the user.
    pred_f <- pred[pred$level == "nuts3_tot" & pred$type == "14_day_cases" &
                     pred$quant == paste0(quant * 100, "%"),]
    group_reg <- sub("[.].*", "", colnames(pop))
    group_map <- NULL
  }
  
  # Compute the number of inhabitants per region and age groups, and merge it by region
  pop_per_entry <- pop[1,] * pop_age[1,]
  pop_per_region <- aggregate(pop_per_entry, list(group_reg), sum)
  # Define vec_pop, which contains the number of inhabitants per region
  vec_pop <- pop_per_region$x
  names(vec_pop) <- pop_per_region$Group.1
  
  # Compute the incidence per region by dividing the number of cases (or death) 
  # by the number of inhabitants
  if(type == 1){
    incidence_per_region <- cbind.data.frame(
      region = pred_f$reg, 
      q_inc = pred_f$n_cases / vec_pop[pred_f$reg]
    )
  } else {
    incidence_per_region <- cbind.data.frame(
      region = pred_f$reg, 
      q_inc = pred_f$n_deaths / vec_pop[pred_f$reg]
    )
  }
  
  # Define vector containing the incidence in each region
  vec_inc <- incidence_per_region$q_inc
  names(vec_inc) <- incidence_per_region$region
  
  if(is.null(group_map)){
    # If group_map is not specified, region names is matched to map$key
    if(any(!is.element(incidence_per_region$region, map$key))) 
      stop("If the aggregation names are not map$key, use the argument group_map to set the correspondence between map$key and group_reg")
    # Add incidence to map
    map$inc <- vec_inc[map$key]
  } else {
    # Add region column to map, which are then be matched with incidence_per_region$region
    map$region <- group_map[map$key]
    # Group NUTS-3 regions into NUTS-2 areas
    map <- map %>% group_by(region) %>% summarise()
    
    map$inc <- vec_inc[map$region]
  }
  
  # Generate discrete variable and add it to map
  if(type == 1){
    inc_breaks <- c(0, 50, 100, 500, 1000, 2500, max(c(10000, map$inc)))
    inc_labs <- c("0-50", "50-100", "100-500", "500-1,000", "1,000-2,500", ">2,500")
    title_map <- "Average 14-day case incidence <br>per 100,000 hab."
  } else {
    inc_breaks <- c(-1, .5, 1, 5, 10, 25, max(c(100, map$inc)))
    inc_labs <- c("0-0.5", "0.5-1", "1-5", "5-10", "10-25", ">25")
    title_map <- "Average 14-day death incidence <br>per 100,000 hab."
  }
  
  map$inc_cat <- cut(map$inc, breaks = inc_breaks, labels = inc_labs)
  
  # Define colour scheme
  cols_vect <- c("#2c7bb6", "#67a9cf", "#e0f3f8", "#ffffbf", "#fee090", "#fdae61")
  factpal <- colorFactor(cols_vect, map$inc_cat)
  map$selected <- "black"
  map$weight <- 1
  
  # If a region is selected, the region is highlighted in red on the map
  if(!is.null(region_selected) & nuts == 2){ 
    map[map$key == region_selected,]$selected <- "red"
    map[map$key == region_selected,]$weight <- 3
  } else if(!is.null(region_selected)){
    if(substr(region_selected, 1, 2)[1] == "FR") region_selected <- substr(region_selected, 4, 6)
    if(substr(region_selected, 1, 2)[1] == "CZ") region_selected <- substr(region_selected, 4, 4)
    if(substr(region_selected, 1, 2)[1] == "IT") region_selected <- substr(region_selected, 1, 4)
    
    map[map$region == region_selected,]$selected <- "red"
    map[map$region == region_selected,]$weight <- 3
  }
  # Plot the map
  return(leaflet(map, options = leafletOptions(doubleClickZoom= FALSE)) %>% 
           addPolygons(stroke = TRUE, fillColor = ~factpal(inc_cat), fillOpacity = 1, 
                       color = ~selected, weight = ~weight) %>% 
           addLegend("bottomright", pal = factpal, values = ~map$inc_cat,
                     title = title_map, opacity = 1
           )
  )
}

# Function to generate the time-series plot (Forecasts and Scenario tabs)
plot_time_series <- function(pred, age, reg, obs, reg_groups, condition, name_reg, 
                             max_obs, type, log = F, nb_obs = 14, prev = 0){
  # Select the rows of pred that correspond to the condition specified by the user.
  pred <- pred[pred$conditions == condition, ]
  if(type == 1){ 
    # Compute the prediction dates (t_pred + 1:28)
    date_pred <- max_obs + as.numeric(unique(pred[!is.na(pred$t),]$t)) - nrow(obs[[1]])
    # All observation dates
    date_obs <- max_obs - rev(seq_len(nb_obs) - 1)
    obs <- obs[[1]]@observed
    # First date of prediction
    min_pred <- min(date_pred) - 1
  } else{
    # Compute the prediction dates (t_pred + 1:28)
    date_pred <- max_obs + as.numeric(unique(pred[!is.na(pred$t) & !is.na(pred$n_deaths),]$t)) - 
      nrow(obs[[1]])
    
    # min_date <- min(as.Date(epoch(obs[[1]]), origin = "1970-01-01"))
    min_date <- min(max_obs - nb_obs)
    obs <- obs[[2]][rownames(obs[[2]]) >= min_date,]
    
    # All observation dates
    date_obs <- as.Date(rownames(obs))
    nb_obs <- nrow(obs)
    # First date of prediction
    min_pred <- min(date_pred) - 7
    # Number of previous days:
    prev <- prev/7
  }
  last_t_obs <- nrow(obs)
  date_pred <- c(min_pred, date_pred)
  # Define all dates included in the plot
  all_dates <- c(date_obs, date_pred)
  
  if(age == FALSE){
    if(type == 1){ 
      # If there is no age stratification, select the entries where age == "tot" and 
      # type == "daily_cases"
      pred_f <- pred[pred$reg == reg & pred$type == "daily_cases" & pred$age == "tot",]
      # Compute the median and prediction intervals of the forecasts (50 and 95%)
      quants <- rbind(
        pred_f[pred_f$quant == "2.5%",]$n_cases, pred_f[pred_f$quant == "25%",]$n_cases,
        pred_f[pred_f$quant == "50%",]$n_cases, pred_f[pred_f$quant == "75%",]$n_cases,
        pred_f[pred_f$quant == "97.5%",]$n_cases
      )
    } else {
      # If there is no age stratification, select the entries where age == "tot", 
      # type == "daily_cases", and n_deaths is not NA
      pred_f <- pred[pred$reg == reg & pred$type == "daily_cases" & pred$age == "tot" &
                       !is.na(pred$n_deaths),]
      # Compute the median and prediction intervals of the forecasts (50 and 95%)
      quants <- rbind(
        pred_f[pred_f$quant == "2.5%",]$n_deaths, pred_f[pred_f$quant == "25%",]$n_deaths,
        pred_f[pred_f$quant == "50%",]$n_deaths, pred_f[pred_f$quant == "75%",]$n_deaths,
        pred_f[pred_f$quant == "97.5%",]$n_deaths
      )
    }
    # Define the vector of observed values (prior to the date of prediction)
    if(sum(!is.na(reg_groups)) > 1) obs_cases <- rowSums(obs[last_t_obs - rev(seq_len(nb_obs) - 1), !is.na(reg_groups)])
    if(sum(!is.na(reg_groups)) == 1) obs_cases <- obs[last_t_obs - rev(seq_len(nb_obs) - 1), !is.na(reg_groups)]
    
    # Merge obs_cases with quants
    quants <- cbind(matrix(obs_cases, nrow = 5, ncol = length(obs_cases), byrow = T), 
                    quants)
    
    ymax <- max(quants)
    ymin <- min(quants)
    # Which entries of quants should be plotted (depending on input$prev)
    val_pred <- c(nb_obs - prev, seq_len(ncol(quants))[-seq_len(nb_obs)])
    
    # Compute ylim
    ylim_plot <- c(ifelse(log, max(1,ymin * .9), 0), ymax * 1.2)
    if(all(quants < 5)) ylim_plot <- c(0, 5)
    # Plot median forecasts
    plot(c(NA, quants[3, val_pred]) ~ c(min(all_dates), date_pred), col = "darkred", type = "l", 
         log = ifelse(log, "y", ""), ylim = ylim_plot, 
         xlab = "", ylab = "", xlim = c(min(all_dates), max(all_dates)))
    # Add data points
    if(nb_obs > 0) points(quants[3, seq_len(nb_obs)] ~ date_obs, pch = 16, type = "b")
    # Plot 95% prediction intervals
    polygon(x = c(date_pred, rev(date_pred)), col = transp("orange", .3), border = NA,
            y = c(quants[1, val_pred], rev(quants[5, val_pred])))
    # Plot 50% prediction intervals
    polygon(x = c(date_pred, rev(date_pred)), col = transp("orange", .3), border = NA,
            y = c(quants[2, val_pred], rev(quants[4, val_pred])))
    
    # Vertical line showing the prediction date
    lines(x = c(min_pred, min_pred), y = c(0, ymax), lty = 2)
    
    # Add legend
    legend("topleft", legend = c("Data", "Median", "50% Pred. Int.", "95% Pred. Int."), 
           bty = "n", ncol = 2, border = NA, cex = 1.1, 
           fill = c("black", "darkred", transp("orange", .5), transp("orange", .3)))
    legend("topright", legend = "Prediction date", col = "black", lty = 2,
           bty = "n", ncol = 1, border = NA, cex = 1.1)
    title(main = paste0("Region plotted: ", name_reg), outer = T, line = -1)
  } else{
    # leg is used to avoid adding the legend to every panel, will be set to TRUE
    # after the legend is defined for the first time 
    leg <- FALSE

    for(i in unique(reg_groups[!is.na(reg_groups)])){
      # Select the entries where age is equal to the ith age group, and type == "daily_cases"
      reg <- sub("[:].*", "", i)
      age <- sub(".*[: ]", "", i)
      cols <- which(reg_groups == i)
      # Compute the median and prediction intervals of the forecasts (50 and 95%)
      if(type == 1){ 
        pred_f <- pred[pred$reg == reg & pred$age == age & pred$type == "daily_cases",]
        quants <- rbind(
          pred_f[pred_f$quant == "2.5%",]$n_cases, pred_f[pred_f$quant == "25%",]$n_cases,
          pred_f[pred_f$quant == "50%",]$n_cases, pred_f[pred_f$quant == "75%",]$n_cases,
          pred_f[pred_f$quant == "97.5%",]$n_cases
        )
      } else {
        pred_f <- pred[pred$reg == reg & pred$age == age & pred$type == "daily_cases" &
                         !is.na(pred$n_deaths),]
        quants <- rbind(
          pred_f[pred_f$quant == "2.5%",]$n_deaths, pred_f[pred_f$quant == "25%",]$n_deaths,
          pred_f[pred_f$quant == "50%",]$n_deaths, pred_f[pred_f$quant == "75%",]$n_deaths,
          pred_f[pred_f$quant == "97.5%",]$n_deaths
        )
      }
      
      
      if(log) quants[quants == 0] <- .1
      t_min <- as.numeric(pred_f$t[1])
      # Define the vector of observed values (prior to the date of prediction)
      if(length(cols) > 1) obs_cases <- rowSums(obs[last_t_obs - rev(seq_len(nb_obs) - 1), cols])
      if(length(cols) == 1) obs_cases <- obs[last_t_obs - rev(seq_len(nb_obs) - 1), cols]
      
      # Merge obs_cases with quants
      quants <- cbind(matrix(obs_cases, nrow = 5, ncol = length(obs_cases), byrow = T), 
                      quants)
      ymax <- max(quants)
      ymin <- min(quants)
      
      # Which entries of quants should be plotted (depending on input$prev)
      val_pred <- c(nb_obs - prev, seq_len(ncol(quants))[-seq_len(nb_obs)])
      
      # Compute ylim
      ylim_plot <- c(ifelse(log, max(1,ymin * .9), 0), ymax * 1.2)
      if(all(quants < 5)) ylim_plot <- c(0, 5)
      
      # Plot median forecasts
      plot(quants[3, val_pred] ~ date_pred, col = "darkred", type = "l", 
           log = ifelse(log, "y", ""), ylim = ylim_plot, 
           xlab = "", ylab = "", xlim = c(min(all_dates), max(all_dates)))
      # Add data points
      if(nb_obs > 0) points(quants[3, seq_len(nb_obs)] ~ date_obs, pch = 16, type = "b")
      # Plot 95% prediction intervals
      polygon(x = c(date_pred, rev(date_pred)), col = transp("orange", .3), border = NA,
              y = c(quants[1, val_pred], rev(quants[5, val_pred])))
      # Plot 50% prediction intervals
      polygon(x = c(date_pred, rev(date_pred)), col = transp("orange", .3), border = NA,
              y = c(quants[2, val_pred], rev(quants[4, val_pred])))
      
      # Vertical line showing the prediction date
      lines(x = c(min_pred, min_pred), y = c(0, ymax), lty = 2)
      
      # Add legend and title
      if(leg == FALSE){
        legend("topleft", legend = c("Data", "Median", "50% Pred. Int.", "95% Pred. Int."), 
               bty = "n", ncol = 2, border = NA, cex = .8, 
               fill = c("black", "darkred", transp("orange", .5), transp("orange", .3)))
        legend("topright", legend = "Prediction date", col = "black", lty = 2,
               bty = "n", ncol = 1, border = NA, cex = .8)
        leg <- TRUE
      }
      title(main = paste0("Region plotted: ", name_reg, ", ", age, " year-old"))
      
    }
  }
  
  # Add axis labels
  if(type == 1){
    title(ylab = "Number of daily cases", line = 0, outer = T)
    title(xlab = "Time (Days)", line = 0, outer = T)
  }
  if(type == 2){
    title(ylab = "Number of weekly deaths", line = 0, outer = T)
    title(xlab = "Time (Weeks)", line = 0, outer = T)
  }
}

# Function to replicate the time-series plots over the calibration period
plot_ts_old <- function(calib, age, region, obs, log, n_week, reg_groups, name_reg , type){
  
  if(type == 1){
    dt_pred <- calib$calib$q_pred_reg
  } else {
    dt_pred <- calib$calib_death$q_pred_reg
  }
  if(is.element(region, c("FR", "CZ"))){
    region <- "tot"
  }
  if(region == "IT"){
    if(type == 1){
      dt_pred <- calib$calib$q_pred
    } else {
      dt_pred <- calib$calib_death$q_pred
    }
    dt_pred <- dt_pred[horizon == n_week & quantile %in% c(0.025, 0.25, 0.5, .75, 0.975),]
  } else dt_pred <- dt_pred[horizon == n_week & sub("[.].*", "", reg) == region, 
                            .(reg, date, `2.5%`, `25%`, `50%`, `75%`, `97.5%`)]
  max_date <- max(dt_pred$date)
  dt_pred <- dt_pred[date <= (max_date - 7 *(n_week - 1))]
  # Compute the prediction dates 
  date_pred <- unique(dt_pred$date)
  date_pred <- date_pred[date_pred < "2023-04-10"]
  date_obs <- as.character(date_pred + 7 *(n_week - 1))
  # First date of prediction
  min_pred <- min(date_pred) - 1

  date_obs <- date_obs[is.element(date_obs, rownames(obs))]
  
  if(age == FALSE){
    if(any(grepl("[.]", dt_pred$reg))) dt_pred <- dt_pred[sub(".*[.]", "", reg) == "tot", ]
    
    if(region == "IT"){
      quants <- rbind(dt_pred[quantile == 0.025, prediction],
                      dt_pred[quantile == 0.25, prediction],
                      dt_pred[quantile == 0.5, prediction],
                      dt_pred[quantile == 0.75, prediction],
                      dt_pred[quantile == 0.975, prediction])
    } else {
      quants <- t(as.matrix(dt_pred[, .(`2.5%`, `25%`, `50%`, `75%`, `97.5%`)]))
    }
    
    quants <- quants[, seq_along(date_pred)]
    
    # Define the vector of observed values (prior to the date of prediction)
    if(sum(!is.na(reg_groups)) > 1) obs_cases <- rowSums(obs[date_obs, !is.na(reg_groups)])
    if(sum(!is.na(reg_groups)) == 1) obs_cases <- obs[date_obs, !is.na(reg_groups)]
    
    ymax <- max(quants)
    ymin <- min(quants)
    
    # Compute ylim
    ylim_plot <- c(ifelse(log, max(1,ymin * .9), 0), ymax * 1.2)
    if(all(quants < 5)) ylim_plot <- c(0, 5)
    # Plot median forecasts
    plot(quants[3, ] ~ date_pred, col = "darkred", type = "l", 
         log = ifelse(log, "y", ""), ylim = ylim_plot, 
         xlab = "", ylab = "", xlim = c(min(date_pred), max(date_pred)))
    # Add data points
    points(obs_cases ~ date_pred[seq_along(date_obs)], pch = 16, type = "b")
    # Plot 95% prediction intervals
    polygon(x = c(date_pred, rev(date_pred)), col = transp("orange", .3), border = NA,
            y = c(quants[1, ], rev(quants[5, ])))
    # Plot 50% prediction intervals
    polygon(x = c(date_pred, rev(date_pred)), col = transp("orange", .3), border = NA,
            y = c(quants[2, ], rev(quants[4, ])))
    

    # Add legend
    legend("topleft", legend = c("Data", "Median", "50% Pred. Int.", "95% Pred. Int."), 
           bty = "n", ncol = 2, border = NA, cex = 1.1, 
           fill = c("black", "darkred", transp("orange", .5), transp("orange", .3)))
    title(main = paste0("Region plotted: ", name_reg), outer = T, line = -1)
  } else{
    # leg is used to avoid adding the legend to every panel, will be set to TRUE
    # after the legend is defined for the first time 
    leg <- FALSE
    
    for(i in unique(reg_groups[!is.na(reg_groups)])){
      # Select the entries where age is equal to the ith age group, and type == "daily_cases"
      reg <- sub("[:].*", "", i)
      age <- sub(".*[: ]", "", i)
      cols <- which(reg_groups == i)
      # Compute the median and prediction intervals of the forecasts (50 and 95%)
      dt_pred_age <- dt_pred[sub(".*[.]", "", reg) == age, ]
      
      quants <- t(as.matrix(dt_pred_age[, .(`2.5%`, `25%`, `50%`, `75%`, `97.5%`)]))
      
      if(log) quants[quants == 0] <- .1
      # Define the vector of observed values (prior to the date of prediction)
      if(length(cols) > 1) obs_cases <- rowSums(obs[date_obs, cols])
      if(length(cols) == 1) obs_cases <- obs[date_obs, cols]
      
      quants <- quants[, seq_along(date_pred)]
      
      ymax <- max(quants)
      ymin <- min(quants)
      
      # Compute ylim
      ylim_plot <- c(ifelse(log, max(1,ymin * .9), 0), ymax * 1.2)
      if(all(quants < 5)) ylim_plot <- c(0, 5)
      
      # Plot median forecasts
      plot(quants[3, ] ~ date_pred, col = "darkred", type = "l", 
           log = ifelse(log, "y", ""), ylim = ylim_plot, 
           xlab = "", ylab = "", xlim = c(min(date_pred), max(date_pred)))
      # Add data points
      points(obs_cases ~ date_pred[seq_along(date_obs)], pch = 16, type = "b")
      # Plot 95% prediction intervals
      polygon(x = c(date_pred, rev(date_pred)), col = transp("orange", .3), border = NA,
              y = c(quants[1, ], rev(quants[5, ])))
      # Plot 50% prediction intervals
      polygon(x = c(date_pred, rev(date_pred)), col = transp("orange", .3), border = NA,
              y = c(quants[2, ], rev(quants[4, ])))
      
      # Add legend and title
      if(leg == FALSE){
        legend("topleft", legend = c("Data", "Median", "50% Pred. Int.", "95% Pred. Int."), 
               bty = "n", ncol = 2, border = NA, cex = .8, 
               fill = c("black", "darkred", transp("orange", .5), transp("orange", .3)))
        legend("topright", legend = "Prediction date", col = "black", lty = 2,
               bty = "n", ncol = 1, border = NA, cex = .8)
        leg <- TRUE
      }
      title(main = paste0("Region plotted: ", name_reg, ", ", age, " year-old"))
      
    }
  }
  
  # Add axis labels
  if(type == 1){
    title(ylab = "Number of daily cases", line = 0, outer = T)
    title(xlab = "Time (Days)", line = 0, outer = T)
  }
  if(type == 2){
    title(ylab = "Number of weekly deaths", line = 0, outer = T)
    title(xlab = "Time (Weeks)", line = 0, outer = T)
  }
  
  
}

# Function to generate the interactive map showing the incidence in each region 
# over the first week of the calibration period
leaflet_old <- function(calib, nuts, map, pop, pop_age, type, n_week, region_selected){
  if(type == 1){
    pred_per_reg <- calib$calib$q_pred_reg
  } else if(type == 2){ 
    pred_per_reg <- calib$calib_death$q_pred_reg
  }
  pred_per_reg <- pred_per_reg[, c("date", "horizon", "reg", "50%")]
  min_date <- min(pred_per_reg$date)
  pred_per_reg <- pred_per_reg[pred_per_reg$horizon == n_week & pred_per_reg$date == min_date, ]
  if(any(grepl("[.]", pred_per_reg$reg))){
    pred_per_reg$age <- sub(".*[.]", "", pred_per_reg$reg)
    pred_per_reg$reg <- sub("[.].*", "", pred_per_reg$reg)
    pred_per_reg <- pred_per_reg[age == "tot",]
    
  }
  pred_per_reg <- pred_per_reg[reg != "tot",]
  
  if(nuts == 1) {
    # If nuts-2 is selected, the way to extract region names depends on the selected country
    if(substr(colnames(pop), 1, 2)[1] == "FR"){
      group_reg <- substr(colnames(pop), 4, 6)
      group_map <- substr(map$key, start = 4, stop = 6)
    } else if(substr(colnames(pop), 1, 2)[1] == "CZ"){
      group_reg <- substr(colnames(pop), 4, 4)
      group_map <- substr(map$key, start = 4, stop = 4)
    } else if(substr(colnames(pop), 1, 2)[1] == "IT"){
      group_reg <- substr(colnames(pop), 1, 4)
      group_map <- substr(map$key, start = 1, stop = 4)
    }
    names(group_map) <- map$key
  } else if(nuts == 2){
    group_reg <- sub("[.].*", "", colnames(pop))
    group_map <- NULL
  }

  # Compute the number of inhabitants per region and age groups, and merge it by region
  pop_per_entry <- pop[1,] * pop_age[1,]
  pop_per_region <- aggregate(pop_per_entry, list(group_reg), sum)
  # Define vec_pop, which contains the number of inhabitants per region
  vec_pop <- pop_per_region$x
  names(vec_pop) <- pop_per_region$Group.1
  
  # Compute the incidence per region by dividing the number of cases (or death)
  # by the number of inhabitants
  incidence_per_region <- cbind.data.frame(
    region = pred_per_reg$reg,
    q_inc = pred_per_reg$`50%` / vec_pop[pred_per_reg$reg]
  )

  # Define vector containing the incidence in each region
  vec_inc <- incidence_per_region$q_inc
  names(vec_inc) <- incidence_per_region$region
  if(is.null(group_map)){
    # If group_map is not specified, region names is matched to map$key
    if(any(!is.element(incidence_per_region$region, map$key)))
      stop("If the aggregation names are not map$key, use the argument group_map to set the correspondence between map$key and group_reg")
    # Add incidence to map
    map$inc <- vec_inc[map$key]
  } else {
    # Add region column to map, which are then be matched with incidence_per_region$region
    map$region <- group_map[map$key]
    # Group NUTS-3 regions into NUTS-2 areas
    map <- map %>% group_by(region) %>% summarise()

    map$inc <- vec_inc[map$region]
  }

  # # Generate discrete variable and add it to map
  if(type == 1){
    inc_breaks <- c(0, 5, 10, 50, 100, 250, 500, max(c(1000, map$inc)))
    inc_labs <- c("0-5", "5-10", "10-50", "50-100", "100-250", "250-500", ">500")
    title_map <- "Average 7-day case incidence <br>per 100,000 hab."
  } else {
    inc_breaks <- c(-1, .1, .5, 1, 5, 10, max(c(100, map$inc)))
    inc_labs <- c("0-0.1", "0.1-0.5", "0.5-1", "1-5", "5-10", ">10")
    title_map <- "Average 7-day death incidence <br>per 100,000 hab."
  }
  # 
  map$inc_cat <- cut(map$inc, breaks = inc_breaks, labels = inc_labs)

  # Define colour scheme
  cols_vect <- c("#2c7bb6", "#67a9cf", "#e0f3f8", "#ffffbf", "#fee090", "#fdae61")
  factpal <- colorFactor(cols_vect, map$inc_cat)
  map$selected <- "black"
    map$weight <- 1

    # If a region is selected, the region is highlighted in red on the map
    if(!is.null(region_selected) & nuts == 2){
      map[map$key == region_selected,]$selected <- "red"
        map[map$key == region_selected,]$weight <- 3
    } else if(!is.null(region_selected)){
      if(substr(region_selected, 1, 2)[1] == "FR") region_selected <- substr(region_selected, 4, 6)
      if(substr(region_selected, 1, 2)[1] == "CZ") region_selected <- substr(region_selected, 4, 4)
      if(substr(region_selected, 1, 2)[1] == "IT") region_selected <- substr(region_selected, 1, 4)

      map[map$region == region_selected,]$selected <- "red"
        map[map$region == region_selected,]$weight <- 3
    }
    # Plot the map
    return(leaflet(map, options = leafletOptions(doubleClickZoom= FALSE)) %>%
             addPolygons(stroke = TRUE, fillColor = ~factpal(inc_cat), fillOpacity = 1,
                         color = ~selected, weight = ~weight) %>%
             addLegend("bottomright", pal = factpal, values = ~map$inc_cat,
                       title = title_map, opacity = 1
             )
    )
    
}
