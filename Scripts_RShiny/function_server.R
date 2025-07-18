# Function wrapping plot_time_series depending on the selected inputs
generate_figure_ts <- function(pred, map, obs, nuts, age, days, prev, log, condition, 
                               region, type, max_obs){
  cols <- NULL
  if(type == 1) obs_output <- obs[[1]] else obs_output <- obs[[2]]
  # If a region is selected (i.e. "region" is not one of the countries)
  if(!is.element(region, c("FR", "CZ", "IT"))){
    # If nuts-2 is selected, the way to extract region names depends on the country
    if(nuts == 1){
      if(substr(colnames(obs_output), 1, 2)[1] == "FR"){
        reg_groups_nuts <- substr(colnames(obs_output), 4, 6)
        map_group <- substr(map$key, start = 4, stop = 6)
      } else if(substr(colnames(obs_output), 1, 2)[1] == "CZ"){
        reg_groups_nuts <- substr(colnames(obs_output), 4, 4)
        map_group <- substr(map$key, start = 4, stop = 4)
      } else if(substr(colnames(obs_output), 1, 2)[1] == "IT"){
        reg_groups_nuts <- substr(colnames(obs_output), 1, 4)
        map_group <- substr(map$key, start = 1, stop = 4)
      }
      names(map_group) <- map$key
      region <- map_group[region]
      # Cols correspond to the entries in reg_groups_nuts corresponding to the 
      # selected region
      cols <- which(reg_groups_nuts == region)
    } else{
      # If nuts-3 is selected, remove age from column names and set map_group to NULL
      cols <- which(sub("[.].*", "", colnames(obs_output)) == region)
    }
  }
  
  # If age is equal to 1, generate age stratified plots 
  if(age){
    par(mfrow = c(2, 2), mar = c(3,4, 1, 0), oma = c(2,2, 0, 2), las = 1, bty = "l", 
        cex.axis = 1.1, cex.main = 1.2, cex.lab = 1.2)
    
    # Use four age groups: 0-20 ; 2-60 ; 60-80 ; 80+
    age_groups <- sub(".*[.]", "", colnames(obs_output))
    age_groups[age_groups == "0-9" | age_groups == "10-19"] <- "0-20"
    age_groups[is.element(age_groups, c("20-29", "30-39", "40-49", "50-59"))] <- "20-60"
    age_groups[is.element(age_groups, c("60-69", "70-79"))] <- "60-80"
    age_groups[is.element(age_groups, c("80-89", "90+"))] <- "80+"
    # Define the title of each plot
    reg_groups_nuts <- paste(region, ": ", age_groups, sep = "")
  } else{
    reg_groups_nuts <- colnames(obs_output)
    reg_groups_nuts[cols] <- region
    par(mfrow = c(1, 1), mar = c(3,4, 1, 0), oma = c(2,2, 0, 2), las = 1, bty = "l", 
        cex.axis = 1.1, cex.main = 1.2, cex.lab = 1.2)
  }
  
  name <- map[map$FID == region, ]$NAME_LATN
  if(length(name) == 0) name <- map[grep(region, map$key),]$NAME_LATN
  
  if(length(name) > 1){
    nuts3 <- map[grep(region, map$key),]$FID
    name <- map[map$LEVL_CODE == 2 & map$FID == substr(nuts3[1], 1, 4),]$NAME_LATN
  }
  # Set unselected entries to NA (i.e. corresponding to other regions than the one selected)
  if(!is.null(cols)) reg_groups_nuts[-cols] <- NA
  
  # nb_obs correspond to the number of days before the prediction date included 
  # in the time-series 
  nb_obs <- days + prev
  
  # Generate plot
  plot_time_series(pred = pred, age = age, condition = condition, reg = region, 
                   obs = obs, max_obs = max_obs, log = log, nb_obs = nb_obs, 
                   reg_groups = reg_groups_nuts, prev = prev, name_reg = name, 
                   type = type)
  
}

# Function wrapping plot_ts_old depending on the selected inputs
generate_ts_old <- function(calib, nuts, age, days, log,  map,
                            region, type, max_obs, n_week){
  
  
  cols <- NULL
  if(type == 1){
    obs_output <- calib$calib$obs_reg
  } else {
    obs_output <- calib$calib_death$obs_reg
    if(substr(region, 1, 2) == "CZ") colnames(obs_output) <- paste0("CZ_", colnames(obs_output)) 
    if(substr(region, 1, 2) == "FR") colnames(obs_output) <- paste0("FR_", colnames(obs_output)) 
  }
  
  # If a region is selected (i.e. "region" is not one of the countries)
  if(!is.element(region, c("FR", "CZ", "IT"))){
    # If nuts-2 is selected, the way to extract region names depends on the country
    if(nuts == 1){
      if(substr(colnames(obs_output), 1, 2)[1] == "FR"){
        reg_groups_nuts <- substr(colnames(obs_output), 4, 6)
        map_group <- substr(map$key, start = 4, stop = 6)
      } else if(substr(colnames(obs_output), 1, 2)[1] == "CZ"){
        reg_groups_nuts <- substr(colnames(obs_output), 4, 4)
        map_group <- substr(map$key, start = 4, stop = 4)
      } else if(substr(colnames(obs_output), 1, 2)[1] == "IT"){
        reg_groups_nuts <- substr(colnames(obs_output), 1, 4)
        map_group <- substr(map$key, start = 1, stop = 4)
      }
      names(map_group) <- map$key
      region <- map_group[region]
      # Cols correspond to the entries in reg_groups_nuts corresponding to the 
      # selected region
      cols <- which(reg_groups_nuts == region)
    } else{
      # If nuts-3 is selected, remove age from column names and set map_group to NULL
      cols <- which(sub("[.].*", "", colnames(obs_output)) == region)
    }
  }
  
  # If age is equal to 1, generate age stratified plots 
  if(age){
    par(mfrow = c(2, 2), mar = c(3,4, 1, 0), oma = c(2,2, 0, 2), las = 1, bty = "l", 
        cex.axis = 1.1, cex.main = 1.2, cex.lab = 1.2)
    
    # Use four age groups: 0-20 ; 2-60 ; 60-80 ; 80+
    age_groups <- sub(".*[.]", "", colnames(obs_output))
    age_groups[age_groups == "0-9" | age_groups == "10-19"] <- "0-20"
    age_groups[is.element(age_groups, c("20-29", "30-39", "40-49", "50-59"))] <- "20-60"
    age_groups[is.element(age_groups, c("60-69", "70-79"))] <- "60-80"
    age_groups[is.element(age_groups, c("80-89", "90+"))] <- "80+"
    # Define the title of each plot
    reg_groups_nuts <- paste(region, ": ", age_groups, sep = "")
  } else{
    reg_groups_nuts <- colnames(obs_output)
    reg_groups_nuts[cols] <- region
    par(mfrow = c(1, 1), mar = c(3,4, 1, 0), oma = c(2,2, 0, 2), las = 1, bty = "l", 
        cex.axis = 1.1, cex.main = 1.2, cex.lab = 1.2)
  }
  
  name <- map[map$FID == region, ]$NAME_LATN
  if(length(name) == 0) name <- map[grep(region, map$key),]$NAME_LATN
  
  if(length(name) > 1){
    nuts3 <- map[grep(region, map$key),]$FID
    name <- map[map$LEVL_CODE == 2 & map$FID == substr(nuts3[1], 1, 4),]$NAME_LATN
  }
  if(region == "IT") name <- "Italy"
  if(region == "CZ") name <- "Czechia"
  if(region == "FR") name <- "France"
  
  # Set unselected entries to NA (i.e. corresponding to other regions than the one selected)
  if(!is.null(cols)) reg_groups_nuts[-cols] <- NA
  # Generate plot
  plot_ts_old(calib = calib, age = age, region = region, obs = obs_output, log = log, 
              n_week = as.numeric(n_week), reg_groups = reg_groups_nuts, 
              name_reg = name, type = type)
  
}

