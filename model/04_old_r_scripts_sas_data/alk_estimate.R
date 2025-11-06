alk_estimate <- function(path_data, data_rca, data_rhh, tab_levels, tab_space, tab_time, space_first, time_first,
                         specie, ages, years, uncertainty, min_sfish, recruit_month, type_length, length_pred, length_obs, stratif,
                         return_graph, path_graph) {
  
  # Attributing values according to table given
  if(type_length == 'scm') {
    length_pred[3] <- 0.5
    length_pred[4] <- 2
  }
  length_pred[5] <- (length_pred[2] - length_pred[1]) * length_pred[4] + 1
  ages[3] <- ages[2] - ages[1] + 1
  
  # Levels : first level and last level numbers
  levels <- c(tab_levels$level[1], tail(tab_levels$level, 1))
  
  # Initialisation
  vars_r <- list()
  list_text <- list() # Needed later for aglg4 list 
  
  # Creating variables that will include variables names and stratification variables names
  if(length(stratif) != 0) { # stratification
    tab_levels$vars <- tab_levels$strat1
    for(l in levels[1]:levels[2]) {
      vars_r[[l]] <- tab_levels$strat1[l]
      list_text[[l]] <- paste0("aglg4$", tab_levels$strat1[[l]])
      if(length(stratif) > 1) {
        for(n in 2:length(stratif)) {
          strat_col <- paste0("strat", n)
          if(tab_levels[[strat_col]][l] != "") {
            tab_levels$vars[l] <- paste(tab_levels$vars[l], tab_levels[[strat_col]][l], sep = ", ")
            vars_r[[l]] <- c(vars_r[[l]], tab_levels[[strat_col]][l])
            list_text[[l]] <- c(list_text[[l]], paste0("aglg4$", tab_levels[[strat_col]][[l]]))
          }
        }
        for(n in 1:tab_levels$nb_var[l]) {
          var_col <- paste0("var", n)
          tab_levels$vars[l] <- paste(tab_levels$vars[l], tab_levels[[var_col]][l], sep = ", ")
          vars_r[[l]] <- c(vars_r[[l]], tab_levels[[var_col]][l])
          list_text[[l]] <- c(list_text[[l]], paste0("aglg4$", tab_levels[[var_col]][[l]]))
        }
      } else {
        for(n in 1:tab_levels$nb_var[l]) {
          var_col <- paste0("var", n)
          tab_levels$vars[l] <- paste(tab_levels$vars[l], tab_levels[[var_col]][l], sep = ", ")
          vars_r[[l]] <- c(vars_r[[l]], tab_levels[[var_col]][l])
          list_text[[l]] <- c(list_text[[l]], paste0("aglg4$", tab_levels[[var_col]][[l]]))
        }
      }
    }
    # Remove leading commas
    for(i in 1:levels[2]) {
      if(substr(tab_levels$vars[i], 1, 1) == ",") {
        tab_levels$vars[i] <- substring(tab_levels$vars[i], 2)
        vars_r[[i]] <- vars_r[[i]][-1]
        list_text[[i]] <- list_text[[i]][-1]
      }
    }
  } else { # no stratification
    tab_levels$vars <- tab_levels$var1
    for(l in levels[1]:levels[2]) {
      vars_r[[l]] <- tab_levels$var1[l]
      list_text[[l]] <- paste0("aglg4$", tab_levels$var1[[l]])
      if(tab_levels$nb_var[l] != 1) {
        for(n in 2:tab_levels$nb_var[l]) {
          var_col <- paste0("var", n)
          tab_levels$vars[l] <- paste(tab_levels$vars[l], tab_levels[[var_col]][l], sep = ", ")
          vars_r[[l]] <- c(vars_r[[l]], tab_levels[[var_col]][l])
          list_text[[l]] <- c(list_text[[l]], paste0("aglg4$", tab_levels[[var_col]][[l]]))
        }
      }
    }
  }

  
  pre_key_sas <- haven::read_sas("C:/Users/kibi/OneDrive - Danmarks Tekniske Universitet/gits/spr.27.3a4_commercial_catch/output/01_benchmark_2018_2025_rerun/in5.sas7bdat")
  pre_key_sas <- dplyr::rename(pre_key_sas, length = scm, area1 = area)
  pre_key_sas$halfyear <- 2
  pre_key_sas$halfyear[pre_key_sas$quarter %in% c(1,2)] <- 1
  pre_key_sas <- dplyr::select(pre_key_sas, -`_FREQ_`, -`_TYPE_`)
  
  # ALK by level
  list_lvl <- list()
  for(l in levels[1]:levels[2]) {
    list_lvl[[paste0("lv", l)]] <- alk_estimate_per_level(
      pre_key = pre_key_sas,
      lvl = l,
      ages = ages,
      tab_levels = tab_levels,
      vars_r = vars_r,
      list_text = list_text,
      levels = levels,
      space_first = space_first,
      uncertainty = uncertainty,
      min_sfish = min_sfish
    )
  }
  
  if(length(stratif) == 0) {
    ALK <- alk_select(
      ages = ages,
      levels = levels,
      years = years,
      list_lvl = list_lvl,
      specie = specie,
      length_pred = length_pred,
      vars_r = vars_r,
      tab_levels = tab_levels,
      tab_space = tab_space,
      tab_time = tab_time,
      space_first = space_first,
      time_first = time_first,
      aglg5 = aglg5
    )
    if(return_graph == "Yes") {
      alk_plot(
        path_graph = path_graph,
        data_graph = ALK,
        years = years,
        ages = ages,
        tab_time = tab_time,
        tab_levels = tab_levels,
        time_first = time_first
      )
    }
  } else {
    ALK <- f_ALK_strat(
      ages = ages,
      levels = levels,
      years = years,
      list_lvl = list_lvl,
      specie = specie,
      length_pred = length_pred,
      vars_r = vars_r,
      tab_levels = tab_levels,
      tab_space = tab_space,
      tab_time = tab_time,
      stratif = stratif,
      space_first = space_first,
      time_first = time_first,
      aglg5 = aglg5
    )
    if(return_graph == "Yes") {
      f_graph_strat(
        path_graph = path_graph,
        data_graph = ALK,
        years = years,
        ages = ages,
        tab_time = tab_time,
        tab_levels = tab_levels,
        time_first = time_first
      )
    }
  }
  
  return(list(ALK = ALK, list_lvl = list_lvl))
  
}