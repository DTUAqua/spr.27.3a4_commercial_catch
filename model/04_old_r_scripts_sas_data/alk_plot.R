
alk_plot <- function(path_graph, data_graph, years, ages, tab_time, tab_levels, time_first){
  
  # Get time scale
  eval(parse(text=paste0("tab_time2 <- tab_time[!duplicated(tab_time$", time_first, "),]")))
  eval(parse(text=paste0("t <- unlist(subset(tab_time2, select = ", time_first, ", subset = is.na(", time_first, ") == F))")))
  
  # Path to save the graphs
  setwd(path_graph)
  
  # Loop to write p0, p1 ...
  text_p <- c()
  for (i in ages[1]:ages[2]){
    text_p <- c(text_p, paste('p', i, sep = ""))
  }
  text_p <- paste(text_p, collapse = ", ")
  
  for(y in years[1]:years[2]){
    for (i in 1:length(t)){
      eval(parse(text=paste0("png('graph_",y, "_", t[i], ".png', width =800, height = 800)")))
      par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
      eval(parse(text=paste0("matplot(data_graph$a", y, "$length[data_graph$a", y, "$", time_first, " == t[i]], subset(data_graph$a", y, "[data_graph$a", y, "$", time_first, " == t[i],], select = c(", text_p, ")), type = 'b', lty = 'solid', lwd = 1.8,
                             pch = seq(1:ages[3]), col = rainbow(ages[3]), xlab = 'cm', ylab = 'p', cex = 0.7,
                             main = 'Year = ", y, ", ", time_first, " = ", t[i],"')")))
      axis(2,at = seq(0,1, by = 0.1))
      legend("topright", inset=c(-0.1,0), title = "Age", legend = c(ages[1]:ages[2]), col = rainbow(ages[3]), cex=1, pch = seq(1:ages[3]), box.lty = 0)
      dev.off()
    }
  }
}