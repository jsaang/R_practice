#### One variable EDA function ####

# if var = numeric: histogram / if var = char: bar-plot

# example data: diamonds data
mydata <- diamonds

# factor var to character
mydata$cut <- as.character(mydata$cut)
mydata$color <- as.character(mydata$color)
mydata$clarity <- as.character(mydata$clarity)

# define function
one_var_eda <- function(data) {
  
  # save var names in list
  data_var = as.list(names(data))
  
  # iteratively create plot by var type
  for(i in seq_along(data_var)) {
    
    # if var type = numeric: histogram
    if(class(data[[i]]) == 'numeric') { 
      gg = ggplot(data = data, aes_string(x = data_var[[i]])) +
        geom_histogram()
      print(gg)
      # save plot file in png
      ggsave(filename = paste0('EDA_plot_', i, '.png', plot = gg))
      
    # if var type = char : bar-plot
    } else if (class(data[[i]]) == 'character') {
      gg = ggplot(data = data, aes_string(x = data_var[[i]])) +
        geom_bar()
      print(gg)
      # save plot file in png
      ggsave(filename = paste0('EDA_plot_', i, '.png', plot = gg))
    }
  }
}

one_var_eda(mydata)
