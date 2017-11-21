#### One variable EDA function ####

# if var = numeric: histogram / if var = char: bar-plot
# require dplyr, purrr libraries
library(tidyverse)

# example data: diamonds data
mydata <- diamonds

# define function
one_var_eda <- function(data) {
  
  # save var names in list
  data_var = as.list(names(data))
  
  # dplyr & purrr characterize
  data = data %>%
    map_if(~ inherits(.x, 'factor'), ~ as.character(.x)) %>%
    bind_cols()
  
  # iteratively create plot by var type
  for(i in seq_along(data_var)) {
    
    # if var type = numeric: histogram
    if(class(data[[i]]) == 'numeric') { 
      gg = ggplot(data = data, aes_string(x = data_var[[i]])) +
        geom_histogram()
      print(gg)
      # save plot file in png by paste0
      ggsave(filename = paste0('EDA_plot_', i, '.png'), plot = gg)
      
    # if var type = char : bar-plot
    } else if (class(data[[i]]) == 'character') {
      gg = ggplot(data = data, aes_string(x = data_var[[i]])) +
        geom_bar()
      print(gg)
      # save plot file in png by sprintf
      ggsave(filename = sprintf('EDA_plot_%d.png', i), plot = gg)
    }
  }
}

# test the result
one_var_eda(mydata)

## another charaterize method

# # Base R
# mydata <- mydata %>%
#   lapply(function(x){
#     if(inherits(x, 'factor')) 
#       x = as.character()
#     return(x)
#   }) %>%
#   data.frame(., stringsAsFactors = FALSE)
# 
# # for loop
# charaterize = function(.data){
#   for(i in seq_along(.data)){
#     if(inherits(.data[[i]], 'factor'))
#       .data[[i]] = as.character(.data[[i]])
#   }
#   .data
# }
# 
# charaterize(mydata)