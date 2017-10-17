library(tidyverse)

data <- read.csv('bankdata.csv')

data_names <- names(data)[-1]
var_combn <- as.list(combn(data_names, 2, FUN = paste, collapse = ","))
var_combn <- lapply(var_combn, function(x) strsplit(x, ",")[[1]])

for(i in seq_along(var_combn)){
  plot <- ggplot(data, aes_string(var_combn[[i]][1], y = var_combn[[i]][2])) +
    geom_point(aes(color = GROUP_ID), size = 3) +
    theme_minimal() +
    theme(legend.position = 'none')

    print(plot)
}

