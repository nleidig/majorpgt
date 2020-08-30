library(tidyverse)

content <- read_html("http://www.puntingform.com.au/form-guide/race/caulfield-29-08-2020-9/")

tables <- content %>% html_table(fill = TRUE)

first_table <- tables[[1]]

first_table <- first_table[-1,]
