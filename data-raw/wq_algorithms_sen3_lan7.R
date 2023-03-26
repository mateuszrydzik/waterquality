library(tidyverse)
library(waterquality)
library(dplyr)


testing <- tribble(
  ~name, ~funs, ~type, ~satellite, ~bands,
  "Al10SABI", Al10SABI, "chlorophyll", "sentinel3", c(17, 8, 3, 5),
  "Al10SABI", Al10SABI, "chlorophyll", "landsat7", c(4, 3, 1, 2)
)

wq_algorithms <- rbind(wq_algorithms, testing)
usethis::use_data(wq_algorithms, overwrite = TRUE)

