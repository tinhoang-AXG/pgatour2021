## code to prepare `DATASET` dataset goes here

library(tidyr)
library(dplyr)
library(readr)
library(stringr)
library(purrr)

ftin_to_feet <- function(ftin){
  as_tibble(ftin) %>% 
    separate(value, c("ft", "inches"), sep = " ") %>% 
    mutate_all(~as.integer(str_extract(.x, "(\\d)+"))) %>% 
    transmute(ft = ft + (inches/12)) %>% 
    pull()
}

##---
PGA_Tour_2021_Stats <- read_csv("data-raw/2021 PGA Tour Filtered Stats.csv")
colnames(PGA_Tour_2021_Stats) <- str_replace_all(colnames(PGA_Tour_2021_Stats), c("#" = "NUMBER", "%" = "PCT"))

PGA_Tour_2021_Stats <- PGA_Tour_2021_Stats %>% 
  mutate_at(vars(`AVG---average distance of birdie putts made---PUTTING`,
                 `AVG---average putting distance - all 1 putts---PUTTING`,
                 `AVG DTP---proximity to hole (arg)---AROUND THE GREEN`,
                 `AVG---distance from center of fairway---OFF THE TEE`,
                 `AVG---distance from edge of fairway---OFF THE TEE`,
                 `AVG---rough proximity---APPROACH THE GREEN`,
                 `AVG---proximity to hole---APPROACH THE GREEN`),
            ~ftin_to_feet(.)) %>% 
  mutate_at(vars(`SHORTEST MADE (FT)---average distance of birdie putts made---PUTTING`),
            ~as.numeric(str_extract(.x, "(\\d)+"))/12)

PGA_Tour_2021_Stats <- PGA_Tour_2021_Stats %>%
  mutate_at(vars(`TOTAL ADJUSTMENT---scoring average---SCORING`),
            ~as.numeric(gsub(" ", "",.))) %>% 
  mutate_at(vars(`AVG RTP SCORE---right tendency---OFF THE TEE`,
                 `RELATIVE TO PAR---rough tendency---OFF THE TEE`,
                 `AVG RTP SCORE---left tendency---OFF THE TEE`,
                 `RTP-NOT GOING FOR THE GRN---going for the green - birdie or better---APPROACH THE GREEN`,
                 `RTP-NOT GOING FOR THE GRN---going for the green - hit green pct.---APPROACH THE GREEN`,
                 `RELATIVE TO PAR---rough proximity---APPROACH THE GREEN`),
            ~as.numeric(gsub("E",0,.)))

PGA_Tour_2021_Stats <- PGA_Tour_2021_Stats %>% 
  select(-`GIR RANK---proximity to hole---APPROACH THE GREEN`) %>% 
  pivot_longer(-`PLAYER NAME`, names_to = "Category", values_to = "Value") %>% 
  separate(Category, into = c("Measure_Type", "Sub_Category", "Category"), sep = "---") %>% 
  select(Player_Name = `PLAYER NAME`, Category, Sub_Category, Measure_Type, Value) %>%
  mutate(Sub_Category = str_replace(Sub_Category, "sg:", "stroke gain:")) %>% 
  arrange(Player_Name, Category)


usethis::use_data(PGA_Tour_2021_Stats, overwrite = TRUE)
