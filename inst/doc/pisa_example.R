## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.align = 'center',
  fig.width = 6,
  fig.height = 5
)

## ---- message = FALSE---------------------------------------------------------
library(perccalc)
library(tidyr)
library(ggplot2)
library(dplyr)

## ----setup--------------------------------------------------------------------
order_edu <- c("None",
               "ISCED 1",
               "ISCED 2",
               "ISCED 3A, ISCED 4",
               "ISCED 3B, C",
               "ISCED 5A, 6",
               "ISCED 5B")

# Make ordered categories of our categorical variables and calculate avgerage
# math test scores for each year
pisa_2012 <-
  pisa_2012 %>%
  mutate(father_edu = factor(father_edu, levels = order_edu, ordered = TRUE))
         

pisa_2006 <-
  pisa_2006 %>%
  mutate(father_edu = factor(father_edu, levels = order_edu, ordered = TRUE))


# Merge them together
pisa <- rbind(pisa_2006, pisa_2012)

## -----------------------------------------------------------------------------
perc_diff(data_model = pisa,
          categorical_var = father_edu,
          continuous_var = avg_math,
          percentiles = c(90, 10))

## -----------------------------------------------------------------------------

cnt_diff <-
  pisa %>%
  nest(data = c(-year, -CNT)) %>%
  mutate(edu_diff = lapply(data, function(x) perc_diff_df(x, father_edu, avg_math, percentiles = c(90, 10)))) %>%
  select(-data) %>% 
  unnest(edu_diff)

cnt_diff

## -----------------------------------------------------------------------------
cnt_diff %>% 
  ggplot(aes(year, difference, group = CNT, color = CNT)) +
  geom_point() +
  geom_line() +
  theme_minimal() +
  scale_y_continuous(name = "Achievement gap in Math between the 90th and \n 10th percentile of father's education") +
  scale_x_continuous(name = "Year")

## -----------------------------------------------------------------------------

# Calculate the gap for the 90/50 gap
cnt_half <-
  pisa %>%
  nest(data = c(-year, -CNT)) %>%
  mutate(edu_diff = lapply(data,
                           function(x) perc_diff_df(x, father_edu, avg_math, percentiles = c(90, 50)))) %>%
  select(-data) %>% 
  unnest(edu_diff)

# Calculate the gap for the 50/10 gap
cnt_bottom <-
  pisa %>%
  nest(data = c(-year, -CNT)) %>%
  mutate(edu_diff = lapply(data,
                           function(x) perc_diff_df(x, father_edu, avg_math, percentiles = c(50, 10)))) %>%
  select(-data) %>% 
  unnest(edu_diff)

cnt_diff$type <- "90/10"
cnt_half$type <- "90/50"
cnt_bottom$type <- "50/10"

final_cnt <- rbind(cnt_diff, cnt_half, cnt_bottom)
final_cnt$type <- factor(final_cnt$type, levels = c("90/10", "90/50", "50/10"))
final_cnt


## -----------------------------------------------------------------------------
final_cnt %>% 
  ggplot(aes(year, difference, group = CNT, color = CNT)) +
  geom_point() +
  geom_line() +
  theme_minimal() +
  scale_y_continuous(name = "Achievement gap in Math between the 90th and \n 10th percentile of father's education") +
  scale_x_continuous(name = "Year") +
  facet_wrap(~ type)

## -----------------------------------------------------------------------------
perc_dist(pisa, father_edu, avg_math)

## -----------------------------------------------------------------------------
cnt_dist <-
  pisa %>%
  nest(data = c(-year, -CNT)) %>%
  mutate(edu_diff = lapply(data, function(x) perc_dist(x, father_edu, avg_math))) %>%
  select(-data) %>% 
  unnest(edu_diff)

cnt_dist

## -----------------------------------------------------------------------------

cnt_dist %>%
  mutate(year = as.character(year)) %>% 
  filter(percentile %in% seq(0, 100, by = 10)) %>%
  ggplot(aes(percentile, estimate, color = year, group = percentile)) +
  geom_point() +
  geom_line(color = "black") +
  scale_y_continuous(name = "Math test score") +
  scale_x_continuous(name = "Percentiles from father's education") +
  scale_color_discrete(name = "Year") +
  facet_wrap(~ CNT) +
  theme_minimal()

## ----echo = FALSE-------------------------------------------------------------
citation("perccalc")

