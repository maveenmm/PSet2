---
  title: "PSet2 Mallah"
output: html_document
date: "2024-01-28"
---
#Answer 1: https://github.com/maveenmm/PSet2.git  
#Answer 2: see below
#rsetup,
include=FALSE
knitr::opts_chunk$set(
  eval = TRUE, 
  echo = FALSE, 
  warning = FALSE, 
  message = FALSE,
  fig.align = 'center', 
)

library(tidyverse)
library(knitr)
library(reader)
library(tibble)
library(dplyr)
library(gapminder)


# open my data
gspace = read_csv('greenspace_data_share.csv')

# summarize average urban greenspace by region

table = 
  gspace |>
  group_by(Major_Geo_Region) |>
  summarise(
    obs = n(),
    avg = mean(annual_avg_2020, na.rm = TRUE),
    'weighted_avg' = mean(annual_weight_avg_2020, na.rm = TRUE)
    ) |>
  rename("Weighted Average" = weighted_avg, "Major Geographical Region" = Major_Geo_Region, "Observation" = obs, "Average" = avg)

# output as table
kable(table, digits = 1L
)

#Answer 3: 1038
length(gspace$City)
length(gspace$indicator_2015)

#Answer 4: 
library(knitr)
answer4 = 
  gspace |>
count(indicator_2021)
kable(answer4)

#Answer 5: 
#a 69
sum(gspace$peak_NDVI_2015 >=0.5)

#b 71
library(dplyr)
answer5b= gspace|>
  filter(indicator_2010 =="Exceptionally Low", 
         indicator_2015=="Exceptionally Low",  
         indicator_2020=="Exceptionally Low",
         indicator_2021=="Exceptionally Low") |>
summarise(obs = n())
print(answer5b)

#c:225
library(dplyr)
library(knitr)
answer5c = gspace |>
  filter(Climate_region == "Arid") |>
  mutate(green = ifelse(annual_weight_avg_2020 - annual_weight_avg_2010 > 0, 'increase', 'decrease')
  )
increase = sum(answer5c$green == 'increase')
decrease = sum(answer5c$green == 'decrease')
table(increase, decrease)

#Answer 6: 128
answer6 <- gspace |>
  mutate(
    green = ifelse(annual_avg_2021 - annual_avg_2010 > 0, 'increase', 'decrease')
  )
table(answer6$green)

#Answer 7:see histograph below 
gspace <- gspace |>
  mutate(
    change = annual_avg_2021 - annual_avg_2010
  )
hist(gspace$change, main = "Change in Greenspace (2010 to 2021)", xlab = "Change in Annual Average Greenspace", col='pink')


#Answer 8: see scatterplot below
gspace <- gspace |>
  mutate(
    change = annual_weight_avg_2021 - annual_weight_avg_2010
  )

plot(
  gspace$annual_weight_avg_2010,
  gspace$annual_weight_avg_2021,
  main = "Weighted Greenspace Population 2021 vs 2010",
  xlab = "2010",
  ylab = "2021",
  col = ifelse(gspace$change < 0, "red", "blue")
)

#Extra Credit 
abline(a = 0, b = 1, lwd = 3, lty = 4, col = "purple")

  
answer7 = gspace |>
  mutate(
    change = annual_avg_2021 - annual_avg_2010
  )
hist(gspace$change, main = "Change in Greenspace (2010 to 2021)", xlab = "Change in Annual Average Greenspace", col='pink')

