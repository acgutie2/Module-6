########################################################################################
# Summary: Tidying and analyzing cotton production trends in NC
# Date: September 25, 2019
########################################################################################

# Clear workspace & load packages ----
rm(list=ls(all=TRUE))
library(tidyverse)

# 2. Read & inspect the dataset ----
cotton <- read_csv("data/cotton-usda-nass.csv")

str(cotton)
head(cotton)
tail(cotton)
dim(cotton)
summarize(cotton)


head(cotton)

# 3.1. Create a NC data subset ----
cotton %>%
  filter(state == "NORTH CAROLINA") 
  select(cotton, year , state, ag_district, county, data_item, value) -> cotton
head(cotton)

# 3.2. Divide the data_item column ----
  cotton %>%
    separate(data_item,
             into = c("cotton_type", "measurement"),
             sep = "-" ) -> cotton
head(cotton)

# 3.3. Convert the value column to numeric type ----
cotton %>%
  filter(value != "(D)") -> cotton

as.numeric(cotton$value)
cotton$value <- as.numeric(cotton$value)
head(cotton)


# 4. Visualizing trends ----
cotton %>%
  ggplot(mapping = aes(x = year, y = value))+
  geom_point()+
  geom_line()+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90))+ 
  facet_grid(measurement~ag_district, scales = "free_y")

  
# 5. Summarize data from 2018 ----
cotton %>%
  filter(year == "2018") -> cotton_18
head(cotton_18)

spread(cotton_18, measurement, value) -> cotton_18

head(cotton_18)

total_lbs <- cotton_18$` ACRES HARVESTED`*cotton_18$` YIELD, MEASURED IN LB / ACRE`

view(total_lbs)

add_column(cotton_18, total_lbs) -> cotton_18
 
cotton_18 %>%
  group_by(county, total_lbs) %>%
  summarise(total_lbs = max(total_lbs)) %>% 
  ungroup() %>%
  group_by(county, total_lbs) %>%
  mutate(rank = min_rank(desc(total_lbs))) %>% 
  arrange(rank) %>%
  top_n(2)

 
head(cotton_18) 
  
 