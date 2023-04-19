
library(dplyr)
library(ggplot2)
library(purrr)
library(tidyr)


commune_level_data <- read.csv("data//commune_level_data.csv")
country_level_data <- read.csv("data//country_level_data.csv")


##Laspeyeres index

commune_level_data <- commune_level_data %>%
  group_by(locality) %>%
  mutate(p0 = ifelse(year == "2010", average_price_nominal_euros, NA)) %>%
  fill(p0, .direction = "down") %>%
  mutate(p0_m2 = ifelse(year == "2010", average_price_m2_nominal_euros, NA)) %>%
  fill(p0_m2, .direction = "down") %>%
  ungroup() %>%
  mutate(pl = average_price_nominal_euros/p0*100,
         pl_m2 = average_price_m2_nominal_euros/p0_m2*100)

country_level_data <- country_level_data %>%
  group_by(locality) %>%
  mutate(p0 = ifelse(year == "2010", average_price_nominal_euros, NA)) %>%
  fill(p0, .direction = "down") %>%
  mutate(p0_m2 = ifelse(year == "2010", average_price_m2_nominal_euros, NA)) %>%
  fill(p0_m2, .direction = "down") %>%
  ungroup() %>%
  mutate(pl = average_price_m2_nominal_euros/p0*100,
         pl_m2 = average_price_m2_nominal_euros/p0_m2*100)
 
##PLot for 5 communes, and comparing price evolution in the communes
##to the national price evolution

communes <- c("Luxembourg",
              "Esch-sur-Alzette",
              "Mamer",
              "Schengen",
              "Wincrange")

##Luxembourg

filtered_data <- commune_level_data %>%
  filter(locality == communes[1])

data_to_plot <- bind_rows(country_level_data,
                          filtered_data)
lux_plot <- ggplot(data_to_plot) +
  geom_line(aes(year, pl_m2,
                group = locality,
                color = locality))
# Esch sur Alzette

filtered_data <- commune_level_data %>%
  filter(locality == communes[2])

data_to_plot <- bind_rows(
  country_level_data,
  filtered_data
)

esch_plot <- ggplot(data_to_plot) +
  geom_line(aes(y = pl_m2,
                x = year,
                group = locality,
                colour = locality))

# Mamer

filtered_data <- commune_level_data %>%
  filter(locality == communes[3])

data_to_plot <- bind_rows(
  country_level_data,
  filtered_data
)

mamer_plot <- ggplot(data_to_plot) +
  geom_line(aes(y = pl_m2,
                x = year,
                group = locality,
                colour = locality))

# Schengen

filtered_data <- commune_level_data %>%
  filter(locality == communes[4])

data_to_plot <- bind_rows(
  country_level_data,
  filtered_data
)

schengen_plot <- ggplot(data_to_plot) +
  geom_line(aes(y = pl_m2,
                x = year,
                group = locality,
                colour = locality))

# Wincrange

filtered_data <- commune_level_data %>%
  filter(locality == communes[5])

data_to_plot <- bind_rows(
  country_level_data,
  filtered_data
)

wincrange_plot <- ggplot(data_to_plot) +
  geom_line(aes(y = pl_m2,
                x = year,
                group = locality,
                colour = locality))

##Saving
ggsave("plots//lux_plot.pdf", lux_plot)
ggsave("plots//esch_plot.pdf", esch_plot)
ggsave("plots//mamer_plot.pdf", mamer_plot)
ggsave("plots//schengen_plot.pdf", schengen_plot)
ggsave("plots//wincrange_plot.pdf", wincrange_plot)
