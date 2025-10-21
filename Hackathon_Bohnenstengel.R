################################################################################
########################  Hackathon in Economic Policy  ########################
##################################  13.01.2025  ################################
################################################################################

# Setup  -----------------------------------------------------------------------

#install.packages("grImport2")
rm(list = ls())
library(tidyverse)
library(janitor)

library(gghighlight)

library(countrycode)
library(eurostat)
library(ggflags)
library(countrycode)
library(scales)
library(sysfonts)
library(showtext)


sysfonts::font_add_google(name = "Roboto Condensed",
                          family = "Roboto Condensed")

showtext_auto()
showtext_opts(dpi = 320)

setwd("C:/Users/janni/Uni lokal/WU/Economic_Policy/Hackathon")

getwd()


# Data Import and Manipulation  -----------------------------------------------
deficit <- get_eurostat("TEC00127", time_format = "raw", type = "label")

deficit <- deficit |> 
  filter(TIME_PERIOD == 2023) |> 
  filter(unit == "Percentage of gross domestic product (GDP)")
  

debt <- get_eurostat("sdg_17_40", time_format = "raw", type = "label")

debt <- debt |> 
  filter(TIME_PERIOD == 2023) |> 
  filter(unit == "Percentage of gross domestic product (GDP)")

##  Merging data sets  ---------------------------------------------------------
merge <- merge(debt, deficit, by = c("geo", "TIME_PERIOD", "freq", "sector",
                                     "unit"))

merge <- merge |> 
  rename(debt = values.x,
         deficit = values.y)

merge <- merge[, -c(6,8)]

merge <- merge |> 
  mutate(iso2 = tolower(countrycode(geo, origin = "country.name",
                                    destination = "iso2c"))) |> 
  filter(!is.na(iso2))


# Plotting  -------------------------------------------------------------------

plot <- merge |> 
  ggplot(aes(x = debt, y = deficit, country = iso2)) +
  geom_flag(size = 8) + 
  geom_vline(xintercept = 60, color = "red", linetype = "solid") + 
  geom_hline(yintercept = -3, color = "blue", linetype = "solid") +
  geom_rect(aes(xmin=15, xmax = 60, ymin = -3, ymax = 3.8),
            fill = "green", alpha = 0.005) +
  scale_x_continuous(labels = percent_format(scale = 1), limits = c(15, 170),
                     expand= c(0, 0), breaks = seq(40, 160, by= 40)) + 
  scale_y_continuous(labels = percent_format(scale = 1), limits = c(-7.7, 3.8),
                     expand = c(0, 0), breaks = seq(-6, 2, 2)) + 
  labs(x = "Government Debt (% of GDP)",
       y = "Government Deficit (% of GDP)",
       title = "Crossing the Lines: Tracking Compliance \nwith the Maastricht Rules in 2023",
       caption = "Source: EUROSTAT; Graph: Bauer, Berben, Bohnenstengel") +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold"))
plot

ggsave("Maastricht_Plot.png", plot, bg="white", height = 5, width = 6, dpi=320)
