library(tidyverse) # TODO: replace with data.table code
require(data.table)
require(ggplot2)
library(viridis)
library(sf)

# Read and wrangle the case data
x <- fread(here::here('Covid_Map', "ltla_2021-05-17_Cases.csv"))
x <- x[date == '2020-12-04']
x[, Lacode := areaCode]
x[, `% Change In Number Of New Cases` := newCasesBySpecimenDateChangePercentage]

# Read and prep the shape data
Background <- st_read(
  here::here('Covid_Map', "LocalAuthorities-lowertier.gpkg"),
  layer="7 Background")

# Prep the label alignment
Group_labels <- st_read(
  here::here('Covid_Map', "LocalAuthorities-lowertier.gpkg"),
  layer="1 Group labels") %>% 
  mutate(just=if_else(LabelPosit=="Left", 0, 1))

# merge on the case data to the shapes
ltladata <- st_read(
  here::here('Covid_Map', "LocalAuthorities-lowertier.gpkg"),
  layer="4 LTLA-2019") %>% 
  left_join(x, by="Lacode")


# Plot
ggplot()+
  geom_sf(
    data = Background,
    aes(geometry = geom),
    fill = NA, size = .5, color = 'grey40')+
  geom_sf(
    data=ltladata, 
    aes(geometry=geom, fill=`% Change In Number Of New Cases`),
    colour="Black",
    size=.35)+
  geom_sf_text(
    data=Group_labels,
    aes(geometry=geom, label= Group.labe, hjust=just),
    size=rel(2.4),
    colour="white",
    family = 'DIN Next LT Pro Light') + 
  guides(fill = guide_colourbar(
    ticks = FALSE,
    barwidth = 15,
    barheight = 0.5,
    title.position = 'top')) + 
  theme_void() +
  labs(title = 'Percentage Change In Daily Covid-19 Infections\nBy Lower Tier Local Authority\n(2020-12-04)',
       caption = "Visualisation by Joe O'Reilly (github.com/josephedwardoreilly)") + 
  theme(
    plot.background = element_rect(fill = '#19323C'),
    legend.justification = 'top',
    plot.margin = margin(10, 20, 10, 20),
    legend.position = 'bottom',
    legend.title = element_text(
      size = 8, hjust = 0.5,
      family = 'DIN Next LT Pro Bold',
      color = 'white'),
    legend.text = element_text(
      size = 7,
      family = 'DIN Next LT Pro Light',
      color = 'white'),
    plot.title = element_text(
      family = 'DIN Next LT Pro Bold',
      color = 'white',
      size = 11),
    plot.title.position = "plot",
    plot.caption = element_text(
      family = 'DIN Next LT Pro Light',
      color = 'white',
      size = 7,
      hjust = 1,
      margin = margin(10, 0, 0, 0)),
    plot.caption.position = 'plot') + 
  scale_fill_viridis(
    option = "turbo", 
    breaks = c(-100, 0, 100),
    labels = c('Less than -100 %', '0 %', 'Greater than 100 %'),
    limits = c(-100, 100), oob = scales::squish) +
  ggsave(
    filename = here::here(
      'Covid_Map',
      'covid_daily_perc_change.png'),
    width = 6, 
    height = 8,
    device = 'png')


