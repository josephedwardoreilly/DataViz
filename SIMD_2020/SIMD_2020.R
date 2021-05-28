library(tidyverse) 
library(data.table)
library(ggplot2)
library(viridis)
library(sf)
library(patchwork)
library(cowplot)
library(ggtext)
library(biscale)

extrafont::loadfonts()

# Read area mapping data
x <- fread(here::here('SIMD_2020', 'OA_MSOA_LSOA_LA_SCOTLAND.csv'))
# Just LSOA, MSOA, and LA
x <- x[, .(LSOA11CD, LSOA11NM, MSOA11CD,  MSOA11NM, LAD17CD, LAD17NM)]

# Read SIMD data
y <- fread(here::here('SIMD_2020', 'SIMD2020.csv'))
y[, LSOA11CD := Data_Zone] # Add column for LSOA

# Merge SIMD and area data
z <- merge(
  x,
  y[, .(
    LSOA11CD,
    SIMD2020v2_Rank,
    SIMD2020_Housing_Domain_Rank,
    SIMD2020v2_Income_Domain_Rank,
    SIMD2020_Employment_Domain_Rank,
    SIMD2020_Health_Domain_Rank,
    SIMD2020_Education_Domain_Rank, 
    SIMD2020_Access_Domain_Rank,
    SIMD2020_Crime_Domain_Rank)], 
  by = 'LSOA11CD'
)

# Collapse redundant rows following the merge
z <- unique(z)

# Add a 'reverse' SIMD, where 1 is least deprived
z[order(SIMD2020v2_Rank, decreasing = TRUE), SIMD2020v2_Rank := 1:nrow(z)]

z[order(SIMD2020v2_Income_Domain_Rank, decreasing = TRUE),
  SIMD2020v2_Income_Domain_Rank := 1:nrow(z)]

z[order(SIMD2020_Housing_Domain_Rank, decreasing = TRUE),
  SIMD2020_Housing_Domain_Rank := 1:nrow(z)]

z[order(SIMD2020_Employment_Domain_Rank, decreasing = TRUE),
  SIMD2020_Employment_Domain_Rank := 1:nrow(z)]

z[order(SIMD2020_Health_Domain_Rank, decreasing = TRUE),
  SIMD2020_Health_Domain_Rank := 1:nrow(z)]

z[order(SIMD2020_Education_Domain_Rank, decreasing = TRUE),
  SIMD2020_Education_Domain_Rank := 1:nrow(z)]

z[order(SIMD2020_Access_Domain_Rank, decreasing = TRUE),
  SIMD2020_Access_Domain_Rank:= 1:nrow(z)]

z[order(SIMD2020_Crime_Domain_Rank, decreasing = TRUE),
  SIMD2020_Crime_Domain_Rank := 1:nrow(z)]

# Summarise the SIMD info in each LTLA 
z.summ <- z[, .(
  SIMD_Rm = mean(SIMD2020v2_Rank),
  SIMD_Inc_Rm = mean(SIMD2020v2_Income_Domain_Rank),
  SIMD_Hou_Rm = mean(SIMD2020_Housing_Domain_Rank),
  SIMD_Emp_Rm = mean(SIMD2020_Employment_Domain_Rank),
  SIMD_Hea_Rm = mean(SIMD2020_Health_Domain_Rank),
  SIMD_Edu_Rm = mean(SIMD2020_Education_Domain_Rank),
  SIMD_Acc_Rm = mean(SIMD2020_Access_Domain_Rank),
  SIMD_Cri_Rm = mean(SIMD2020_Crime_Domain_Rank)),
  by = .(Laname = LAD17NM)]


# Read and prep the shape data
Background <- st_read(
  here::here('SIMD_2020',"LocalAuthorities-lowertier.gpkg"),
  layer="7 Background") %>% filter(Name == 'Scotland')

# Prep the label alignment
Group_labels <- st_read(
  here::here('SIMD_2020',"LocalAuthorities-lowertier.gpkg"),
  layer="1 Group labels") %>% 
  filter(RegionNation == 'Scotland') %>%
  # Reformat some labels to fit onto plots nicely
  mutate(just=if_else(LabelPosit=="Left", 0, 1)) %>%
  mutate(
    Group.labe = if_else(
      Group.labe == "Glasgow & Clyde",
      'Glasgow &\n Clyde',
      Group.labe)) %>%
  mutate(
    Group.labe = if_else(
      Group.labe == "Lothian & Borders",
      'Lothian &\n Borders',
      Group.labe))


# merge on the case data to the shapes
ltladata <- st_read(
  here::here('SIMD_2020',"LocalAuthorities-lowertier.gpkg"),
  layer="4 LTLA-2019") %>% 
  filter(RegionNation == 'Scotland') %>%
  left_join(z.summ, by="Laname")


# Build the basic map elements
map.base <- ggplot() +
  geom_sf(
    data = Background,
    aes(geometry = geom),
     size = .5, color = NA,
    fill = '#F4F6F6') + 
  # Extend the x axis to fit the annotations on the first map
  # Limits are derived from the 'Background' object
  scale_x_continuous(limits = c(13.78, 35)) + 
  theme_void() + 
  theme(
    plot.margin = margin(1, 1, 1,1),
    axis.title = element_blank(),
    plot.title = element_text(family = 'Helvetica Neue'),
    plot.background = element_rect(color = NA, fill = '#DDE3E3')) 


# Build the fill for a given SIMD domain
data_hea <- bi_class(
  ltladata,
  x = SIMD_Rm,
  y = SIMD_Hea_Rm,
  style = "quantile",
  dim = 3)

data_inc <- bi_class(
  ltladata,
  x = SIMD_Rm,
  y = SIMD_Inc_Rm,
  style = "quantile",
  dim = 3)

data_hou <- bi_class(
  ltladata,
  x = SIMD_Rm,
  y = SIMD_Hou_Rm,
  style = "quantile",
  dim = 3)

data_emp <- bi_class(
  ltladata,
  x = SIMD_Rm,
  y = SIMD_Emp_Rm,
  style = "quantile",
  dim = 3)

data_edu <- bi_class(
  ltladata,
  x = SIMD_Rm,
  y = SIMD_Edu_Rm,
  style = "quantile",
  dim = 3)

data_acc <- bi_class(
  ltladata,
  x = SIMD_Rm,
  y = SIMD_Acc_Rm,
  style = "quantile",
  dim = 3)

data_cri <- bi_class(
  ltladata,
  x = SIMD_Rm,
  y = SIMD_Cri_Rm,
  style = "quantile",
  dim = 3)

# Build the individual maps [this could be function-ified]
p.hea <- map.base + 
  geom_sf(
    data=data_hea, 
    aes(geometry=geom, fill=bi_class),
    colour="Black",
    show.legend = FALSE,
    size=.35) +
  bi_scale_fill(
    pal = 'DkViolet',
    dim = 3) + 
  ggtitle('Health') + 
  geom_sf_text(
    data = Group_labels,
    family = 'Helvetica Neue', size = 3.5,
    aes(geometry=geom, label= Group.labe, hjust=just)) 

p.inc <- map.base + 
  geom_sf(
    data=data_inc, 
    aes(geometry=geom, fill=bi_class),
    colour="Black",
    show.legend = FALSE,
    size=.35) +
  bi_scale_fill(
    pal = 'DkViolet',
    dim = 3) + 
  ggtitle('Income') 
  

p.hou <- map.base + 
  geom_sf(
    data=data_hou, 
    aes(geometry=geom, fill=bi_class),
    colour="Black",
    show.legend = FALSE,
    size=.35) +
  bi_scale_fill(
    pal = 'DkViolet',
    dim = 3) + 
  ggtitle('Housing') 

p.emp <- map.base + 
  geom_sf(
    data=data_emp, 
    aes(geometry=geom, fill=bi_class),
    colour="Black",
    show.legend = FALSE,
    size=.35) +
  bi_scale_fill(
    pal = 'DkViolet',
    dim = 3) + 
  ggtitle('Employment') 

p.edu<- map.base + 
  geom_sf(
    data=data_edu, 
    aes(geometry=geom, fill=bi_class),
    colour="Black",
    show.legend = FALSE,
    size=.35) +
  bi_scale_fill(
    pal = 'DkViolet',
    dim = 3) + 
  ggtitle('Education') 

p.acc <- map.base + 
  geom_sf(
    data=data_acc, 
    aes(geometry=geom, fill=bi_class),
    colour="Black",
    show.legend = FALSE,
    size=.35) +
  bi_scale_fill(
    pal = 'DkViolet',
    dim = 3) + 
  ggtitle('Services Access') 

p.cri<- map.base + 
  geom_sf(
    data=data_cri, 
    aes(geometry=geom, fill=bi_class),
    colour="Black",
    show.legend = FALSE,
    size=.35) +
  bi_scale_fill(
    pal = 'DkViolet',
    dim = 3) + 
  ggtitle('Crime') 


legend.inset <- bi_legend(
  pal = "DkViolet",
  dim = 3,
  xlab = "Greater Mean\n SIMD Rank ",
  ylab = "Greater Mean\n Domain Rank",
  size = 10) + 
  theme(plot.background = element_blank(),
        axis.title = element_text(
          family = 'Helvetica Neue', vjust = 0.5),
        plot.margin = margin(5, 5, 5, 5))

p.text <- ggplot() +
  geom_sf(
    data = Background,
    aes(geometry = geom),
    size = .5, color = NA,
    fill = NA) + 
  theme_void() + 
  theme(
    plot.margin = margin(0, 0, 0, 0),
    axis.title = element_blank(),
    plot.background = element_rect(
      color = NA,
      fill = '#DDE3E3')) 

p.text.body <- ggplot() + 
  # Text body
  geom_textbox(
    data = data.frame(
      x = 0.1,
      y = 0.70,
      label = 'The Scottish Index Of Multiple Deprivation 2020 (SIMD) is an area based ranked index of social deprivation. It is constructed from seven domains, or aspects, of deprivation. Each area is ranked within each domain and then the ranks over all seven domains are combined to create the final SIMD rank. These plots show the difference between the mean SIMD rank for a local area, and the domain specific rank for that same area. **Note: SIMD ranks most to least deprived in ascending order, for these plots the ranking is inverted, such that a lower rank indicates less social deprivation. This has been done to improve ease of interpretation**'),
  aes(x, y, label = label),
  color = 'black',
  box.color = NA,
  fill = '#DDE3E3',
  family = "Helvetica Neue" ,
  size = 5,
  width = grid::unit(0.75, "npc"), # 73% of plot panel width
  hjust = 0, vjust = 0.5
  ) +
  # Text title
  geom_richtext(
    data = data.frame(
      x = 0.5,
      y = 0.95,
      label = 'SIMD Domain Disparity'),
    aes(x, y, label = label),
    color = 'black',
    label.color = NA,
    family = "Helvetica Neue" ,
    fill = NA,
    size = 12,
    hjust = 0.5, vjust = 0.5
  ) +
  xlim(0, 1) +
  ylim(0.5, 1) +
  theme_void()+
  theme(
    plot.margin = margin(5, 0, 0, 0),
    plot.background = element_rect(fill = '#DDE3E3', color = NA)
  )

legend <- ggdraw() +
  draw_plot(p.text, 0, 0, 1, 1) +
  draw_plot(legend.inset, 0.5, 0.125, 0.75, 0.75, hjust = 0.5)

# Arrange all plots together
p.text.body +
  legend + 
  p.hea +
  p.acc +
  p.cri +
  p.inc + 
  p.emp + 
  p.edu +
  p.hou +
  plot_layout(
    design = "1111
              2345
              6789", heights = c(.5,1,1)) +
  # Exploit plot_annotation to colour the background fully
  plot_annotation(
    caption = "Visualisation by Joe O'Reilly (github.com/josephedwardoreilly)",
    theme = theme(
      plot.margin = margin(5, 10, 5, 10),
      plot.caption = element_text(
        family = 'Helvetica Neue',
        size = 12,
        colour = 'grey10'),
      plot.background = element_rect(
        fill = '#DDE3E3',
        color = NA))) + 
    ggsave(
      filename = here::here('SIMD_2020', 'SIMD_Disparity.png'),
      device = 'png',
      width = 18,
      height = 12)





