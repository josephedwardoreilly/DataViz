library(data.table)
library(patchwork)
library(ggplot2)
library(ggtext)


# define functions --------------------------------------------------------
# Build the x, y coordinates to plot the results
.build.poly <- function(
  md,
  gf,
  ga,
  width = 0.35,
  angle = 14){
  
  
  offset = (gf + ga) * sin(angle)
  offset = offset/10
  
  if(ga < gf){ # if a win, (md, md +offset, md + offset + width, md + width)
    dt <- data.table(
      y = c(-ga,
            gf,
            gf,
            -ga),
      x = c(md,
            md + offset,
            md + offset + width,
            md + width),
      md = rep(as.character(md), 4))
  } 
  
  
  if(ga > gf){ # if a loss, (md, md - offset, md - offset + width, md + width)
    dt <- data.table(
      y = c(-ga,
            gf,
            gf,
            -ga),
      x = c(md + offset,
            md ,
            md + width,
            md + offset + width),
      md = rep(as.character(md), 4))
  }
  
  if(ga == gf){ # if a draw, no offset
    offset <- 0
    dt <- data.table(
      y = c(-ga,
            gf,
            gf,
            -ga),
      x = c(md,
            md,
            md + width,
            md + width),
      md = rep(as.character(md), 4))
  }
  
  return(dt)
}

# collect all results in the required format
.collect.results <- function(country, dt){
  zz <- rbind(
    dt[home == country, .(yearly_game_id, gf = home_score, ga = away_score)], 
    dt[away == country, .(yearly_game_id, gf = away_score, ga = home_score)])
  setkey(zz, yearly_game_id)
  zz[, id := c(1:nrow(zz))]
  zz[, nation := country ]
  return(zz)
}

# Place plot construction in a function, use patchwork to arrange multiple 
# ggplot objects created with this function
build.plot <- function(plot.data){
  
  dt.polygons <- plot.data$poly
  dt.points <- plot.data$point
  
  p <- ggplot(dt.polygons) + 
    # add timeline 
    geom_hline(yintercept = 0, size = .5) +
    # Draw bars
    geom_polygon(
      aes(
        fill = outcome,
        group = md,
        x = x,
        y = y),
      color = 'black',
      size = .5) +
    # draw 'no goal' points
    geom_point(
      data = dt.points,
      inherit.aes = FALSE,
      aes(x = x, y = y),
      fill = 'black',
      shape = 21,
      size = 1,
      stroke = 1) +
    # draw 'no score draw' points
    geom_point(
      data = dt.points[score == FALSE],
      inherit.aes = FALSE,
      aes(x = x, y = y),
      fill = 'white',
      shape = 21,
      size = 2,
      stroke = 1) +
    scale_x_continuous(
      expand = c(0, 0),
      limits = as.numeric(
        dt.polygons[, c(min(md), max(id) + 1)])) + 
    coord_equal(clip = 'off') + 
    guides(color = FALSE,
           fill = FALSE) +
    theme_void() +
    theme(panel.spacing.y = unit(0, "lines"),
          plot.margin = margin(10, 10, 10, 10),
          plot.background = element_rect(fill = '#f5f9ff', color = NA),
          panel.border = element_blank(),
          plot.title = element_text(family = "DIN Next LT Pro" ),
          plot.caption = element_text(
            family = "DIN Next LT Pro" ,
            color = 'grey30',
            size = 7))
  
  return(p)
}

# Build a small version of elements from the main plot to act as a guide 
# presenting how the plot is to be interpreted
build.guide <- function(dt.polygons, dt.points){
  p <- ggplot(dt.polygons) + 
    # add timeline 
    geom_hline(yintercept = 0, size = .5) +
    # Draw bars
    geom_polygon(
      aes(
        fill = outcome,
        group = md,
        x = x,
        y = y),
      color = 'black',
      size = .5) +
    # draw 'no goal' points
    geom_point(
      data = dt.points,
      inherit.aes = FALSE,
      aes(x = x, y = y),
      fill = 'black',
      shape = 21,
      size = 1,
      stroke = 1) +
    # draw 'no score draw' points
    geom_point(
      data = dt.points[score == FALSE],
      inherit.aes = FALSE,
      aes(x = x, y = y),
      fill = 'white',
      shape = 21,
      size = 2,
      stroke = 1) +
    coord_equal(clip = 'off') + 
    guides(color = FALSE,
           fill = FALSE) +
    theme_void() +
    theme(panel.spacing.y = unit(0, "lines"),
          plot.margin = margin(10, 30, 10, 30),
          plot.background = element_rect(fill = '#f5f9ff', color = 'grey50'),
          panel.border = element_blank(),
          plot.title = element_text(family = 'DIN Next LT Pro'),
          plot.caption = element_text(
            family = 'DIN Next LT Pro',
            color = 'grey30',
            size = 7))
  
  return(p)
}


data.prep <- function(team = 'Scunthorpe', dt){
  x <- x[HomeTeam == team | AwayTeam == team,
         .(Date, home = HomeTeam, away = AwayTeam, home_score = FTHG, away_score = FTAG)]
  x[, yearly_game_id := c(1:nrow(x))]
  
  z <- .collect.results(team, x)
  z[gf == ga, outcome := 'draw']
  z[gf > ga, outcome := 'win']
  z[gf < ga, outcome := 'loss']
  
  dt.polygons <- z[, .build.poly(md = id, gf = gf, ga = ga, width = width), by = .(id, nation)]
  
  dt.polygons <- merge(
    dt.polygons,
    z[, .(md = as.character(id), nation, outcome)],
    by = c('nation', 'md'))
  
  dt.points <- 
    z[gf == 0 | ga == 0,
      .(nation, gf, ga, x = id + (width/2), y = 1)][
        ga == 0 & gf != 0, y := -y][, score := TRUE]
  # then deal with goalless draws
  dt.points <- rbind(
    dt.points,
    dt.points[gf == 0 & ga == 0, .(nation, gf, ga, x, y= -y, score = TRUE)], 
    dt.points[gf == 0 & ga == 0, .(nation, gf, ga, x, y= 0, score = FALSE)])
  
  
  output <- c()
  output$poly <- dt.polygons
  output$point <- dt.points
  output$pts <- z[, (sum(gf > ga) * 3) + sum(gf == ga)] # season points
  output$gd <- z[, sum(gf) - sum(ga)] # season goal diff.
  
  
  return(output)
  
}



# Clean Data --------------------------------------------------------------
x <- fread(here::here('Soccerbars', 'League1_2008_9', "E2.csv"))

width = .35 # width of bars


teams <- unique(x$HomeTeam)
# Polygon info calculated for all teams
plot.data <- lapply(teams, FUN = data.prep, dt = x)
names(plot.data) <- teams

# Build the league table 
league.table <- data.table(
  team = teams,
  pts = sapply(plot.data, "[[", "pts"),
  gd = sapply(plot.data, "[[", "gd"))
# Correctly order the table
setkey(league.table, pts, gd)
teams <- rev(league.table$team) # order teams by final position

# Order the list to match the league table 
plot.data <- plot.data[match(teams, names(plot.data))]

# source the team colour pal 
source(here::here('Soccerbars', 'League1_2008_9','team_colours.R'))

pal.teams <- pal.teams[match(teams, names(pal.teams))]

# Build a plot for each team 
p <- lapply(plot.data, FUN = build.plot)

# extract the max and min y-values, used to fix the y-scale to be uniform 
y.range <- c(
  min(sapply(plot.data, FUN = function(x) min(x$poly$y))),
  max(sapply(plot.data, FUN = function(x) max(x$poly$y))))

# Add team specific elements to plots, a loop takes more lines but is easier 
# to read here (in my opinion)
for(i in seq_along(p)){
  p[[i]] <- p[[i]] +
    labs(title = teams[[i]]) + 
    scale_y_continuous(
      limits = y.range,
      expand = c(0, 0)) +
    scale_fill_manual(values = pal.teams[[i]]) + # get colours correct for team
    theme(
      plot.background = element_rect( # add a nice light version of main coloir
        fill = adjustcolor(           # as a background
          pal.teams[[i]]['win'], alpha.f = 0.2),
        color = 'grey50'))
}

# Build the legend/guide
nation.results.guide <- data.table(id = c(1, 3, 5, 7),
                                   gf = c(2, 1, 1, 0),
                                   ga = c(1, 2, 1, 0),
                                   outcome = c('win', 'loss', 'draw',
                                               'draw'))

dt.polygons.guide <- 
  nation.results.guide[,.build.poly(md = id, gf = gf, ga = ga, width = width), 
                       by = .(id)]
dt.polygons.guide <- merge(
  dt.polygons.guide,
  nation.results.guide[, .(md = as.character(id), outcome)],
  by = c('md'))

dt.points.guide <- 
  nation.results.guide[gf == 0 | ga == 0,
                       .(gf, ga, x = id + (width/2), y = 1)][
                         ga == 0 & gf != 0, y := -y][, score := TRUE]
# then deal with goalless draws
dt.points.guide <- rbind(
  dt.points.guide,
  dt.points.guide[gf == 0 & ga == 0, .(gf, ga, x, y= -y, score = TRUE)], 
  dt.points.guide[gf == 0 & ga == 0, .(gf, ga, x, y= 0, score = FALSE)])

p.guide <- build.guide(dt.polygons.guide, dt.points.guide)+
  scale_fill_manual(values = pal.guide) + 
  theme_void() + 
  scale_x_continuous(limits = c(0, 8.5)) + 
  scale_y_continuous(limits = c(-2.5, 2.5)) +
  theme_void() + 
  geom_richtext(
    data = data.table(
      # Account for offset
      x = dt.polygons.guide[ , .SD[2, .(x = x + (width/2))], by = .(id)][, x],
      y = c(2.5, 1.5, 2, 2),
      label = c(
        'win',
        'loss',
        'score<br>draw',
        '0-0<br>draw')), # Add  annotations
    mapping = aes(x = x, y = y, label = label),
    family = "DIN Next LT Pro Light",
    size = 3,
    fill = NA,
    label.color = NA,
    hjust = 0.5,
    vjust = 0.5) +
  # Add vertical GF/GA labels
  geom_richtext(
    data = data.table(
      x = c(0.5, 0.5),
      y = c(-1.5, 1.5),
      label = c(
        'GA',
        'GF')), 
    mapping = aes(x = x, y = y, label = label),
    angle = 270,
    family = "DIN Next LT Pro Light",
    size = 3,
    fill = NA,
    label.color = NA,
    hjust = 0.5,
    vjust = 1)



p.title <- ggplot() + 
  # Text title
  geom_richtext(
    data = data.frame(
      x = 40,
      y = 0,
      label = 'League 1 : 2008-09'),
    aes(x, y, label = label),
    color = 'black',
    label.margin = margin(0,10,0,0),
    label.color = NA,
    family = "DIN Next LT Pro Bold" ,
    fill = NA,
    size = 12,
    hjust = 0.5, vjust = 0.5
  ) + 
  geom_text(
    data = data.frame(
      x = 37,
      y = -0.6,
      label = "Visualisation by Joe O'Reilly - github.com/josephedwardoreilly\nsoccerbars : https://sn.ethz.ch/research/soccerbars.html"),
    aes(x, y, label = label),
    color = 'grey50',
    family = "DIN Next LT Pro Light" ,
    size = 3,
    hjust = 0, vjust = 0.5
  ) + 
  scale_x_continuous(limits = c(36.6, 49)) + 
  scale_y_continuous(
    limits = c(-1,1),
    expand = c(0, 0)) +
  theme_void()

# Inset the legend into the title pane
p.title <- p.title + inset_element(
  p.guide,
  left = 0.6,
  bottom = 0.1, 
  right = 1,
  top = 0.9, align_to = 'full')



# Wrap all of the individual plots together and save to disk
all.p <- c(list(p.title), p)
wrap_plots(all.p,ncol = 1, heights = 0.5) + 
  ggsave(filename = here::here('Soccerbars', 'League1_2008_9','08.png'),
    width = 8, height = 50, device = 'png', limitsize = FALSE)






