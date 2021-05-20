library(data.table)
library(ggplot2)
library(ggrepel)
library(patchwork)

source('team_colors.R') # bring in the team colour tables

# Define the ESL six
ESL <- c('Man United', 'Liverpool', 'Chelsea', 'Arsenal', 'Tottenham', 'Man City')

# Define functions for data wrangling -------------------------------------
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

# Get the time updated points and goal difference across the season
data.prep <- function(team = 'Scunthorpe', x){
  x <- x[HomeTeam == team | AwayTeam == team,
         .(Date, home = HomeTeam, away = AwayTeam, home_score = FTHG, away_score = FTAG)]
  x[, yearly_game_id := c(1:nrow(x))]
  
  z <- .collect.results(team, x)
  # Calculate results and points earned
  z[gf == ga, outcome := 'draw']
  z[gf > ga, outcome := 'win']
  z[gf < ga, outcome := 'loss']
  z[, pts := 0]
  z[gf > ga, pts := 3]
  z[gf == ga, pts := 1]
  z[, gd := cumsum(gf-ga)] # time updated points
  z[, pts.tu := cumsum(pts)]# time updated gd
  
  output <- z
  
  return(output)
  
}

# For a given set of fixtures, build a league table
league.table <- function(x, ESL){
  
  # Drop games containing ESL members
  x <- x[!(HomeTeam %in% ESL)]
  x <- x[!(AwayTeam %in% ESL)]
  
  # get unique teams 
  teams <- unique(x$HomeTeam)
  
  # For each team, get their season progression
  z <- lapply(teams, FUN = data.prep, x)
  
  # Summarise each team, in terms of points gained and goal difference
  zz <- 
    rbindlist(lapply(
      z,
      FUN = function(x) x[, .(nation = head(nation,1), pts = sum(pts), gd = sum(gf -ga))]))
  
  # Order all teams on points, then goals
  setkey(zz, pts, gd)
  
  # Return the table
  standings <- rev(zz$nation)
  return(standings)
}



# Read in data ------------------------------------------------------------
wd <- getwd()

# Read in the post 2000 seasons, they all have the same format
seasons <- c(0:20)
seasons <- paste0(seasons, seasons+1)
files <- paste0(wd, '/season-', seasons,'_csv.csv')
x <- lapply(files, function(x) fread(x)[, .(Date, HomeTeam, AwayTeam, FTHG, FTAG)])
names(x) <- seasons

# Read in the pre 2000 seasons, they have a different format to post 2000
seasons <- c(1993:1999)
seasons <- paste0(seasons, '_', seasons+1)
files <- paste0(wd, '/season-', seasons,'_csv.csv')
y <- lapply(files, function(x) fread(x)[, .(Date, HomeTeam, AwayTeam, FTHG, FTAG)])
y <- lapply(y, function(x) x[complete.cases(x)])  #clean dodgy rows
names(y) <- seasons

# Bind the pre and post 2000 seasons into one list
x <- c(y, x)


# Data preparation
# Calculate the winner of each counter-factual league 
champions <- lapply(X = x, FUN = league.table, ESL = ESL) # full table for each year
champions <- lapply(champions, FUN = head, 1) # extract the winner for each year
champions <- data.table(
  champion = unlist(champions),
  season = names(champions),
  season.int = 1993:2020,
  id = 1:length(champions))

# Bring all tables into a single one
x <- rbindlist(x, use.names = TRUE, idcol = 'season')

# Drop the ESL teams 
x <- x[!(HomeTeam %in% ESL)]
x <- x[!(AwayTeam %in% ESL)]

# Construct the plot data
teams <- unique(x$HomeTeam)
plot.data <- rbindlist(lapply(teams, FUN = data.prep, x))

# Derive summary statistics for each team
plot.data <-
  plot.data[, .(
    gp = .N,
    ppg = sum(pts)/.N, 
    gd = sum(gf - ga),
    gdpg = sum(gf - ga)/.N),
    by = nation]


# Plotting ----------------------------------------------------------------

# Performance plot
p.1 <- ggplot(
  plot.data,
  aes(
    x = gp,
    y = ppg,
    color = nation)) +
  geom_text_repel(
    aes(x = gp, y = ppg, label = nation),
    inherit.aes = FALSE,
    nudge_y = .03,
    size = 3,
    family = "DIN Next LT Pro Light" ,
    color = 'grey10',
    segment.color = 'grey70',
    box.padding = 0.3, # additional padding around each text label
    min.segment.length = 0, 
    xlim = c(NA, Inf)
  ) +
  geom_point(
    pch = 21,
    aes(
      fill = nation,
      color = nation,
      size = exp(gdpg)))+
  scale_fill_manual(values = sapply(pal.teams, FUN = `[[`, 'win')) + 
  scale_color_manual(values = sapply(pal.teams, FUN = `[[`, 'loss')) + 
  scale_y_continuous(limits = c(0.55, 1.75)) + 
  guides(color = "none", fill = "none", size = 'none') +
  coord_cartesian(clip = "off") +
  ylab("Points Per Game") + 
  xlab("Number Of Matches Played") + 
  labs(title = 'The EPL Without The ESL',
       subtitle = "In April 2021 six English Premier League (EPL) teams announced that they would join an emerging, automatic qualification, European Super League (ESL); outside of the control of UEFA and FIFA.\nWhich teams would have performed best if the ESL 6 (Manchester United, Manchester City, Arsenal, Chelsea, Tottenham, Liverpool) hadn't competed in the EPL following its inception in 1993-94?\nThis visualisation shows the points per game each team would have acheived plotted against the number of games the team would have played.\nThe size of each point is proportional to the per game goal difference achieved by each team.",
       caption = "Visualisation by Joe O'Reilly - (josephedwardoreilly.github.com)") + 
  theme(
    text = element_text(family = "DIN Next LT Pro Light"),
    plot.margin = margin(10, 4, 4, 4),
    plot.background = element_rect(fill = '#F5F5F5', color = NA),
    plot.title = element_text(size = 30),
    panel.background = element_rect(fill = '#F5F5F5', color = NA),
    panel.grid.major.x  = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_line(linetype = 2, color = 'grey80'),
    plot.caption = element_text(color = 'grey50'),
    axis.ticks = element_blank()) 

# Champions plot 
p.2 <- ggplot(
  champions,
  aes(
    x = id - 1,
    y = 0,
    fill = champion,
    label = paste0(
      champion, 
      '\n',
      season.int,
      '-',
      season.int + 1))) + 
  geom_tile(aes(color = champion))+
  coord_equal() + 
  guides(fill = FALSE, color = FALSE) + 
  geom_text(aes(color = champion), size = 1.8, family = "DIN Next LT Pro" ) + 
  labs(title= 'Who Would Have Been EPL Champion In Each Season ?') + 
  scale_x_continuous(expand = c(0,0)) + 
  scale_color_manual(values = sapply(pal.teams, FUN = `[[`, 'loss')) + 
  scale_fill_manual(values = sapply(pal.teams, FUN = `[[`, 'win')) + 
  theme_void()+ 
  theme(
    text = element_text(family = "DIN Next LT Pro Bold" ),
    plot.margin = margin(0, 4, 4, 4),
    plot.background = element_rect(fill = '#E0E0E0', color = 'black'),
    panel.background = element_rect(fill = '#E0E0E0', color = NA),
    plot.title.position = 'plot',
    plot.title = element_text(hjust = 0.5)) 

# Construct the final plot    
p.1 +
  inset_element(
    p.2,
    left = 0.10, 
    right = 0.975,
    bottom = 0.025,
    top = 0.25,
    clip = FALSE,
    align_to = "full") + 
  ggsave(
    filename = 'ESL.png',
    width = 16,
    height = 11,
    device = 'png')
  

