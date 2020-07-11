library(plotly)
library(gapminder)
g <- highlight_key(gapminder, ~continent)
gg <- ggplot(g, aes(gdpPercap, lifeExp, 
                    color = continent, frame = year)) +
  geom_point(aes(size = pop, ids = country)) +
  geom_smooth(se = FALSE, method = "lm") +
  scale_x_log10()
highlight(ggplotly(gg), "plotly_hover")


data=data.frame(days=rep(c('Mon','Tu','Wed','Thur','Fri','Sat','Sun'),3),
           value=rnorm(21,10,20),
           class=rep(c('A','B','C'),each=7)
           )

sindex=highlight_key(data,~days)
base=plot_ly(sindex,height=800)%>%group_by(class)
g1=base%>%filter(class=='A')%>%add_bars(x=~days,y=~value)
g2=base%>%filter(class=='B')%>%add_bars(x=~days,y=~value)
g3=base%>%filter(class=='C')%>%add_bars(x=~days,y=~value)
subplot(g1,g2,g3,titleX = TRUE,nrows = 1,shareY = T) %>% 
  highlight(on='plotly_hover')



########################
data(gap, package = "plotlyBook")

gapKey <- highlight_key(gap, ~country)

p1 <- plot_ly(gap, y = ~country, x = ~popDen, hoverinfo = "x") %>%
  add_markers(alpha = 0.1, color = I("black")) %>%
  add_markers(
    data = gapKey, 
    frame = ~year, 
    ids = ~country, 
    color = I("red")
  ) %>%
  layout(xaxis = list(type = "log"))

p2 <- plot_ly(gap, x = ~gdpPercap, y = ~lifeExp, size = ~popDen, 
              text = ~country, hoverinfo = "text") %>%
  add_markers(color = I("black"), alpha = 0.1) %>%
  add_markers(
    data = gapKey, 
    frame = ~year, 
    ids = ~country, 
    color = I("red")
  ) %>%
  layout(xaxis = list(type = "log"))

subplot(p1, p2, nrows = 1, widths = c(0.3, 0.7), titleX = TRUE) %>%
  hide_legend() %>%
  animation_opts(1000, redraw = FALSE) %>%
  layout(hovermode = "y", margin = list(l = 100)) %>%
  highlight(
    "plotly_selected", 
    color = "blue", 
    opacityDim = 1, 
    hoverinfo = "none"
  )





 library(plotly)

 # shape data into desired format
   dat <- england %>% 
     gather(location, team, home, visitor) %>% 
     # focus on tier 1 teams that are still playing in 2015
     filter(team %in% maketable_eng(england, 2015, 1)[["team"]]) %>%
     mutate(
         pts = ifelse(location == "home" & goaldif > 0, 3, 
                                         ifelse(location == "away" & goaldif < 0, 3, 1))
       ) %>%
     arrange(Date) %>%
     group_by(Season, team) %>% 
     mutate(gameno = row_number(), cumpts = cumsum(pts)) %>%
     ungroup() %>%
     group_by(gameno) %>%
     mutate(meanP = mean(cumpts)) %>%
     filter(Season > 2006)

 sd <- highlight_key(dat, ~team, "Select a team")

 # a 'wormchart' like fig 8 here http://www.gradaanwr.net/wp-content/uploads/2016/06/dataApr16.pdf
   p <- ggplot(sd, aes(x = gameno, y = cumpts - meanP)) + 
     geom_line(aes(group = team), alpha = 0.5) + 
     facet_wrap(~ Season, ncol = 3) + 
     labs(
    +     title = "English Premier League Performance",
         x = "Game in season",
    +     y = "Cumulative points (above/below) average"
       ) 

 gg <- ggplotly(p, tooltip = "team")

 highlight(
     gg, 
     dynamic = TRUE, 
     selectize = TRUE,
     color = RColorBrewer::brewer.pal(12, "Paired")
   )
 
 
 
 
 ########################
 mtcars %>%
   highlight_key(~cyl) %>%
   plot_ly(
     x = ~wt, y = ~mpg, text = ~cyl, mode = "markers+text", 
     textposition = "top", hoverinfo = "x+y"
   ) %>%
   highlight(on = "plotly_hover", off = "plotly_doubleclick")
 ###################
 library(leaflet)
 library(crosstalk)
 eqs <- highlight_key(quakes)
 stations <- filter_slider(
   "station", "Number of Stations", 
   eqs, ~stations
 )
 
 p <- plot_ly(eqs, x = ~depth, y = ~mag) %>% 
   add_markers(alpha = 0.5) %>% 
   highlight("plotly_selected")
 
 map <- leaflet(eqs) %>% 
   addTiles() %>% 
   addCircles()
 
 bscols(
   widths = c(6, 6, 3), 
   p, map, stations
 )
##################
 library(gapminder)
 g <- highlight_key(gapminder, ~country)
 continent_filter <- filter_select(
   "filter", "Select a country", 
   g, ~continent
 )
 
 p <- plot_ly(g) %>%
   group_by(country) %>%
   add_lines(x = ~year, y = ~lifeExp, color = ~continent) %>%
   layout(xaxis = list(title = "")) %>%
   highlight(selected = attrs_selected(showlegend = FALSE))
 
 bscols(continent_filter, p, widths = 12)
 
 
 #######################
 d <- highlight_key(mpg)
 base <- plot_ly(d, color = I("black"), showlegend = FALSE)
 
 subplot(
   add_histogram(base, x = ~class),
   add_markers(base, x = ~displ, y = ~hwy)
 ) %>%
   # Selections are actually additional traces, and, by default, 
   # plotly.js will try to dodge bars placed under the same category
   layout(barmode = "overlay", dragmode = "lasso") %>%
   highlight(on="plotly_selected",off="plotly_doubleclick")
 