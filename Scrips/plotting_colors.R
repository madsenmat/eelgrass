library(tidyverse)
library(gapminder)

#save existing parameter settings
opar <- par(pch = 19)

##OUR CODE GOES HERE 
jdat <- gapminder %>%
  filter(country %in% c("Eritrea", "Nepal", "Chad", "Jamaica",
                        "Cuba", "Costa Rica", "Germany", "Norway"),
         year == 2007) %>%
  arrange(gdpPercap)
jdat

#Basic plot set up 
j_xlim <- c(460, 60000)
j_ylim <- c(47, 82)
plot(lifeExp ~ gdpPercap, jdat, log = 'x', xlim = j_xlim, ylim = j_ylim,
     main = "Start your engines ...")

#makes everything red, just recycled red 
plot(lifeExp ~ gdpPercap, jdat, log = 'x', xlim = j_xlim, ylim = j_ylim,
     col = "red", main = 'col = "red"')

#proved recycling with blue/orange, get replicated 
plot(lifeExp ~ gdpPercap, jdat, log = 'x', xlim = j_xlim, ylim = j_ylim,
     col = c("blue", "orange"), main = 'col = c("blue", "orange")')

#using number, without declaring number will just add 
plot(lifeExp ~ gdpPercap, jdat, log = 'x', xlim = j_xlim, ylim = j_ylim,
     col = 1:8, main = paste0('col = 1:', 8))
with(jdat, text(x = gdpPercap, y = lifeExp, pos = 1))

#need to declare under main to show color
plot(lifeExp ~ gdpPercap, jdat, log = 'x', xlim = j_xlim, ylim = j_ylim,
     col = 1:8, main = 'the default palette()')
with(jdat, text(x = gdpPercap, y = lifeExp, labels = palette(),
                pos = rep(c(1, 3, 1), c(5, 1, 2)))) 

#providing own colors
j_colors <- c('chartreuse3', 'cornflowerblue', 'darkgoldenrod1', 'peachpuff3',
              'mediumorchid2', 'turquoise3', 'wheat4', 'slategray2')
plot(lifeExp ~ gdpPercap, jdat, log = 'x', xlim = j_xlim, ylim = j_ylim,
     col = j_colors, main = 'custom colors!')
with(jdat, text(x = gdpPercap, y = lifeExp, labels = j_colors,
                pos = rep(c(1, 3, 1), c(5, 1, 2)))) 

#what colors are available 

head(colors())

#can use viridis, most common one used now 

#vignette
<https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html>
  
library(ggplot2)
library(viridis)
ggplot(data.frame(x = rnorm(10000), y = rnorm(10000)), aes(x = x, y = y)) +
  geom_hex() + coord_fixed() +
  scale_fill_viridis() + theme_bw()


#Taking Control of ggplot

<http://stat545.com/block019_enforce-color-scheme.html>

library(ggplot2)
library(gapminder)
suppressPackageStartupMessages(library(dplyr))
jdat <- gapminder %>% 
  filter(continent != "Oceania") %>% 
  droplevels() %>% 
  mutate(country = reorder(country, -1 * pop)) %>% 
  arrange(year, country)

j_year <- 2007
q <-
  jdat %>% 
  filter(year == j_year) %>% 
  ggplot(aes(x = gdpPercap, y = lifeExp)) +
  scale_x_log10(limits = c(230, 63000))
q + geom_point()

##now do obnoxiously 
## do I have control of size and fill color? YES!
q + geom_point(pch = 21, size = 8, fill = I("darkorchid1"))

q + geom_point(aes(size = pop), pch = 21)

#suppressed legend and set the limits for the scale of size 
(r <- q +
    geom_point(aes(size = pop), pch = 21, show.legend = FALSE) +
    scale_size_continuous(range = c(1,40)))

# use facet to ensure that fill is right 
(r <- r + facet_wrap(~ continent) + ylim(c(39, 87)))
r + aes(fill = continent)

#setting own fill colours
r + aes(fill = country) + scale_fill_manual(values = country_colors)


#worked examples 

japan_dat <- gapminder %>%
  filter(country == "Japan")
japan_tidy <- japan_dat %>%
  gather(key = var, value = value, pop, lifeExp, gdpPercap)
dim(japan_dat)
dim(japan_tidy)

p <- ggplot(japan_tidy, aes(x = year, y = value)) +
  facet_wrap(~ var, scales="free_y")

p + geom_point() + geom_line() +
  scale_x_continuous(breaks = seq(1950, 2011, 15))
##restore previous parameter settings


test  <- data.frame(person=c("group 1", "group 2", "group 3"), 
                    value1=c(100,150,120),  # male   
                    value2=c(25,30,45) ,    # female
                    value3=c(25,30,45),     # male
                    value4=c(100,120,150),  # female
                    value5=c(10,12,15),     # male
                    value6=c(50,40,70))     # female

library(reshape2) # for melt

melted <- melt(test, "person")

melted$cat <- ''
melted[melted$variable == 'value1' | melted$variable == 'value2',]$cat <- "sub group 1"
melted[melted$variable == 'value3' | melted$variable == 'value4',]$cat <- "sub group 2"
melted[melted$variable == 'value5' | melted$variable == 'value6',]$cat <- "sub group 3"
melted$gender <- ''
melted[melted$variable %in% sprintf("value%i",c(1,3,5)),]$gender <- "female"
melted[melted$variable %in% sprintf("value%i",c(2,4,6)),]$gender <- "male"


p = ggplot(melted, aes(x = cat, y = value, fill = gender)) 

p + geom_bar(stat = 'identity', position = 'stack') +   facet_grid(~ person) + 
  scale_fill_manual(values = c("orangered","dodgerblue2")) + 
  theme(panel.background = element_rect(fill = 'white'))

par(opar)