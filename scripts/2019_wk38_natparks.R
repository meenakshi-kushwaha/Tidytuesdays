

library(tidyr)
library(dplyr)
library(lubridate)
library(scales)
library(gganimate)
library(gifski)
library(ggthemes)

##Read data
park_visits <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-17/national_parks.csv")
state_pop <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-17/state_pop.csv")
gas_price <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-17/gas_price.csv")
##Cleaning data 
#convert variables to factors
#convert year to date format
#filter national parks
parks<-park_visits%>%
  mutate(unit_type = as.factor(unit_type)) %>% 
  filter(unit_type == "National Park")%>%
  select(year, parkname, region, state, visitors)%>%
  mutate(year=as.factor(year),
         year=ymd(year, truncated = 2L),
         parkname=as.factor(parkname),
         region=as.factor(region),
         state=as.factor(state))
gas_price<-gas_price%>%
  mutate(year=as.factor(year),
         year=ymd(year, truncated = 2L))

visitors_year<-parks%>%
  group_by(year)%>%
  summarise(tot_year=sum(visitors))
##final dataframe
gas_park_visit<-inner_join(visitors_year, gas_price, by="year")
##plotting data
#compute scalefactor for secondary axis
scaleFactor <-  max(gas_park_visit$tot_year/1000000)/max(gas_park_visit$gas_current)
plot_gas<-ggplot(data=gas_park_visit, aes(x=year))+
  geom_line(aes(y=tot_year/1000000), color = "dark green")+
  geom_line(aes(y=gas_current*scaleFactor), color = "orange")+
  scale_x_date(labels = date_format("%Y"), date_breaks = "10 years")+
  labs(title="US annual national park visitors (millions)",
       subtitle="Compared to gas price trends ($/gal)",
       x="",
       caption="Data from data.world")+
  scale_y_continuous("National park visitors (millions)", 
                     sec.axis = sec_axis( ~./scaleFactor, name="Gas price ($/gallon)"))
p<-plot_gas+theme_fivethirtyeight()+
  theme(plot.title= element_text(size=20, color = "dark green"),
        plot.subtitle = element_text(size=14, color = "orange"),
        axis.text.y.left = element_text(size = 12, color = "dark green", face = "bold"),
        axis.text.y.right = element_text(size= 12, color = "#FF9933", face = "bold"))+
  transition_reveal(year)
animate(p)
anim_save(filename = "parks.gif", last_animation())
##experimented with 
#dual axes
#basic gganimate
#gg themese package (using five 38)