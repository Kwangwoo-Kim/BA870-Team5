library(tidyverse)
library(ggthemes)
library(plotly)
library(readxl)
library(gganimate)
games <- read_csv("/cloud/project/sandbox/games.csv") 


YearBin <- cut(games$Year, breaks = c(1980, 1985, 1990, 1995, 2000, 2005, 2010, 2016))
games$YearBin <- YearBin
View(games)
select(games,
       YearBin,
       )
#select(flights,
       -year,
       contains("dealy"),
       starts_with("arr"),
       everything())

a <- select(games,
       YearBin,
       Genre,
       GLobal_Sales)
View(a)
YearBin1980_1985 <- filter(a, 
                           YearBin == "(1980,1985]")
YearBin1985_1990 <- filter(a, 
                           YearBin == "(1985,1990]")
YearBin1990_1995 <- filter(a, 
                           YearBin == "(1990,1995]")
YearBin1995_2000 <- filter(a, 
                           YearBin == "(1995,2000]")
YearBin2000_2005 <- filter(a, 
                           YearBin == "(2000,2005]")
YearBin2005_2010 <- filter(a, 
                           YearBin == "(2005,2010]")
YearBin2010_2016 <- filter(a, 
                           YearBin == "(2010,2016]")
ggplot(a) +
  
  geom_histogram(aes(x = Genre), stat = "count")
View(b)
unique(games$YearBin)
unique(games$Genre)
ggplot(a, aes(x = Genre, y = GLobal_Sales, group = YearBin, color = Genre)) +

  geom_point(position = "jitter", size = 0.5, alpha = 0.4) + 
  ggtitle("Monthly Average Temperature") +
  theme_hc(bgcolor = "darkunica") +
  scale_fill_hc("darkunica")

ggplot(a, aes(x = Genre, y = GLobal_Sales, group = YearBin, color = YearBin)) +

  geom_point(position = "jitter", size = 0.5, alpha = 0.4) + 
  ggtitle("Monthly Average Temperature") +
  theme_hc(bgcolor = "darkunica") +
  scale_fill_hc("darkunica")

total <- ggplot(a, aes(x=Genre)) + 
  geom_line(aes(y=GLobal_Sales, color = YearBin)) + 
  labs(title="US economic time series", 
       subtitle = "Personal Savings Rate",
       caption="Source: Economics", 
       y="Savings Rate %") +
  theme_classic()
ggplotly(total)

time1 <- ggplot(YearBin1980_1985, aes(x=Genre)) + 
  geom_line(aes(y=GLobal_Sales)) + 
  labs(title="US economic time series", 
       subtitle = "Personal Savings Rate",
       caption="Source: Economics", 
       y="Savings Rate %") +
  theme_classic()

ggplotly(time1)
ggplot(a, aes(x = Genre, y = temp, group = city, color = city)) +
  geom_line() +
  geom_point(size = 1.1) + 
  ggtitle("Monthly Average Temperature") +
  theme_hc() +
  scale_colour_hc()

p <- ggplot(a, aes(x = Genre, y = GLobal_Sales, colour = factor(YearBin))) +
  geom_point(position = "jitter", size = 0.5, alpha = 0.5)

ggplotly(p)


yearbin1 <- ggplot(YearBin2010_2016, aes(x = Genre, y = GLobal_Sales, colour = factor(Genre))) +
  geom_point(position = "jitter", size = 0.5, alpha = 0.5)

ggplotly(yearbin1)

yearbin2 <- ggplot(a) +
  geom_bar(aes(x = Genre, y =sum(GLobal_Sales), fill = sum(GLobal_Sales), col = YearBin), position = "dodge")
ggplotly(yearbin2)

####formal version
####without yearbin
justyear <- select(games,
            Year,
            Genre,
            GLobal_Sales)
View(justyear)
formal1 <- ggplot(justyear, aes(x = Year, y = GLobal_Sales, group = Genre, color = factor(Genre))) +
  geom_line() +
  coord_cartesian(ylim = c(0, 45)) +
  #geom_point(position = "jitter", size = 0.5, alpha = 0.5) + 
  #ggtitle("Monthly Average Temperature") +
  theme_hc() +
  scale_colour_hc()
ggplotly(formal1)

formal3 <- ggplot(a, aes(x = Genre, y = GLobal_Sales, color = factor(Genre))) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  geom_line() +
  coord_cartesian(ylim = c(0, 45)) +
  ggtitle("XXX") +
  theme_hc() +
  scale_colour_hc() +
  facet_wrap(YearBin~.)
ggplotly(formal3)

formal2 <- ggplot(justyear) +
  geom_histogram(aes(x=Genre, fill = Year), position = "dodge", stat = "count")
formal2

View(games)
games %>% select(Year,GLobal_Sales,Genre)%>%group_by(Year,Genre)%>%
  summarise(Total_sales=sum(GLobal_Sales))%>%ggplot(aes(x=Year,y=Total_sales,group=Genre,fill=Genre))+
  geom_area()+theme(legend.position="none",axis.text.x=element_text(angle=90),panel.background = element_rect(fill="black"),
                    panel.grid.major = element_blank(),
                    panel.grid.minor=element_blank())+labs(title="Year wise Total sales")


library(plotly)
library(plotly)
packageVersion('plotly')

library(plotly)



p <- ggplot(games, aes(Year, GLobal_Sales, color = Genre)) +
  geom_point(aes(size = GLobal_Sales, frame = Year, ids = Genre)) +
  coord_cartesian(ylim = c(0, 45)) +
  scale_x_log10()

p <- ggplotly(p)
p

p <- ggplot(games, aes(Year, )) +
  geom_point(aes(frame = f))

p <- ggplotly(p)

###animation
ok <- games %>% select(Year,GLobal_Sales,Genre)%>%group_by(Year,Genre)%>%
  summarise(Total_sales=sum(GLobal_Sales)) 
ok
ok1 <- arrange(ok, desc(Year))
View(ok1)
okk <- ggplot(ok1, aes(Year, Total_sales, col = factor(Genre))) +
  geom_point(aes(size = Total_sales, frame = Year, ids = Genre)) +
  coord_cartesian(ylim = c(0, 45)) + 
  scale_x_log10()

ggplotly(okk)
###animation####

okkk <- ggplot(ok, aes(Year, Total_sales, col = Genre)) +
  geom_point(aes(frame = Year, ids = Genre)) +
  coord_cartesian(ylim = c(0, 85)) +
  scale_x_log10()
ggplotly(okkk)

#3D chart #Q1
plot_ly(ok1, x = ~Year, y = ~Total_sales, z = ~Genre) %>%
  add_markers(color = ~Genre, size = 0.5)
#3D chart

Year1985_1989 <- filter(games, 
                        Year == c("1985","1986","1987","1988","1989"))
Year2012 <- filter(games, 
                        Year == c("2012"))

Y2012 <- Year2012 %>% select(GLobal_Sales,Genre)%>%group_by(Genre)%>%
  summarise(Total_sales=sum(GLobal_Sales))
View(games)

#animation #q1
ok11 <- ggplot(games, aes(Year, GLobal_Sales, color = Genre)) +
  geom_point(aes(frame = Year, size = GLobal_Sales),position = "jitter", alpha = 0.5) +
  geom_line() +
  scale_x_log10()
ggplotly(ok11)

ok11 <- ggplot(games, aes(Year, GLobal_Sales, color = Genre)) +
  geom_point(aes(frame = Year, size = GLobal_Sales),position = "jitter", alpha = 0.5) +
  scale_x_log10()
ggplotly(ok11)
unique(games$Genre)

ok11 <- ggplot(games, aes(x=Genre, y=GLobal_Sales), color = Genre) +
  geom_point(aes(frame = Year), position = "jitter", alpha = 0.5) +
  geom_line()
  scale_x_log10()
ggplotly(ok11)

class(games$Genre)

ok111 <- ggplot(
  games, 
  aes(x = Year, y=GLobal_Sales, col = Genre)
) +
  geom_point(position = "jitter", show.legend = FALSE, alpha = 0.7) +
  scale_color_viridis_d() +
  scale_x_log10() 
ok111
ok111 + transition_time(Year) +
  labs(title = "Year: {frame_time}")
View(games)
####Q2
games %>% select(Year, Publisher, Platform, Genre, Name, GLobal_Sales)%>%group_by(Year,Publisher, Platform, Genre, Name)%>%
  summarise(Max_Global_sales=max(GLobal_Sales)) %>% arrange(desc(Year),desc(Max_Global_sales))
