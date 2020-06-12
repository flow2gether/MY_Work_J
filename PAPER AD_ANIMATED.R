library(tidyverse)
library(gganimate)
library(gapminder)
library(lubridate)

theme_set(theme_classic())

PAPER_OFF_ANI<-PAPER_OFF_ON %>%
  filter(year(POST_DAY)<2020) %>% 
  group_by(YEAR=year(POST_DAY),REAL_ADVERTISER) %>% 
  summarise(SUM=sum(AMOUNT)/100)

gap <- PAPER_OFF_ANI %>%
  group_by(YEAR) %>%
  # The * 1 makes it possible to have non-integer ranks while sliding
  mutate(rank = min_rank(-SUM) * 1,
         Value_rel = SUM/SUM[rank==1],
         Value_lbl = paste0(" ",round(SUM,1))) %>%
  filter(rank <=30) %>%
  ungroup()

p <- ggplot(gap, aes(rank, group = REAL_ADVERTISER, 
                     fill = as.factor(REAL_ADVERTISER), color = as.factor(REAL_ADVERTISER))) +
  geom_tile(aes(y = SUM/2,
                height = SUM,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(REAL_ADVERTISER, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y=SUM,label = Value_lbl, hjust=0)) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  
  labs(title='{closest_state}', x = "", y = "TOP30 광고주 매출(억)",
       caption = "Sources: MIS DATA | 광고지원팀") +
  theme(plot.title = element_text(hjust = 0, size = 22),
        axis.ticks.y = element_blank(),  # These relate to the axes post-flip
        axis.text.y  = element_blank(), # These relate to the axes post-flip
        plot.margin =  ggplot2::margin(1, 1, 1, 5, "cm"))  +
  
  transition_states(YEAR, transition_length = 4, state_length = 1) +
  ease_aes('cubic-in-out')

animate(p, 200, fps = 10, duration = 40, width = 800, height = 600, renderer = gifski_renderer("gganim.gif"))

animate(p, 200, fps = 10, duration = 40, width = 800, height = 600, renderer = av_renderer("gganim.mp4"))


#읿자별 DATA
library(gganimate)
set_theme(theme_bw())

PAPER_OFF_ON %>% 
  filter(year(POST_DAY)>2018,month(POST_DAY)==3) %>%  
  group_by(YEAR=year(POST_DAY),MONTH=month(POST_DAY),DAY=day(POST_DAY))   %>% 
  summarise(SUM=sum(AMOUNT)) %>% mutate(CUM=round(cumsum(SUM/100),0)) %>%
  ggplot(aes(x=DAY,y=CUM,color=factor(YEAR)))+labs(title='AD SALES ({frame_along} DAY)',x="날짜",y="매출",color="YEAR")+
  geom_line()+ scale_color_brewer(palette = "Set2") +
  geom_segment(aes(xend=max(DAY), yend=CUM), linetype=3)+ scale_x_continuous(breaks=seq(0,31,7))+
  geom_text(aes(x=max(DAY)+3,label=comma(CUM)))+geom_point()+transition_reveal(DAY) +facet_wrap(~MONTH,nrow=1)->P2



animate(P2, 50, fps = 5, duration = 40, width = 1200, height = 800, renderer = gifski_renderer("gganim.gif"))
animate(P2, 50, fps = 5, duration = 40, width = 1200, height = 800, renderer = av_renderer("gganim.mp4"))