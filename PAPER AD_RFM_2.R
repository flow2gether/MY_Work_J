library(tidyverse)
library(tidymodels)
library(lubridate)
library(skimr)
library(patchwork)


#Data Import
PAPER_OFF_RFM<-
  PAPER_OFF_ON%>% 
  filter(POST_DAY<=ymd('2020-04-30'), POST_DAY>=ymd('2019-05-01')) %>%
  group_by(REAL_ADVERTISER) %>% 
  summarise(RECENCY= as.numeric(as.Date('2020-04-30')-max(POST_DAY)),FREQUENCY=n(),MONETARY=sum(AMOUNT))

#EDL
PAPER_OFF_RFM %>% glimpse()
PAPER_OFF_RFM %>% skim()

PAPER_OFF_RFM %>% ggplot(aes(x=RECENCY,y=MONETARY))+
  geom_point()+
  geom_smooth(method='lm',se=F,color='orange')->p1
PAPER_OFF_RFM %>% ggplot(aes(x=RECENCY,y=FREQUENCY))+
  geom_point()+
  geom_smooth(method='lm',se=F,color='orange')->p2
PAPER_OFF_RFM %>% ggplot(aes(x=FREQUENCY,y=MONETARY))+
  geom_jitter()+
  geom_smooth(method='lm',se=F,color='orange')->p3

p1+p2+p3

#kmeans
PAPER_POINTS<-PAPER_OFF_RFM %>%
  select(-REAL_ADVERTISER)

PAPER_KMEANS<-
  tibble(k=1:9) %>%
  mutate(
    kclust=map(k,~kmeans(PAPER_POINTS,center=.x)),
    tidied=map(kclust, tidy),
    glanced = map(kclust, glance),
    augmented = map(kclust, augment, PAPER_POINTS)
  )

clusters<-
  PAPER_KMEANS %>%
  unnest(cols=c(tidied)) %>%
  rename(RECENCY=x1, FREQUENCY=x2, MONETARY=x3)

assignments<-
  PAPER_KMEANS %>%
  unnest(cols=c(augmented))

clusterings<- 
  PAPER_KMEANS %>%
  unnest(cols=c(glanced))

#chart
p4<-ggplot(assignments,aes(x=RECENCY,y=MONETARY,size=FREQUENCY))+
  geom_jitter(aes(color=.cluster),alpha=0.8)+
  facet_wrap(~k) 

p5<-p4+geom_point(data=clusters ,size = 6, shape = "*",color='red')


ggplot(clusterings, aes(k, tot.withinss)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = 4,color='red',linetype='dashed') ->p6


#chart1
assignments %>% filter(k==4) %>% 
  bind_cols(.,PAPER_OFF_RFM[1]) %>% 
  select(RECENCY:REAL_ADVERTISER) %>%
  ggplot(aes(x=RECENCY,y=MONETARY,size=FREQUENCY,color=.cluster))+
  geom_jitter()+
  geom_point(data=clusters %>% filter(k==4) ,size = 8, shape = "*",color='black') ->p7
  
#chart2
assignments %>% filter(k==4) %>% 
  bind_cols(.,PAPER_OFF_RFM[1]) %>% 
  select(RECENCY:REAL_ADVERTISER) %>%
  ggplot(aes(x=RECENCY,y=MONETARY,size=FREQUENCY,color=.cluster))+
  geom_jitter() + 
  facet_wrap(~.cluster)+ theme(legend.position='none')->p8
  
#chart3
assignments %>% filter(k==4) %>% 
  bind_cols(.,PAPER_OFF_RFM[1]) %>% 
  select(RECENCY:REAL_ADVERTISER) %>%
  filter(.cluster==1) %>%
  ggplot(aes(x=RECENCY,y=MONETARY,size=FREQUENCY,color=REAL_ADVERTISER,label=REAL_ADVERTISER))+
  geom_text(vjust = -1) +guides(color=FALSE)+geom_jitter() +
  scale_x_continuous(limits = c(-5,20))+
  scale_y_continuous(limits = c(0,10000))

assignments %>% filter(k==4) %>% 
  bind_cols(.,PAPER_OFF_RFM[1]) %>% 
  select(RECENCY:REAL_ADVERTISER) %>%
  filter(.cluster==2) %>%
  ggplot(aes(x=RECENCY,y=MONETARY,size=FREQUENCY,color=REAL_ADVERTISER,label=REAL_ADVERTISER))+
  geom_text(vjust = -1) +guides(color=FALSE)+geom_jitter() +
  scale_x_continuous(limits = c(-10,180))+
  scale_y_continuous(limits = c(0,1500))

assignments %>% filter(k==4) %>% 
  bind_cols(.,PAPER_OFF_RFM[1]) %>% 
  select(RECENCY:REAL_ADVERTISER) %>%
  filter(.cluster==3) %>%
  ggplot(aes(x=RECENCY,y=MONETARY,size=FREQUENCY,color=REAL_ADVERTISER,label=REAL_ADVERTISER))+
  geom_text(vjust = -1) +guides(color=FALSE)+geom_jitter() +
  scale_x_continuous(limits = c(60,220))+
  scale_y_continuous(limits = c(0,200))

assignments %>% filter(k==4) %>% 
  bind_cols(.,PAPER_OFF_RFM[1]) %>% 
  select(RECENCY:REAL_ADVERTISER) %>%
  filter(.cluster== 4) %>%
  ggplot(aes(x=RECENCY,y=MONETARY,size=FREQUENCY,color=REAL_ADVERTISER,label=REAL_ADVERTISER))+
  geom_text(vjust = -1) +guides(color=FALSE)+geom_jitter() +
  scale_y_continuous(limits = c(0,300))

clusters %>%
  ggplot(aes(x=RECENCY,y=MONETARY,size=size))+
  geom_jitter()

