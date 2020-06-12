#  Install packages
install.packages("rJava")
install.packages("DBI")
install.packages("RJDBC")

# LIBRARY LOAD
library(tidyverse)
library(lubridate)

# CONNECTING ORACLE DB 
library(rJava)
library(DBI)
library(RJDBC)
jdbcDriver<-JDBC(driverClass="oracle.jdbc.OracleDriver",classPath = "C://ORAWIN95/ODBC/ojdbc14.jar")
CONNECT<-dbConnect(jdbcDriver,"jdbc:oracle:thin:@//203.249.128.136:1521/AP1.WORLD","V_ACNAD","VACNAD")


# GETTING DATA from ORACLE 
QUERY_PAPER_OFF<-"SELECT  
                    gajaeday as POST_DAY, 
                    gajaeseq as POST_NO, pageno, dansu, length, adamt/1000000 as AMOUNT, 
                    a.gcode as AGENCY_CODE, gcode1 as ADVERTISER_CODE, rcode as REAL_ADVERTISER_CODE, 
                    adkind as AD_TYPE_CODE,
                    b.gname as AGENCY, b.sanum as AGENCY_LICENSE_CODE,
                    bb.gname as ADVERTISER, bb.sanum as ADVERTISER_LICENSE_CODE,
                    bbb.gname as REAL_ADVERTISER, bbb.sanum as REAL_ADVERTISER_LICENSE_CODE,
                    c.codnam as SALESMAN,
                    d.codnam4 as AD_TYPE,
                    a.content
                  FROM 
                    acnad.natb_ad_center_gajae A 
                    left join acnad.natb_gmst_giup B ON (a.gcode=b.gcode)
                    left join acnad.natb_gmst_giup BB ON (a.gcode1=bb.gcode)
                    left join acnad.natb_gmst_giup BBB ON (a.rcode=bbb.gcode)
                    left join acnad.natb_comcod_nsale_man C ON (a.nsalman=c.code)
                    left join acnad.natb_comcod_adver_kinds D ON (a.adkind=d.code)
        
                where
                    a.gajaeday >= '2002-01-01' AND a.gajaeday <= '2020-06-04'
                    and a.pageno <> '500'
                    and a.adamt <> '0'
        
                GROUP BY 
                    a.gajaeday, gajaeseq , pageno, dansu, length, adamt, a.gcode, gcode1, rcode, adkind,
                    b.gname, b.sanum, bb.sanum, bbb.sanum, bb.gname, bb.sanum, bbb.gname,
                    c.codnam, d.codnam4,a.content"


PAPER_DB<-as_tibble(dbGetQuery(CONNECT,QUERY_PAPER_OFF))

# PAPER_OFF (본지매출 가져오기)

PAPER_OFF<-PAPER_DB %>% 
  select(POST_DAY,ADVERTISER,REAL_ADVERTISER_CODE,AGENCY,REAL_ADVERTISER,AMOUNT,SALESMAN,POST_NO,AD_TYPE_CODE,AD_TYPE,CONTENT)

# 광고주 이름 전처리 

TEXT<-'\\(.{1,}\\)|\\（.{1,}\\)|\\（.{1,}\\）|-|\\(주）|주식회사|유한회사|학교법인|\\s'

PAPER_OFF$ADVERTISER <-str_replace_all(PAPER_OFF$ADVERTISER,TEXT,'')
PAPER_OFF$REAL_ADVERTISER <-str_replace_all(PAPER_OFF$REAL_ADVERTISER,TEXT,'')
PAPER_OFF$AGENCY <-str_replace_all(PAPER_OFF$AGENCY,TEXT,'')
PAPER_OFF$POST_DAY <-as.Date(PAPER_OFF$POST_DAY)
PAPER_OFF$POST_NO <-as.character(PAPER_OFF$POST_NO)


# PAPER_ON(온라인 기타매출 가져오기)

PAPER_ON<-as_tibble(read.csv('ON.csv', 
                             stringsAsFactor = FALSE))
PAPER_ON$POST_DAY<-as.Date(PAPER_ON$POST_DAY)
PAPER_ON$AD_TYPE_CODE<-as.character(PAPER_ON$AD_TYPE_CODE)
PAPER_OFF_ON <-full_join(PAPER_OFF,PAPER_ON)

#CHECK

PAPER_OFF_ON %>% filter(year(POST_DAY)==2020) %>% group_by(MONTH=month(POST_DAY)) %>% summarise(SUM=sum(AMOUNT))
PAPER_OFF_ON %>% filter(year(POST_DAY)==2020,month(POST_DAY)==6) %>% group_by(POST_DAY) %>% summarise(SUM=sum(AMOUNT))

#REPORTING

PAPER_OFF_ON_2020 %>% 
  group_by(MONTH=month(POST_DAY),CATEGORY) %>% 
  summarise(SUM=sum(AMOUNT)) %>%  
  pivot_wider(names_from='MONTH',values_from = 'SUM')

PAPER_OFF_ON_2020 %>% 
  group_by(MONTH=month(POST_DAY),CATEGORY,TEAM,SALESMAN) %>% 
  summarise(SUM=sum(AMOUNT)) %>%  
  pivot_wider(names_from='MONTH',values_from = 'SUM')

PAPER_OFF_ON %>% 
  filter(year(POST_DAY)==2020) %>%
  group_by(POST_DAY,ADVERTISER,AGENCY,REAL_ADVERTISER_CODE,REAL_ADVERTISER,POST_NO,SALESMAN) %>% 
  summarise(SUM=sum(AMOUNT)) %>% 
  write.csv("act_detailed.csv")



# BASE(PLAN & ACT)

PAPER_OFF_ON %>% 
  filter(year(POST_DAY)==2020) %>%
  group_by(MONTH=month(POST_DAY),ADVERTISER,AGENCY,REAL_ADVERTISER_CODE,REAL_ADVERTISER,SALESMAN,CONTENT) %>% 
  summarise(SUM=sum(AMOUNT)) %>% 
  pivot_wider(names_from='MONTH',values_from = 'SUM')%>%
  replace(.,is.na(.),0) %>% 
  write.csv("BASE.csv")
  


  
# PAPER_AD_TYPE, 협찬 마케팅 구분
PAPER_AD_TYPE<-as_tibble(read.csv('AD_TYPE_CODE.csv', stringsAsFactor = FALSE))
PAPER_AD_TYPE$AD_TYPE_CODE<-as.character(PAPER_AD_TYPE$AD_TYPE_CODE)

PAPER_OFF_ON_TYPE<-left_join(PAPER_OFF_ON,PAPER_AD_TYPE,by="AD_TYPE_CODE")

#현대기아차는 협찬으로 분류
PAPER_OFF_ON_TYPE$MKT<- ifelse(PAPER_OFF_ON_TYPE$REAL_ADVERTISER == "현대자동차","N-MKT",
                               ifelse(PAPER_OFF_ON_TYPE$REAL_ADVERTISER == "기아자동차","N-MKT",
                                      PAPER_OFF_ON_TYPE$MKT))

#협찬 마케팅 분류 리포팅
PAPER_OFF_ON_TYPE %>% filter(year(POST_DAY)>2018,month(POST_DAY)<=5) %>% 
  group_by(YEAR=year(POST_DAY),MKT,NEW_TYPE) %>% 
  summarise(SUM=sum(AMOUNT)) %>% 
  pivot_wider(names_from = YEAR,values_from = SUM) %>%
  replace(.,is.na(.),0) %>% 
  mutate(GAP=`2020`- `2019`) %>% 
  arrange(desc(GAP)) %>% write.csv("협찬마케팅_5월누계.csv")

#협찬 차이 상새

PAPER_OFF_ON_TYPE %>% filter(year(POST_DAY)>2018,month(POST_DAY)==3,MKT!='N-MKT') %>% 
  group_by(YEAR=year(POST_DAY),MKT,REAL_ADVERTISER) %>% 
  summarise(SUM=sum(AMOUNT)) %>% 
  pivot_wider(names_from = YEAR,values_from = SUM) %>%
  replace(.,is.na(.),0) %>% 
  mutate(GAP=`2020`- `2019`) %>% 
  arrange(desc(GAP))

#대기업 일반기업 구분
COMPANY_DISTINCT<-read.csv("COMPANY_Distinct.csv") %>% as_tibble
left_join(PAPER_OFF_ON,COMPANY_DISTINCT) ->PAPER_OFF_ON_DISTINCT

PAPER_OFF_ON_DISTINCT %>% filter(year(POST_DAY)>2015,구분=='B. 중견기업') %>%
  group_by(YEAR=year(POST_DAY),구분, REAL_ADVERTISER) %>% 
  summarise(SUM=sum(AMOUNT)/100) %>%
  pivot_wider(names_from = YEAR,values_from = SUM) %>%
  replace(.,is.na(.),0) %>%
  write.csv('2019_2.csv')

#트리맵
TREEMAP<-PAPER_OFF_ON_TYPE_GROUP %>% 
  filter(year(POST_DAY)==2018) %>% 
  group_by(MKT,BIG,GROUP) %>% 
  summarise(AMOUNT=sum(AMOUNT)) %>%
  mutate(NEWMKT=ifelse(MKT=="관공서","N-MKT",MKT))

library(treemap)
treemap(TREEMAP,index=c("NEWMKT","BIG","GROUP"),vSize="AMOUNT",vColor = "AMOUNT",type="value", palette=terrain.colors(100))
