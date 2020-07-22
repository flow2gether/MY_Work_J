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
CONNECT<-dbConnect(jdbcDriver,"jdbc:oracle:thin:@//000.000.000.000:0000/000","id","password")


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
        
                WHERE
                    a.gajaeday >= '2002-01-01' AND a.gajaeday <= '2020-06-30'
                    and a.pageno <> '500'
                    and a.adamt <> '0'
        
                GROUP BY 
                    a.gajaeday, gajaeseq , pageno, dansu, length, adamt, a.gcode, gcode1, rcode, adkind,
                    b.gname, b.sanum, bb.sanum, bbb.sanum, bb.gname, bb.sanum, bbb.gname,
                    c.codnam, d.codnam4,a.content"


PAPER_DB<-as_tibble(dbGetQuery(CONNECT,QUERY_PAPER_OFF))
TEXT<-'\\([^()]*\\)|[:punct:]|[:space:]|주식회사|유한회사|학교법인|X'


# PAPER_OFF (본지매출 가져오기) 

PAPER_OFF<-
  PAPER_DB %>% 
  select(
    POST_DAY,ADVERTISER,REAL_ADVERTISER_CODE,AGENCY,REAL_ADVERTISER,
    AMOUNT,SALESMAN,POST_NO,AD_TYPE_CODE,AD_TYPE,CONTENT
    )  %>% 
  mutate_at(vars(c("ADVERTISER","REAL_ADVERTISER","AGENCY")),~str_replace_all(.,'（','(')) %>% 
  mutate_at(vars(c("ADVERTISER","REAL_ADVERTISER","AGENCY")),~str_replace_all(.,'）',')')) %>%
  mutate_at(vars(c("ADVERTISER","REAL_ADVERTISER","AGENCY")),~str_replace_all(.,'）',')')) %>%
  mutate_at(vars(c("ADVERTISER","REAL_ADVERTISER","AGENCY")),~str_replace_all(.,TEXT,"")) %>% 
  mutate_at(vars(c("POST_DAY")),as.Date) %>% 
  mutate_at(vars(c("POST_NO")),as.character) %>% 
  mutate_at(vars(c("ADVERTISER","REAL_ADVERTISER","AGENCY")),~replace_na(.,"unknown"))
                          
              
# PAPER_ON(온라인매출 합치기)

PAPER_OFF_ON<-as_tibble(read.csv('ON.csv',stringsAsFactor = FALSE)) %>% 
  mutate_at(vars(c("POST_DAY")),as.Date) %>% 
  mutate_at(vars(c("AD_TYPE_CODE")),as.character) %>% 
  full_join(PAPER_OFF)

#CHECK

PAPER_OFF_ON %>% 
  filter(year(POST_DAY)==2020) %>%
  group_by(MONTH=month(POST_DAY)) %>% 
  summarise(SUM=sum(AMOUNT))


#REPORTING 

PAPER_OFF_ON %>% 
  filter(year(POST_DAY)==2020) %>%
  group_by(POST_DAY,ADVERTISER,AGENCY,REAL_ADVERTISER_CODE,REAL_ADVERTISER,POST_NO,SALESMAN) %>% 
  summarise(SUM=sum(AMOUNT)) %>% 
  write.csv("act_detailed.csv")


# TYPE
PAPER_OFF_ON_TYPE <-
  as_tibble(read.csv('AD_TYPE_CODE.csv', stringsAsFactor = FALSE)) %>% 
  mutate_at(vars("AD_TYPE_CODE"),as.character) %>% 
  left_join(PAPER_OFF_ON,PAPER_AD_TYPE,by="AD_TYPE_CODE") %>% 
  mutate_at(vars("MKT"), 
            ~ ifelse(REAL_ADVERTISER == "현대자동차"|REAL_ADVERTISER == "기아자동차",
                     "N-MKT",
                     MKT)
            )

MKT_FACTOR<- factor(c("부동산/임시물","유통","교육","패션/뷰티/스포츠","기타","레저","자동차"))

PAPER_OFF_ON_TYPE %>% 
  filter(year(POST_DAY)>2018,month(POST_DAY)<=6) %>% 
  group_by(YEAR=year(POST_DAY),MKT,NEW_TYPE)%>% 
  summarise(SUM=sum(AMOUNT)) %>% 
  pivot_wider(names_from = YEAR,values_from = SUM, values_fill = list(SUM=0)) %>%
  mutate(GAP=`2020`- `2019`)

PAPER_OFF_ON_TYPE %>%
  filter(year(POST_DAY)>2011,month(POST_DAY)<=6,MKT=="MKT") %>% 
  group_by(YEAR=year(POST_DAY),MKT,NEW_TYPE) %>% 
  summarise(SUM=sum(AMOUNT)) %>% 
  ggplot(aes(x=YEAR,y=SUM,fill=factor(NEW_TYPE,levels=MKT_FACTOR)))+  
  labs(fill="NEW_TYPE")+  
  scale_x_continuous(breaks=seq(2011,2020,1)) +
  geom_col(position=position_stack(reverse = TRUE)) +
  geom_text(
    aes(label=round(SUM/100)),size=3,color="white",position=position_stack(vjust = 0.5,reverse = T))+
  ylab("매출")

