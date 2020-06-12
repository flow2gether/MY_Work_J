install.packages('RDCOMClient', repos = 'http://www.omegahat.net/R/')


# 일자별 게재실적 공유
# 라이브러리 활용

library (RDCOMClient)


Path <-'C:/Users/N170328001/Desktop/My Work/★ R Analysis/R/2020/'
Body <- "<p> 목표 대비 실적 (백만원): 광고주별 매출내역 첨부</p>"



My_email<-function(x){
  Title <-paste0("TEST: ",Sys.Date(),"_",x,"_Sales Dashboard")
  AttachedFile<-paste0(Sys.Date(),"_",x,".csv")  
  PAPER_OFF_ON %>% 
    filter(lubridate::year(POST_DAY)==2020,SALESMAN==x) %>% 
    write.csv(AttachedFile)
  
  P1<-
    PA %>% 
    filter(MONTH<=6,SALESMAN== x) %>%
    ggplot(aes(x=MONTH,y=SALES,fill=TYPE))+
    geom_col(width=0.8,position = position_dodge2(0.9,reverse=T))+ 
    geom_text(aes(label=comma_format()(round(SALES,-1))),size=4,vjust=-0.2,position = position_dodge2(0.9,reverse=T)) +
    scale_fill_manual(values = c("#FAAB18","#1380A1")) + labs(title=x ,subtitle= "월별")+
    theme(legend.position = "none")
  
  P2<-
    PA %>% 
    filter(MONTH<=5,SALESMAN== x) %>% 
    group_by(SALESMAN,TYPE) %>% summarise(SALES=sum(SALES)) %>%
    ggplot(aes(x=SALESMAN,y=SALES,fill=TYPE))+
    geom_col(width=0.5,position = position_dodge2(0.9,reverse=T))+ 
    geom_text(aes(label=comma_format()(round(SALES))),size=4,vjust=-0.2,width=-0.2,position = position_dodge2(width = 0.5,reverse=T)) + guides(fill = guide_legend(reverse = TRUE))+
    scale_fill_manual(values = c("#FAAB18","#1380A1")) + labs(subtitle='누계(5월)')  
  
  
  library('patchwork') 
  SimplePlot<-P1+P2  
  
  # Create a temporary file path for the image that we will attach to our email
  # 차트를 붙여서 jpeg 파일로 만들고 임시파일에 저장하기
  SimplePlot.file <- tempfile(fileext = ".jpeg")
  # Save the ggplot we just created as an image with the temporary file path
  ggsave(plot = SimplePlot, file = SimplePlot.file,
         device = "jpeg", width = 12, height = 8)
  
  # 아웃룩 컨트롤
  OutApp <- COMCreate("Outlook.Application")
  outMail = OutApp$CreateItem(0)
  outMail[["To"]] = "형철희"
  outMail[["Cc"]] = "신재만;박중선"
  outMail[["subject"]] = Title
  outMail[["attachments"]]$Add(SimplePlot.file)
  
  # Refer to the attachment with a cid
  # "basename" returns the file name without the directory.
  
  SimplePlot.inline <- paste0( "<img src='cid:",
                               basename(SimplePlot.file),
                               "' width = '1200' height = '800'>")
  
  outMail[["HTMLbody"]] = paste0(Body, SimplePlot.inline)
  outMail[["attachments"]]$Add(paste0(Path,AttachedFile))
  outMail$Send()
  
  unlink(SimplePlot.file)
}


# 이메일 보내기

SALESMAN<-c("이주형","양용열","양승균")
map(SALESMAN, ~ My_email(.x))

  SALESMAN<-c("이주형","양용열","양승균","김현재","최명기","김영택","구명서","서주환","김영득","이경문","박영민","엄태규","최유리","진정훈")

# Refenrence-------------------------------------------------------------------------------------------#
# Create a temporary file path for the image that we will attach to our email
SimplePlot.file <- tempfile(fileext = ".jpeg")
# Save the ggplot we just created as an image with the temporary file path
ggsave(plot = SimplePlot, file = SimplePlot.file,
       device = "jpeg", width = 6, height = 4)

OutApp <- COMCreate("Outlook.Application")
outMail = OutApp$CreateItem(0)
outMail[["To"]] = "형철희"
outMail[["Cc"]] = "신재만;박중선"
outMail[["subject"]] = Title
outMail[["body"]] = BOE 
outMail[["attachments"]]$Add(SimplePlot.file)

# Refer to the attachment with a cid
# "basename" returns the file name without the directory.

SimplePlot.inline <- paste0( "<img src='cid:",
                             basename(SimplePlot.file),
                             "' width = '600' height = '400'>")

outMail[["HTMLbody"]] = paste0(Body, SimplePlot.inline)
outMail$Send()

unlink(SimplePlot.file)

