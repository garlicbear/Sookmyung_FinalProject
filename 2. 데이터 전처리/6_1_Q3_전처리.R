setwd("C:/CYS/의생명통계학/기말과제_청년고용조사")

library(readxl)
#0. 데이터 불러오기 
#df1<-read_excel("ypdata_w09.xlsx",col_types = 'guess',guess_max = 10000) #2016
df2<-read_excel("ypdata_w10.xlsx",col_types = 'guess',guess_max = 10000) #2016
df3<-read_excel("ypdata_w11.xlsx",col_types = 'guess',guess_max = 10000)
df4<-read_excel("ypdata_w12.xlsx",col_types = 'guess',guess_max = 10000)
df5<-read_excel("ypdata_w13.xlsx",col_types = 'guess',guess_max = 10000)
df6<-read_excel("ypdata_w14.xlsx",col_types = 'guess',guess_max = 10000)

#1. 필요한 변수만 select
##출생연도, 성별
##최종학력(w**edu),취업 유무(w**type)
##배우자 월평균 소득(y**g115), 아이주양육자(0: 서비스, 1: 본인 혹은 배우자, 2: 그 외)(), 산업군 대분류(y**b156z), 고용형태(y**b308)
##표본 선택 시 사용할 것: 혼인여부(y**g101), 6세 이하 아이 유무(y**g500)
select_var<-function(df,num,year){
  var_0<-c("yob","gender","sampid") #변수명 변경 없이 그대로 사용할 것.
  var_1<-c("edu","type")#최종학력. w**edu 취업여부.w**type 
  var_2<-c("g101","g500","g115","b156z","b308")
  var_3<-paste0("g",seq(871,920))
  var_4<-paste0("g",seq(806,810))
  #혼인여부, 아이 유무, 배우자 소득, 산업군, 고용형태
  #아이 양육자 관련 변수 
  #돌봄 서비스 이용
  w_var<-paste0("w",num,var_1)
  y_var<-paste0("y",num,c(var_2,var_3,var_4))
  new_df<-df[,c(var_0,w_var,y_var)]
  new_df$year<-year
  #변수명 통일
  colnames(new_df)<-c(var_0,var_1,var_2,var_3,var_4,"year")
  return (new_df)
}

#new_df1<-select_var(df1,"09",2015)
new_df2<-select_var(df2,10,2016)
new_df3<-select_var(df3,11,2017)
new_df4<-select_var(df4,12,2018)
new_df5<-select_var(df5,13,2019)
new_df6<-select_var(df6,14,2020)

total_df<-rbind(new_df2,new_df3,new_df4,new_df5,new_df6)
write.csv(total_df,"total_df.csv",row.names=FALSE)
total_df<-read.csv("total_df.csv")

#2. 유효한 값만 남기기.
##출생연도
total_df1<-total_df
table(total_df1$yob) #결측치만 삭제
sum(is.na(total_df1$yob)) #결측치 없음
##성별 
table(total_df1$gender) #결측치만 삭제
sum(is.na(total_df1$gender)) 
##최종학력 완료. 
##배우자 월평균 소득, 고용형태
#table(total_df1$b308) #고용형태
#sum(is.na(total_df1$b308)) #결측치 삭제 필요
total_df2<-total_df1%>%filter(g115 %in% 1:9)#배우자 월평균 소득 결측치 처리 완료
##산업군, 고용형태
total_df2_1<-total_df2%>%
  filter(((type==5)&(b156z !="99"))|(type!=5))%>%
  filter(((type==5)&(b308%in% c(1,2)))|(type!=5))
###결측치에는 -1 적기
total_df2_1[is.na(total_df2_1$b156z),]$b156z<--1
total_df2_1[is.na(total_df2_1$b308),]$b308<--1

table(total_df2_1$type)
#3. 아이 양육자 변수 생성
##3-1. 돌봄 서비스 이용자
##우선, 돌봄서비스랑 양육자에 대해 아무 답을 하지 않은 변수 제거. 
var<-paste0("g",seq(806,810))
var_me<-paste0("g",seq(871,920))
total_df3<-total_df2_1%>%
  filter_at(vars(var),any_vars(. %in% c(1,2)))%>%
  filter_at(vars(var_me),any_vars(. ==1))
total_df3[var_me] #제대로 제거된 것 확인. 

child_care_service<-total_df3%>%
  filter_at(vars(var),any_vars(. ==1))  #서비스 이용자

var_me1<-paste0("g",seq(871,920,5))
child_care_me<-total_df3%>%
  filter_at(vars(var_me1),any_vars(. ==1)) #본인 혹은 배우자가 사용

total_df3_2<-total_df3%>%
  mutate(child_care=ifelse(sampid %in% child_care_service$sampid,0,
                           ifelse(sampid %in% child_care_me,1,2)))
#1. 2016년 기준 대졸, 취업, 기혼 유배우, 아이 있는 표본만 추출
sample_df <- new_df2%>%
  filter(type ==5)%>%
  filter(edu %in% c(4,5))%>% #대졸자, 석사
  filter(g101==2)%>% #유배우
  filter(g500==1)#아이 있음.
total_df__<-total_df3_2%>%filter(sampid %in% sample_df$sampid)
table(total_df__%>%filter((year==2016))%>%select(type))

#4. 최종 데이터셋 저장
final_df_0602<-total_df__%>%select(c("yob","gender","sampid","edu",
                                    "g115","child_care","b156z","b308","type","year"))
table(final_df_0602%>%filter((year==2016))%>%select(type))

table(final_df_0602$type) #다행이다...있다..
##변수명 바꾸기. 
colnames(final_df_0602)<-c("출생연도","성별","id","최종학력",
                           "배우자월평균소득","아이양육자","직업군대분류","고용형태","취업유무","조사연도")
write.csv(final_df_0602,"final_df_0602.csv",row.names=FALSE)

#b<-total_df1%>%filter( %in% c(1,2,3,4,5))%>%
#  filter_at(vars(c(y10b225,y10b220,y10b221,y10b222,y10b223)),all_vars(. <9090908))




#var<-paste0("g",seq(871,916,5))
#child_care<-total_df1%>%
#  filter_at(vars(var),any_vars(. %in% 1)) #2487개

#total_df2<-total_df1%>%
#  mutate(child_care=ifelse(sampid %in% child_care$sampid,1,0))
