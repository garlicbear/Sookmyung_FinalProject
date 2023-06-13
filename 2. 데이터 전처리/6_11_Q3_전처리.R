setwd("C:/CYS/의생명통계학/기말과제_청년고용조사")

library(readxl)
library(dplyr)
#0. 데이터 불러오기 
df2<-read_excel("ypdata_w10.xlsx",col_types = 'guess',guess_max = 10000) #2016
df3<-read_excel("ypdata_w11.xlsx",col_types = 'guess',guess_max = 10000)
df4<-read_excel("ypdata_w12.xlsx",col_types = 'guess',guess_max = 10000)
df5<-read_excel("ypdata_w13.xlsx",col_types = 'guess',guess_max = 10000)
df6<-read_excel("ypdata_w14.xlsx",col_types = 'guess',guess_max = 10000)

#1. 필요한 변수만 select
select_var<-function(df,num,year){
  var_0<-c("sampid","yob","gender") #변수명 변경 없이 그대로 사용할 것.1),2),3)
  var_2<-c("g115","b156z","b308","g101","g506","g112")#4),6),7),혼인여부,6세 이하 아이 유무 *)
  var_1<-c("edu","type")#8),9)
  var_3<-paste0("g",seq(871,920)) #5)
  
  w_var<-paste0("w",num,var_1) #8), 9)
  y_var<-paste0("y",num,c(var_2,var_3)) #4), 5), 6), 7)
  new_df<-df[,c(var_0,w_var,y_var)] 
  new_df$year<-year
  #변수명 통일
  colnames(new_df)<-c(var_0,var_1,var_2,var_3,"year")
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
write.csv(new_df2,"new_df2.csv",row.names=FALSE)

total_df<-read.csv("total_df.csv")
new_df2<-read.csv("new_df2.csv")

#2. 유효한 값만 남기기.

##0) 표본 select: 2016년 기준 1) 대졸이상이며 학생이 아님(대학원생 아님) 2) 취업 3) 6세 미만 아이 있는 4) 기혼 유배우
sample_id_df<-new_df2%>%
  filter((edu %in% c(4,5))&
           (type %in% c(5,6))&
           (g506 %in% c(1,2,3,4,5))&
           (g101==2))
total_df0<-total_df%>%filter(sampid %in% sample_id_df$sampid)
##1) 식별키: 생략
##2) 출생연도
table(total_df0$yob) #결측치만 삭제
sum(is.na(total_df0$yob)) #결측치 없음

##3) 성별 
table(total_df0$gender) #결측치만 삭제
sum(is.na(total_df0$gender)) 
##성별을 0, 1로 바꾸기(남자:0, 여자:1)
total_df0$gender<-total_df0$gender-1
##4) 배우자 월평균 소득
##**추가) 배우자가 직업이 없으면 0.
total_df4<-total_df0%>%filter(((g112 %in% c(1,2,3,4))&(g115 %in% 1:9))|
                                g112 %in% c(5,6,7,8,9,10,97))#배우자 월평균 소득 결측치 처리 완료

total_df4["g115"][is.na(total_df4["g115"])]<-0  
total_df4<-total_df4%>%
  mutate(spouse_money_1=ifelse((g115<=6),1,0))
sum(is.na(total_df4$spouse_money)) 

##5) 부모 외에 아이를 돌보는 사람이 있는가. 
var_b1<-paste0("g",seq(871,920)) 
total_df5_1<-total_df4[var_b1]#양육 관련 변수만 추출
total_df5_2<-total_df4[rowSums(is.na(total_df5_1))!=ncol(total_df5_1),]#아예 답하지 않은 사람은 제외. 

var_b2<-paste0("g",seq(871,920,5)) 
var_baby<-setdiff(var_b1,var_b2) #부모 이외의 사람이 돌보는지 여부.
total_df5_2[var_baby][is.na(total_df5_2[var_baby])]<-0

#871,876,...916를 뺀 모든 것을 더하기. 
total_df5_2$baby_care<-rowSums(total_df5_2[var_baby])
table(total_df5_2$baby_care) #아무도 안 도와주는 사람이..이렇게나 많아..?흠..


##아이양육자 변수를 1또는 0으로 바꾸기. 
total_df5_2[total_df5_2$baby_care!=0,]$baby_care<-1
##6) 직업군 대분류, 7) 고용형태
total_df6<-total_df5_2%>%
  filter(((type==5)&(b156z !="99"))|(type!=5))%>%
  filter(((type==5)&(b308 %in% c(1,2)))|(type!=5))
sum(is.na(total_df6$type)) #취업자 유형 결측치 처리 완료. 
###결측치에는 -1 적기
total_df6[is.na(total_df6$b156z),]$b156z<--1
total_df6[is.na(total_df6$b308),]$b308<--1
###
total_df7<-total_df6
total_df7[total_df7$b156z>=2,"b156z"]<-2
table(total_df7$b156z)
#8) 최종학력
total_df8_1<-total_df7%>%filter(edu %in% c(4,5))
#최종학력: 석사이상이면 1, 아니면 0. 
total_df8_1$edu<-total_df8_1$edu-4
#9) 취업여부
total_df9_1<-total_df8_1%>%filter(type %in% c(3,4,5,6))
total_df9<-total_df9_1
total_df9[total_df9_1$type==5,]$type<-1 #취업함
total_df9[total_df9_1$type!=5,]$type<-0 #취업 안함




#id값을 factor로
total_df9$sampid<-as.factor(total_df9$sampid)
#id변수 정렬하기
total_df9<-total_df9[order(total_df9$sampid),]


#4. 최종 데이터셋 저장
final_df_0612<-total_df9%>%select(c("sampid","yob","gender","spouse_money_1","baby_care",
                                     "b156z","b308","edu","type","year","g115"))
table(final_df_0612%>%filter((year==2016))%>%select(type))

table(final_df_0612$type) #다행이다...있다..
##변수명 바꾸기. 
colnames(final_df_0612)<-c("id","출생연도","성별","배우자월평균소득_400미만","아이양육자",
                           "직업군대분류","고용형태","최종학력","취업유무","조사연도","g115")
write.csv(final_df_0612,"final_df_0613_1.csv",row.names=FALSE)

#b<-total_df1%>%filter( %in% c(1,2,3,4,5))%>%
#  filter_at(vars(c(y10b225,y10b220,y10b221,y10b222,y10b223)),all_vars(. <9090908))




#var<-paste0("g",seq(871,916,5))
#child_care<-total_df1%>%
#  filter_at(vars(var),any_vars(. %in% 1)) #2487개

#total_df2<-total_df1%>%
#  mutate(child_care=ifelse(sampid %in% child_care$sampid,1,0))
