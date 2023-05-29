setwd("C:/CYS/의생명통계학/기말과제_청년고용조사")
library(readxl)
#데이터 불러오기 
df2<-read_excel("ypdata_w10.xlsx",col_types = 'guess',guess_max = 10000) #2016
df3<-read_excel("ypdata_w11.xlsx",col_types = 'guess',guess_max = 10000)
df4<-read_excel("ypdata_w12.xlsx",col_types = 'guess',guess_max = 10000)
df5<-read_excel("ypdata_w13.xlsx",col_types = 'guess',guess_max = 10000)
df6<-read_excel("ypdata_w14.xlsx",col_types = 'guess',guess_max = 10000)

#출산관련 변수 선택하기 
select_var<-function(df,num,year){
  var_1<-c("g500","g506", "g115")#자녀유무, 6세 미만 자녀 수, 배우자 월평균 소득득
  var_2<-paste0("g",871:920) #자녀 돌보는 사람 관련 변수
  var<-paste0(num,c(var_1,var_2))
  new_df<-df[,c("sampid",var)]
  new_df$year<-year
  #변수명 통일
  colnames(new_df)<-c("sampid",var_1,var_2,"year")
  return (new_df)
}
new_df2<-select_var(df2,"y10",2016)
new_df3<-select_var(df3,"y11",2017)
new_df4<-select_var(df4,"y12",2018)
new_df5<-select_var(df5,"y13",2019)
new_df6<-select_var(df6,"y14",2020)

final_df_born<-rbind(new_df2,new_df3,new_df4,new_df5,new_df6)
write.csv(final_df_born,"final_df_born.csv")
#1. 자녀유무. 1: 예, 2: 아니오. 나머지: 응답거절
library(dplyr)
f_df1<-final_df_born%>%filter(g500 %in% c(1,2))
table(f_df1$g500) #34707명이 자녀유무를 답함. 나머지는 삭제해야.

#2. 배우자 월평균 소득. 1: 100만원 미만..설문지와 동일하게 코딩됨. 
f_df2<-f_df1%>%filter(g115 %in% 1:9)
table(f_df2$g115)

#자녀 있는 사람 대상.. 
#3.  6세 미만 자녀 수. 이 문항부터는 자녀가 있는 사람만 한 거라서 다른 데이터셋으로 만듬
df_child_yes<-f_df2%>%filter(g500==1)
table(df_child_yes$g506) #0에서 3까지. 
f_df3<-df_child_yes%>%filter(g506 %in% c(0,1,2,3))
#4. 자녀 돌보는 사람1) 서비스 이용 시간 외 돌봄자. 
#아이를 누가 돌보는지, 를 나타내는 변수 생성.(몇 째 아이인지 상관 없이)
#본인 또는 배우자가 한 명이라도 돌볼 경우 0, 그렇지 않으면 1. 
child_care<-f_df3%>%
  filter_at(vars(c(g871,g876,g881,g886,g891)),any_vars(. %in% 1))

f_df4<-f_df3%>%
  mutate(child_care=ifelse(sampid %in% child_care$sampid,1,0))

#5. 자녀 돌보는 사람2) 서비스 이용 않는 가구 대상/돌봄자
child_care2<-f_df4%>%
  filter_at(vars(c(g896,g901,g906,g911,g916)),any_vars(. %in% 1))

f_df5<-f_df4%>%
  mutate(child_care2=ifelse(sampid %in% child_care2$sampid,1,0))

#6. 최종 변수 선택
#결혼한 사람. 
f_df6<-f_df2%>%select(c(g500,g115,year))
sum(is.na(f_df6)) #결측치 없음. 
colnames(f_df6)<-c("child_yes","spouse_money","year")

#자녀가 있는 사람
f_df6_child<-f_df5%>%select(c(g500,g506,g115,child_care,child_care2,year))
f_df6_child[is.na(f_df6_child)]<-0 #NA를 0으로 바꿈. 
colnames(f_df6_child)<-c("child_yes","child_under_6","spouse_money","child_care","child_care2","year")

#시간 가변 공변량 만들기-혹시 몰라서 배우자 돈만 예시로 한번 해봄. /결혼한 사람 데이터로
