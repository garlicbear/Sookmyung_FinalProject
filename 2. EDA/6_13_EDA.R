setwd("C:/CYS/의생명통계학/기말과제_청년고용조사")
final_df<-read.csv("final_df_0612_2.csv")
final_df[1:10,]

df_2016<-final_df[final_df$조사연도==2016,]

#2016년에 대해 ..
#0. 바 그래프 그리기
library(ggplot2)
#0) 조사연도
bar_year<-ggplot(data=final_df,aes(x=조사연도))+
  geom_bar(color="black", fill="gray")+
  labs(title="조사연도 분포")+
  theme_classic()+
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "white")
bar_year
#1) 성별
final_df$성별<-as.factor(final_df$성별)

bar_sex2<-ggplot(data=final_df,aes(x=조사연도,group=성별,fill=성별))+
  geom_bar(position = 'dodge' )  +
  labs(title="조사연도에 따른 성별 분포")+
  scale_fill_discrete(labels=c("남성","여성"))+
  theme_classic()+
  geom_text(aes(label = after_stat(count),group=성별), 
            stat = "count", 
            position=position_dodge(width=0.9),
            colour = "black",
            vjust=-0.5)
bar_sex2
#2) 출생연도
final_df$출생연도<-as.factor(final_df$출생연도)

bar_sex2<-ggplot(data=final_df,aes(x=조사연도,group=출생연도,fill=출생연도))+
  geom_bar(position = 'dodge' )  +
  labs(title="조사연도에 따른 출생연도 분포")+
  theme_classic()
bar_sex2

#####
#0) 취업 유무
final_df$취업유무<-as.factor(final_df$취업유무)

bar_sex2<-ggplot(data=final_df,aes(x=조사연도,group=취업유무,fill=취업유무))+
  geom_bar(position = 'dodge' )  +
  labs(title="조사연도에 따른 취업유무 분포")+
  scale_fill_discrete(labels=c("미취업","취업"))+
  theme_classic()+
  geom_text(aes(label = after_stat(count),group=취업유무), 
            stat = "count", 
            position=position_dodge(width=0.9),
            colour = "black",
            vjust=-0.5)
bar_sex2
#1) 배우자월평균소득_없음
final_df$배우자월평균소득_없음<-as.factor(final_df$배우자월평균소득_없음)

bar_sex2<-ggplot(data=final_df,aes(x=조사연도,group=배우자월평균소득_없음,fill=배우자월평균소득_없음))+
  geom_bar(position = 'dodge' )  +
  labs(title="조사연도에 따른 배우자월평균소득_없음 분포")+
  scale_fill_discrete(labels=c("있음","없음"))+
  theme_classic()+
  geom_text(aes(label = after_stat(count),group=배우자월평균소득_없음), 
            stat = "count", 
            position=position_dodge(width=0.9),
            colour = "black",
            vjust=-0.5)
bar_sex2
#2) 배우자월평균소득_300미만
final_df$배우자월평균소득_300미만<-as.factor(final_df$배우자월평균소득_300미만)

bar_sex2<-ggplot(data=final_df,aes(x=조사연도,group=배우자월평균소득_300미만,fill=배우자월평균소득_300미만))+
  geom_bar(position = 'dodge' )  +
  labs(title="조사연도에 따른 배우자월평균소득_300미만 분포")+
  scale_fill_discrete(labels=c("0만원 초과 300만원미만","그외"))+
  theme_classic()+
  geom_text(aes(label = after_stat(count),group=배우자월평균소득_300미만), 
            stat = "count", 
            position=position_dodge(width=0.9),
            colour = "black",
            vjust=-0.5)
bar_sex2
#3) 최종학력
final_df$최종학력<-as.factor(final_df$최종학력)

bar_sex2<-ggplot(data=final_df,aes(x=조사연도,group=최종학력,fill=최종학력))+
  geom_bar(position = 'dodge' )  +
  labs(title="조사연도에 따른 최종학력 분포")+
  scale_fill_discrete(labels=c("학사","석사"))+
  theme_classic()+
  geom_text(aes(label = after_stat(count),group=최종학력), 
            stat = "count", 
            position=position_dodge(width=0.9),
            colour = "black",
            vjust=-0.5)
bar_sex2
#4) 아이양육자
final_df$아이양육자<-as.factor(final_df$아이양육자)

bar_sex2<-ggplot(data=final_df,aes(x=조사연도,group=아이양육자,fill=아이양육자))+
  geom_bar(position = 'dodge' )  +
  labs(title="조사연도에 따른 아이양육자 분포")+
  scale_fill_discrete(labels=c("부모","그외"))+
  theme_classic()+
  geom_text(aes(label = after_stat(count),group=아이양육자), 
            stat = "count", 
            position=position_dodge(width=0.9),
            colour = "black",
            vjust=-0.5)
bar_sex2

#6) 성별에 따른 취업자 분포
df_2016$성별<-as.factor(df_2016$성별)

bar_sex2<-ggplot(data=df_2016,aes(x=성별,group=취업유무,fill=취업유무))+
  geom_bar(position = 'dodge' )  +
  labs(title="2016년 기준 성별에 따른 취업 유무 분포")+
  scale_x_discrete(name ="성별", 
                   labels=c("남자","여자"))+
  scale_fill_discrete(labels=c('미취업', '취업'))+
  geom_text(aes(label = after_stat(count),group=취업유무), 
            stat = "count", 
            position=position_dodge(width=0.9),
            colour = "black",
            vjust=-0.5)
bar_sex2
#7) 아이양육자에 따른 취업자 분포
final_df$아이양육자<-as.factor(final_df$아이양육자)
final_df$취업유무<-as.factor(final_df$취업유무)

bar_sex2<-ggplot(data=final_df,aes(x=아이양육자,group=취업유무,fill=취업유무))+
  geom_bar(position = 'dodge' )  +
  labs(title="아이양육자에 따른 취업 유무 분포")+
  scale_x_discrete(name ="아이양육자", 
                   labels=c("부모만","그외"))+
  scale_fill_discrete(labels=c('미취업', '취업'))+
  geom_text(aes(label = after_stat(count),group=취업유무), 
            stat = "count", 
            position=position_dodge(width=0.9),
            colour = "black",
            vjust=-0.5)
bar_sex2
#8) 배우자월평균소득에 따른 취업자 분포
final_df$g115<-as.factor(final_df$g115)

bar_sex2<-ggplot(data=final_df,aes(x=g115,group=취업유무,fill=취업유무))+
  geom_bar(position = 'dodge' )  +
  labs(title="g115에 따른 취업 유무 분포")+
  scale_fill_discrete(labels=c('미취업', '취업'))+
  geom_text(aes(label = after_stat(count),group=취업유무), 
            stat = "count", 
            position=position_dodge(width=0.9),
            colour = "black",
            vjust=-0.5)
bar_sex2
#1. 시간별 취업자 분포 
table(final_df$취업유무, final_df$조사연도) #2016년 기준 취업자 212명
table(final_df[final_df$조사연도==2016,]$성별) #2016년 기준 남자 296명, 여자 354명 
#2. 성별에 따른 취업 여부
tab0<-table(final_df[final_df$성별==0,]$취업유무,final_df[final_df$성별==0,]$조사연도)
tab1<-table(final_df[final_df$성별==1,]$취업유무,final_df[final_df$성별==1,]$조사연도)

prop.table(tab0,2)
prop.table(tab1,2)
final_df$취업유무==5