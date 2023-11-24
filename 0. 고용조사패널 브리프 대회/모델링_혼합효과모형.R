setwd("C:/CYS/의생명통계학/기말과제_청년고용조사")
final_df<-read.csv("final_df_0613_1.csv")
final_df[1:10,]
final_df[(final_df$배우자월평균소득_300미만==1)&(final_df$배우자월평균소득_없음==0),]
table(final_df[(final_df$배우자월평균소득_400미만==0),]$g115)
#table(final_df[(final_df$배우자월평균소득_300미만==1)&(final_df$배우자월평균소득_없음==0),]$g115)
#table(final_df[(final_df$배우자월평균소득_300미만==0)&(final_df$배우자월평균소득_없음==0),]$g115)
#변수명 바꾸기
#library(data.table)

#setnames(final_df,"배우자월평균소득_300미만","배우자월평균소득_0초과400미만")

#1. Mixed effect model
#1) 랜덤절편
library(lme4)
f1<-formula(취업유무~출생연도+성별 +아이양육자+배우자월평균소득_400미만+최종학력+조사연도+(1|id))
fit1=glmer(f1,data=final_df,family=binomial)
summary(fit1)

#2) 랜덤절편+기울기
f2<-formula(취업유무~출생연도+성별 +아이양육자+배우자월평균소득_400미만+최종학력+조사연도+(조사연도|id))
fit2=glmer(f2,data=final_df,family=binomial)
summary(fit2)
#3) 랜덤절편/교호작용
f3<-formula(취업유무~출생연도+성별 +아이양육자+배우자월평균소득_400미만+최종학력+조사연도+성별*아이양육자+(1|id))
fit3=glmer(f3,data=final_df,family=binomial)
summary(fit3)

#4) 랜덤절편+랜덤기울기/교호작용
f4<-formula(취업유무~출생연도+성별 +아이양육자+배우자월평균소득_400미만+최종학력+조사연도+성별*아이양육자+(조사연도|id))
fit4=glmer(f4,data=final_df,family=binomial)
summary(fit4)

#5) 랜덤절편/교호작용
f5<-formula(취업유무~출생연도+성별 +아이양육자+배우자월평균소득_400미만+최종학력+조사연도+성별*배우자월평균소득_400미만+(1|id))
fit5=glmer(f5,data=final_df,family=binomial)
summary(fit5)

#6) 랜덤절편+랜덤기울기/교호작용
f6<-formula(취업유무~출생연도+성별 +아이양육자+배우자월평균소득_400미만+최종학력+조사연도+성별*배우자월평균소득_400미만+(조사연도|id))
fit6=glmer(f6,data=final_df,family=binomial)
summary(fit6)


#7) 최종 모형: 
f7<-formula(취업유무~성별 +아이양육자+배우자월평균소득_400미만+조사연도+(1|id))
fit7=glmer(f7,data=final_df,family=binomial)
summary(fit7)


table(final_df2$성별,final_df2$배우자월평균소득)

library(glmnet)
