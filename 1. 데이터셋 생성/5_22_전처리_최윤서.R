library(dplyr)
setwd("C:/CYS/의생명통계학/기말과제")
df<-read.csv("final_df.csv",header=T)
df<-final_df
colnames(df)
#1. age가 청소년인 것만 추출
df1<-df[df$AGE2<=6,] #총 68263건

#2. Adolscent Depression>YETCGJOB,YEPGDJOB 합치기
##1,2,3,4를 제외한 값 삭제 
df2<-df1[(df1$YETCGJOB<5)&(df1$YEPGDJOB<5),] #총 64433건. (개수 별로 안 줆.)
##두 변수를 더하기
df2$goodjob<-df2$YETCGJOB+df2$YEPGDJOB

#3. IRSEX 결측치 확인
unique(df2$IRSEX) #삭제할 값 없음 

#4. HALLYRLST 결측치 처리
df4<-df2%>%filter(HALLYRLST %in% c(2013,2014,2015,2016,2017,2018,2019,9991))
#5. IRFAMIN3
unique(df4$IRFAMIN3) #삭제할 값 없음
#6. PRVHLTIN
df6<-df4%>%filter(PRVHLTIN %in% c(1,2))
#7. IMOTHER, IFATHER
df7<-df6%>%filter((IMOTHER %in% c(1,2))&(IFATHER %in% c(1,2)))
#8. IRHHSIZ2
unique(df7$IRHHSIZ2)#삭제할 값 없음

#9. WRKDHRSWK2
sort(unique(df7$WRKDHRSWK2)) #정상적인 값 중 최대치는 61. 999도 살려둠.  
df9<-df7%>%filter(WRKDHRSWK2 %in% c(1:62,999))
unique(df9$WRKDHRSWK2) #제대로 필터링됨.

#10. WRKSTATWK2
sort(unique(df9$WRKSTATWK2)) 
df10<-df9
df10$WRKSTATWK2_new<-replace(df9$WRKSTATWK2, df9$WRKSTATWK2>=3,0)
unique(df10$WRKSTATWK2_new) #완료. 

#11. EDUSKPMON
sort(unique(df10$EDUSKPMON)) #don't know까지는...남겨둬야하지 않을지...일단은 삭제. 
df11<-df10%>%filter(EDUSKPMON %in% c(0:31,99)) 
sort(unique(df10$EDUSKPMON)) 

#12. MOVSINPYR2
df12<-df11%>%filter(MOVSINPYR2 %in% c(0,1,2,3)) 
sort(unique(df12$MOVSINPYR2))  #완료

#13. EDUSCHGRD2-->삭제해야할 것. 왜 있지?? 
#13. YOSEEDOC
df13<-df12%>%filter(YOSEEDOC %in% c(1,2,99)) 
unique(df13$YOSEEDOC)

#14. YEYFGTSW, YEYSTOLE
df14<-df13%>%filter((YEYFGTSW %in% c(1,2,3,4,5))&(YEYSTOLE %in% c(1,2,3,4,5))) #l.s 제외해도 ok. 
unique(df14$YEYFGTSW)
unique(df14$YEYSTOLE)
#15. HPDRGTALK 
df15<-df14%>%filter(HPDRGTALK %in% c(1,2,91,93))#l.s 제외해도 ok. 
unique(df15$HPDRGTALK)
#16. SNRLGSVC
#df16<-df15%>%filter(SNRLGSVC %in% c(1,2,3,4,5,6))#값이 없다...없애야 할듯 흑흑. 
#unique(df15$SNRLGSVC)
#17. PRXYDATA
df17<-df15%>%filter(PRXYDATA %in% c(2))
table(df15$PRXYDATA) #무시..할까..? 아니면 yes만 남길까?

write.csv(df17,"final_df2.csv")
