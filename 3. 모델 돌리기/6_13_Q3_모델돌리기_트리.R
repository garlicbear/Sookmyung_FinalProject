#0. 2020년 자료만 가지고 tree 모형 적용하기. 
df_2016<-final_df[final_df$조사연도==2016,]
##변수값들 알아보기 쉽게 바꾸기. 
df_2016<-df_2016%>%
  mutate(취업유무=ifelse(취업유무==0,"미취업","취업"))%>%
  mutate(성별=ifelse(성별==0,"남자","여자"))%>%
  mutate(아이양육자=ifelse(아이양육자==0,"부모만","부모외에도"))
library(rpart)
library(rpart.plot)
tree <- rpart(취업유무 ~ 출생연도+성별 +아이양육자+배우자월평균소득_400미만, data=df_2016, method = "class") #최종학력 제외
rpart.plot(tree) 

#과적합 방지 위해 cross validation
printcp(tree)
plotcp(tree)

ptree<-prune(tree, cp= tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"])
rpart.plot(ptree)
