library(dplyr)

df16 <- read.csv("C:/Users/kimch/Desktop/의생명통계/기말프로젝트/df16.csv")
df17 <- read.csv("C:/Users/kimch/Desktop/의생명통계/기말프로젝트/df17.csv")
df18 <- read.csv("C:/Users/kimch/Desktop/의생명통계/기말프로젝트/df18.csv")
df19 <- read.csv("C:/Users/kimch/Desktop/의생명통계/기말프로젝트/df19.csv")
df20 <- read.csv("C:/Users/kimch/Desktop/의생명통계/기말프로젝트/df20.csv")

# 2016
df16_ <- df16%>%mutate(workday_per_week=y10b222+y10b223) #주당 근로일수

df16_$y10b224[df16_$y10b224==1]<-1 # 그대로
df16_$y10b224[df16_$y10b224==2]<-12 # 월급*12개월
df16_$y10b224[df16_$y10b224==3]<-52 # 주급*52주

df16_<-df16_%>%mutate(
  money=ifelse(
    y10b224 %in% c(1,2,3), y10b224*y10b225,
    ifelse(
      y10b224==4, workday_per_week*y10b225*52, y10b22*workday_per_week*52
    )
  )
)

# 2017
df17_ <- df17%>%mutate(workday_per_week=y11b222+y11b223) #주당 근로일수

df17_$y11b224[df17_$y11b224==1]<-1
df17_$y11b224[df17_$y11b224==2]<-12
df17_$y11b224[df17_$y11b224==3]<-52

df17_<-df17_%>%mutate(
  money=ifelse(
    y11b224 %in% c(1,2,3), y11b224*y11b225,
    ifelse(
      y11b224==4, workday_per_week*y11b225*52, y11b22*workday_per_week*52
    )
  )
)

# 2018
df18_ <- df18%>%mutate(workday_per_week=y12b222+y12b223) #주당 근로일수

df18_$y12b224[df18_$y12b224==1]<-1
df18_$y12b224[df18_$y12b224==2]<-12
df18_$y12b224[df18_$y12b224==3]<-52

df18_<-df18_%>%mutate(
  money=ifelse(
    y12b224 %in% c(1,2,3), y12b224*y12b225,
    ifelse(
      y12b224==4, workday_per_week*y12b225*52, y12b22*workday_per_week*52
    )
  )
)

# 2019
df19_ <- df19%>%mutate(workday_per_week=y13b222+y13b223) #주당 근로일수

df19_$y13b224[df19_$y13b224==1]<-1
df19_$y13b224[df19_$y13b224==2]<-12
df19_$y13b224[df19_$y13b224==3]<-52

df19_<-df19_%>%mutate(
  money=ifelse(
    y13b224 %in% c(1,2,3), y13b224*y13b225,
    ifelse(
      y13b224==4, workday_per_week*y13b225*52, y13b22*workday_per_week*52
    )
  )
)

# 2020
df20_ <- df20%>%mutate(workday_per_week=y14b222+y14b223) #주당 근로일수

df20_$y14b224[df20_$y14b224==1]<-1
df20_$y14b224[df20_$y14b224==2]<-12
df20_$y14b224[df20_$y14b224==3]<-52

df20_<-df20_%>%mutate(
  money=ifelse(
    y14b224 %in% c(1,2,3), y14b224*y14b225,
    ifelse(
      y14b224==4, workday_per_week*y14b225*52, y14b22*workday_per_week*52
    )
  )
)


# 생성한 데이터프레임 저장
write.csv(df16_, file="C:/Users/kimch/Desktop/의생명통계/기말프로젝트/df16_.csv", row.names=FALSE)
write.csv(df17_, file="C:/Users/kimch/Desktop/의생명통계/기말프로젝트/df17_.csv", row.names=FALSE)
write.csv(df18_, file="C:/Users/kimch/Desktop/의생명통계/기말프로젝트/df18_.csv", row.names=FALSE)
write.csv(df19_, file="C:/Users/kimch/Desktop/의생명통계/기말프로젝트/df19_.csv", row.names=FALSE)
write.csv(df20_, file="C:/Users/kimch/Desktop/의생명통계/기말프로젝트/df20_.csv", row.names=FALSE)
