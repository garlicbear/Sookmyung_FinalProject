# Q1: 취업 log rank

## 2016년
# year, 취업, 결혼, 출산 컬럼만
library(readxl)
q1_16 <- read_excel("C:/Users/kimch/Desktop/원표본/ypdata_w10.xlsx")
q1_16 <- q1_16[(q1_16$w10edu==4)|(q1_16$w10edu==5),]

# id, 결혼, 출산(자녀), 취업여부
col <- c("sampid", "y10g101", "y10g500", "w10type")
q1_16 <- q1_16[,col]

# 결측치 처리 -> 다 개수가 같음
colSums(is.na(q1_16))
q1_16 <- q1_16[!is.na(q1_16$w10type),]
# 결측치 0개 됨
colSums(is.na(q1_16))

# 우리의 표본 id
id <- unique(q1_16$sampid) 

## 2017년
q1_17 <- read_excel("C:/Users/kimch/Desktop/원표본/ypdata_w11.xlsx")
# id, 결혼, 출산(자녀), 취업여부
col <- c("sampid", "y11g101", "y11g500", "w11type")
q1_17 <- subset(q1_17[id,], select=col)

# 결측치 처리 -> 다 개수가 같음
colSums(is.na(q1_17))
q1_17 <- q1_17[!is.na(q1_17$w11type),]
# 결측치 0개 됨
colSums(is.na(q1_17))


## 2018년
q1_18 <- read_excel("C:/Users/kimch/Desktop/원표본/ypdata_w12.xlsx")
# id, 결혼, 출산(자녀), 취업여부
col <- c("sampid", "y12g101", "y12g500", "w12type")
q1_18 <- subset(q1_18[id,], select=col)

# 결측치 처리 -> 다 개수가 같음
colSums(is.na(q1_18))
q1_18 <- q1_18[!is.na(q1_18$w12type),]
# 결측치 0개 됨
colSums(is.na(q1_18))


## 2019년
q1_19 <- read_excel("C:/Users/kimch/Desktop/원표본/ypdata_w13.xlsx")
# id, 결혼, 출산(자녀), 취업여부
col <- c("sampid", "y13g101", "y13g500", "w13type")
q1_19 <- subset(q1_19[id,], select=col)

# 결측치 처리 -> 다 개수가 같음
colSums(is.na(q1_19))
q1_19 <- q1_19[!is.na(q1_19$w13type),]
# 결측치 0개 됨
colSums(is.na(q1_19))


## 2020년
q1_20 <- read_excel("C:/Users/kimch/Desktop/원표본/ypdata_w14.xlsx")
# id, 결혼, 출산(자녀), 취업여부
col <- c("sampid", "y14g101", "y14g500", "w14type")
q1_20 <- subset(q1_20[id,], select=col)

# 결측치 처리 -> 다 개수가 같음
colSums(is.na(q1_20))
q1_20 <- q1_20[!is.na(q1_20$w14type),]
# 결측치 0개 됨
colSums(is.na(q1_20))


## 모름, 응답거절 처리
library(dplyr)
names(q1_16)
q1_16<-q1_16%>%filter_at(vars(c(sampid, y10g101, y10g500, w10type)),all_vars(. <9090908))
q1_17<-q1_17%>%filter_at(vars(c(sampid, y11g101, y11g500, w11type)),all_vars(. <9090908))
q1_18<-q1_18%>%filter_at(vars(c(sampid, y12g101, y12g500, w12type)),all_vars(. <9090908))
q1_19<-q1_19%>%filter_at(vars(c(sampid, y13g101, y13g500, w13type)),all_vars(. <9090908))
q1_20<-q1_20%>%filter_at(vars(c(sampid, y14g101, y14g500, w14type)),all_vars(. <9090908))


# 컬럼명 바꾸기
names(q1_16) <- c("id", "결혼", "출산", "취업")
names(q1_17) <- c("id", "결혼", "출산", "취업")
names(q1_18) <- c("id", "결혼", "출산", "취업")
names(q1_19) <- c("id", "결혼", "출산", "취업")
names(q1_20) <- c("id", "결혼", "출산", "취업")


library(dplyr)
q1_16 <- q1_16%>%mutate(year=2016)
q1_17 <- q1_17%>%mutate(year=2017)
q1_18 <- q1_18%>%mutate(year=2018)
q1_19 <- q1_19%>%mutate(year=2019)
q1_20 <- q1_20%>%mutate(year=2020)


# 생성한 데이터프레임 저장
write.csv(q1_16, file="C:/Users/kimch/Desktop/의생명통계/기말프로젝트/q1_16.csv", row.names=FALSE)
write.csv(q1_17, file="C:/Users/kimch/Desktop/의생명통계/기말프로젝트/q1_17.csv", row.names=FALSE)
write.csv(q1_18, file="C:/Users/kimch/Desktop/의생명통계/기말프로젝트/q1_18.csv", row.names=FALSE)
write.csv(q1_19, file="C:/Users/kimch/Desktop/의생명통계/기말프로젝트/q1_19.csv", row.names=FALSE)
write.csv(q1_20, file="C:/Users/kimch/Desktop/의생명통계/기말프로젝트/q1_20.csv", row.names=FALSE)


# combine dataset for log rank test
total_q1 <- rbind(q1_16, q1_17, q1_18, q1_19, q1_20)
library(plyr)
total_q1 <- arrange(total_q1, id, year)
write.csv(total_q1, file="C:/Users/kimch/Desktop/의생명통계/기말프로젝트/log_rank_df", row.names=FALSE)


# log rank dataset 참고
library(carData)
recid <- data.frame(Rossi)
