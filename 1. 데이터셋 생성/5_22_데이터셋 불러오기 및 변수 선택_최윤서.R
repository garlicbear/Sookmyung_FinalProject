library(dplyr)
#2015
##데이터셋 불러오기
setwd("C:/CYS/의생명통계학/기말과제")
load("C:/CYS/의생명통계학/기말과제/NSDUH_2015.RData")
df_1<-PUF2015_021518 
rm(PUF2015_021518)

##변수명 바꾸기
col<-colnames(df_1)
new_col<-toupper(col)
colnames(df_1)<-new_col
var<-c("QUESTID2","IRSEX",
       "HALLYRLST","IRFAMIN3","PRVHLTIN",
       "IMOTHER","IFATHER", "IRHHSIZ2", 
       "WRKDHRSWK2","WRKSTATWK2",
       "EDUSKIPMO","MOVSINPYR2", "EDUGRDNOW2",
       "AGE2",
       "YETCGJOB","YEPGDJOB","YOSEEDOC",
       "YEYFGTSW","YEYSTOLE",
       "HPDRGTALK","SNRLGSVC",
       "PRXYDATA",
       "YODPREV","YOWRHRS","YOLOSEV","YODSCEV")
df_2015<-df_1[,var]       
df_2015$YEAR<-2015 #year변수 생성

#2016
load("C:/CYS/의생명통계학/기말과제/NSDUH_2016.RData")
df_2<-PUF2016_022818
rm(PUF2016_022818)

##변수명 바꾸기(함수 생성)
select_var<-function(df,new_df,year){
  col<-colnames(df)
  new_col<-toupper(col)
  colnames(df)<-new_col
  var<-c("QUESTID2","IRSEX",
         "HALLYRLST","IRFAMIN3","PRVHLTIN",
         "IMOTHER","IFATHER", "IRHHSIZ2", 
         "WRKDHRSWK2","WRKSTATWK2",
         "EDUSKPMON","MOVSINPYR2", "EDUSCHGRD2",
         "AGE2",
         "YETCGJOB","YEPGDJOB","YOSEEDOC",
         "YEYFGTSW","YEYSTOLE",
         "HPDRGTALK","SNRLGSVC",
         "PRXYDATA",
         "YODPREV","YOWRHRS","YOLOSEV","YODSCEV")
  new_df<-df[,var]       
  new_df$YEAR<-year #year변수 생성
  return (new_df)
}
df_2016<-select_var(df_2,new_df,2016)
#2017
load("C:/CYS/의생명통계학/기말과제/NSDUH_2017.RData")
df_3<-PUF2017_100918
rm(PUF2017_100918)

df_2017<-select_var(df_3,new_df,2017)


#2018
load("C:/CYS/의생명통계학/기말과제/NSDUH_2018.RData")
df_4<-PUF2018_100819
rm(PUF2018_100819)
df_2018<-select_var(df_4,new_df,2018)

#2019
load("C:/CYS/의생명통계학/기말과제/NSDUH_2019.RData")
df_5<-PUF2019_100920
rm(PUF2019_100920)
df_2019<-select_var(df_5,new_df,2019)

#변수명 통일하기(2015년도 것만 바꿈. )
var<-c("QUESTID2","IRSEX",
       "HALLYRLST","IRFAMIN3","PRVHLTIN",
       "IMOTHER","IFATHER", "IRHHSIZ2", 
       "WRKDHRSWK2","WRKSTATWK2",
       "EDUSKPMON","MOVSINPYR2", "EDUSCHGRD2",
       "AGE2",
       "YETCGJOB","YEPGDJOB","YOSEEDOC",
       "YEYFGTSW","YEYSTOLE",
       "HPDRGTALK","SNRLGSVC",
       "PRXYDATA",
       "YODPREV","YOWRHRS","YOLOSEV","YODSCEV","YEAR")
colnames(df_2015)<-var
final_df<-rbind(df_2015,df_2016,df_2017,df_2018,df_2019)
write.csv(final_df,file="final_df.csv")
